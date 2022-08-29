------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Ordered_Sets;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Numerics.Float_Random;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;

with GNAT.MD5;
with GNAT.OS_Lib;
with GNAT.Sockets;

with GPR2.Compilation.Process;
with GPR2.Compilation.Slave.List;
with GPR2.Compilation.Sync;
with GPR2.Message;
with GPR2.Project.Registry.Attribute;
with GPR2.Source_Reference;

with GPRtools.Util;

package body GPR2.Compilation.Registry is

   package PRA renames GPR2.Project.Registry.Attribute;

   use Ada.Exceptions;

   use GNAT;

   Slaves_Data : Slave.List.Object;

   type Slave_Data is record
      Sock          : Integer;
      Data          : Slave.Object;
      Channel       : Compilation.Protocol.Communication_Channel;
      Current       : Natural := 0;
      Max_Processes : Positive := 1;
      Root_Dir      : Unbounded_String;
      Rsync_Pid     : GNAT.OS_Lib.Process_Id;
   end record;

   function "<" (K1, K2 : Slave_Data) return Boolean is (K1.Sock < K2.Sock);

   overriding function "="
     (K1, K2 : Slave_Data) return Boolean is (K1.Sock = K2.Sock);

   Undefined : constant Slave_Data :=
                 (-1, Slave.Undefined, Current => Natural'Last, others => <>);

   package Slave_S is new Ada.Containers.Ordered_Sets (Slave_Data);
   --  The key is the C socket number

   function Connect
     (Tree                       : not null access GPR2.Project.Tree.Object;
      S_Data                     : Slave.Object;
      Project_Name               : Name_Type;
      Sync                       : Boolean;
      Options                    : GPRtools.Options.Base_Options'Class;
      Sloc                       : Source_Reference.Object'Class;
      Included_Artifact_Patterns : String := "") return Slave_Data;
   --  Connect to the slave and return the corresponding object

   function Parse (Host_Name : Name_Type) return Slave.Object;
   --  Parse a host[:port] string and returns corresponding Slave_Data record

   procedure Register_Remote_Slave
     (Tree                       : not null access Project.Tree.Object;
      S_Data                     : Slave.Object;
      Project_Name               : Name_Type;
      Excluded_Patterns          : Containers.Value_List;
      Included_Patterns          : Containers.Value_List;
      Included_Artifact_Patterns : Containers.Value_List;
      Synchronize                : Boolean;
      Options                    : GPRtools.Options.Base_Options'Class);
   --  Register a slave living on Host for the given project name. User is
   --  used when calling rsync, it is the remote machine user name, if empty
   --  the local user name is used.

   procedure Start_Waiting_Task;

   --  Ack transient signal stored into this variable

   protected Wait_Ack is
      procedure Set (Pid : Remote_Id);
      entry Get (Pid : out Remote_Id);
   private
      Is_Set : Boolean := False;
      Id     : Remote_Id;
   end Wait_Ack;

   task type Wait_Remote;
   --  Wait for incoming data from all registered slaves

   type Wait_Remote_Ref is access Wait_Remote;
   WR : Wait_Remote_Ref;
   --  Will be initialized only if the distributed mode is activated

   Compiler_Path : constant GNAT.OS_Lib.String_Access :=
                     GNAT.OS_Lib.Locate_Exec_On_Path ("gnatls");

   Remote_Process : Shared_Counter;
   Slaves_Sockets : Sockets.Socket_Set_Type;
   Max_Processes  : Natural := 0;
   Root_Dir       : Unbounded_String;

   R_Gen          : Numerics.Float_Random.Generator;

   protected Slaves is

      procedure Insert (S : Slave_Data);
      --  Add a slave into the pool

      function Find (Socket : Integer) return Slave_Data;
      --  Find a slave given the socket number

      function Find (Host : Name_Type) return Slave_Data;
      --  Find a slave give a host[:port] name

      function Get_Free return Slave_Data;
      --  Returns a slave with free compilation slot

      function Count return Natural;
      --  Returns the number of registered slaves

      procedure Increment_Current (S : in out Slave_Data);
      --  Increment the number of processes handled by slave

      procedure Decrement_Current (S : in out Slave_Data);
      --  Decrement the number of processes handled by slave

      procedure Set_Rewrite_CD (S : in out Slave_Data; Path : String);
      --  Record rewriting of the compiler directory

      procedure Set_Rewrite_WD (S : in out Slave_Data; Path : String);
      --  Record rewriting of the wording directory

      procedure Iterate (Proc : access procedure (S : in out Slave_Data));
      --  Iterate over all slaves in the pool and call proc

      procedure Clear;
      --  Clear the pool

   private
      Pool : Slave_S.Set;
   end Slaves;

   -------------
   -- Channel --
   -------------

   function Channel (Host : Name_Type) return Protocol.Communication_Channel is
      S : constant Slave_Data := Slaves.Find (Host);
   begin
      if S = Undefined then
         return Protocol.No_Channel;
      else
         return S.Channel;
      end if;
   end Channel;

   ----------------------------
   -- Clean_Up_Remote_Slaves --
   ----------------------------

   procedure Clean_Up_Remote_Slaves
     (Project : GPR2.Project.View.Object;
      Options : GPRtools.Options.Base_Options'Class)
   is
      Tree : constant not null access GPR2.Project.Tree.Object := Project.Tree;

      Project_Name : constant Name_Type := Project.Path_Name.Base_Name;
      Sloc         : constant Source_Reference.Object'Class :=
                       Source_Reference.Create (Project.Path_Name.Value, 0, 0);

      procedure Clean_Up_Remote_Slave (S_Data : Slave.Object);
      --  Clean-up slave

      ---------------------------
      -- Clean_Up_Remote_Slave --
      ---------------------------

      procedure Clean_Up_Remote_Slave (S_Data : Slave.Object) is
         use all type Compilation.Protocol.Command_Kind;

         S : Slave_Data;

      begin
         S := Connect
           (Tree, S_Data, Project_Name,
            Options => Options, Sloc => Sloc, Sync => False);

         if not Tree.Log_Messages.Has_Error then
            --  Send the clean-up request

            Protocol.Send_Clean_Up (S.Channel, String (Project_Name));

            declare
               Cmd : constant Compilation.Protocol.Command :=
                       Compilation.Protocol.Get_Command (S.Channel);
            begin
               if Compilation.Protocol.Kind (Cmd) = OK then
                  if Options.Verbose then
                     Tree.Append_Message
                       (Message.Create
                          (Message.Information,
                           "Clean-up done on " & String (S_Data.Host),
                           Sloc => Sloc));
                  end if;

               elsif Compilation.Protocol.Kind (Cmd) = KO then
                  Tree.Append_Message
                    (Message.Create
                       (Message.Error,
                        "Slave cannot clean-up " & String (S_Data.Host),
                        Sloc => Sloc));

               else
                  Tree.Append_Message
                    (Message.Create
                       (Message.Error,
                        "protocol error: "
                        & Compilation.Protocol.Command_Kind'Image (Kind (Cmd)),
                        Sloc => Sloc));
               end if;
            end;

            Protocol.Send_End_Of_Compilation (S.Channel);

            --  Wait for acknowledge to ensure the clean-up is terminated on
            --  the slave.

            declare
               Cmd : constant Compilation.Protocol.Command :=
                       Compilation.Protocol.Get_Command (S.Channel)
                       with Unreferenced;
            begin
               null;
            end;

            Compilation.Protocol.Close (S.Channel);
         end if;

      exception
         when others =>
            Compilation.Protocol.Close (S.Channel);
            raise;
      end Clean_Up_Remote_Slave;

   begin
      for S of Slaves_Data loop
         Clean_Up_Remote_Slave (S);
      end loop;

      if Tree.Log_Messages.Has_Error then
         raise Processing_Error with "remote compilation error";
      end if;
   end Clean_Up_Remote_Slaves;

   -----------------
   -- Compute_Env --
   -----------------

   function Compute_Env
     (Tree : GPR2.Project.Tree.Object; Auto : Boolean) return String
   is
      use Ada.Command_Line;
      use GNAT.MD5;

      use type GNAT.OS_Lib.String_Access;
      use all type GPR2.Project.Registry.Attribute.Value_Kind;

      User      : OS_Lib.String_Access := OS_Lib.Getenv ("USER");
      User_Name : OS_Lib.String_Access := OS_Lib.Getenv ("USERNAME");
      Default   : constant String :=
                    (if User = null
                     then (if User_Name = null
                       then "unknown" else User_Name.all)
                     else User.all)
                    & '@' & GNAT.Sockets.Host_Name;

      package S_Set is new Ada.Containers.Indefinite_Ordered_Sets (String);

      Set : S_Set.Set;
      Ctx : Context;

   begin
      OS_Lib.Free (User);
      OS_Lib.Free (User_Name);

      if Auto then
         --  In this mode the slave environment is computed based on
         --  the project variable value and the command line arguments.

         --  First adds all command line arguments

         for K in 1 .. Argument_Count loop
            --  Skip arguments that are not changing the actual compilation and
            --  this will ensure that the same environment will be created for
            --  gprclean.

            if Argument (K) not in "-p" | "-d" | "-c" | "-q"
              and then
                (Argument (K)'Length < 2
                 or else Argument (K) (1 .. 2) /= "-j")
            then
               Set.Insert (Argument (K));
            end if;
         end loop;

         --  Then all the global variables for the project tree

         for Project of Tree loop
            if Project.Has_Variables then
               for V of Project.Variables loop
                  if V.Kind = Single then
                     Set.Include
                       (String (V.Name.Text) & "=" & String (V.Value.Text));
                  end if;
               end loop;
            end if;
         end loop;

         --  Compute the MD5 sum of the sorted elements in the set

         for S of Set loop
            Update (Ctx, S);
         end loop;

         return Default & "-" & Digest (Ctx);

      else
         --  Otherwise use the default <user_name> & '@' & <host_name>
         return Default;
      end if;
   end Compute_Env;

   -------------
   -- Connect --
   -------------

   function Connect
     (Tree                       : not null access GPR2.Project.Tree.Object;
      S_Data                     : Slave.Object;
      Project_Name               : Name_Type;
      Sync                       : Boolean;
      Options                    : GPRtools.Options.Base_Options'Class;
      Sloc                       : Source_Reference.Object'Class;
      Included_Artifact_Patterns : String := "") return Slave_Data
   is
      use GNAT.Sockets;

      Address : Sock_Addr_Type;
      Sock    : Socket_Type;
      S       : Slave_Data := Undefined;
      Status  : Selector_Status;

   begin
      S.Data := S_Data;

      Address.Addr := Addresses (Get_Host_By_Name (String (S.Data.Host)), 1);
      Address.Port := S_Data.Port;

      Create_Socket (Sock);
      Set_Socket_Option (Sock, Socket_Level, (Reuse_Address, True));

      begin
         Connect_Socket (Sock, Address, Timeout => 2.0, Status => Status);
      exception
         when Socket_Error =>
            Tree.Append_Message
              (Message.Create
                 (Message.Error,
                  "Cannot connect to slave "
                  & String (S_Data.Host) & ", aborting",
                  Sloc => Sloc));
            return S;
      end;

      if Status in Expired .. Aborted then
         Tree.Append_Message
           (Message.Create
              (Message.Error,
               "Cannot connect to slave "
               & String (S_Data.Host) & ", aborting",
               Sloc => Sloc));
         return S;
      end if;

      S.Channel := Compilation.Protocol.Create (Sock);

      --  Do initial handshake

      Compilation.Protocol.Send_Context
        (Channel                    => S.Channel,
         Target                     => String (Tree.Target),
         Project_Name               => String (Project_Name),
         Build_Env                  => To_String (Options.Slave_Env),
         Sync                       => Sync,
         Hash                       => To_String (Options.Hash_Value),
         Included_Artifact_Patterns => Included_Artifact_Patterns);

      declare
         use all type Compilation.Protocol.Command_Kind;

         Cmd        : constant Compilation.Protocol.Command :=
                        Compilation.Protocol.Get_Command (S.Channel);
         Parameters : constant OS_Lib.Argument_List_Access :=
                        Compilation.Protocol.Args (Cmd);
      begin
         if Kind (Cmd) = OK and then Parameters'Length = 3 then
            S.Max_Processes := Natural'Value (Parameters (1).all);
            S.Root_Dir := To_Unbounded_String (Parameters (2).all);

            if not Boolean'Value (Parameters (3).all) then
               Tree.Append_Message
                 (Message.Create
                    (Message.Warning,
                     "non synchronized clock detected for "
                     & String (S.Data.Host),
                     Sloc => Sloc));
            end if;

         elsif Kind (Cmd) = KO then
            Tree.Append_Message
              (Message.Create
                 (Message.Error,
                  (if Parameters'Length = 1 and then Parameters (1).all /= ""
                   then Parameters (1).all
                   else "build slave is not compatible")
                  & " : " & String (S_Data.Host),
                  Sloc => Sloc));
            return S;

         else
            Tree.Append_Message
              (Message.Create
                 (Message.Error,
                  "protocol error: "
                  & Protocol.Command_Kind'Image (Kind (Cmd)),
                  Sloc => Sloc));
            return S;
         end if;
      end;

      return S;
   end Connect;

   ---------------
   -- Get_Hosts --
   ---------------

   function Get_Hosts return Containers.Name_List is
      Hosts : Containers.Name_List;
   begin
      if Environment_Variables.Exists ("GPR_SLAVES")
        and then Environment_Variables.Value ("GPR_SLAVES") /= ""
      then
         Hosts := Containers.Create
           (Name_Type (Environment_Variables.Value ("GPR_SLAVES")),
            Separator => ",");

      elsif Environment_Variables.Exists ("GPR_SLAVES_FILE") then
         declare
            F_Name : constant String :=
                       Environment_Variables.Value ("GPR_SLAVES_FILE");
            F      : Text_IO.File_Type;
            Buffer : String (1 .. 100);
            Last   : Natural;
         begin
            if Directories.Exists (F_Name) then
               Text_IO.Open (F, Text_IO.In_File, F_Name);

               while not Text_IO.End_Of_File (F) loop
                  Text_IO.Get_Line (F, Buffer, Last);

                  if Last > 0 then
                     Hosts.Append (Name_Type (Buffer (1 .. Last)));
                  end if;
               end loop;

               Text_IO.Close (F);

            else
               GPRtools.Util.Fail_Program
                 ("hosts distributed file " & F_Name & " not found");
            end if;
         end;
      end if;

      return Hosts;
   end Get_Hosts;

   -----------------------
   -- Get_Max_Processes --
   -----------------------

   function Get_Max_Processes return Natural is
   begin
      return Max_Processes;
   end Get_Max_Processes;

   -----------
   -- Parse --
   -----------

   function Parse (Host_Name : Name_Type) return Slave.Object is
      V    : constant String := String (Host_Name);
      I    : constant Natural := Strings.Fixed.Index (V, ":");
      Host : Unbounded_String;
      Port : Sockets.Port_Type := Sockets.Port_Type (Default_Port);
   begin
      --  Get for port

      if I = 0 then
         Host := To_Unbounded_String (V (V'First .. V'Last));

      else
         Host := To_Unbounded_String (V (V'First .. I - 1));

         declare
            Port_Str : constant String := V (I + 1 .. V'Last);
         begin
            if Strings.Maps.Is_Subset
              (Strings.Maps.To_Set (Port_Str),
               Strings.Maps.Constants.Decimal_Digit_Set)
            then
               Port := Sockets.Port_Type'Value (V (I + 1 .. V'Last));
            else
               return Slave.Undefined;
            end if;
         end;
      end if;

      return Slave.Create (Name_Type (To_String (Host)), Port);
   end Parse;

   -------------------
   -- Record_Slaves --
   -------------------

   procedure Record_Slaves (Slaves : Containers.Name_List) is

      procedure Parse_Build_Slave (V : Name_Type);
      --  Parse the build slave V

      -----------------------
      -- Parse_Build_Slave --
      -----------------------

      procedure Parse_Build_Slave (V : Name_Type) is
         use type Slave.Object;

         S_Data : constant Slave.Object := Parse (V);
      begin
         if S_Data = Slave.Undefined then
            raise Constraint_Error
              with "error: invalid port value in " & String (V);
         else
            Slaves_Data.Append (S_Data);
         end if;
      end Parse_Build_Slave;

   begin
      for S of Slaves loop
         Parse_Build_Slave (S);
      end loop;
   end Record_Slaves;

   ---------------------------
   -- Register_Remote_Slave --
   ---------------------------

   procedure Register_Remote_Slave
     (Tree                       : not null access Project.Tree.Object;
      S_Data                     : Slave.Object;
      Project_Name               : Name_Type;
      Excluded_Patterns          : Containers.Value_List;
      Included_Patterns          : Containers.Value_List;
      Included_Artifact_Patterns : Containers.Value_List;
      Synchronize                : Boolean;
      Options                    : GPRtools.Options.Base_Options'Class)
   is
      S    : Slave_Data;
      IAP  : Unbounded_String;
      Sloc : constant Source_Reference.Object'Class :=
               Source_Reference.Create
                 (Tree.Root_Project.Path_Name.Value, 0, 0);
   begin
      for P of Included_Artifact_Patterns loop
         if IAP /= Null_Unbounded_String then
            Append (IAP, ";");
         end if;
         Append (IAP, P);
      end loop;

      S := Connect
        (Tree,
         S_Data,
         Project_Name,
         Sync                       => Synchronize,
         Options                    => Options,
         Sloc                       => Sloc,
         Included_Artifact_Patterns => To_String (IAP));

      Sockets.Set (Slaves_Sockets, Protocol.Sock (S.Channel));

      --  Sum the Max_Process values

      Max_Processes := Max_Processes + S.Max_Processes;

      if Options.Verbose then
         Tree.Append_Message
           (Message.Create
              (Message.Information,
               "Register slave " & String (S_Data.Host)
               & "," & Integer'Image (S.Max_Processes)
               & " process(es)",
               Sloc => Sloc));

         Tree.Append_Message
           (Message.Create
              (Message.Information,
               "  location: " & To_String (S.Root_Dir),
               Sloc => Sloc));
      end if;

      --  Let's double check that Root_Dir and Projet_Name are not empty,
      --  this is a safety check to avoid rsync destroying remote environment
      --  as rsync is using the --delete options.

      if Length (S.Root_Dir) = 0 then
         Tree.Append_Message
           (Message.Create
              (Message.Error,
               "error: Root_Dir cannot be empty",
               Sloc => Sloc));
         raise Processing_Error;
      end if;

      if Synchronize then
         Compilation.Sync.Send_Files
           (Channel           => S.Channel,
            Root_Dir          => To_String (Root_Dir),
            Included_Patterns => Included_Patterns,
            Excluded_Patterns => Excluded_Patterns,
            Mode              => Sync.To_Slave);
      end if;

      --  Now that all slave's data is known and set, record it

      S.Sock := Sockets.To_C (Protocol.Sock (S.Channel));

      Slaves.Insert (S);

   exception
      when Sockets.Host_Error =>
         Tree.Append_Message
           (Message.Create
              (Message.Error,
                "cannot connect to " & String (S_Data.Host),
               Sloc => Sloc));
         raise Processing_Error;
   end Register_Remote_Slave;

   ----------------------------
   -- Register_Remote_Slaves --
   ----------------------------

   procedure Register_Remote_Slaves
     (Tree        : GPR2.Project.Tree.Object;
      Options     : GPRtools.Options.Base_Options'Class;
      Synchronize : Boolean)
   is
      use Ada.Directories;
      use GNAT.OS_Lib;

      use type GPRtools.Verbosity_Level;
      use type Calendar.Time;
      use type Containers.Count_Type;

      Start, Stop : Calendar.Time;

      procedure Insert
        (List   : out Containers.Value_List;
         Values : GPR2.Containers.Source_Value_List);
      --  Inserts all values into the vector

      Excluded_Patterns          : Containers.Value_List;
      Included_Patterns          : Containers.Value_List;
      Included_Artifact_Patterns : Containers.Value_List;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (List   : out Containers.Value_List;
         Values : GPR2.Containers.Source_Value_List) is
      begin
         for V of Values loop
            List.Append (V.Text);
         end loop;
      end Insert;

      Project  : constant GPR2.Project.View.Object :=
                   Tree.Root_Project;

   begin
      Root_Dir := To_Unbounded_String (Remote_Root_Directory (Project));

      --  Check for Root_Dir attribute and Excluded_Patterns

      if Project.Has_Attribute (PRA.Remote.Excluded_Patterns)
      then
         Insert
           (Excluded_Patterns,
            Project.Attribute (PRA.Remote.Excluded_Patterns).Values);

      elsif Project.Has_Attribute (PRA.Remote.Included_Patterns)
      then
         Insert
           (Included_Patterns,
            Project.Attribute (PRA.Remote.Included_Patterns).Values);

      elsif Project.Has_Attribute (PRA.Remote.Included_Artifact_Patterns)
      then
         Insert
           (Included_Artifact_Patterns,
            Project.Attribute (PRA.Remote.Included_Artifact_Patterns).Values);
      end if;

      if not Exists (To_String (Root_Dir))
        or else not Is_Directory (To_String (Root_Dir))
      then
         Tree.Root_Project.Tree.Append_Message
           (Message.Create
              (Message.Error,
               To_String (Root_Dir) & " is not a directory or does not exist",
               Sloc => Source_Reference.Object'Class
                         (Source_Reference.Undefined)));
         raise Constraint_Error;

      else
         Tree.Root_Project.Tree.Append_Message
           (Message.Create
              (Message.Information,
               "root dir : " & To_String (Root_Dir),
               Sloc => Source_Reference.Object'Class
                         (Source_Reference.Undefined)));
      end if;

      --  Check if Excluded_Patterns and Included_Patterns are set

      if Included_Patterns.Length /= 0
        and then Excluded_Patterns.Length /= 0
      then
         Tree.Root_Project.Tree.Append_Message
           (Message.Create
              (Message.Error,
               "Excluded_Patterns and Included_Patterns are exclusive",
               Sloc => Source_Reference.Object'Class
                         (Source_Reference.Undefined)));
         raise Constraint_Error;
      end if;

      --  Then registers the build slaves

      Start := Calendar.Clock;

      for S of Slaves_Data loop
         Register_Remote_Slave
           (Tree.Root_Project.Tree,
            S,
            Project.Name,
            Excluded_Patterns,
            Included_Patterns,
            Included_Artifact_Patterns,
            Synchronize,
            Options);
      end loop;

      if Synchronize then
         Sync.Wait;
      end if;

      Stop := Calendar.Clock;

      if Synchronize and then Options.Verbosity > GPRtools.Quiet then
         Tree.Root_Project.Tree.Append_Message
           (Message.Create
              (Message.Information,
               "All data synchronized in "
               & Duration'Image (Stop - Start) & " seconds",
               Sloc => Source_Reference.Object'Class
                         (Source_Reference.Undefined)));
      end if;

      --  We are in remote mode, the initialization was successful, start tasks
      --  now.

      Start_Waiting_Task;
   end Register_Remote_Slaves;

   ---------------------------
   -- Remote_Root_Directory --
   ---------------------------

   function Remote_Root_Directory
     (Project : GPR2.Project.View.Object) return String
   is
      use GNAT.OS_Lib;

      Root_Dir : constant String := Project.Dir_Name.Value;
   begin
      if Project.Has_Attribute (PRA.Remote.Root_Dir)
      then
         declare
            RD : constant String :=
                   Project.Attribute (PRA.Remote.Root_Dir).Value.Text;
         begin
            if Is_Absolute_Path (RD) then
               return RD;
            else
               return Normalize_Pathname
                 (Root_Dir & Directory_Separator & RD);
            end if;
         end;
      end if;

      return Root_Dir;
   end Remote_Root_Directory;

   ---------
   -- Run --
   ---------

   function Run
     (Project  : GPR2.Project.View.Object;
      Language : Language_Id;
      Options  : Containers.Value_List;
      Obj_Name : Name_Type;
      Dep_Name : String := "";
      Env      : String := "") return Compilation.Id
   is
      use type GNAT.OS_Lib.String_Access;

      Tree : constant not null access GPR2.Project.Tree.Object := Project.Tree;

      CWD : constant String := Directories.Current_Directory;
      --  CWD is the directory from which the command is run

      RD  : constant String := To_String (Root_Dir);

      S   : Slave_Data := Slaves.Get_Free;
      --  Get a free slave for conducting the compilation

      function Filter_String
        (O : String; Sep : String := Protocol.WD_Path_Tag) return String;
      --  Make O PATH relative to RD. For option -gnatec and -gnatem makes
      --  the specified filename absolute in the slave environment and send
      --  the file to the slave.

      -------------------
      -- Filter_String --
      -------------------

      function Filter_String
        (O   : String;
         Sep : String := Protocol.WD_Path_Tag) return String
      is
         Pos : constant Natural := Strings.Fixed.Index (O, RD);
      begin
         if Pos = 0 then
            return O;

         else
            --  Note that we transfer files only when they are under the
            --  project root.

            if O'Length > 8
              and then O (O'First .. O'First + 7) in "-gnatem=" | "-gnatec="
            then
               --  Send the corresponding file to the slave
               declare
                  File_Name : constant String := O (O'First + 8 .. O'Last);
               begin
                  if Directories.Exists (File_Name) then
                     Protocol.Send_File
                       (S.Channel, File_Name,
                        Rewrite         => True,
                        Keep_Time_Stamp => True);

                  else
                     Tree.Append_Message
                       (Message.Create
                          (Message.Error,
                           "File not found " & File_Name,
                           Sloc => Source_Reference.Object'Class
                                     (Source_Reference.Undefined)));

                     Tree.Append_Message
                       (Message.Create
                          (Message.Error,
                           "Please check that Built_Root is properly set",
                           Sloc => Source_Reference.Object'Class
                                     (Source_Reference.Undefined)));
                  end if;

                  return O (O'First .. O'First + 7)
                    & Protocol.Translate_Send (S.Channel, File_Name);
               end;

            elsif O'Length > 7
              and then O (O'First .. O'First + 6) = "-specs="
            then
               --  Send the corresponding file to the slave
               declare
                  File_Name : constant String := O (O'First + 7 .. O'Last);
                  File      : Text_IO.File_Type;
                  Line      : String (1 .. 2_048);
                  Last      : Natural;
               begin
                  if Directories.Exists (File_Name) then
                     Protocol.Send_File
                       (S.Channel, File_Name,
                        Rewrite         => True,
                        Keep_Time_Stamp => True);

                     --  And now send the spec filename in the second line

                     Text_IO.Open (File, Text_IO.In_File, File_Name);
                     Text_IO.Skip_Line (File);
                     Text_IO.Get_Line (File, Line, Last);
                     Text_IO.Close (File);

                     --  A spec filename starts with '+ @', so 3 characters

                     declare
                        Filename_Offset : constant := 3;
                        Spec_Filename   : constant String :=
                                            Line (1 + Filename_Offset .. Last);
                     begin
                        if Directories.Exists (Spec_Filename) then
                           Protocol.Send_File
                             (S.Channel, Spec_Filename,
                              Rewrite         => True,
                              Keep_Time_Stamp => True);
                        else
                           Tree.Append_Message
                             (Message.Create
                                (Message.Error,
                                 "Spec file not found " & Spec_Filename,
                                 Sloc => Source_Reference.Object'Class
                                           (Source_Reference.Undefined)));

                           Tree.Append_Message
                             (Message.Create
                                (Message.Error,
                                 "Please check that Built_Root is "
                                 & "properly set",
                                 Sloc => Source_Reference.Object'Class
                                           (Source_Reference.Undefined)));
                        end if;
                     end;

                  else
                     Tree.Append_Message
                       (Message.Create
                          (Message.Error,
                           "File not found " & File_Name,
                           Sloc => Source_Reference.Object'Class
                                     (Source_Reference.Undefined)));

                     Tree.Append_Message
                       (Message.Create
                          (Message.Error,
                           "Please check that Built_Root is properly set",
                           Sloc => Source_Reference.Object'Class
                                     (Source_Reference.Undefined)));
                  end if;

                  return O (O'First .. O'First + 6)
                    & Protocol.Translate_Send (S.Channel, File_Name);
               end;
            end if;

            return O (O'First .. Pos - 1)
              & Sep & Filter_String (O (Pos + RD'Length + 1 .. O'Last));
         end if;
      end Filter_String;

      Pid : Remote_Id;

   begin
      --  Record the rewrite information for this channel

      Slaves.Set_Rewrite_WD (S, Path => RD);

      if Compiler_Path /= null then
         Slaves.Set_Rewrite_CD
           (S,
            Path => Directories.Containing_Directory
                      (Directories.Containing_Directory (Compiler_Path.all)));
      end if;

      Protocol.Send_Exec
        (S.Channel,
         Project.Path_Name.Value,
         Filter_String (CWD, Sep => ""),
         String (Name (Language)),
         String (Tree.Target),
         String (Tree.Runtime (Language)),
         Options, Obj_Name, Dep_Name, Env,
         Filter_String'Access);

      Remote_Process.Increment;

      --  Wait for the Ack from the remote host, this is set by the Wait_Remote
      --  task.

      Wait_Ack.Get (Pid);

      return Process.Create_Remote (Pid);

   exception
      when E : others =>
         raise Constraint_Error
           with "Unexpected exception: " & Exception_Information (E);
   end Run;

   ------------
   -- Slaves --
   ------------

   protected body Slaves is

      --------------------
      -- Change_Current --
      --------------------

      procedure Change_Current (S : in out Slave_Data; Value : Integer) is
         Position : constant Slave_S.Cursor := Pool.Find (S);
      begin
         Pool (Position).Current := Pool (Position).Current + Value;
      end Change_Current;

      -----------
      -- Clear --
      -----------

      procedure Clear is
      begin
         Pool.Clear;
      end Clear;

      -----------
      -- Count --
      -----------

      function Count return Natural is
      begin
         return Natural (Pool.Length);
      end Count;

      -----------------------
      -- Decrement_Current --
      -----------------------

      procedure Decrement_Current (S : in out Slave_Data) is
      begin
         Change_Current (S, -1);
      end Decrement_Current;

      ----------
      -- Find --
      ----------

      function Find (Socket : Integer) return Slave_Data is
         S        : constant Slave_Data := (Sock => Socket, others => <>);
         Position : constant Slave_S.Cursor := Pool.Find (S);
      begin
         if Slave_S.Has_Element (Position) then
            return Slave_S.Element (Position);
         else
            return Undefined;
         end if;
      end Find;

      function Find (Host : Name_Type) return Slave_Data is
         use type Slave.Object;

         S_Data : constant Slave.Object := Parse (Host);
      begin
         for S of Pool loop
            if S.Data = S_Data then
               return S;
            end if;
         end loop;

         return Undefined;
      end Find;

      --------------
      -- Get_Free --
      --------------

      function Get_Free return Slave_Data is
         use type Containers.Count_Type;

         Random  : constant Float := Numerics.Float_Random.Random (R_Gen);
         S_Count : constant Containers.Count_Type := Pool.Length;
         Index   : constant Positive :=
                     Natural (Float (S_Count - 1) * Random) + 1;
         --  Index of the slave to return if available
         Result  : Slave_Data := Undefined;
         K       : Positive := 1;
      begin
         --  We want to have a random pick of one slave

         Search_Slaves : for S of Pool loop
            if S.Current < S.Max_Processes then
               Result := S;

               --  Slave is ready and this is the one picked-up randomly, stop
               --  searching now.

               exit Search_Slaves when K = Index;
            end if;
            K := K + 1;

            --  We are past the random slave and we have found one slave ready,
            --  stop search here.

            exit Search_Slaves when K > Index and then Result /= Undefined;
         end loop Search_Slaves;

         return Result;
      end Get_Free;

      -----------------------
      -- Increment_Current --
      -----------------------

      procedure Increment_Current (S : in out Slave_Data) is
      begin
         Change_Current (S, 1);
      end Increment_Current;

      ------------
      -- Insert --
      ------------

      procedure Insert (S : Slave_Data) is
      begin
         Pool.Insert (S);
      end Insert;

      -------------
      -- Iterate --
      -------------

      procedure Iterate (Proc : access procedure (S : in out Slave_Data)) is
      begin
         for C in Pool.Iterate loop
            declare
               S : Slave_Data := Slave_S.Element (C);
            begin
               Proc (S);
               Pool.Replace_Element (C, S);
            end;
         end loop;
      end Iterate;

      --------------------
      -- Set_Rewrite_CD --
      --------------------

      procedure Set_Rewrite_CD (S : in out Slave_Data; Path : String) is
         Position : constant Slave_S.Cursor := Pool.Find (S);
      begin
         Protocol.Set_Rewrite_CD (Pool (Position).Channel, Path => Path);
         S := Pool (Position);
      end Set_Rewrite_CD;

      --------------------
      -- Set_Rewrite_WD --
      --------------------

      procedure Set_Rewrite_WD (S : in out Slave_Data; Path : String) is
         Position : constant Slave_S.Cursor := Pool.Find (S);
      begin
         Protocol.Set_Rewrite_WD (Pool (Position).Channel, Path => Path);
         S := Pool (Position);
      end Set_Rewrite_WD;

   end Slaves;

   ------------------------
   -- Start_Waiting_Task --
   ------------------------

   procedure Start_Waiting_Task is
   begin
      if WR = null then
         WR := new Wait_Remote;
      end if;
   end Start_Waiting_Task;

   ------------------------------
   -- Unregister_Remote_Slaves --
   ------------------------------

   procedure Unregister_Remote_Slaves
     (Tree        : GPR2.Project.Tree.Object;
      Options     : GPRtools.Options.Base_Options'Class;
      From_Signal : Boolean := False)
   is
      use type Ada.Calendar.Time;

      procedure Unregister (S : in out Slave_Data);
      --  Unregister given slave

      Start, Stop : Calendar.Time;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister (S : in out Slave_Data) is
      begin
         if not From_Signal then
            Protocol.Send_End_Of_Compilation (S.Channel);

            --  Wait for acknowledge to ensure the clean-up is terminated on
            --  on the slave.

            declare
               Cmd : constant Protocol.Command :=
                       Protocol.Get_Command (S.Channel) with Unreferenced;
            begin
               null;
            end;
         end if;

         Protocol.Close (S.Channel);
      exception
         when others =>
            Protocol.Close (S.Channel);
      end Unregister;

   begin
      Start := Calendar.Clock;

      Slaves.Iterate (Unregister'Access);

      if not From_Signal then
         Sync.Wait;
      end if;

      Stop := Calendar.Clock;

      if not From_Signal
        and then Options.Verbose
        and then Slaves.Count > 0
      then
         Tree.Root_Project.Tree.Append_Message
           (Message.Create
              (Message.Error,
               "  All data synchronized in "
               & Duration'Image (Stop - Start) & " seconds",
               Sloc => Source_Reference.Object'Class
                         (Source_Reference.Undefined)));
      end if;

      Slaves.Clear;
   end Unregister_Remote_Slaves;

   --------------
   -- Wait_Ack --
   --------------

   protected body Wait_Ack is

      ---------
      -- Set --
      ---------

      procedure Set (Pid : Remote_Id) is
      begin
         Id := Pid;
         Is_Set := True;
      end Set;

      ---------
      -- Get --
      ---------

      entry Get (Pid : out Remote_Id) when Is_Set is
      begin
         Pid := Id;
         Is_Set := False;
      end Get;

   end Wait_Ack;

   -----------------
   -- Wait_Remote --
   -----------------

   task body Wait_Remote is
      use all type Protocol.Command_Kind;
      use GNAT.Sockets;

      Proc         : Id;
      Pid          : Remote_Id;
      Selector     : Selector_Type;
      Status       : Selector_Status;
      R_Set, W_Set : Socket_Set_Type;
      Sock         : Socket_Type;
      S            : Slave_Data;
   begin
      --  In this task we are only interested by the incoming data, so we do
      --  not wait on socket ready for writing.

      Sockets.Empty (W_Set);

      Create_Selector (Selector);

      loop
         --  Let's wait for at least some process to monitor

         Remote_Process.Wait_Non_Zero;

         --  Wait for response from all registered slaves

         Copy (Slaves_Sockets, R_Set);

         Check_Selector (Selector, R_Set, W_Set, Status);

         if Status = Completed then
            Get (R_Set, Sock);

            pragma Assert
              (Sock /= No_Socket, "no socket returned by selector");

            S := Slaves.Find (To_C (Sock));

            if S /= Undefined then
               declare
                  Cmd     : constant Protocol.Command :=
                              Protocol.Get_Command (S.Channel);
                  Success : Boolean;
               begin
                  --  A display output

                  if Kind (Cmd) = DP then
                     --  Write output to the console

                     Text_IO.Put (To_String (Protocol.Output (Cmd)));

                     Protocol.Get_Pid (S.Channel, Pid, Success);

                     Proc := Process.Create_Remote (Pid);

                     Remote_Process.Decrement;
                     Slaves.Decrement_Current (S);

                     Process.Add_Result (Proc, Success, S.Data.Host);

                  --  An acknowledgment of an compilation job

                  elsif Kind (Cmd) = AK then
                     declare
                        Pid : constant Remote_Id :=
                                Remote_Id'Value (Protocol.Args (Cmd)(1).all);
                     begin
                        Slaves.Increment_Current (S);
                        Wait_Ack.Set (Pid);
                     end;

                  elsif Kind (Cmd) in EC | SI then
                     null;

                  else
                     raise Constraint_Error with "Unexpected command: "
                       & Protocol.Command_Kind'Image (Kind (Cmd));
                  end if;
               end;
            end if;

         else
            null;
         end if;

         Sockets.Empty (R_Set);
      end loop;
   exception
      when E : others =>
         Text_IO.Put_Line (Exception_Information (E));
         OS_Lib.OS_Exit (1);
   end Wait_Remote;

end GPR2.Compilation.Registry;
