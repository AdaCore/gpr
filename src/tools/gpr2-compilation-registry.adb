------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.MD5;
with GNAT.OS_Lib;
with GNAT.Sockets;

with GPR.Opt;

with GPR2.Compilation.Slave;
with GPR2.Compilation.Sync;
with GPR2.Containers;
with GPR2.Message;
with GPR2.Project.Pack;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Source_Reference;

with GPRtools.Util;

package body GPR2.Compilation.Registry is

   use Ada;
   use Ada.Strings.Unbounded;

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
      Options                    : GPRtools.Options.Object'Class;
      Included_Artifact_Patterns : String := "") return Slave_Data;
   --  Connect to the slave and return the corresponding object

   ----------------------------
   -- Clean_Up_Remote_Slaves --
   ----------------------------

   procedure Clean_Up_Remote_Slaves
     (Project : GPR2.Project.View.Object;
      Options : GPRtools.Options.Object'Class)
   is
      Tree : constant not null access GPR2.Project.Tree.Object := Project.Tree;

      procedure Clean_Up_Remote_Slave
        (S_Data       : Slave.Object;
         Project_Name : Name_Type);
      --  Clean-up slave

      ---------------------------
      -- Clean_Up_Remote_Slave --
      ---------------------------

      procedure Clean_Up_Remote_Slave
        (S_Data       : Slave.Object;
         Project_Name : Name_Type)
      is
         use all type Compilation.Protocol.Command_Kind;

         S : Slave_Data;

      begin
         S := Connect
           (Tree, S_Data, Project_Name,
            Options => Options, Sync => False);

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
                           Sloc => Source_Reference.Object'Class
                             (Source_Reference.Undefined)));
                  end if;

               elsif Compilation.Protocol.Kind (Cmd) = KO then
                  Tree.Append_Message
                    (Message.Create
                       (Message.Error,
                        "Slave cannoe clean-up " & String (S_Data.Host),
                        Sloc => Source_Reference.Object'Class
                          (Source_Reference.Undefined)));

               else
                  Tree.Append_Message
                    (Message.Create
                       (Message.Error,
                        "protocol error: "
                        & Compilation.Protocol.Command_Kind'Image (Kind (Cmd)),
                        Sloc => Source_Reference.Object'Class
                          (Source_Reference.Undefined)));
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
      end Clean_Up_Remote_Slave;

   begin
      for S of Slaves_Data loop
         Clean_Up_Remote_Slave (S, Project.Name);
      end loop;

      if Tree.Log_Messages.Has_Error then
         raise Constraint_Error with "remote compilation error";
      end if;
   end Clean_Up_Remote_Slaves;

   -----------------
   -- Compute_Env --
   -----------------

   function Compute_Env
     (Tree : GPR2.Project.Tree.Object; Auto : Boolean) return String
   is
      use Ada.Command_Line;
      use GNAT;
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
      Options                    : GPRtools.Options.Object'Class;
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
                  Sloc => Source_Reference.Object'Class
                            (Source_Reference.Undefined)));
            return S;
      end;

      if Status in Expired .. Aborted then
         Tree.Append_Message
           (Message.Create
              (Message.Error,
               "Cannot connect to slave "
               & String (S_Data.Host) & ", aborting",
               Sloc => Source_Reference.Object'Class
                         (Source_Reference.Undefined)));
         return S;
      end if;

      S.Channel := Compilation.Protocol.Create (Sock);

      --  Do initial handshake

      Compilation.Protocol.Send_Context
        (S.Channel,
         String (GPRtools.Options.Get_Target (Options)),
         String (Project_Name),
         To_String (Options.Slave_Env),
         Sync,
         (if Options.Hash_Value = Null_Unbounded_String
          then ""
          else To_String (Options.Hash_Value)),
         Included_Artifact_Patterns);

      declare
         use GNAT;
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
                     Sloc => Source_Reference.Object'Class
                               (Source_Reference.Undefined)));
            end if;

         elsif Kind (Cmd) = KO then
            Tree.Append_Message
              (Message.Create
                 (Message.Error,
                  (if Parameters'Length = 1 and then Parameters (1).all /= ""
                   then Parameters (1).all
                   else "build slave is not compatible")
                  & " : " & String (S_Data.Host),
                  Sloc => Source_Reference.Object'Class
                    (Source_Reference.Undefined)));
            return S;

         else
            Tree.Append_Message
              (Message.Create
                 (Message.Error,
                  "protocol error: "
                  & Protocol.Command_Kind'Image (Kind (Cmd)),
                  Sloc => Source_Reference.Object'Class
                            (Source_Reference.Undefined)));
            return S;
         end if;
      end;

      return S;
   end Connect;

   ---------------
   -- Get_Hosts --
   ---------------

   function Get_Hosts return String is
      Hosts : Unbounded_String;
   begin
      if Environment_Variables.Exists ("GPR_SLAVES") then
         Hosts := To_Unbounded_String
           (Environment_Variables.Value ("GPR_SLAVES"));

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
                     if Hosts /= Null_Unbounded_String then
                        Append (Hosts, ",");
                     end if;
                     Append (Hosts, Buffer (1 .. Last));
                  end if;
               end loop;

               Text_IO.Close (F);

            else
               GPRtools.Util.Fail_Program
                 ("hosts distributed file " & F_Name & " not found");
            end if;
         end;
      end if;

      return To_String (Hosts);
   end Get_Hosts;

   ----------------------------
   -- Register_Remote_Slaves --
   ----------------------------

   procedure Register_Remote_Slaves
     (Tree        : GPR2.Project.Tree.Object;
      Synchronize : Boolean)
   is
      use Ada.Directories;
      use GNAT.OS_Lib;

      use type GPR.Opt.Verbosity_Level_Type;
      use type Calendar.Time;
      use type Containers.Count_Type;

      Start, Stop : Calendar.Time;

      procedure Insert
        (List   : out Sync.Str_Vect.Vector;
         Values : GPR2.Containers.Source_Value_List);
      --  Inserts all values into the vector

      Excluded_Patterns          : Sync.Str_Vect.Vector;
      Included_Patterns          : Sync.Str_Vect.Vector;
      Included_Artifact_Patterns : Sync.Str_Vect.Vector;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (List   : out Sync.Str_Vect.Vector;
         Values : GPR2.Containers.Source_Value_List) is
      begin
         for V of Values loop
            List.Append (V.Text);
         end loop;
      end Insert;

      package Attrs renames GPR2.Project.Registry.Attribute;

      Project  : constant GPR2.Project.View.Object :=
                   Tree.Root_Project;

      Root_Dir : Unbounded_String renames GPR.Compilation.Slave.Root_Dir;

   begin
      Root_Dir := To_Unbounded_String (Remote_Root_Directory (Project));

      --  Check for Root_Dir attribute and Excluded_Patterns

      if Project.Has_Packages (GPR2.Project.Registry.Pack.Remote) then
         declare
            Pck : constant GPR2.Project.Pack.Object :=
                    Project.Packages.Element
                      (GPR2.Project.Registry.Pack.Remote);
         begin
            if Pck.Has_Attributes (Attrs.Excluded_Patterns) then
               Insert
                 (Excluded_Patterns,
                  Pck.Attribute (Attrs.Excluded_Patterns).Values);

            elsif Pck.Has_Attributes (Attrs.Included_Patterns) then
               Insert
                 (Included_Patterns,
                  Pck.Attribute (Attrs.Included_Patterns).Values);

            elsif Pck.Has_Attributes (Attrs.Included_Artifacts_Patterns) then
               Insert
                 (Included_Artifact_Patterns,
                  Pck.Attribute (Attrs.Included_Artifacts_Patterns).Values);
            end if;
         end;
      end if;

      if not Exists (To_String (Root_Dir))
        or else not Is_Directory (To_String (Root_Dir))
      then
         Text_IO.Put_Line
           ("error: "
            & To_String (Root_Dir) & " is not a directory or does not exist");
         OS_Exit (1);

      else
         Text_IO.Put_Line ("root dir : " & To_String (Root_Dir));
      end if;

      --  Check if Excluded_Patterns and Included_Patterns are set

      if Included_Patterns.Length /= 0
        and then Excluded_Patterns.Length /= 0
      then
         Text_IO.Put_Line
           ("error: Excluded_Patterns and Included_Patterns are exclusive");
         OS_Exit (1);
      end if;

      --  Then registers the build slaves

      Start := Calendar.Clock;

      for S of GPR.Compilation.Slave.Slaves_Data loop
         GPR.Compilation.Slave.Register_Remote_Slave
           (S,
            String (Project.Name),
            Excluded_Patterns,
            Included_Patterns,
            Included_Artifact_Patterns,
            Synchronize);
      end loop;

      if Synchronize then
         Sync.Wait;
      end if;

      Stop := Calendar.Clock;

      if Synchronize and then GPR.Opt.Verbosity_Level > GPR.Opt.Low then
         Text_IO.Put ("  All data synchronized in ");
         Text_IO.Put (Duration'Image (Stop - Start));
         Text_IO.Put_Line (" seconds");
      end if;

      --  We are in remote mode, the initialization was successful, start tasks
      --  now.

      GPR.Compilation.Slave.Start_Waiting_Task;
   end Register_Remote_Slaves;

   ---------------------------
   -- Remote_Root_Directory --
   ---------------------------

   function Remote_Root_Directory
     (Project : GPR2.Project.View.Object) return String
   is
      use GNAT.OS_Lib;

      package Attrs renames GPR2.Project.Registry.Attribute;

      Root_Dir : constant String := Project.Dir_Name.Value;
   begin
      if Project.Has_Packages (GPR2.Project.Registry.Pack.Remote) then
         declare
            Pck : constant GPR2.Project.Pack.Object :=
                    Project.Packages.Element
                      (GPR2.Project.Registry.Pack.Remote);
         begin
            if Pck.Has_Attributes (Attrs.Root_Dir) then
               declare
                  RD : constant String :=
                         Pck.Attribute (Attrs.Root_Dir).Value.Text;
               begin
                  if Is_Absolute_Path (RD) then
                     return RD;
                  else
                     return Normalize_Pathname
                       (Root_Dir & Directory_Separator & RD);
                  end if;
               end;
            end if;
         end;
      end if;

      return Root_Dir;
   end Remote_Root_Directory;

end GPR2.Compilation.Registry;
