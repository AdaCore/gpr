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

with Ada.Command_Line;
with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Exception_Traces;
with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;

with GPR2.Compilation.Protocol;
with GPR2.Compilation.Registry;
with GPR2.Compilation.Sync;
with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Time_Stamp;
with GPR2.Version;

with GPRtools.Options;
with GPRtools.Util;

procedure GPRremote is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   use GPR2;
   use GPRtools;

   procedure Parse_Command_Line;
   --  Parse command line parameters

   procedure Activate_Symbolic_Traceback;
   --  Activate symbolic trace-back

   procedure Cmd_Info;

   procedure Cmd_Exec;

   procedure Cmd_Syncfrom;

   procedure Cmd_Syncexec;

   Arg_Host         : constant := 1;
   Arg_Cmd          : constant := 2;
   Arg_Project      : constant := 3;
   Arg_First_Option : constant := 4;

   Args : array (1 .. Ada.Command_Line.Argument_Count) of Unbounded_String;
   Last : Natural := 0;

   Exit_Status : Natural := 0;
   --  GPRremote's exit status

   Project     : GPR2.Project.Tree.Object;
   Options     : GPRtools.Options.Base_Options;

   type Command_Kind is (Info, Exec, Syncto, Syncfrom, Syncexec);

   procedure Prolog (Cmd : Command_Kind);
   --  The prolog to each command to setup the communication layer

   procedure Epilog (Cmd : Command_Kind);
   --  The epilog to each command to close the communication layer

   ---------------------------------
   -- Activate_Symbolic_Traceback --
   ---------------------------------

   procedure Activate_Symbolic_Traceback is
      use GNAT;
   begin
      Exception_Traces.Trace_On (Exception_Traces.Unhandled_Raise);
      Exception_Traces.Set_Trace_Decorator
        (Traceback.Symbolic.Symbolic_Traceback'Access);
   end Activate_Symbolic_Traceback;

   --------------
   -- Cmd_Exec --
   --------------

   procedure Cmd_Exec is
      use all type Compilation.Protocol.Command_Kind;

      Host         : constant Optional_Name_Type :=
                       Optional_Name_Type (To_String (Args (Arg_Host)));
      Project_Name : constant String := To_String (Args (Arg_Project));
      Channel      : Compilation.Protocol.Communication_Channel;
      Root_Dir     : Unbounded_String;

      function Filter_Path
        (O   : String;
         Sep : String := Compilation.Protocol.WD_Path_Tag) return String;
      --  Make O PATH relative to RD. For option -gnatec and -gnatem makes
      --  the specified filename absolute in the slave environment and send
      --  the file to the slave.

      -----------------
      -- Filter_Path --
      -----------------

      function Filter_Path
        (O   : String;
         Sep : String := Compilation.Protocol.WD_Path_Tag) return String
      is
         RD  : constant String := To_String (Root_Dir);
         Pos : constant Natural := Strings.Fixed.Index (O, RD);
      begin
         if Pos = 0 then
            return O;
         else
            return O (O'First .. Pos - 1)
              & Sep & Filter_Path (O (Pos + RD'Length + 1 .. O'Last));
         end if;
      end Filter_Path;

      Options : GPR2.Containers.Value_List;

   begin
      Root_Dir := To_Unbounded_String
        (Compilation.Registry.Remote_Root_Directory (Project.Root_Project));

      --  Get the channel for the given host

      Channel := Compilation.Registry.Channel (Host);

      --  Set options

      for K in Arg_First_Option .. Last loop
         Options.Append (To_String (Args (K)));
      end loop;

      --  Send sync command to slave

      Compilation.Protocol.Send_Exec
        (Channel,
         Project_Name,
         ".",
         Language => "",
         Target   => "",
         Runtime  => "",
         Options  => Options,
         Obj_Name => "",
         Dep_Name => "",
         Env      => "",
         Filter   => Filter_Path'Access);

      Wait_Ack : declare
         Cmd : constant Compilation.Protocol.Command :=
                 Compilation.Protocol.Get_Command (Channel);
      begin
         if Cmd.Kind = AK then
            null;
         else
            raise Compilation.Protocol.Wrong_Command
              with "expected AK command, found " & Cmd.Kind'Img;
         end if;
      end Wait_Ack;

      --  In this mode the output will be sent first

      declare
         Cmd : constant Compilation.Protocol.Command :=
                 Compilation.Protocol.Get_Command (Channel);
      begin
         if Cmd.Kind = DP then
            Put_Line (To_String (Cmd.Output));
         else
            raise Compilation.Protocol.Wrong_Command
              with "expected DP command, found " & Cmd.Kind'Img;
         end if;
      end;

      --  And then a KO or OK depending on the exit status of the remote
      --  command is sent.

      declare
         Cmd : constant Compilation.Protocol.Command :=
                 Compilation.Protocol.Get_Command (Channel);
      begin
         if Cmd.Kind in OK then
            null;
         elsif Cmd.Kind = KO then
            Exit_Status := 1;
         else
            raise Compilation.Protocol.Wrong_Command
              with "expected OK/NOK command, found " & Cmd.Kind'Img;
         end if;
      end;
   end Cmd_Exec;

   --------------
   -- Cmd_Info --
   --------------

   procedure Cmd_Info is

      Host    : constant Optional_Name_Type :=
                  Optional_Name_Type (To_String (Args (Arg_Host)));
      Channel : Compilation.Protocol.Communication_Channel;

      Version_String   : Unbounded_String;
      Current_UTC_Time : GPR2.Time_Stamp.Data;
      GPR_Hash         : Unbounded_String;
      Success          : Boolean;
   begin
      --  Get the channel for the given host

      Channel := Compilation.Registry.Channel (Host);

      Compilation.Protocol.Send_Info_Request (Channel);

      Compilation.Protocol.Get_Info_Response
        (Channel, Version_String, Current_UTC_Time, GPR_Hash, Success);

      if Success then
         Put_Line ("version  : " & To_String (Version_String));
         Put_Line ("UTC time : " & String (Current_UTC_Time));

      else
         raise Compilation.Protocol.Wrong_Command
           with "cannot get information from slave";
      end if;
   end Cmd_Info;

   ------------------
   -- Cmd_Syncexec --
   ------------------

   procedure Cmd_Syncexec is
   begin
      Cmd_Exec;
      Cmd_Syncfrom;
   end Cmd_Syncexec;

   ------------------
   -- Cmd_Syncfrom --
   ------------------

   procedure Cmd_Syncfrom is

      procedure Output (Message : String);

      ------------
      -- Output --
      ------------

      procedure Output (Message : String) is
      begin
         Text_IO.Put_Line (Message);
         Text_IO.Flush;
      end Output;

      Host    : constant Optional_Name_Type :=
                  Optional_Name_Type (To_String (Args (Arg_Host)));
      Channel : Compilation.Protocol.Communication_Channel;
      Result  : Compilation.Protocol.Command_Kind with Unreferenced;

      Total_File        : Natural;
      Total_Transferred : Natural;
      Remote_Files      : GPR2.Containers.Value_Set;

   begin
      --  Get the channel for the given host

      Channel := Compilation.Registry.Channel (Host);

      --  Send sync command to slave

      Compilation.Protocol.Send_Sync_Request (Channel);

      --  Wait back for the files

      Result := Compilation.Sync.Receive_Files
        (Channel,
         Compilation.Registry.Remote_Root_Directory (Project.Root_Project),
         Total_File,
         Total_Transferred,
         Remote_Files,
         False, Output'Access);
   end Cmd_Syncfrom;

   ------------
   -- Epilog --
   ------------

   procedure Epilog (Cmd : Command_Kind) is
      pragma Unreferenced (Cmd);
   begin
      Compilation.Registry.Unregister_Remote_Slaves (Project, Options);
   end Epilog;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      use GNAT.Command_Line;
      use GNAT.OS_Lib;
      Parser : constant GPRtools.Options.Command_Line_Parser :=
                 GPRtools.Options.Create
                   (Initial_Year       => "2017",
                    No_Project_Support => True,
                    Allow_Quiet        => False);

   begin
      GPRtools.Options.Setup (GPRtools.Remote);

      Options.Tree := Project.Reference;
      Parser.Get_Opt (Options);

      --  Now read arguments

      for Arg of Options.Args loop
         Last := Last + 1;
         Args (Last) := To_Unbounded_String (Arg);
      end loop;

   exception
      when Invalid_Switch =>
         OS_Exit (1);

      when Exit_From_Command_Line =>
         OS_Exit (1);
   end Parse_Command_Line;

   ------------
   -- Prolog --
   ------------

   procedure Prolog (Cmd : Command_Kind) is

      procedure Load_Project (Filename : String);
      --  Load a project file

      ------------------
      -- Load_Project --
      ------------------

      procedure Load_Project (Filename : String) is
         Pathname : constant GPR2.Path_Name.Object :=
                      GPR2.Project.Create (GPR2.Filename_Type (Filename));
         Context  : GPR2.Context.Object;
      begin
         if Options.Verbose then
            Put_Line ("loading project: " & Pathname.Value);
         end if;

         Project.Load
           (Pathname, Context,
            Check_Shared_Lib => not Options.Unchecked_Shared_Lib);
      end Load_Project;

      Project_Name : constant String := To_String (Args (Arg_Project));
      Sync         : Boolean := False;
   begin
      Load_Project (Project_Name);

      if Cmd in Syncto | Syncexec then
         Sync := True;
      else
         Sync := False;
      end if;

      if Cmd in Exec | Syncexec then
         if Last < Arg_First_Option then
            raise Usage_Error with "missing aguments (command to execute)";
         end if;

      else
         if Last >= Arg_First_Option then
            raise Usage_Error with "too many aguments";
         end if;
      end if;

      Compilation.Registry.Register_Remote_Slaves
        (Project, Options, Synchronize => Sync);
   end Prolog;

begin
   GPRtools.Util.Set_Program_Name ("gprremote");

   Parse_Command_Line;

   Activate_Symbolic_Traceback;

   if Args'Last < Arg_Cmd then
      Version.Display
        ("GPRREMOTE", "2017", Version_String => Version.Long_Value);

      return;
   end if;

   --  Set corresponding slave environment

   if Options.Slave_Env = Null_Unbounded_String
     and then Options.Distributed_Mode
   then
      Options.Slave_Env := To_Unbounded_String
        (GPR2.Compilation.Registry.Compute_Env
           (Project, Options.Slave_Env_Auto));

      if Options.Slave_Env_Auto and then Options.Verbose then
         Text_IO.Put_Line
           ("slave environment is " & To_String (Options.Slave_Env));
      end if;
   end if;

   declare
      Host    : constant Name_Type := Name_Type (To_String (Args (Arg_Host)));
      Command : constant String :=
                  Characters.Handling.To_Upper (To_String (Args (Arg_Cmd)));
      Cmd     : Command_Kind;
      Hosts   : GPR2.Containers.Name_List;
   begin
      --  Check that we have a valid command

      if (for some V in Command_Kind => Command_Kind'Image (V) = Command) then
         Cmd := Command_Kind'Value (Command);

         --  First connect to the host

         Hosts.Append (Host);

         Compilation.Registry.Record_Slaves (Hosts);

         Prolog (Cmd);

         case Cmd is
            when Info     => Cmd_Info;
            when Exec     => Cmd_Exec;
            when Syncto   => null; --  all is done in prolog/epilog
            when Syncfrom => Cmd_Syncfrom;
            when Syncexec => Cmd_Syncexec;
         end case;

         Epilog (Cmd);

      else
         Put_Line ("GPRremote: unknown command '" & Command & ''');
      end if;

   end;

   GNAT.OS_Lib.OS_Exit (Exit_Status);

exception
   when E : Usage_Error =>
      Put_Line ("gprremote: " & Exception_Message (E));
      GNAT.OS_Lib.OS_Exit (1);

   when E : others =>
      Put_Line
        ("Unrecoverable error in GPRremote :" & Exception_Information (E));
      GNAT.OS_Lib.OS_Exit (1);
end GPRremote;
