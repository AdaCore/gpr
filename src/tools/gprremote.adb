------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Exception_Traces;
with GNAT.Traceback.Symbolic;

with GPR.Opt;
with GPR.Util;
with GPR_Version;

with GNAT.OS_Lib;

with GPR2.Compilation.Protocol;
with GPR2.Compilation.Slave;
with GPR2.Compilation.Sync;
with GPR2.Context;
with GPR2.Project.Tree;

procedure GPRremote is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   use GPR2;

   use type GNAT.OS_Lib.String_Access;

   procedure Parse_Command_Line;
   --  Parse command line parameters

   procedure Activate_Symbolic_Traceback;
   --  Activate symbolic trace-back

   procedure Cmd_Info;

   procedure Cmd_Exec is null;

   procedure Cmd_Syncto;

   procedure Cmd_Syncfrom;

   procedure Cmd_Sync_Exec is null;

   Arg_Host        : constant := 1;
   Arg_Cmd         : constant := 2;
   Arg_First_Param : constant := 3;

   Help    : aliased Boolean := False;
   Verbose : aliased Boolean := False;
   Version : aliased Boolean := False;
   Args    : array (1 .. Command_Line.Argument_Count) of Unbounded_String;
   Last    : Natural := 0;

   Project : GPR2.Project.Tree.Object;

   procedure Load_Project (Filename : String);
   --  Load a project file

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
   -- Cmd_Info --
   --------------

   procedure Cmd_Info is
   begin
      null;
   end Cmd_Info;

   ------------------
   -- Cmd_Syncfrom --
   ------------------

   procedure Cmd_Syncfrom is
      Host    : constant String := To_String (Args (Arg_Host));
      Channel : constant Compilation.Protocol.Communication_Channel :=
                  Compilation.Slave.Channel (Host);
      Result  : Compilation.Protocol.Command_Kind with Unreferenced;

      Total_File        : Natural;
      Total_Transferred : Natural;
      Remote_Files      : Compilation.Sync.Files.Set;
   begin
      Load_Project (To_String (Args (Arg_First_Param)));

      GPR2.Compilation.Slave.Register_Remote_Slaves
        (Project, Synchronize => False);

      --  Send sync command to slave

      Compilation.Protocol.Send_Sync_Request (Channel);

      --  Wait back for the files

      Result := Compilation.Sync.Receive_Files
        (Channel,
         Directories.Current_Directory,  -- ???
         Total_File,
         Total_Transferred,
         Remote_Files,
         False, null);

      Compilation.Slave.Unregister_Remote_Slaves;
   end Cmd_Syncfrom;

   ----------------
   -- Cmd_Syncto --
   ----------------

   procedure Cmd_Syncto is
   begin
      Load_Project (To_String (Args (Arg_First_Param)));

      Compilation.Slave.Register_Remote_Slaves
        (Project, Synchronize => True);
      Compilation.Slave.Unregister_Remote_Slaves;
   end Cmd_Syncto;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project (Filename : String) is
      Pathname : constant GPR2.Path_Name_Type :=
                   GPR2.Create (GPR2.Optional_Name_Type (Filename));
      Context  : GPR2.Context.Object;
   begin
      if Verbose then
         Put_Line ("loading project: " & GPR2.Value (Pathname));
      end if;

      Project.Load (Pathname, Context);
   end Load_Project;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      use GNAT.Command_Line;
      use GNAT.OS_Lib;
      use GPR.Util;

      procedure Usage;

      procedure Check_Version_And_Help is new
        Check_Version_And_Help_G (Usage);

      Config : Command_Line_Configuration;

      -----------
      -- Usage --
      -----------

      procedure Usage is
      begin
         Display_Help (Config);
      end Usage;

   begin
      Define_Switch
        (Config, Help'Access,
         "-h", Long_Switch => "--help",
         Help => "display this help message and exit");

      Define_Switch
        (Config, Version'Access,
         "-V", Long_Switch => "--version",
         Help => "display version and exit");

      Define_Switch
        (Config, Verbose'Access,
         "-v", Long_Switch => "--verbose",
         Help => "verbose mode, display extra information");

      Set_Usage (Config, Usage => "[switches] [host] [command] [parameters]");

      Check_Version_And_Help
        ("GPRREMOTE",
         "2016",
         Version_String => GPR_Version.Gpr_Version_String);

      Getopt (Config);

      --  Now read arguments

      Read_Arguments : loop
         declare
            Arg : constant String := Get_Argument;
         begin
            exit Read_Arguments when Arg = "";

            Last := Last + 1;
            Args (Last) := To_Unbounded_String (Arg);
         end;
      end loop Read_Arguments;

   exception
      when Invalid_Switch =>
         OS_Exit (1);

      when Exit_From_Command_Line =>
         OS_Exit (1);
   end Parse_Command_Line;

begin
   Parse_Command_Line;

   Activate_Symbolic_Traceback;

   --  Set corresponding slave environment

   if GPR.Util.Slave_Env = null then
      GPR.Util.Slave_Env := new String'
        (Compilation.Slave.Compute_Env (Project, GPR.Util.Slave_Env_Auto));

      if GPR.Util.Slave_Env_Auto and not GPR.Opt.Quiet_Output then
         Put ("slave environment is ");
         Put (GPR.Util.Slave_Env.all);
         New_Line;
      end if;
   end if;

   declare
      Host    : constant String := To_String (Args (Arg_Host));
      Command : constant String := To_String (Args (Arg_Cmd));
   begin
      --  First connect to the host

      Compilation.Slave.Record_Slaves (Host);

      if Command = "info" then
         Cmd_Info;

      elsif Command = "exec" then
         Cmd_Exec;

      elsif Command = "syncto" then
         Cmd_Syncto;

      elsif Command = "syncfrom" then
         Cmd_Syncfrom;

      elsif Command = "syncexec" then
         Cmd_Sync_Exec;

      else
         Put_Line ("GPRremote: unknown command '" & Command & ''');
      end if;
   end;

   GNAT.OS_Lib.OS_Exit (0);

exception
   when E : others =>
      Put_Line
        ("Unrecoverable error in GPRremote :" & Exception_Information (E));
      GNAT.OS_Lib.OS_Exit (1);
end GPRremote;
