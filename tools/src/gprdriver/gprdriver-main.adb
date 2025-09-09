------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                        Copyright (C) 2025, AdaCore                       --
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
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with GNATCOLL.OS.Process;
with GNATCOLL.Utils;
with GNATCOLL.VFS;

with GPR2.Options;
with GPRtools.Command_Line;
with GPRtools.Util;
with GPRtools.Program_Termination;

function GPRDriver.Main return Ada.Command_Line.Exit_Status is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   use GPRtools.Program_Termination;
   use GNATCOLL;
   use GNATCOLL.OS.Process;

   package CLI renames Ada.Command_Line;

   Default_Version : constant Character := '1';
   --  Version to use if the environment variable is not set

   CTL_VAR : constant String := "GNAT_GPR_ENGINE";
   --  The environment variable to be used to control the GPR tool
   --  version to use. If variable is not defined or empty use Default_Version.
   --
   --  Supported values:
   --
   --  "1" | "legacy" => GPR1
   --  "2" | "new"    => GPR2

   Engine : Unbounded_String :=
              To_Unbounded_String
                (Environment_Variables.Value (CTL_VAR,
                                              String'(1 => Default_Version)));

   function Get_GPR_Version return Character
     with Post => Get_GPR_Version'Result in '1' | '2';

   function Get_Command return String;
   --  Returns the full-pathname of the command. It is important to get the
   --  full-pathname as we want to ensure that the corresponding tool version
   --  (1 or 2) is run from the same directory where the driver has been found.

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command return String is
      Cmd   : constant String := CLI.Command_Name;
      V_Cmd : constant VFS.Virtual_File :=
              VFS.Create (VFS.Filesystem_String (Cmd));
   begin
      if VFS.Is_Absolute_Path (V_Cmd) then
         return Cmd;

      else
         declare
            C : constant VFS.Virtual_File :=
                  VFS.Locate_On_Path (VFS.Filesystem_String (Cmd));
         begin
            return String (VFS.Filesystem_String'(VFS.Full_Name (C)));
         end;
      end if;
   end Get_Command;

   ---------------------
   -- Get_GPR_Version --
   ---------------------

   function Get_GPR_Version return Character is
      V : constant String := To_String (Engine);
   begin
      if V in "new" | "2" then
         return '2';
      elsif V in "legacy" | "1" then
         return '1';
      else
         raise GPR2.Options.Usage_Error with
           "unknown value '" & V & "' for " & CTL_VAR;
      end if;
   end Get_GPR_Version;

   Command : Argument_List;
   --  The command to be executed

begin
   GPRtools.Util.Set_Program_Name (GPRtools.Command_Line.Get_Executable);

   --  Append all arguments

   for I in 1 .. CLI.Argument_Count loop
      --  Check and skip --gpr=<n> option. Note that this option can't be
      --  reported by GPRdriver as it is just a proxy between two other
      --  executables. We catch this option here and remove it from the list
      --  of arguments for the actual command executed. Note that the --gpr=<n>
      --  option override the value passed into the environment variable.

      declare
         O : constant String := CLI.Argument (I);
      begin
         if Utils.Starts_With (O, "--gpr=") then
            if O'Length > 6 then
               Engine := To_Unbounded_String (O (O'First + 6 .. O'Last));
            end if;
         else
            Command.Append (CLI.Argument (I));
         end if;
      end;
   end loop;

   --  Get command name and switch to the set GPR version

   declare
      Cmd : constant String := Get_Command;
   begin
      --  Check for start of GPR command

      if GNATCOLL.Utils.Ends_With (Cmd, ".exe")
        or else GNATCOLL.Utils.Ends_With (Cmd, ".EXE")
      then
         declare
            C : constant String := Cmd (Cmd'First .. Cmd'Last - 4)
                  & Get_GPR_Version & Cmd (Cmd'Last - 3 .. Cmd'Last);
         begin
            if not Directories.Exists (C) then
               raise GPR2.Options.Usage_Error with
               C & ": not found";
            else
               Command.Prepend (C);
            end if;
         end;
      else
         declare
            C : constant String := Cmd & Get_GPR_Version;
         begin
            if not Directories.Exists (C) then
               raise GPR2.Options.Usage_Error with
                 C & ": not found";
            else
               Command.Prepend (C);
            end if;
         end;
      end if;
   end;

   --  Now run the command

   declare
      Proc_Handle : constant Process_Handle := Start (Command);
      Ret         : constant Integer        := Wait (Proc_Handle);
   begin
      return Command_Line.Exit_Status (Ret);
   end;

exception
   when E : others =>
      Handle_Program_Termination
        (Display_Command_Line_Help => False,
         Force_Exit                => False,
         Message                   => Exception_Message (E));
      return To_Exit_Status (E_Fatal);
end GPRDriver.Main;
