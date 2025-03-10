------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2024, AdaCore                     --
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

with Ada;
with Ada.Command_Line;
with Ada.Exceptions;

with GPR2.Options;
with GPR2.Reporter;

with GPRdoc.Process;

with GPRtools;
with GPRtools.Command_Line;
with GPRtools.Interrupt_Handler;
with GPRtools.Options;
with GPRtools.Program_Termination;
with GPRtools.Sigint;
with GPRtools.Util;

function GPRdoc.Main return Ada.Command_Line.Exit_Status is

   use Ada;
   use Ada.Exceptions;

   use GPRtools.Program_Termination;

   Options : GPRdoc.GPRdoc_Options;

   procedure On_Switch
     (Parser : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res    : not null access GPRtools.Command_Line.Command_Line_Result'Class;
      Arg    : GPRtools.Command_Line.Switch_Type;
      Index  : String;
      Param  : String);

   procedure Parse_Command_Line;
   --  Parse command line parameters

   ---------------
   -- On_Switch --
   ---------------

   procedure On_Switch
     (Parser : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res    : not null access GPRtools.Command_Line.Command_Line_Result'Class;
      Arg    : GPRtools.Command_Line.Switch_Type;
      Index  : String;
      Param  : String)
   is
      pragma Unreferenced (Parser, Index);
      use type GPRtools.Command_Line.Switch_Type;

      Result : constant access GPRdoc_Options :=
                 GPRdoc_Options (Res.all)'Access;
   begin
      Result.Console_Reporter.Set_Verbosity (GPR2.Reporter.Quiet);

      --  We want a clean output to be JSON compliant

      if Arg = "--display" then
         if Param = "json" then
            Options.Kind_Of_Display := GPRtools.K_JSON;

         elsif Param = "json-compact" then
            Options.Kind_Of_Display := GPRtools.K_JSON_Compact;

         elsif Param = "textual" then
            Options.Kind_Of_Display := GPRtools.K_Textual_IO;

         else
            raise GPR2.Options.Usage_Error with
              "use --display=<value> "
              & "with <value>=[json, json-compact, textual]";
         end if;
      end if;
   end On_Switch;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      use GPRtools.Command_Line;
      use GPRtools.Options;

      Parser : GPRtools.Options.Command_Line_Parser :=
                 Create
                   (Initial_Year       => "2022",
                    No_Project_Support => True,
                    Allow_Quiet        => False);
      Group  : constant GPRtools.Command_Line.Argument_Group :=
                 Parser.Add_Argument_Group
                   ("gprdoc", On_Switch'Unrestricted_Access);
   begin
      Setup (Tool => GPRtools.Inspect);

      Parser.Add_Argument
        (Group,
         Create (Name       => "--display",
                 Help       => "output formatting",
                 Delimiter  => Equal,
                 Parameter  => "json|json-compact|textual",
                 Default    => "json-compact"));

      --  implicit --no-project switch.
      Options.Add_Switch (GPR2.Options.No_Project);

      Parser.Get_Opt (Options);
   end Parse_Command_Line;

begin
   --  Install the Ctrl-C handler

   GPRtools.Interrupt_Handler.Install_Sigint (GPRtools.Sigint.Handler'Access);

   --  Set program name

   GPRtools.Util.Set_Program_Name ("gprdoc");

   --  Run the GPRdoc main procedure depending on command line options

   Parse_Command_Line;
   GPRdoc.Process (Options => Options);

   return To_Exit_Status (E_Success);
exception
   when E : GPR2.Options.Usage_Error =>
      Handle_Program_Termination
        (Display_Command_Line_Help => True,
         Force_Exit                => False,
         Message                   => Exception_Message (E));
      return To_Exit_Status (E_Fatal);

   when E_Program_Termination =>
      return To_Exit_Status (E_Fatal);

   when E : others =>
      Handle_Program_Termination
        (Force_Exit => False,
         Exit_Cause => E_Generic,
         Message    => Exception_Message (E));
      return To_Exit_Status (E_Fatal);
end GPRdoc.Main;
