------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2022-2024, AdaCore                     --
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
with Ada.Exceptions; use Ada.Exceptions;

with GNATCOLL.Traces;

with GPR2.Containers;
with GPR2.Options;

with GPRtools;
with GPRtools.Command_Line;
with GPRtools.Util;
with GPRtools.Options;
with GPRtools.Program_Termination;

with GPRinspect.External_Tools_Support;
with GPRinspect.Process;

function GPRinspect.Main return Ada.Command_Line.Exit_Status is

   use Ada;
   use GPR2;

   use GPRtools.Program_Termination;

   Options : GPRinspect.GPRinspect_Options;

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

      Result : constant access GPRinspect_Options :=
                 GPRinspect_Options (Res.all)'Access;
   begin
      if Arg = "--display" then
         if Param = "json" then
            Result.Kind_Of_Display := GPRtools.K_JSON;
            Result.Verbosity := GPRtools.Quiet;
         elsif Param = "json-compact" then
            Result.Kind_Of_Display := GPRtools.K_JSON_Compact;
            Result.Verbosity := GPRtools.Quiet;
         elsif Param = "textual" then
            Result.Kind_Of_Display := GPRtools.K_Textual_IO;
         else
            raise GPR2.Options.Usage_Error with "use --display=<value> "
              & "with <value>=[json, json-compact, textual]";
         end if;

      elsif Arg = "-r" then
         Result.All_Projects := True;

      elsif Arg = "--all" then
         Result.Display_Everything := True;

      elsif Arg = "--attributes" then
         Result.Display_Attributes := True;

      elsif Arg = "-c" then
         Result.Display_Config_Attributes := True;

      elsif Arg = "--packages" then
         Result.Display_Packages := True;

      elsif Arg = "--variables" then
         Result.Display_Variables := True;

      elsif Arg = "--views" then
         declare
            Scope : Restricted_Scope (Restrict => True);
         begin
            Scope.Views := GPR2.Containers.Create (GPR2.Name_Type (Param),
                                                   Separator => ",");
            Result.All_Projects := True;
            Result.Restricted_Views := Scope;
         end;

      elsif Arg = "--gpr-registry-file" then
         if Param /= "" then
            Result.Gpr_Registry_File :=
              GPR2.Path_Name.Create_File (GPR2.Filename_Type (Param));
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
                   (Initial_Year => "2022",
                    Allow_No_Project => False,
                    Allow_Quiet      => False);
      Group  : constant GPRtools.Command_Line.Argument_Group :=
                 Parser.Add_Argument_Group
                   ("gprinspect", On_Switch'Unrestricted_Access);

   begin
      Setup (Tool => GPRtools.Inspect);

      Parser.Add_Argument
        (Group,
         Create (Name       => "--display",
                 Help       => "output formatting",
                 Delimiter  => Equal,
                 Parameter  => "json|json-compact|textual",
                 Default    => "textual"));
      Parser.Add_Argument
        (Group,
         Create (Name           => "--views",
                 Help           => "Select the view to display. Only "
                 & "available when using [--display=textual].",
                 In_Switch_Attr => False,
                 Delimiter      => Equal,
                 Parameter      => "view1[,view2]",
                 Default        => ""));
      Parser.Add_Argument
        (Group,
         Create ("-r", "--recursive",
           Help => "All none external projects recursively"));
      Parser.Add_Argument
        (Group,
         Create ("--all",
           Help => "Display everything"));
      Parser.Add_Argument
        (Group,
         Create ("--attributes",
           Help => "Display attributes"));
      Parser.Add_Argument
        (Group,
         Create ("-c", "--from-config",
           Help => "Display attributes inherited from configuration"));
      Parser.Add_Argument
        (Group,
         Create ("--packages",
           Help => "Display packages"));
      Parser.Add_Argument
        (Group,
         Create ("--variables",
           Help => "Display variables & types"));
      Parser.Add_Argument
        (Group,
         Create
           ("--gpr-registry-file",
            Help      => "Recognize additional attributes defined in <file>",
            Delimiter => Equal,
            Parameter => "<file>"));

      Parser.Get_Opt (Options);
   end Parse_Command_Line;

begin
   GNATCOLL.Traces.Parse_Config_File;

   --  Set program name

   GPRtools.Util.Set_Program_Name ("gprinspect");

   --  Run the GPRinspect main procedure depending on command line options

   Parse_Command_Line;

   GPRinspect.External_Tools_Support.Import_External_Tools_Registry
     (Options.Gpr_Registry_File);

   GPRinspect.Process (Options => Options);

   return To_Exit_Status (E_Success);
exception
   when E : GPR2.Options.Usage_Error =>
      Handle_Program_Termination
        (Opt                       => Options,
         Display_Command_Line_Help => True,
         Force_Exit                => False,
         Message                   => Exception_Message (E));
      return To_Exit_Status (E_Fatal);

   when E : Project_Error | Processing_Error =>
      Handle_Program_Termination
        (Opt                   => Options,
         Display_Tree_Messages => True,
         Force_Exit            => False,
         Message               => Exception_Message (E));
      return To_Exit_Status (E_Fatal);

   when E_Program_Termination =>
      return To_Exit_Status (E_Fatal);

   when E : others =>
      Handle_Program_Termination
        (Opt        => Options,
         Force_Exit => False,
         Exit_Cause => E_Generic,
         Message    => Exception_Message (E));
      return To_Exit_Status (E_Fatal);
end GPRinspect.Main;
