------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;

with GNATCOLL.Traces;

with GPR2.Containers;

with GPRtools;
with GPRtools.Command_Line;
with GPRtools.Util;
with GPRtools.Options;

with GPRinspect.Process;

procedure GPRinspect.Main is

   use Ada;

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
         elsif Param = "json-compact" then
            Result.Kind_Of_Display := GPRtools.K_JSON_Compact;
         elsif Param = "textual" then
            Result.Kind_Of_Display := GPRtools.K_Textual_IO;
         else
            raise GPRtools.Usage_Error with "use --display=<value> "
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

      Parser.Get_Opt (Options);
   end Parse_Command_Line;

begin

   GNATCOLL.Traces.Parse_Config_File;

   --  Set program name

   GPRtools.Util.Set_Program_Name ("gprinspect");

   --  Run the GPRinspect main procedure depending on command line options

   Parse_Command_Line;
   GPRinspect.Process (Options => Options);

exception
   when E : GPRtools.Usage_Error =>
      Text_IO.Put_Line
        (Text_IO.Standard_Error,
         "gprinspect: " & Exception_Message (E));
      GPRtools.Command_Line.Try_Help;
      GPRtools.Util.Exit_Program (GPRtools.Util.E_Fatal);

   when E : others =>
      GPRtools.Util.Fail_Program
        ("Fatal error: " & Exception_Information (E));
end GPRinspect.Main;
