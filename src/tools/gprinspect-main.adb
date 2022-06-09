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

with Ada.Command_Line;
with Ada.Text_IO;

with GNAT.Command_Line;

with GNATCOLL.Traces;

with GPRtools;
with GPRtools.Command_Line;
with GPRtools.Util;
with GPRtools.Options;

with GPR2.Message;
with GPR2.Project.Tree;

with GPRinspect.Process;

procedure GPRinspect.Main is

   use Ada;
   use GPR2;

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
   when GNAT.Command_Line.Invalid_Switch
      | GNAT.Command_Line.Exit_From_Command_Line
      =>
      Command_Line.Set_Exit_Status (Command_Line.Failure);

   when others =>
      if Options.Tree.Has_Messages then
         for M of Options.Tree.Log_Messages.all loop
            Text_IO.Put_Line (M.Format);
         end loop;
      end if;

      Command_Line.Set_Exit_Status (Command_Line.Failure);
end GPRinspect.Main;
