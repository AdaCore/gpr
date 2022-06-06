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

with Ada;
with Ada.Exceptions;
with Ada.Text_IO;

with GPR2.Interrupt_Handler;

with GPRdoc.Process;

with GPRtools;
with GPRtools.Command_Line;
with GPRtools.Options;
with GPRtools.Sigint;
with GPRtools.Util;

procedure GPRdoc.Main is

   use Ada;
   use Ada.Exceptions;

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
      Result.Verbosity := GPRtools.Quiet;
      --  We want a clean output to be JSON compliant

      if Arg = "--display" then
         if Param = "json" then
            Options.Kind_Of_Display := GPRtools.K_JSON;
         elsif Param = "json-compact" then
            Options.Kind_Of_Display := GPRtools.K_JSON_Compact;
         elsif Param = "textual" then
            Options.Kind_Of_Display := GPRtools.K_Textual_IO;
         else
            raise GPRtools.Usage_Error with "use --display=<value> "
              & "with <value>=[json, json-compact, textual]";
         end if;
      end if;

   end On_Switch;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line
   is
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
      Options.No_Project := True;

      Parser.Get_Opt (Options);
   end Parse_Command_Line;

begin
   --  Install the Ctrl-C handler

   GPR2.Interrupt_Handler.Install_Sigint (GPRtools.Sigint.Handler'Access);

   --  Set program name

   GPRtools.Util.Set_Program_Name ("gprdoc");

   --  Run the GPRdoc main procedure depending on command line options

   Parse_Command_Line;
   GPRdoc.Process (Options => Options);

exception
   when E : others =>
      Text_IO.Put_Line ("error: " & Exception_Information (E));
end GPRdoc.Main;
