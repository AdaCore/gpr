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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GPR2.Compilation.Registry;
with GPR2.Path_Name;
with GPR2.Project.Registry.Pack;

with GPRtools.Command_Line;

package body GPRclean.Options is

   procedure On_Switch
     (Parser : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res    : not null access GPRtools.Command_Line.Command_Line_Result'Class;
      Arg    : GPRtools.Command_Line.Switch_Type;
      Index  : String;
      Param  : String);

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
      Result : constant access Object := Object (Res.all)'Access;
   begin
      if Arg = "-r" then
         Result.All_Projects := True;
      elsif Arg = "-n" then
         Result.Dry_Run := True;
      elsif Arg = "--autoconf" then
         Result.Config_Project :=
           GPR2.Path_Name.Create_File (GPR2.Filename_Type (Param));
         Result.Remove_Config  := True;
      elsif Arg = "-c" then
         Result.Remain_Useful := True;
      elsif Arg = "-p" then
         Result.Remove_Empty_Dirs := True;
      elsif Arg = "-f" then
         Result.Force_Deletions := True;
      end if;
   end On_Switch;

   ------------------------------
   -- Parse_Attribute_Switches --
   ------------------------------

   procedure Parse_Attribute_Switches
     (Parser  : GPRtools.Options.Command_Line_Parser;
      Options : in out Object;
      Values  : GPR2.Containers.Source_Value_List)
   is
      package PRP renames GPR2.Project.Registry.Pack;
   begin
      Parser.Get_Opt
        (From_Pack => PRP.Clean, Values => Values, Result => Options);
   end Parse_Attribute_Switches;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line
     (Parser       : GPRtools.Options.Command_Line_Parser;
      Options      : in out Object)
   is
      use Ada.Strings.Unbounded;

   begin
      Parser.Get_Opt (Options);

      --  Now read arguments

      for Arg of Options.Args loop
         Options.Mains.Insert (Filename_Type (Arg));
      end loop;

      Options.Arg_Mains := not Options.Mains.Is_Empty;

      if Options.Slave_Env = Null_Unbounded_String
        and then Options.Distributed_Mode
      then
         Options.Slave_Env := To_Unbounded_String
           (GPR2.Compilation.Registry.Compute_Env
              (Options.Tree.all, Options.Slave_Env_Auto));

         if Options.Slave_Env_Auto and then Options.Verbose then
            Ada.Text_IO.Put_Line
              ("slave environment is " & To_String (Options.Slave_Env));
         end if;
      end if;
   end Parse_Command_Line;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Parser : out GPRtools.Options.Command_Line_Parser)
   is
      use GPRtools.Command_Line;
      Clean_Group : GPRtools.Command_Line.Argument_Group;
   begin
      GPRtools.Options.Setup (GPRtools.Clean);

      Parser := GPRtools.Options.Create
        ("2018",
         Cmd_Line          => "[-P<proj>|<proj.gpr>] [opts] [mains]",
         Help              => "'mains' being zero or more file names",
         Allow_Distributed => True,
         Allow_Autoconf    => False);

      Clean_Group := Parser.Add_Argument_Group
        ("clean",
         Callback => On_Switch'Access,
         Help     => "gprclean specific switches.");

      Parser.Add_Argument
        (Clean_Group,
         Create
           ("-r",
            Help      => "Clean all projects recursively"));
      Parser.Add_Argument
        (Clean_Group,
         Create
           ("-n",
            Help      => "Nothing to do: only list files to delete"));
      Parser.Add_Argument
        (Clean_Group,
         Create
           (Name           =>  "--autoconf",
            Help           =>  "Specify generated config project file name",
            In_Switch_Attr => False,
            Delimiter      =>  Equal,
            Parameter      =>  "file.cgpr"));
      Parser.Add_Argument
        (Clean_Group,
         Create
           ("-c",
            Help      => "Only delete compiler generated files"));
      Parser.Add_Argument
        (Clean_Group,
         Create
           ("-p",
            Help      => "Remove empty build directories"));
      Parser.Add_Argument
        (Clean_Group,
         Create
           ("-f",
            Help      => "Force deletions of unwritable files"));
   end Setup;

end GPRclean.Options;
