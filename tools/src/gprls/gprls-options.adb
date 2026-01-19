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

with Ada.Exceptions;
with Ada.Text_IO;
with GPR2.Options;

with GPRtools.Command_Line;

package body GPRls.Options is

   use Ada;

   use type Path_Name.Object;

   function Get_Files_From_List_File
     (File : GPR2.Path_Name.Object) return GPR2.Containers.Value_Set;

   procedure On_Switch
     (Parser : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res    : not null access GPRtools.Command_Line.Command_Line_Result'Class;
      Arg    : GPRtools.Command_Line.Switch_Type;
      Index  : String;
      Param  : String);

   ---------------------
   -- Append_Argument --
   ---------------------

   overriding procedure Append_Argument
     (Result : in out GPRls.Options.Object; Value : GPR2.Value_Type) is
   begin
      if not Result.On_Extra_Arg (Value) then
         Result.Args.Include (Value);
      end if;
   end Append_Argument;

   function Get_Files_From_List_File
     (File : GPR2.Path_Name.Object) return GPR2.Containers.Value_Set
   is
      use Ada.Text_IO;

      F   : File_Type;
      Ret : GPR2.Containers.Value_Set;
   begin
      Open (F, In_File, File.String_Value);
      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
         begin
            if Line /= "" then
               Ret.Include (Line);
            end if;
         end;
      end loop;

      Close (F);
      return Ret;
   exception
      when others =>
         raise GPR2.Options.Usage_Error
           with "Could not read file '" & String (File.Name) & "'";
   end Get_Files_From_List_File;

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
      if Arg = "-files" then
         Result.List_File := Path_Name.Create_File (Filename_Type (Param));

      elsif Arg = "-vP" then
         if Param = "0" then
            Result.Project_Parsing_Verbosity := 0;
         elsif Param = "1" then
            Result.Project_Parsing_Verbosity := 1;
         elsif Param = "2" then
            Result.Project_Parsing_Verbosity := 2;
         else
            raise GPR2.Options.Usage_Error
              with "verbosity level must be 0, 1 or 2";
         end if;

      elsif Arg = "-a" then
         Result.With_Predefined_Units := True;

         if Param = "0" then
            Result.Hide_Predefined_Path := True;
         elsif Param /= "default" then
            raise GPR2.Options.Usage_Error with "use -a or -a0 only";
         end if;

      elsif Arg = "-u" then
         Result.Selective_Output (Units) := True;

      elsif Arg = "-s" then
         Result.Selective_Output (Sources) := True;
         Result.Source_Parser := True;

      elsif Arg = "-o" then
         Result.Selective_Output (Objects) := True;

      elsif Arg = "-d" then
         Result.Dependency_Mode := True;

      elsif Arg = "--closure" then
         Result.Closure_Mode := True;

      elsif Arg = "-U" then
         Result.All_Projects := True;

      elsif Arg = "--source-parser" then
         Result.Source_Parser := True;

      elsif Arg = "--hide-status" then
         Result.Hide_Status := True;

      elsif Arg = "-V" then
         Result.Gnatdist := True;

      else
         raise GPR2.Options.Usage_Error
           with "unexpected switch " & String (Arg);
      end if;
   end On_Switch;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line
     (Parser : GPRtools.Options.Command_Line_Parser; Options : in out Object)
   is
   begin
      Parser.Get_Opt (Options);

      if Options.List_File.Is_Defined then
         if not Options.List_File.Exists then
            raise GPR2.Options.Usage_Error
              with String (Options.List_File.Value) & "does not exist";
         else
            Options.Args.Union (Get_Files_From_List_File (Options.List_File));
         end if;
      end if;

      if Options.Gnatdist then
         Options.Closure_Mode := False;
         Options.Dependency_Mode := False;

         if not Options.Files.Is_Empty then
            Options.All_Projects := True;
         end if;

      elsif Options.Closure_Mode then
         --  Closure mode has precedence over dependency mode
         Options.Dependency_Mode := False;
      end if;
   end Parse_Command_Line;

   ------------------------------
   -- Get_Files_From_List_File --
   ------------------------------

   -----------
   -- Print --
   -----------

   procedure Print (Self : Object) is
   begin
      Text_IO.Put_Line ("Files:");

      for F of Self.Files loop
         Text_IO.Put_Line ("   " & F);
      end loop;

      Text_IO.Put_Line
        ("Project file: " & Self.Tree.Root_Project.Path_Name.String_Value);

      Text_IO.Put_Line ("Project search path:");

      for P of Self.Tree.Project_Search_Paths loop
         Text_IO.Put_Line ("   " & P.String_Value);
      end loop;

      Text_IO.Put_Line ("Target: " & String (Self.Target));

      declare
         RTS_Map : constant GPR2.Containers.Lang_Value_Map := Self.RTS_Map;
      begin
         for R in RTS_Map.Iterate loop
            declare
               Lang : constant Language_Id :=
                 GPR2.Containers.Lang_Value_Maps.Key (R);
            begin
               Text_IO.Put_Line
                 ("RTS"
                  & (if Lang = Ada_Language
                     then ""
                     else '(' & Image (Lang) & ')')
                  & ": "
                  & RTS_Map (R));
            end;
         end loop;
      end;

      if Self.List_File /= Path_Name.Undefined then
         Text_IO.Put_Line ("List file: " & Self.List_File.String_Value);
      end if;

      Text_IO.Put_Line ("Project context:");

      for Curs in Self.Project_Context.Iterate loop
         declare
            K : constant External_Name_Type :=
              GPR2.Context.Key_Value.Key (Curs);
            V : constant Value_Type := GPR2.Context.Key_Value.Element (Curs);
         begin
            Text_IO.Put_Line ("   " & String (K) & " => " & V);
         end;
      end loop;

      Text_IO.Put_Line
        ("With predefined units: " & Self.With_Predefined_Units'Img);
      Text_IO.Put_Line ("Print units: " & Self.Print_Units'Img);
      Text_IO.Put_Line ("Print sources: " & Self.Print_Sources'Img);
      Text_IO.Put_Line ("Print object files: " & Self.Print_Object_Files'Img);
      Text_IO.Put_Line ("Dependency mode: " & Self.Dependency_Mode'Img);
      Text_IO.Put_Line ("Closure mode: " & Self.Closure_Mode'Img);
      Text_IO.Put_Line ("All projects: " & Self.All_Projects'Img);
      Text_IO.Put_Line
        ("Project parsing verbosity: " & Self.Project_Parsing_Verbosity'Img);

      Text_IO.Put_Line ("Verbosity: " & Self.Verbosity'Img);
   end Print;

   -----------
   -- Setup --
   -----------

   procedure Setup (Parser : out GPRtools.Options.Command_Line_Parser) is
      use GPRtools.Command_Line;

      Ls_Group : Argument_Group;
   begin
      Parser :=
        GPRtools.Options.Create
          ("2025",
           Cmd_Line         => "[-P<proj>|<proj.gpr>] [opts] [object files]",
           Allow_No_Project => False,
           Allow_Quiet      => False);
      GPRtools.Options.Setup (GPRtools.Ls);

      Ls_Group :=
        Parser.Add_Argument_Group
          ("list", On_Switch'Access, Help => "gprls specific switches.");

      Parser.Add_Argument
        (Ls_Group,
         Create
           ("-files",
            Help      => "File to list are contained in <file>",
            Delimiter => Equal,
            Parameter => "<file>"));
      Parser.Add_Argument
        (Ls_Group,
         Create
           ("-vP",
            Help      =>
              "Use <level> verbosity (0 .. 2) for the" & " project parsing",
            Delimiter => None,
            Parameter => "<level>"));
      Parser.Add_Argument
        (Ls_Group,
         Create
           ("-a",
            Help      =>
              "Include predefined units. Use -a0 to hide "
              & "predefined sources directory",
            Delimiter => None,
            Parameter => "0",
            Default   => "default"));
      Parser.Add_Argument
        (Ls_Group, Create ("-u", Help => "Print unit names"));
      Parser.Add_Argument (Ls_Group, Create ("-s", Help => "Print sources"));
      Parser.Add_Argument
        (Ls_Group, Create ("-o", Help => "Print object files"));
      Parser.Add_Argument
        (Ls_Group,
         Create ("-d", Help => "Print source dependencies with status"));
      Parser.Add_Argument
        (Ls_Group, Create ("--closure", Help => "Closure mode"));
      Parser.Add_Argument
        (Ls_Group, Create ("-U", Help => "Browse the entire project tree"));
      Parser.Add_Argument
        (Ls_Group,
         Create
           ("--source-parser",
            Help =>
              "Use gprls's Ada source parser to retrieve the "
              & "dependencies"));
      Parser.Add_Argument
        (Ls_Group,
         Create
           ("--hide-status",
            Help =>
              "Do not print artifact status (OK, DIF)"));
      Parser.Add_Argument
        (Ls_Group, Create ("-V", Help => "", Hidden => True));

   exception
      when E : GPR2.Options.Usage_Error =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "gprls: " & Ada.Exceptions.Exception_Message (E));
         GPRtools.Command_Line.Try_Help;
   end Setup;
end GPRls.Options;
