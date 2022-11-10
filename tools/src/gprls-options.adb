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

with Ada.Exceptions;
with Ada.Text_IO;

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

   -----------------------------
   -- Build_From_Command_Line --
   -----------------------------

   function Build_From_Command_Line (Self : in out Object) return Boolean is
      use GPRtools.Command_Line;

      Parser : GPRtools.Options.Command_Line_Parser :=
                 GPRtools.Options.Create
                   ("2018",
                    Cmd_Line         =>
                                "[-P<proj>|<proj.gpr>] [opts] [object files]",
                    Allow_No_Project => False,
                    Allow_Quiet      =>  False);
      Ls_Group : Argument_Group;
   begin
      GPRtools.Options.Setup (GPRtools.Ls);

      Ls_Group := Parser.Add_Argument_Group
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
            Help      => "Use <level> verbosity (0 .. 2) for the" &
                         " project parsing",
            Delimiter => None,
            Parameter => "<level>"));
      Parser.Add_Argument
        (Ls_Group,
         Create
           ("-a",
            Help      => "Include predefined units. Use -a0 to hide " &
                         "predefined sources directory",
            Delimiter => None,
            Parameter => "0",
            Default   => "default"));
      Parser.Add_Argument
        (Ls_Group,
         Create
           ("-u",
            Help => "Print unit names"));
      Parser.Add_Argument
        (Ls_Group,
         Create
           ("-s",
            Help => "Print sources"));
      Parser.Add_Argument
        (Ls_Group,
         Create
           ("-o",
            Help => "Print object files"));
      Parser.Add_Argument
        (Ls_Group,
         Create
           ("-d",
            Help => "Print source dependencies with status"));
      Parser.Add_Argument
        (Ls_Group,
         Create
           ("--closure",
            Help => "Closure mode"));
      Parser.Add_Argument
        (Ls_Group,
         Create
           ("-U",
            Help => "Browse the entire project tree"));
      Parser.Add_Argument
        (Ls_Group,
         Create
           ("--source-parser",
            Help => "Use gprls's Ada source parser to retrieve the " &
                    "dependencies"));
      Parser.Add_Argument
        (Ls_Group,
         Create
           ("-V",
            Help   => "",
            Hidden => True));

      Parser.Get_Opt (Self);

      if Self.List_File.Is_Defined then
         if not Self.List_File.Exists then
            raise GPRtools.Usage_Error with String (Self.List_File.Value) &
              "does not exist";
         else
            Self.Args.Union (Get_Files_From_List_File (Self.List_File));
         end if;
      end if;

      if not Self.Project_Is_Defined
        and then Self.Files.Is_Empty
        and then Self.Verbose
      then
         Self.Only_Display_Paths := True;
      end if;

      if Self.Gnatdist then
         Self.Closure_Mode := False;
         Self.Dependency_Mode := False;

         if not Self.Files.Is_Empty then
            Self.All_Projects := True;
         end if;

      elsif Self.Closure_Mode then
         --  Closure mode has precedence over dependency mode
         Self.Dependency_Mode := False;
      end if;

      return True;

   exception
      when E : GPRtools.Usage_Error =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "gprls: " & Ada.Exceptions.Exception_Message (E));
         GPRtools.Command_Line.Try_Help;

         return False;
   end Build_From_Command_Line;

   ------------------------------
   -- Get_Files_From_List_File --
   ------------------------------

   function Get_Files_From_List_File
     (File : GPR2.Path_Name.Object) return GPR2.Containers.Value_Set
   is
      use Ada.Text_IO;

      F   : File_Type;
      Ret : GPR2.Containers.Value_Set;
   begin
      Open (F, In_File, File.Value);
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
         raise GPRtools.Usage_Error with
           "Could not read file '" & String (File.Name) & "'";
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
         declare
            Verbose_Parsing : Integer := 0;
         begin
            Verbose_Parsing := Natural'Value (Param);

            Result.Verbosity :=
              (case Verbose_Parsing is
                  when 0      => GPRtools.Regular,
                  when 1      => GPRtools.Verbose,
                  when 2      => GPRtools.Very_Verbose,
                  when others => raise GPRtools.Usage_Error with
                                   "-vP accepts a value in the range 0 .. 2");
         exception
            when Constraint_Error =>
               raise GPRtools.Usage_Error with
                 "-vP accepts a value in the range 0 .. 2";
         end;

      elsif Arg = "-a" then
         Result.With_Predefined_Units := True;

         if Param = "0" then
            Result.Hide_Predefined_Path := True;
         elsif Param /= "default" then
            raise GPRtools.Usage_Error with "use -a or -a0 only";
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

      elsif Arg = "-V" then
         Result.Gnatdist := True;

      else
         raise GPRtools.Command_Line.Command_Line_Definition_Error
           with "unexpected switch " & String (Arg);
      end if;
   end On_Switch;

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
        ("Project file: " & Self.Tree.Root_Project.Path_Name.Value);

      Text_IO.Put_Line ("Project search path:");

      for P of Self.Tree.Project_Search_Paths loop
         Text_IO.Put_Line ("   " & P.Value);
      end loop;

      Text_IO.Put_Line ("Target: " & To_String (Self.Target));

      for R in Self.RTS_Map.Iterate loop
         declare
            Lang : constant Language_Id :=
                     GPR2.Containers.Lang_Value_Maps.Key (R);
         begin
            Text_IO.Put_Line
              ("RTS"
               & (if Lang = Ada_Language then ""
                 else '(' & Image (Lang) & ')')
               & ": " & Self.RTS_Map (R));
         end;
      end loop;

      if Self.List_File /= Path_Name.Undefined then
         Text_IO.Put_Line ("List file: " & Self.List_File.Value);
      end if;

      Text_IO.Put_Line ("Project context:");

      for Curs in Self.Project_Context.Iterate loop
         declare
            K : constant Name_Type := Context.Key_Value.Key (Curs);
            V : constant Value_Type := Context.Key_Value.Element (Curs);
         begin
            Text_IO.Put_Line ("   " & String (K) & " => " & V);
         end;
      end loop;

      Text_IO.Put_Line ("With predefined units: " &
                          Self.With_Predefined_Units'Img);
      Text_IO.Put_Line ("Print units: " & Self.Print_Units'Img);
      Text_IO.Put_Line ("Print sources: " & Self.Print_Sources'Img);
      Text_IO.Put_Line ("Print object files: " & Self.Print_Object_Files'Img);
      Text_IO.Put_Line ("Dependency mode: " & Self.Dependency_Mode'Img);
      Text_IO.Put_Line ("Closure mode: " & Self.Closure_Mode'Img);
      Text_IO.Put_Line ("All projects: " & Self.All_Projects'Img);

      Text_IO.Put_Line ("Verbosity: " & Self.Verbosity'Img);
      Text_IO.Put_Line
        ("Parsing verbosity: "
         & (case Self.Verbosity is
              when GPRtools.Quiet        => "0",
              when GPRtools.Regular      => "0",
              when GPRtools.Verbose      => "1",
              when GPRtools.Very_Verbose => "2"));
   end Print;

end GPRls.Options;
