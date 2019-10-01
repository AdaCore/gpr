------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.OS_Lib;

with GPR2;
with GPR2.Project;

with GPRtools.Util;

separate (GPRls.Options)
procedure Build_From_Command_Line (Self : in out Object) is

   use GNAT;
   use GNAT.Command_Line;

   use GPRtools.Util;

   use type GNAT.OS_Lib.String_Access;

   --  TODO: put relevant programs into the Options API

   procedure Add_Search_Path (Switch, Value : String);
   --  Add Value to project search path (-aP option)

   procedure Add_Scenario_Variable (Switch, Value : String);
   --  Add a scenario variable (-X option)

   function Get_Files_From_List_File
     (File : Path_Name.Object) return String_Vector.Vector;
   --  Add files from a list file
   --  ??? This is almost a duplicate of:
   --     GPRname.Section.Prepare.Get_Source_Dirs_From_File
   --  (make it a generic GPR2 utility?)

   procedure Handle_List_File (Switch, Value : String);
   --  Read a list file and interpret each line as a new element to Self.Files

   procedure Set_Verbose (Switch, Value : String);

   procedure Set_Project (Switch, Value : String);
   --  Set the project file

   procedure Set_Print_Units (Switch, Value : String);
   procedure Set_Print_Object_Files (Switch, Value : String);
   procedure Set_Print_Sources (Switch, Value : String);
   procedure Set_Target (Switch, Value : String);

   procedure Set_Selective_Output;

   ---------------------------
   -- Add_Scenario_Variable --
   ---------------------------

   procedure Add_Scenario_Variable (Switch, Value : String) is
      pragma Unreferenced (Switch);
      I : constant Natural := Strings.Fixed.Index (Value, "=");
   begin
      if I = 0 then
         Self.Project_Context.Include (Name_Type (Value), "");
      else
         Self.Project_Context.Include
           (Name_Type (Value (Value'First .. I - 1)),
            Value (I + 1 .. Value'Last));
      end if;
   end Add_Scenario_Variable;

   ---------------------
   -- Add_Search_Path --
   ---------------------

   procedure Add_Search_Path (Switch, Value : String) is
      pragma Unreferenced (Switch);
   begin
      Self.Project_Search_Paths.Append
        (Path_Name.Create_Directory (Name_Type (Value)));
   end Add_Search_Path;

   ------------------------------
   -- Get_Files_From_List_File --
   ------------------------------

   function Get_Files_From_List_File
     (File : Path_Name.Object) return String_Vector.Vector
   is
      use Ada.Text_IO;

      F   : File_Type;
      Ret : String_Vector.Vector;
   begin
      Open (F, In_File, File.Value);
      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
         begin
            Ret.Append (Line);
         end;
      end loop;
      Close (F);
      return Ret;
   exception
      when others =>
         Finish_Program
           (E_Errors, "Could not read file '" & String (File.Name) & "'");
         return String_Vector.Empty_Vector;
   end Get_Files_From_List_File;

   ----------------------
   -- Handle_List_File --
   ----------------------

   procedure Handle_List_File (Switch, Value : String) is
      pragma Unreferenced (Switch);
   begin
      Self.List_File := Path_Name.Create_File (Name_Type (Value));
   end Handle_List_File;

   ----------------------------
   -- Set_Print_Object_Files --
   ----------------------------

   procedure Set_Print_Object_Files (Switch, Value : String) is
      pragma Unreferenced (Switch, Value);
   begin
      Set_Selective_Output;
      Self.Print_Object_Files := True;
   end Set_Print_Object_Files;

   -----------------------
   -- Set_Print_Sources --
   -----------------------

   procedure Set_Print_Sources (Switch, Value : String) is
      pragma Unreferenced (Switch, Value);
   begin
      Set_Selective_Output;
      Self.Print_Sources := True;
   end Set_Print_Sources;

   ---------------------
   -- Set_Print_Units --
   ---------------------

   procedure Set_Print_Units (Switch, Value : String) is
      pragma Unreferenced (Switch, Value);
   begin
      Set_Selective_Output;
      Self.Print_Units := True;
   end Set_Print_Units;

   -----------------
   -- Set_Project --
   -----------------

   procedure Set_Project (Switch, Value : String) is
      pragma Unreferenced (Switch);
   begin
      if Value /= GPR2.No_Value then
         Self.Project_File := Project.Create (Name_Type (Value));
      end if;
   end Set_Project;

   --------------------------
   -- Set_Selective_Output --
   --------------------------

   procedure Set_Selective_Output is
   begin
      if not Self.Selective_Output then
         Self.Selective_Output := True;
         Self.Print_Units := False;
         Self.Print_Sources := False;
         Self.Print_Object_Files := False;
      end if;
   end Set_Selective_Output;

   ----------------
   -- Set_Target --
   ----------------

   procedure Set_Target (Switch, Value : String) is
      pragma Unreferenced (Switch);
   begin
      Self.Target := To_Unbounded_String (Value);
   end Set_Target;

   ---------------------
   -- Set_Low_Verbose --
   ---------------------

   procedure Set_Verbose (Switch, Value : String) is
      pragma Unreferenced (Switch, Value);
   begin
      if Self.Verbosity = None then
         Self.Verbosity := Low;
      end if;
   end Set_Verbose;

   Config : Command_Line_Configuration;

   Tmp_RTS : aliased OS_Lib.String_Access;

begin
   Define_Switch
     (Config, Self.Usage_Needed'Unrestricted_Access,
      "-h", Long_Switch => "--help",
      Help => "Display this help message and exit");

   Define_Switch
     (Config, Self.Version_Needed'Unrestricted_Access,
      Long_Switch => "--version",
      Help => "Display version and exit");

   Define_Switch
     (Config, Set_Project'Unrestricted_Access,
      "-P:",
      Help => "Path to the project to browse");

   Define_Switch
     (Config, Add_Search_Path'Unrestricted_Access,
      "-aP:",
      Help     => "Add directory <dir> to project search path",
      Argument => "<dir>");

   Define_Switch
     (Config, Set_Target'Unrestricted_Access,
      Long_Switch => "--target=",
      Help        => "Specify a target for cross platforms",
      Argument    => "<name>");

   Define_Switch
     (Config, Tmp_RTS'Access,
      "--RTS=",
      Help     => "Use runtime <runtime> for language Ada",
      Argument => "<runtime>");

   Define_Switch
     (Config, Handle_List_File'Unrestricted_Access,
      "-files=",
      Help     => "File containing the list of ???",
      Argument => "<file>");

   Define_Switch
     (Config, Add_Scenario_Variable'Unrestricted_Access,
      "-X!",
      Help     => "Add scenario variable",
      Argument => "<NAME>=<VALUE>");

   Define_Switch
     (Config, Set_Verbose'Unrestricted_Access,
      "-v",
      Help => "Set verbose");

   Define_Switch
     (Config, Self.Verbose_Parsing'Unrestricted_Access,
      "-vP!",
      Help     => "Use <level> verbosity (0..2) for the project parsing",
      Argument => "<level>");

   Define_Switch
     (Config, Self.With_Predefined_Units'Unrestricted_Access,
      "-a",
      Help => "Include predefined units");

   Define_Switch
     (Config, Set_Print_Units'Unrestricted_Access,
      "-u",
      Help => "Print unit names");

   Define_Switch
     (Config, Set_Print_Sources'Unrestricted_Access,
      "-s",
      Help => "Print sources");

   Define_Switch
     (Config, Set_Print_Object_Files'Unrestricted_Access,
      "-o",
      Help => "Print object files");

   Define_Switch
     (Config, Self.Dependency_Mode'Unrestricted_Access,
      "-d",
      Help => "For every source, also print its dependencies with status");

   Define_Switch
     (Config, Self.Closure_Mode'Unrestricted_Access,
      "--closure",
      Help => "Closure mode");

   Define_Switch
     (Config, Self.All_Projects'Unrestricted_Access,
      "-U",
      Help => "Browse the entire project tree");

   Getopt (Config, Concatenate => False);

   if Tmp_RTS /= null then
      Self.RTS := +Tmp_RTS.all;
      OS_Lib.Free (Tmp_RTS);
   end if;

   --  Now read the specified files from which we will browse, if any

   Read_Arguments : loop
      declare
         Arg : constant String := Get_Argument;
      begin
         exit Read_Arguments when Arg = "";
         Self.Files.Append (Arg);
      end;
   end loop Read_Arguments;

   --  Check parsing verbosity value

   if Self.Verbose_Parsing not in 0 | 1 | 2 then
      raise Usage_Error with "wrong parsing verbosity level";
   end if;

   --  Check that the list file, if defined, is readable

   if Self.List_File /= Path_Name.Undefined then
      if not Self.List_File.Exists then
         Finish_Program
           (E_Errors,
            "list file " & String (Self.List_File.Name) & " not found.");
      else
         Self.Files.Append (Get_Files_From_List_File (Self.List_File));
      end if;
   end if;

   --  Check that we have a project file to use.
   --  If not and there is no file on the command line, check if -v is provided
   --  so that we enter the "only display paths" mode.

   if Self.Project_File.Is_Defined then
      if not Self.Project_File.Exists then
         Finish_Program
           (E_Errors,
           "project file " & String (Self.Project_File.Name) & " not found.");
      end if;
   else
      if Self.Files.Is_Empty and then Self.Verbosity = Low then
         Self.Only_Display_Paths := True;
      end if;

      Self.Project_File :=
        Look_For_Default_Project
          (Self.Verbose_Parsing = 0, Implicit_Only => False);

      if not Self.Project_File.Is_Defined then
         Finish_Program
           (E_Errors,
            "no project file specified and no default project file.");
      end if;
   end if;

   --  The "very verbose" mode (when Verbosity is High) has its own special
   --  output, so we switch off closure and dependency modes.

   if Self.Verbosity = High then
      Self.Closure_Mode := False;
      Self.Dependency_Mode := False;

      if not Self.Files.Is_Empty then
         Self.All_Projects := True;
      end if;

   elsif Self.Closure_Mode then
      Self.Dependency_Mode := False;
      --  Closure mode has precedence over dependency mode
   end if;

   --  ??? Some of the legacy gprls preliminary code is missing, but
   --      nothing important.

end Build_From_Command_Line;
