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

with Ada.Text_IO;

with GNAT.Command_Line;

with GPR2;

with GPRtools.Util;

separate (GPRls.Options)
procedure Build_From_Command_Line (Self : in out Object) is

   use GNAT;
   use GNAT.Command_Line;

   use GPRtools.Util;

   --  TODO: put relevant programs into the Options API

   function Get_Files_From_List_File
     (File : Path_Name.Object) return GPR2.Containers.Value_Set;
   --  Add files from a list file
   --  ??? This is almost a duplicate of:
   --     GPRname.Section.Prepare.Get_Source_Dirs_From_File
   --  (make it a generic GPR2 utility?)

   procedure Handle_List_File (Switch, Value : String);
   --  Read a list file and interpret each line as a new element to Self.Files

   procedure Set_Print_Units (Switch, Value : String);
   procedure Set_Print_Object_Files (Switch, Value : String);
   procedure Set_Print_Sources (Switch, Value : String);

   procedure Set_Selective_Output;

   ------------------------------
   -- Get_Files_From_List_File --
   ------------------------------

   function Get_Files_From_List_File
     (File : Path_Name.Object) return GPR2.Containers.Value_Set
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
         Finish_Program
           (E_Errors, "Could not read file '" & String (File.Name) & "'");
         return GPR2.Containers.Value_Type_Set.Empty_Set;
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

   Config : Command_Line_Configuration renames Self.Config;

begin
   Self.Setup (Tool => GPRtools.Ls);

   Define_Switch
     (Config, Handle_List_File'Unrestricted_Access,
      "-files=",
      Help     => "File containing the list of ???",
      Argument => "<file>");

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

   --  Now read the specified files from which we will browse, if any

   Self.Read_Remaining_Arguments (Tool => GPRtools.Ls);

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
         Self.Args.Union (Get_Files_From_List_File (Self.List_File));
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
      if Self.Files.Is_Empty and then Self.Verbose then
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

   if Self.Closure_Mode then
      Self.Dependency_Mode := False;
      --  Closure mode has precedence over dependency mode
   end if;

   --  ??? Some of the legacy gprls preliminary code is missing, but
   --      nothing important.

end Build_From_Command_Line;
