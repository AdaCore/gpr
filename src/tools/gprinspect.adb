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

with Ada.Calendar.Formatting;
with Ada.Command_Line;
with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.MD5;
with GNAT.OS_Lib;

with GNATCOLL.Traces;
with GNATCOLL.JSON;
with GNATCOLL.Utils;

with GPRtools.Util;
with GPRtools.Options;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.Typ.Set;
with GPR2.Project.Variable.Set;
with GPR2.Project.View.Set;
with GPR2.Version;
with GPR2.View_Ids;

procedure GPRinspect is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   use GNATCOLL;
   use GNATCOLL.JSON;
   use GPR2;

   procedure Parse_Command_Line;
   --  Parse command line parameters

   procedure Inspect_Project (Tree : Project.Tree.Object);
   --  Inspect project and possibly recursively all imports

   package Imported_By_Map is new
     Ada.Containers.Ordered_Maps
       (Project.View.Object,
        Project.View.Set.Object,
        Project.View."<",
        Project.View.Set."=");

   Imported_By  : Imported_By_Map.Map;
   Handled      : Project.View.Set.Object;

   --  Variables for tool's options

   Help                      : aliased Boolean := False;
   Version                   : aliased Boolean := False;
   JSON_Output               : aliased Boolean := False;
   All_Projects              : aliased Boolean := False;
   Display_Everything        : aliased Boolean := False;
   Display_Attributes        : aliased Boolean := False;
   Display_Config_Attributes : aliased Boolean := False;
   Display_Packages          : aliased Boolean := False;
   Display_Variables         : aliased Boolean := False;
   Project_Path              : Unbounded_String;
   Project_Tree              : GPR2.Project.Tree.Object;

   J_Res   : constant JSON_Value := Create_Object;
   --  The JSON response

   O_Array : GPR2.Containers.Value_Set;
   --  Object search-paths, global array with all project's object directories
   --  and the possible runtime object directory.
   S_Array : GPR2.Containers.Value_Set;
   --  Sources search-paths, global array with all project's sources
   --  directories.

   ---------------------
   -- Inspect_Project --
   ---------------------

   procedure Inspect_Project (Tree : Project.Tree.Object) is

      use GNAT;

      function Info_Object return JSON_Value;
      --  Information node (date, toolset version, ...)

      function Tree_Object return JSON_Value;
      --  Information node (date, toolset version, ...)

      procedure Parse_Project
        (Prjs : in out JSON_Array;
         View    : Project.View.Object;
         Parent  : Project.View.Object);
      --  Project information (name, kind, ...)

      function View_Id (View : Project.View.Object) return MD5.Message_Digest;
      --  Return the View Id as a MD5 sum

      function To_JSON_Array (A : GPR2.Containers.Value_Set) return JSON_Array;

      procedure Append_Source_Path
        (Set : in out GPR2.Containers.Value_Set; Value, Directory : String);
      --  Append source path to Set. If Value is not absolute path, prefix it
      --  with Directory. If Value ends with ** output all subdirectories.

      function Image (Kind : Project_Kind) return String;
      --  Returns the Kind only

      ------------------------
      -- Append_Source_Path --
      ------------------------

      procedure Append_Source_Path
        (Set : in out GPR2.Containers.Value_Set; Value, Directory : String)
      is
         use Ada.Directories;
         use GNATCOLL.Utils;
         use GNAT.OS_Lib;

         Recurse   : constant Boolean := Ends_With (Value, "**");
         Base_Path : constant String :=
                       Value (Value'First
                              .. Value'Last - (if Recurse then 2 else 0));

         function With_Last_DS (Path : String) return String is
           (if Path /= "" and then Is_Directory_Separator (Path (Path'Last))
            then Path else Path & Directory_Separator);

         Path : constant String :=
                  (if Is_Absolute_Path (Base_Path) then Base_Path
                   elsif Base_Path (Base_Path'First) = '.'
                     and then
                     (Base_Path'Length = 1
                      or else (Base_Path'Length = 2
                               and then Is_Directory_Separator
                                          (Base_Path (Base_Path'Last))))
                   then Directory
                   else With_Last_DS (Directory) & Base_Path);

         procedure Search_In (Path : String);

         procedure Process (Item : Directory_Entry_Type);

         -------------
         -- Process --
         -------------

         procedure Process (Item : Directory_Entry_Type) is
         begin
            if Ada.Directories.Simple_Name (Item) not in "." | ".." then
               Search_In (Full_Name (Item));
            end if;
         end Process;

         ---------------
         -- Search_In --
         ---------------

         procedure Search_In (Path : String) is
         begin
            Set.Include (Path);

            Search
              (Path, "",
               Filter  => (Ada.Directories.Directory => True, others => False),
               Process => Process'Access);
         end Search_In;

      begin
         if Recurse then
            Search_In (Path);
         else
            Set.Include (Path);
         end if;
      end Append_Source_Path;

      -----------
      -- Image --
      -----------

      function Image (Kind : Project_Kind) return String is
         K : constant String := GPR2.Image (Kind);
      begin
         if K (K'Last - 7 .. K'Last) = " project" then
            return K (K'First .. K'Last - 8);
         else
            return K;
         end if;
      end Image;

      ----------
      -- Info --
      ----------

      function Info_Object return JSON_Value is
         Inf : constant JSON_Value := Create_Object;
      begin
         Set_Field (Inf, "generated-on",
                    Calendar.Formatting.Image (Calendar.Clock));
         Set_Field (Inf, "version", GPR2.Version.Long_Value);

         return Inf;
      end Info_Object;

      -------------------
      -- Parse_Project --
      -------------------

      procedure Parse_Project
        (Prjs  : in out JSON_Array;
         View    : Project.View.Object;
         Parent  : Project.View.Object)
      is
         function Attributes
           (Atts : Project.Attribute.Set.Object) return JSON_Array;
         --  Return the set of attribute as a JSON_Array

         function Variables
           (Vars : Project.Variable.Set.Object) return JSON_Array;
         --  Return the set of attribute as a JSON_Array

         function Types
           (Typs : Project.Typ.Set.Object) return JSON_Array;
         --  Return the set of types as a JSON_Array

         Typs : Project.Typ.Set.Object := View.Types;
         --  The types used in the project, the variables's types will be added
         --  into this set.

         ----------------
         -- Attributes --
         ----------------

         function Attributes
           (Atts : Project.Attribute.Set.Object) return JSON_Array
         is
            A_Array : JSON_Array;
         begin
            for A of Atts loop
               if not A.Is_From_Config or else Display_Config_Attributes then
                  declare
                     Att : constant JSON_Value := Create_Object;
                  begin
                     Set_Field (Att, "name", Image (A.Name.Id));

                     if A.Has_Index then
                        Set_Field (Att, "index", A.Index.Value);
                     end if;

                     case A.Kind is
                        when Project.Registry.Attribute.Single =>
                           Set_Field (Att, "kind", "single");
                           Set_Field (Att, "value", A.Value.Text);
                        when Project.Registry.Attribute.List =>
                           Set_Field (Att, "kind", "list");

                           declare
                              Values : JSON_Array;
                           begin
                              for V of A.Values loop
                                 Append (Values, Create (V.Text));
                              end loop;

                              Set_Field (Att, "values", Values);
                           end;
                     end case;

                     Append (A_Array, Att);
                  end;
               end if;
            end loop;

            return A_Array;
         end Attributes;

         -----------
         -- Types --
         -----------

         function Types
           (Typs : Project.Typ.Set.Object) return JSON_Array
         is
            T_Array : JSON_Array;
         begin
            for T of Typs loop
               declare
                  Typ : constant JSON_Value := Create_Object;
               begin
                  Set_Field (Typ, "name", String (T.Name.Text));

                  declare
                     Values : JSON_Array;
                  begin
                     for V of T.Values loop
                        Append (Values, Create (V.Text));
                     end loop;

                     Set_Field (Typ, "values", Values);
                  end;

                  Append (T_Array, Typ);
               end;
            end loop;

            return T_Array;
         end Types;

         ---------------
         -- Variables --
         ---------------

         function Variables
           (Vars : Project.Variable.Set.Object) return JSON_Array
         is
            V_Array : JSON_Array;
         begin
            for V of Vars loop
               declare
                  Var : constant JSON_Value := Create_Object;
               begin
                  Set_Field (Var, "name", String (V.Name.Text));

                  if V.Has_Type then
                     Set_Field (Var, "type", String (V.Typ.Name.Text));

                     Typs.Include (V.Typ.Name.Text, V.Typ);
                  end if;

                  case V.Kind is
                     when Project.Registry.Attribute.Single =>
                        Set_Field (Var, "kind", "single");
                        Set_Field (Var, "value", V.Value.Text);
                     when Project.Registry.Attribute.List =>
                        Set_Field (Var, "kind", "list");

                        declare
                           Values : JSON_Array;
                        begin
                           for T of V.Values loop
                              Append (Values, Create (T.Text));
                           end loop;

                           Set_Field (Var, "values", Values);
                        end;
                  end case;

                  Append (V_Array, Var);
               end;
            end loop;

            return V_Array;
         end Variables;

         Prj     : constant JSON_Value := Create_Object;
         F_Prj   : constant JSON_Value := Create_Object;
         C_Array : JSON_Array;
         P_Array : JSON_Array;
         A_Array : JSON_Array;

      begin
         if Handled.Contains (View) then
            return;
         end if;

         Handled.Include (View);

         --  Global project information

         Set_Field (Prj, "id", View_Id (View));
         Set_Field (Prj, "name", String (View.Name));
         Set_Field (Prj, "kind", Image (View.Kind));
         Set_Field (Prj, "qualifier", Image (View.Qualifier));
         Set_Field (Prj, "simple-name", String (View.Path_Name.Simple_Name));
         Set_Field (Prj, "file-name", View.Path_Name.Value);
         Set_Field (Prj, "directory", View.Path_Name.Dir_Name);

         if View.Kind /= K_Abstract then
            Set_Field
              (Prj, "object-directory", View.Object_Directory.Dir_Name);

            O_Array.Include (View.Object_Directory.Dir_Name);

            if View.Kind /= K_Aggregate then
               declare
                  Src_Array : GPR2.Containers.Value_Set;
               begin
                  for S of View.Source_Directories.Values loop
                     Append_Source_Path (S_Array, S.Text, View.Dir_Name.Value);
                     Append_Source_Path
                       (Src_Array, S.Text, View.Dir_Name.Value);
                  end loop;

                  Set_Field
                    (Prj, "source-directories", To_JSON_Array (Src_Array));
               end;
            end if;
         end if;

         if View.Is_Library then
            Set_Field (Prj, "library-file-name",
                       String (View.Library_Filename.Value));
            Set_Field (Prj, "library-name", String (View.Library_Name));
            Set_Field (Prj, "library-directory",
                       View.Library_Directory.Dir_Name);
            Set_Field (Prj, "library-ali-directory",
                       View.Library_Ali_Directory.Dir_Name);
         end if;

         Set_Field (F_Prj, "project", Prj);

         --  Extending

         if View.Is_Extending then
            declare
               E : constant JSON_Value := Create_Object;
            begin
               Set_Field (E, "extending-all", Create (View.Is_Extending_All));
               Set_Field (E, "project-id", Create (View_Id (View.Extending)));

               Set_Field (F_Prj, "extending", E);
            end;
         end if;

         --  Extended

         if View.Is_Extended then
            declare
               E : JSON_Array;
            begin
               for V of View.Extended loop
                  Append (E, Create (View_Id (V)));
               end loop;

               Set_Field (F_Prj, "extended", E);
            end;
         end if;

         --  Imported by

         if Parent.Is_Defined then
            Append (P_Array, Create (View_Id (Parent)));

            Set_Field (F_Prj, "imported-by", P_Array);
         end if;

         --  Imported

         if View.Has_Imports then
            for I of View.Imports loop
               Append (C_Array, Create (View_Id (I)));

               if All_Projects then
                  Parse_Project (Prjs, I, View);
               end if;
            end loop;

            Set_Field (F_Prj, "imports", C_Array);
         end if;

         --  Aggregated

         if View.Qualifier in Aggregate_Kind then
            for A of View.Aggregated loop
               Append (A_Array, Create (View_Id (A)));

               if All_Projects then
                  Parse_Project (Prjs, A, View);
               end if;
            end loop;

            Set_Field (F_Prj, "aggregated", A_Array);
         end if;

         --  Package

         if Display_Packages or else Display_Everything then
            declare
               P_Array : JSON_Array := Empty_Array;
            begin
               for P of View.Packages (False, False) loop
                  declare
                     Pck  : constant JSON_Value := Create_Object;
                  begin
                     Set_Field (Pck, "name", Image (P));

                     if Display_Attributes or else Display_Everything then
                        declare
                           Atts : constant Project.Attribute.Set.Object :=
                                    View.Attributes (P);
                        begin
                           if not Atts.Is_Empty then
                              Set_Field (Pck, "attributes", Attributes (Atts));
                           end if;
                        end;
                     end if;

                     Append (P_Array, Pck);
                  end;
               end loop;

               if not Is_Empty (P_Array) then
                  Set_Field (F_Prj, "packages", P_Array);
               end if;
            end;
         end if;

         --  Attributes

         if Display_Attributes or else Display_Everything then
            declare
               Atts : constant Project.Attribute.Set.Object := View.Attributes;
            begin
               if not Atts.Is_Empty then
                  Set_Field (F_Prj, "attributes", Attributes (Atts));
               end if;
            end;
         end if;

         --  Variables

         if Display_Variables or else Display_Everything then
            declare
               Vars : constant Project.Variable.Set.Object := View.Variables;
            begin
               if not Vars.Is_Empty then
                  Set_Field (F_Prj, "variables", Variables (Vars));
               end if;
            end;
         end if;

         --  Types

         if Display_Variables or else Display_Everything then
            declare
               Typs : constant Project.Typ.Set.Object := View.Types;
            begin
               if not Typs.Is_Empty then
                  Set_Field (F_Prj, "types", Types (Typs));
               end if;
            end;
         end if;

         Append (Prjs, F_Prj);
      end Parse_Project;

      -------------------
      -- To_JSON_Array --
      -------------------

      function To_JSON_Array
        (A : GPR2.Containers.Value_Set) return JSON_Array
      is
         Res : JSON_Array;
      begin
         for S of A loop
            Append (Res, Create (S));
         end loop;

         return Res;
      end To_JSON_Array;

      -----------------
      -- Tree_Object --
      -----------------

      function Tree_Object return JSON_Value is
         T       : constant JSON_Value := Create_Object;
         R       : constant JSON_Value := Create_Object;
         Stat    : constant JSON_Value := Create_Object;
         P_Array : JSON_Array;
      begin
         --  Some stats about the tree

         Set_Field (Stat, "project-count", Create (Integer (Handled.Length)));
         Set_Field (T, "stats", Stat);

         --  Project search paths

         for P of Tree.Project_Search_Paths loop
            Append (P_Array, Create (P.Dir_Name));
         end loop;

         Set_Field (T, "project-search-paths", P_Array);

         if Tree.Has_Runtime_Project then
            O_Array.Include (Tree.Runtime_Project.Object_Directory.Dir_Name);

            for S of Tree.Runtime_Project.Source_Directories.Values loop
               S_Array.Include (S.Text);
            end loop;
         end if;

         --  Object search paths

         if not O_Array.Is_Empty then
            Set_Field (T, "object-search-paths", To_JSON_Array (O_Array));
         end if;

         --  Source search path

         if not S_Array.Is_Empty then
            Set_Field (T, "source-search-paths", To_JSON_Array (S_Array));
         end if;

         --  The root project data

         Set_Field (R, "name", String (Tree.Root_Project.Name));
         Set_Field (R, "id", View_Id (Tree.Root_Project));

         Set_Field (T, "root-project", R);

         return T;
      end Tree_Object;

      -------------
      -- View_Id --
      -------------

      function View_Id
        (View : Project.View.Object) return MD5.Message_Digest
      is
         C : MD5.Context;
      begin
         MD5.Update (C, String (GPR2.View_Ids.Image (View.Id)));
         return String'(MD5.Digest (C));
      end View_Id;

      P_Array : JSON_Array;

   begin
      --  Optimize the parsing when in non JSON format.
      --  ??? TO BE REMOVED when text output will be enhanced

      if not JSON_Output then
         Display_Attributes := False;
         Display_Everything := False;
         Display_Packages   := False;
         Display_Variables  := False;
      end if;

      Parse_Project (P_Array, Tree.Root_Project, Project.View.Undefined);

      declare
         T : constant JSON_Value := Tree_Object;
      begin
         if JSON_Output then
            Set_Field (J_Res, "info", Info_Object);

            Set_Field (J_Res, "tree", T);

            Set_Field (J_Res, "projects", P_Array);

         else
            --  No JSON output, in this mode we just output the search-paths

            Text_IO.Put_Line ("project-search-paths");

            for V of JSON_Array'(JSON.Get (T, "project-search-paths")) loop
               Text_IO.Put_Line ("   " & Get (V));
            end loop;

            Text_IO.Put_Line ("object-search-paths");

            for V of O_Array loop
               Text_IO.Put_Line ("   " & V);
            end loop;

            Text_IO.Put_Line ("source-search-paths");

            for V of S_Array loop
               Text_IO.Put_Line ("   " & V);
            end loop;
         end if;
      end;
   end Inspect_Project;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      use GNAT.Command_Line;

      Options : GPRtools.Options.Object;

   begin
      Define_Switch
        (Options.Config, Help'Access,
         "-h", Long_Switch => "--help",
         Help => "display this help message and exit");

      Define_Switch
        (Options.Config, Version'Access,
         Long_Switch => "--version",
         Help        => "display version and exit");

      Define_Switch
        (Options.Config, JSON_Output'Access,
         "-j", Long_Switch => "--json",
         Help        => "output JSON format");

      Define_Switch
        (Options.Config, All_Projects'Access,
         "-r", Long_Switch => "--recursive",
         Help => "All none external projects recursively");

      Define_Switch
        (Options.Config, Display_Everything'Access,
         "-e", Long_Switch => "--all",
         Help => "Display everything");

      Define_Switch
        (Options.Config, Display_Attributes'Access,
         "-a", Long_Switch => "--attributes",
         Help => "Display attributes");

      Define_Switch
        (Options.Config, Display_Config_Attributes'Access,
         "-c", Long_Switch => "--from-config",
         Help => "Display attributes inherited from configuration");

      Define_Switch
        (Options.Config, Display_Packages'Access,
         "-p", Long_Switch => "--packages",
         Help => "Display attributes");

      Define_Switch
        (Options.Config, Display_Variables'Access,
         "-v", Long_Switch => "--variables",
         Help => "Display variables & types");

      GPRtools.Util.Set_Program_Name ("gprinspect");

      Getopt (Options.Config);

      if Version then
         GPR2.Version.Display
           ("GPRINSPECT", "2022", Version_String => GPR2.Version.Long_Value);

         GPR2.Version.Display_Free_Software;
         GNAT.OS_Lib.OS_Exit (0);
         return;
      end if;

      --  Now read arguments

      Read_Arguments : loop
         declare
            Arg : constant String := Get_Argument;
         begin
            exit Read_Arguments when Arg = "";

            if Project_Path = Null_Unbounded_String then
               Project_Path := To_Unbounded_String (Arg);
            else
               raise Invalid_Switch;
            end if;
         end;
      end loop Read_Arguments;

      if Project_Path = Null_Unbounded_String then
         raise Invalid_Switch;
      end if;
   end Parse_Command_Line;

begin
   GNATCOLL.Traces.Parse_Config_File;
   GPRtools.Util.Set_Program_Name ("gprinspect");
   Parse_Command_Line;

   declare
      Pathname : constant GPR2.Path_Name.Object :=
                   GPR2.Project.Create
                     (GPR2.Filename_Type (To_String (Project_Path)));
      Context  : GPR2.Context.Object;
   begin
      Project_Tree.Load_Autoconf
        (Pathname, Context, Check_Shared_Lib => False);

      --  Build list of imported-by projects

      for C in Project_Tree.Iterate loop
         declare
            V : constant Project.View.Object := Project.Tree.Element (C);
         begin
            for I of V.Imports loop
               declare
                  S : constant Imported_By_Map.Cursor := Imported_By.Find (I);
               begin
                  if Imported_By_Map.Has_Element (S) then
                     Imported_By.Reference (S).Insert (V);
                  else
                     declare
                        N : Project.View.Set.Object;
                     begin
                        N.Include (V);
                        Imported_By.Insert (I, N);
                     end;
                  end if;
               end;
            end loop;
         end;
      end loop;

      Inspect_Project (Project_Tree);

      if JSON_Output then
         Text_IO.Put_Line (String'(JSON.Write (J_Res)));
      end if;
   exception
      when others =>
         for M of Project_Tree.Log_Messages.all loop
            Text_IO.Put_Line (M.Format);
         end loop;
   end;

exception
   when GNAT.Command_Line.Invalid_Switch
      | GNAT.Command_Line.Exit_From_Command_Line
      =>
      Command_Line.Set_Exit_Status (Command_Line.Failure);

   when E : others =>
      Text_IO.Put_Line ("cannot parse project: " & Exception_Information (E));
      Command_Line.Set_Exit_Status (Command_Line.Failure);
end GPRinspect;
