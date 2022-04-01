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
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.MD5;
with GNAT.OS_Lib;

with GNATCOLL.Traces;
with GNATCOLL.JSON;
with GNATCOLL.Utils;

with GPRtools.Command_Line;
with GPRtools.Util;
with GPRtools.Options;

with GPR2.Containers;
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

   use GNATCOLL;
   use GNATCOLL.JSON;
   use GPR2;

   procedure Parse_Command_Line;
   --  Parse command line parameters

   procedure Inspect_Project (Tree : Project.Tree.Object);
   --  Inspect project and possibly recursively all imports

   procedure On_Switch
     (Parser : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res    : not null access GPRtools.Command_Line.Command_Line_Result'Class;
      Arg    : GPRtools.Command_Line.Switch_Type;
      Index  : String;
      Param  : String);

   package Imported_By_Map is new
     Ada.Containers.Ordered_Maps
       (Project.View.Object,
        Project.View.Set.Object,
        Project.View."<",
        Project.View.Set."=");

   Imported_By  : Imported_By_Map.Map;
   Handled      : Project.View.Set.Object;

   --  Variables for tool's options

   Project_Tree              : aliased GPR2.Project.Tree.Object;

   type GPRinspect_Options is new GPRtools.Options.Base_Options with record
      JSON_Output               : Boolean := False;
      All_Projects              : Boolean := False;
      Display_Everything        : Boolean := False;
      Display_Attributes        : Boolean := False;
      Display_Config_Attributes : Boolean := False;
      Display_Packages          : Boolean := False;
      Display_Variables         : Boolean := False;
   end record;

   J_Res   : constant JSON_Value := Create_Object;
   --  The JSON response

   O_Array : GPR2.Containers.Value_Set;
   --  Object search-paths, global array with all project's object directories
   --  and the possible runtime object directory.
   S_Array : GPR2.Containers.Value_Set;
   --  Sources search-paths, global array with all project's sources
   --  directories.

   Options : GPRinspect_Options;

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
               if not A.Is_From_Config
                 or else Options.Display_Config_Attributes
               then
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

               if Options.All_Projects then
                  Parse_Project (Prjs, I, View);
               end if;
            end loop;

            Set_Field (F_Prj, "imports", C_Array);
         end if;

         --  Aggregated

         if View.Qualifier in Aggregate_Kind then
            for A of View.Aggregated loop
               Append (A_Array, Create (View_Id (A)));

               if Options.All_Projects then
                  Parse_Project (Prjs, A, View);
               end if;
            end loop;

            Set_Field (F_Prj, "aggregated", A_Array);
         end if;

         --  Package

         if Options.Display_Packages or else Options.Display_Everything then
            declare
               P_Array : JSON_Array := Empty_Array;
            begin
               for P of View.Packages (False, False) loop
                  declare
                     Pck  : constant JSON_Value := Create_Object;
                  begin
                     Set_Field (Pck, "name", Image (P));

                     if Options.Display_Attributes
                       or else Options.Display_Everything
                     then
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

         if Options.Display_Attributes or else Options.Display_Everything then
            declare
               Atts : constant Project.Attribute.Set.Object := View.Attributes;
            begin
               if not Atts.Is_Empty then
                  Set_Field (F_Prj, "attributes", Attributes (Atts));
               end if;
            end;
         end if;

         --  Variables

         if Options.Display_Variables or else Options.Display_Everything then
            declare
               Vars : constant Project.Variable.Set.Object := View.Variables;
            begin
               if not Vars.Is_Empty then
                  Set_Field (F_Prj, "variables", Variables (Vars));
               end if;
            end;
         end if;

         --  Types

         if Options.Display_Variables or else Options.Display_Everything then
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

      if not Options.JSON_Output then
         Options.Display_Attributes := False;
         Options.Display_Everything := False;
         Options.Display_Packages   := False;
         Options.Display_Variables  := False;
      end if;

      Parse_Project (P_Array, Tree.Root_Project, Project.View.Undefined);

      declare
         T : constant JSON_Value := Tree_Object;
      begin
         if Options.JSON_Output then
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
      pragma Unreferenced (Parser, Index, Param);
      use type GPRtools.Command_Line.Switch_Type;
      Result : constant access GPRinspect_Options :=
                 GPRinspect_Options (Res.all)'Access;
   begin
      if Arg = "-j" then
         Result.JSON_Output := True;
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
      Options.Tree := Project_Tree.Reference;

      Setup (Tool => GPRtools.Inspect);

      Parser.Add_Argument
        (Group,
         Create ("-j", "--json",
                 Help => "output JSON format"));
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
   GPRtools.Util.Set_Program_Name ("gprinspect");
   Parse_Command_Line;

   if not
     GPRtools.Options.Load_Project (Options, Absent_Dir_Error => False)
   then
      Command_Line.Set_Exit_Status (Command_Line.Failure);
      return;
   end if;

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

   if Options.JSON_Output then
      Text_IO.Put_Line (String'(JSON.Write (J_Res)));
   end if;

exception
   when GNAT.Command_Line.Invalid_Switch
      | GNAT.Command_Line.Exit_From_Command_Line
      =>
      Command_Line.Set_Exit_Status (Command_Line.Failure);

   when others =>
      for M of Options.Tree.Log_Messages.all loop
         Text_IO.Put_Line (M.Format);
      end loop;

      Command_Line.Set_Exit_Status (Command_Line.Failure);
end GPRinspect;
