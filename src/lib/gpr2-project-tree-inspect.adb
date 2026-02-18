--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Calendar.Formatting;

with GPR2.Containers;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Typ.Set;
with GPR2.Project.Variable.Set;
with GPR2.Project.View.Set;
with GPR2.Version;
with GPR2.View_Ids;

package body GPR2.Project.Tree.Inspect is

   function View_Id (View : Project.View.Object) return String;
   --  Get the View's View_Id image

   ---------------------------------
   -- Inspect_Project_JSON_Output --
   ---------------------------------

   procedure Inspect_Project_JSON_Output
     (JSON_Res                  : JSON_Value;
      Tree                      : Project.Tree.Object;
      All_Projects              : Boolean;
      Display_Everything        : Boolean := False;
      Display_Attributes        : Boolean := False;
      Display_Config_Attributes : Boolean := False;
      Display_Packages          : Boolean := False;
      Display_Variables         : Boolean := False)
   is
      O_Array : GPR2.Containers.Value_Set;
      --  Object search-paths, global array with all project's object
      --  directories and the possible runtime object directory.

      S_Array : GPR2.Containers.Value_Set;
      --  Sources search-paths, global array with all project's sources
      --  directories.

      Handled : Project.View.Set.Object;

      function Info_Object return JSON_Value;
      --  Information node (date, toolset version, ...)

      function Tree_Object return JSON_Value;
      --  Information node (date, toolset version, ...)

      procedure Parse_Project
        (Prjs   : in out JSON_Array;
         View   : Project.View.Object;
         Parent : Project.View.Object);
      --  Project information (name, kind, ...)

      function To_JSON_Array (A : GPR2.Containers.Value_Set) return JSON_Array;

      function Image (Kind : Project_Kind) return String;
      --  Returns the Kind only

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
        (Prjs   : in out JSON_Array;
         View   : Project.View.Object;
         Parent : Project.View.Object)
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
               declare
                  Att : constant JSON_Value := Create_Object;
               begin
                  Set_Field (Att, "name", Image (A.Name.Id.Attr));

                  if A.Has_Index then
                     Set_Field (Att, "index", A.Index.Value);
                  end if;

                  case A.Kind is
                     when Project.Registry.Attribute.Single =>
                        Set_Field (Att, "kind", "single");
                        Set_Field (Att, "value", A.Value.Text);

                     when Project.Registry.Attribute.List   =>
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
         Set_Field (Prj, "file-name", View.Path_Name.String_Value);
         Set_Field (Prj, "directory", String (View.Path_Name.Dir_Name));

         if View.Kind in With_Object_Dir_Kind then
            Set_Field
              (Prj, "object-directory",
               String (View.Object_Directory.Dir_Name));

            O_Array.Include (String (View.Object_Directory.Dir_Name));

            if View.Kind in With_Source_Dirs_Kind then
               declare
                  Src_Array : GPR2.Containers.Value_Set;
               begin
                  for S of View.Source_Directories loop
                     S_Array.Include (S.String_Value);
                     Src_Array.Include (S.String_Value);
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
                       String (View.Library_Directory.Dir_Name));
            Set_Field (Prj, "library-ali-directory",
                       String (View.Library_Ali_Directory.Dir_Name));
         end if;

         Set_Field (F_Prj, "project", Prj);

         --  Extending

         if View.Is_Extended then
            declare
               E : constant JSON_Value := Create_Object;
            begin
               Set_Field (E, "extending-all", Create (View.Is_Extending_All));
               Set_Field (E, "project-id", Create (View_Id (View.Extending)));
               Set_Field (F_Prj, "extending", E);
            end;
         end if;

         --  Extended

         if View.Is_Extending then
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
            for A of View.Aggregated (Recursive => False) loop
               Append (A_Array, Create (View_Id (A)));

               if All_Projects then
                  Parse_Project (Prjs, A, View);
               end if;
            end loop;

            Set_Field (F_Prj, "aggregated", A_Array);
         end if;

         declare
            Attributes_Array : JSON_Array := Empty_Array;
         begin

            --  Package

            if Display_Attributes
              or else Display_Packages
              or else Display_Everything
            then
               declare
                  P_Array : JSON_Array := Empty_Array;
               begin
                  for P of View.Packages
                    (With_Defaults => False,
                     With_Config   => Display_Config_Attributes)
                  loop
                     declare
                        Pck : constant JSON_Value := Create_Object;
                     begin
                        if Display_Packages
                          or else Display_Everything
                        then
                           Set_Field (Pck, "name", Image (P));
                        end if;

                        if Display_Attributes
                          or else Display_Everything
                        then
                           declare
                              Atts : constant Project.Attribute.Set.Object :=
                                View.Attributes
                                  (P,
                                   With_Config => Display_Config_Attributes);
                           begin
                              if not Atts.Is_Empty then
                                 GNATCOLL.JSON.Append
                                   (Attributes_Array, Attributes (Atts));

                                 if Display_Packages
                                   or else Display_Everything
                                 then
                                    --  Print attributes for each package
                                    --  instead of merging them into the
                                    --  global project attributes array.

                                    Set_Field
                                      (Pck, "attributes", Attributes_Array);
                                    Attributes_Array := Empty_Array;
                                    Append (P_Array, Pck);
                                 end if;
                              end if;
                           end;
                        end if;
                     end;
                  end loop;

                  if not Is_Empty (P_Array) then
                     Set_Field (F_Prj, "packages", P_Array);
                  end if;
               end;
            end if;

            --  Attributes

            if Display_Attributes or else Display_Everything
            then
               declare
                  Atts : constant Project.Attribute.Set.Object :=
                    View.Attributes
                      (With_Config => Display_Config_Attributes);
               begin
                  GNATCOLL.JSON.Append (Attributes_Array, Attributes (Atts));

                  if not GNATCOLL.JSON.Is_Empty (Attributes_Array) then
                     Set_Field (F_Prj, "attributes", Attributes_Array);
                  end if;
               end;
            end if;
         end;

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
            Append (P_Array, Create (String (P.Dir_Name)));
         end loop;

         Set_Field (T, "project-search-paths", P_Array);

         if Tree.Has_Runtime_Project then
            O_Array.Include
              (String (Tree.Runtime_Project.Object_Directory.Dir_Name));

            for S of Tree.Runtime_Project.Source_Directories loop
               S_Array.Include (String (S.Dir_Name));
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

   begin
      declare
         P_Array : JSON_Array;
      begin
         Parse_Project (Prjs   => P_Array,
                        View   => Tree.Root_Project,
                        Parent => Project.View.Undefined);

         Set_Field (JSON_Res, "info", Info_Object);
         Set_Field (JSON_Res, "tree", Tree_Object);
         Set_Field (JSON_Res, "projects", P_Array);
      end;
   end Inspect_Project_JSON_Output;

   -------------
   -- View_Id --
   -------------

   function View_Id (View : Project.View.Object) return String is
   begin
      return String (GPR2.View_Ids.Image (View.Id));
   end View_Id;

end GPR2.Project.Tree.Inspect;
