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

with Ada.Calendar.Formatting;
with Ada.Command_Line;
with Ada.Containers.Ordered_Maps;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;

with GNAT.MD5;

with GNATCOLL.JSON;

with GPRtools.Options;

with GPR2.Containers;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.Typ.Set;
with GPR2.Project.Variable.Set;
with GPR2.Project.View.Set;
with GPR2.Version;
with GPR2.View_Ids;

procedure GPRinspect.Process (Options : in out GPRinspect.GPRinspect_Options)
is

   use Ada;

   use GNAT;
   use GNATCOLL;
   use GNATCOLL.JSON;
   use GPR2;
   use GPR2.View_Ids;
   use type GPRtools.Display_Kind;
   use type Project.Registry.Attribute.Value_Kind;

   package PRA renames Project.Registry.Attribute;

   --  Variables for tool's options
   Project_Tree : aliased Project.Tree.Object;

   procedure Inspect_Project_JSON_Output (Tree : Project.Tree.Object;
                                          Compact : Boolean);
   --  Inspect project and possibly recursively all imports

   procedure Inspect_Project_Textual_Output (Tree : Project.Tree.Object);
   --  Inspect project and possibly recursively all imports

   procedure Load_Project (Tree    : in out Project.Tree.Object;
                           Options : in out GPRinspect.GPRinspect_Options);
   --  Load project to inspect

   function View_Id (View : Project.View.Object) return MD5.Message_Digest;
   --  Get the hash from the view ID

   function No_View_Restriction (Views : Restricted_Scope;
                                 VName : Name_Type)
                                 return Boolean;
   --  Return if the view must be processed and displayed or not.

   ---------------------------------
   -- Inspect_Project_JSON_Output --
   ---------------------------------

   procedure Inspect_Project_JSON_Output (Tree    : Project.Tree.Object;
                                          Compact : Boolean)
   is
      J_Res   : constant JSON_Value := Create_Object;
      --  The JSON response

      O_Array : GPR2.Containers.Value_Set;
      --  Object search-paths, global array with all project's object
      --  directories and the possible runtime object directory.

      S_Array : GPR2.Containers.Value_Set;
      --  Sources search-paths, global array with all project's sources
      --  directories.

      Handled      : Project.View.Set.Object;

      function Info_Object return JSON_Value;
      --  Information node (date, toolset version, ...)

      function Tree_Object return JSON_Value;
      --  Information node (date, toolset version, ...)

      procedure Parse_Project
        (Prjs : in out JSON_Array;
         View    : Project.View.Object;
         Parent  : Project.View.Object);
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
                     Set_Field (Att, "name", Image (A.Name.Id.Attr));

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
                  for S of View.Source_Directories loop
                     S_Array.Include (S.Value);
                     Src_Array.Include (S.Value);
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
         M_Array : JSON_Array;
      begin
         --  Messages

         if Tree.Has_Messages then
            for C in Tree.Log_Messages.Iterate
              (Information => False,
               Warning     => False,
               Error       => False,
               Lint        => True,
               Read        => False,
               Unread      => True)
            loop
               declare
                  M : constant Message.Object := GPR2.Log.Element (C);
               begin
                  Append (M_Array, Create (M.Format));
               end;
            end loop;

            Set_Field (T, "messages", M_Array);
         end if;

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

            for S of Tree.Runtime_Project.Source_Directories loop
               S_Array.Include (S.Dir_Name);
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
         Set_Field (J_Res, "info", Info_Object);
         Set_Field (J_Res, "tree", Tree_Object);
         Set_Field (J_Res, "projects", P_Array);
      end;

      Text_IO.Put_Line (Item => JSON.Write (Item    => J_Res,
                                            Compact => Compact));

   end Inspect_Project_JSON_Output;

   ------------------------------------
   -- Inspect_Project_Textual_Output --
   ------------------------------------

   procedure Inspect_Project_Textual_Output (Tree : Project.Tree.Object) is

      procedure Print_Infos;
      procedure Print_Projects;
      procedure Print_Tree;

      procedure Print_Infos is
      begin
         Text_IO.Put_Line (Item => "+--------------------------------------+");
         Text_IO.Put_Line (Item => "|         General Informations         |");
         Text_IO.Put_Line (Item => "+--------------------------------------+");
         Text_IO.Put_Line (Item => "* Generated on : "
                           & Calendar.Formatting.Image (Calendar.Clock));
         Text_IO.Put_Line (Item => "* Version      : "
                           & GPR2.Version.Long_Value);
      end Print_Infos;

      procedure Print_Projects is
      begin

         Text_IO.Put_Line (Item => "+--------------------------------------+");
         Text_IO.Put_Line (Item => "|         Projects Informations        |");
         Text_IO.Put_Line (Item => "+--------------------------------------+");

         for V in Tree.Iterate
         loop
            declare
               View : constant Project.View.Object := Project.Tree.Element (V);
            begin
               if (Options.All_Projects
                   and then No_View_Restriction
                     (Views => Options.Restricted_Views,
                      VName => View.Name))
                 or else (View_Id (View) = View_Id (Tree.Root_Project))
               then
                  Text_IO.Put_Line (Item => "* "
                                    & String (View.Name)
                                    & " (" & View_Id (View) & ") [ "
                                    & Image (View.Kind) & " ]");
                  Text_IO.Put_Line (Item => "    - Project file      : "
                                    & View.Path_Name.Value);
                  Text_IO.Put_Line (Item => "    - Project directory : "
                                    & View.Path_Name.Dir_Name);
                  if View.Kind /= K_Abstract then
                     Text_IO.Put_Line (Item => "    - Object directory  : "
                                       & View.Object_Directory.Dir_Name);
                     if View.Kind /= K_Aggregate then
                        Text_IO.Put_Line (Item => "    - Source directory  :");
                        for S of View.Source_Directories
                        loop
                           Text_IO.Put_Line (Item => "       - " & S.Value);
                        end loop;
                     end if;
                  end if;

                  if View.Is_Library then
                     Text_IO.Put_Line (Item => "    - Library name      : "
                                       & String (View.Library_Name));
                     Text_IO.Put_Line (Item => "    - Library file      : "
                                       & String (View.Library_Filename.Value));
                     Text_IO.Put_Line (Item => "    - Library directory : "
                                       & View.Library_Directory.Dir_Name);
                     Text_IO.Put_Line (Item => "    - Library ALI dir.  : "
                                       & View.Library_Ali_Directory.Dir_Name);
                  end if;

                  if View.Is_Extending then
                     Text_IO.Put_Line (Item => "    - Extending         : "
                                       & View_Id (View.Extending));
                  end if;

                  if View.Is_Extended then
                     Text_IO.Put_Line (Item => "    - Extended by       : ");
                     for Extended_V of View.Extended
                     loop
                        Text_IO.Put_Line (Item => "       - "
                                          & View_Id (Extended_V));
                     end loop;
                  end if;

                  declare
                     First_Import : Boolean := True;
                  begin
                     for I_V in Tree.Iterate
                     loop
                        declare
                           I_View : constant Project.View.Object
                             := Project.Tree.Element (I_V);
                        begin
                           if I_View.Id /= View.Id
                             and then I_View.Has_Imports
                           then
                              for I of I_View.Imports
                              loop
                                 if I.Id = View.Id then
                                    if First_Import then
                                       Text_IO.Put_Line
                                         (Item => "    - Imported-by"
                                          & "        : ");
                                       First_Import := False;
                                    end if;
                                    Text_IO.Put_Line
                                      (Item => "       - "
                                       & String (I_View.Name)
                                       & " (" & View_Id (I_View) & ")");
                                 end if;
                              end loop;
                           end if;
                        end;
                     end loop;
                  end;

                  if View.Has_Imports then
                     Text_IO.Put_Line (Item => "    - Imports           : ");
                     for I of View.Imports
                     loop
                        Text_IO.Put_Line (Item => "       - " & String (I.Name)
                                          & " (" & View_Id (I) & ")");
                     end loop;
                  end if;

                  if View.Qualifier in Aggregate_Kind then
                     Text_IO.Put_Line (Item => "    - Aggregate         : ");
                     for A of View.Aggregated
                     loop
                        Text_IO.Put_Line (Item => "       - " & String (A.Name)
                                          & " (" & View_Id (A) & ")");
                     end loop;
                  end if;

                  if Options.Display_Attributes
                    or else Options.Display_Everything
                  then
                     if not View.Attributes.Is_Empty
                     then
                        Text_IO.Put_Line (Item => "    - Attributes     : ");
                        for Attr of View.Attributes
                        loop
                           if not Attr.Is_From_Config
                             or else Options.Display_Config_Attributes
                           then
                              Text_IO.Put_Line
                                (Item => "       - "
                                 & "Project_Level_Scope."
                                 & Image (Attr.Name.Id.Attr)
                                 & " [ " & Attr.Kind'Img & " ]");
                              if Attr.Has_Index then
                                 Text_IO.Put_Line
                                   (Item => "          - Index value : "
                                    & '"' & Attr.Index.Value & '"');
                              end if;
                              if Attr.Kind = PRA.Single
                              then
                                 Text_IO.Put_Line
                                   (Item => "          - Value : "
                                    & '"' & Attr.Value.Text & '"');
                              elsif Attr.Kind = PRA.List
                              then
                                 Text_IO.Put_Line
                                   (Item => "          - Values : ");
                                 for V of Attr.Values
                                 loop
                                    Text_IO.Put_Line
                                      (Item => "             - "
                                       & '"' & V.Text & '"');
                                 end loop;
                              end if;
                           end if;
                        end loop;
                     end if;
                  end if;

                  if Options.Display_Packages
                    or else Options.Display_Everything
                  then
                     if not View.Packages (False, False).Is_Empty then
                        for P of View.Packages (False, False)
                        loop
                           if Options.Display_Attributes
                             or else Options.Display_Everything
                           then
                              if not View.Attributes (P).Is_Empty then
                                 for Attr of View.Attributes
                                 loop
                                    if not Attr.Is_From_Config
                                      or else Options.Display_Config_Attributes
                                    then
                                       Text_IO.Put_Line
                                         (Item => "       - "
                                          & Image (P) & '.'
                                          & Image (Attr.Name.Id.Attr)
                                          & " [ " & Attr.Kind'Img & " ]");
                                       if Attr.Has_Index then
                                          Text_IO.Put_Line
                                            (Item => "          - "
                                             & "Index value : "
                                             & '"' & Attr.Index.Value & '"');
                                       end if;
                                       if Attr.Kind = PRA.Single
                                       then
                                          Text_IO.Put_Line
                                            (Item => "          - Value : "
                                             & '"' & Attr.Value.Text & '"');
                                       elsif Attr.Kind = PRA.List
                                       then
                                          Text_IO.Put_Line
                                            (Item => "          - Values : ");
                                          for V of Attr.Values
                                          loop
                                             Text_IO.Put_Line
                                               (Item => "             - "
                                                & '"' & V.Text & '"');
                                          end loop;
                                       end if;
                                    end if;
                                 end loop;
                              end if;
                           end if;
                        end loop;
                     end if;
                  end if;

                  if Options.Display_Variables
                    or else Options.Display_Everything
                  then
                     if not View.Variables.Is_Empty
                     then
                        Text_IO.Put_Line (Item => "    - Variables      : ");
                        for Var of View.Variables
                        loop
                           Text_IO.Put_Line
                             (Item => "       - "
                              & String (Var.Name.Text)
                              & " [ " & Var.Kind'Img & " ]");

                           if Var.Has_Type then
                              Text_IO.Put_Line
                                (Item => "          - Variable type : "
                                 & '"' & String (Var.Typ.Name.Text) & '"');
                           end if;

                           if Var.Kind = PRA.Single
                           then
                              Text_IO.Put_Line
                                (Item => "          - Value : "
                                 & '"' & Var.Value.Text & '"');
                           elsif Var.Kind = PRA.List
                           then
                              Text_IO.Put_Line
                                (Item => "          - Values : ");
                              for V of Var.Values
                              loop
                                 Text_IO.Put_Line
                                   (Item => "             - "
                                    & '"' & V.Text & '"');
                              end loop;
                           end if;
                        end loop;
                     end if;
                  end if;

                  if Options.Display_Variables
                    or else Options.Display_Everything
                  then
                     if not View.Types.Is_Empty then
                        Text_IO.Put_Line (Item => "    - Types          : ");
                        for T of View.Types
                        loop
                           Text_IO.Put_Line
                             (Item => "       - "
                              & String (T.Name.Text));
                           Text_IO.Put_Line
                             (Item => "          - Values : ");
                           for V of T.Values loop
                              Text_IO.Put_Line
                                (Item => "             - "
                                 & '"' & V.Text & '"');
                           end loop;
                        end loop;
                     end if;
                  end if;
                  Ada.Text_IO.Put_Line (Item => "");
               end if;
            end;
         end loop;

      end Print_Projects;

      procedure Print_Tree is
      begin

         Text_IO.Put_Line (Item => "+--------------------------------------+");
         Text_IO.Put_Line (Item => "|       Project Tree Informations      |");
         Text_IO.Put_Line (Item => "+--------------------------------------+");

         if Tree.Has_Messages then
            declare
               First_Message : Boolean := False;
            begin
               for C in Project_Tree.Log_Messages.Iterate
                 (Information => False,
                  Warning     => False,
                  Error       => False,
                  Lint        => True,
                  Read        => False,
                  Unread      => True)
               loop
                  if not First_Message
                  then
                     Text_IO.Put_Line (Item => "* Messages :");
                     First_Message := True;
                  end if;

                  declare
                     M : constant Message.Object := GPR2.Log.Element (C);
                  begin
                     Text_IO.Put_Line (Item => "    - " & M.Format);
                  end;
               end loop;
            end;
         end if;

         declare
            Project_Count : Integer := 0;
         begin
            for V in Tree.Iterate
            loop
               declare
                  View : constant Project.View.Object
                    := Project.Tree.Element (V);
               begin
                  if (Options.All_Projects
                      and then No_View_Restriction
                        (Views => Options.Restricted_Views,
                         VName => View.Name))
                    or else (View_Id (View) = View_Id (Tree.Root_Project))
                  then
                     Project_Count := Project_Count + 1;
                  end if;
               end;
            end loop;
            Text_IO.Put_Line (Item => "* Project count        : "
                              & Project_Count'Img);
         end;

         declare
            First_PPath : Boolean := False;
         begin
            for P of Tree.Project_Search_Paths loop
               if not First_PPath
               then
                  Ada.Text_IO.Put_Line (Item => "* Project search paths :");
                  First_PPath := True;
               end if;
               Ada.Text_IO.Put_Line (Item => "    - " & P.Dir_Name);
            end loop;
         end;

         declare
            First_SPath : Boolean := False;
         begin
            if Tree.Has_Runtime_Project then
               Ada.Text_IO.Put_Line (Item => "* Object search paths  :");
               for V in Tree.Iterate
               loop
                  declare
                     View : constant Project.View.Object
                       := Project.Tree.Element (V);
                  begin
                     if (Options.All_Projects
                         and then No_View_Restriction
                           (Views => Options.Restricted_Views,
                            VName => View.Name))
                       or else (View_Id (View) = View_Id (Tree.Root_Project))
                     then
                        if View.Kind /= K_Abstract then
                           Ada.Text_IO.Put_Line
                             (Item => "    - "
                              & View.Object_Directory.Dir_Name);
                        end if;
                     end if;
                  end;
               end loop;
               Ada.Text_IO.Put_Line
                 (Item => "    - "
                  & Tree.Runtime_Project.Object_Directory.Dir_Name);

               for V in Tree.Iterate
               loop
                  declare
                     View : constant Project.View.Object
                       := Project.Tree.Element (V);
                  begin
                     if (Options.All_Projects
                         and then No_View_Restriction
                           (Views => Options.Restricted_Views,
                            VName => View.Name))
                       or else (View_Id (View) = View_Id (Tree.Root_Project))
                     then
                        if View.Kind /= K_Abstract
                          and then View.Kind /= K_Aggregate
                        then
                           for S of View.Source_Directories
                           loop
                              if not First_SPath
                              then
                                 Ada.Text_IO.Put_Line
                                   (Item => "* Source search paths  :");
                                 First_SPath := True;
                              end if;
                              Ada.Text_IO.Put_Line
                                (Item => "    - " & S.Value);
                           end loop;
                        end if;
                     end if;
                  end;
               end loop;
               for S of Tree.Runtime_Project.Source_Directories loop
                  if not First_SPath
                  then
                     Ada.Text_IO.Put_Line (Item => "* Source search paths  :");
                     First_SPath := True;
                  end if;
                  Ada.Text_IO.Put_Line (Item => "    - " & S.Dir_Name);
               end loop;
            end if;
         end;

         Ada.Text_IO.Put_Line (Item => "* Root project :");
         Ada.Text_IO.Put_Line (Item => "    - "
                               & String (Tree.Root_Project.Name)
                               & " (" & View_Id (Tree.Root_Project) & ")");
      end Print_Tree;

      pragma Unreferenced (Tree);

   begin
      Print_Infos;
      Print_Tree;
      Print_Projects;
   end Inspect_Project_Textual_Output;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project (Tree    : in out GPR2.Project.Tree.Object;
                           Options : in out GPRinspect.GPRinspect_Options)
   is
      package Imported_By_Map is new
        Ada.Containers.Ordered_Maps
          (Project.View.Object,
           Project.View.Set.Object,
           Project.View."<",
           Project.View.Set."=");

      Imported_By  : Imported_By_Map.Map;
   begin
      Options.Tree := Project_Tree.Reference;

      if not
        GPRtools.Options.Load_Project
          (Opt                => Options,
           Absent_Dir_Error   => False,
           Handle_Information => Options.Verbose)
      then
         Command_Line.Set_Exit_Status (Command_Line.Failure);
         return;
      end if;

      --  Build list of imported-by projects

      for C in Tree.Iterate loop
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
   end Load_Project;

   ----------------------
   -- View_Restriction --
   ----------------------

   function No_View_Restriction (Views : Restricted_Scope;
                                 VName : Name_Type)
                                 return Boolean
   is
   begin
      return (not Views.Restrict or else (Views.Views.Contains (VName)));
   end No_View_Restriction;

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

begin

   Load_Project (Tree    => Project_Tree,
                 Options => Options);

   case Options.Kind_Of_Display is
      when GPRtools.K_JSON | GPRtools.K_JSON_Compact =>
         Inspect_Project_JSON_Output
           (Tree    => Project_Tree,
            Compact => (Options.Kind_Of_Display = GPRtools.K_JSON_Compact));

      when GPRtools.K_Textual_IO =>
         Inspect_Project_Textual_Output (Tree => Project_Tree);
   end case;

exception
   when E : others =>
      Text_IO.Put_Line ("error: " & Exception_Information (E));
      if Options.Tree /= null
      then
         if Options.Tree.Has_Messages then
            for M of Options.Tree.Log_Messages.all loop
               Text_IO.Put_Line (M.Format);
            end loop;
         end if;
      end if;
end GPRinspect.Process;
