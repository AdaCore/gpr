------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2022, AdaCore                      --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with GPR2.Containers;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;
with GPR2.Source_Info.Parser.Registry;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Value;

package body GPR2.Project.Definition is

   use GNAT;

   package ACH renames Ada.Characters.Handling;
   package ASF renames Ada.Strings.Fixed;
   package PRA renames Project.Registry.Attribute;
   package PRP renames Project.Registry.Pack;
   package SR  renames GPR2.Source_Reference;
   package SRI renames SR.Identifier;

   ----------------------------------
   -- Check_Aggregate_Library_Dirs --
   ----------------------------------

   procedure Check_Aggregate_Library_Dirs (View : Project.View.Object) is
      procedure Process_Aggregate (Proj : Project.View.Object);
      --  Recursive procedure to check the aggregated projects, as they may
      --  also be aggregated library projects.

      -----------------------
      -- Process_Aggregate --
      -----------------------

      procedure Process_Aggregate (Proj : Project.View.Object) is
      begin
         if Proj.Kind = K_Aggregate_Library then
            for V of Get_RO (Proj).Aggregated loop
               if V.Kind not in K_Aggregate_Library | K_Configuration
                 | K_Abstract
                 and then View.Library_Ali_Directory = V.Object_Directory
               then
                  View.Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => SR.Value.Create
                          (Filename => View.Path_Name.Value,
                           Line     => 0,
                           Column   => 0,
                           Text     => ""),
                        Message =>
                          "aggregate library ALI directory cannot be shared " &
                          "with object directory of aggregated project """ &
                          String (V.Path_Name.Base_Name) & """"));
               elsif V.Is_Library
                 and then View.Library_Ali_Directory = V.Library_Directory
               then
                  View.Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => SR.Value.Create
                          (Filename => View.Path_Name.Value,
                           Line     => 0,
                           Column   => 0,
                           Text     => ""),
                        Message =>
                          "aggregate library ALI directory cannot be shared " &
                          "with library directory of aggregated project """ &
                          String (V.Path_Name.Base_Name) & """"));
               elsif V.Kind not in K_Aggregate_Library | K_Configuration
                 | K_Abstract
                 and then View.Library_Directory = V.Object_Directory
               then
                  View.Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => SR.Value.Create
                          (Filename => View.Path_Name.Value,
                           Line     => 0,
                           Column   => 0,
                           Text     => ""),
                        Message =>
                          "aggregate library directory cannot be shared " &
                          "with object directory of aggregated project """ &
                          String (V.Path_Name.Base_Name) & """"));
               elsif V.Is_Library
                 and then View.Library_Directory = V.Library_Directory
               then
                  View.Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => SR.Value.Create
                          (Filename => View.Path_Name.Value,
                           Line     => 0,
                           Column   => 0,
                           Text     => ""),
                        Message =>
                          "aggregate library directory cannot be shared " &
                          "with library directory of aggregated project """ &
                          String (V.Path_Name.Base_Name) & """"));
               end if;

               Process_Aggregate (V);

            end loop;
         end if;
      end Process_Aggregate;
   begin
      Process_Aggregate (View);
   end Check_Aggregate_Library_Dirs;

   --------------------------------
   -- Check_Excluded_Source_Dirs --
   --------------------------------

   procedure Check_Excluded_Source_Dirs (View : Project.View.Object) is
   begin
      for V of View.Tree.Ordered_Views loop
         if V.Kind in With_Source_Dirs_Kind then
            declare
               V_Path : constant Path_Name.Object := V.Dir_Name;
               Attr   : constant Project.Attribute.Object :=
                          V.Attribute (PRA.Excluded_Source_Dirs);
            begin
               if Attr.Is_Defined then
                  for Val of Attr.Values loop
                     if not V_Path.Compose
                       (Filename_Type (Val.Text)).Exists
                     then
                        View.Tree.Log_Messages.Append
                          (Message.Create
                             (Level   => Message.Error,
                              Sloc    => Val,
                              Message =>
                                """" & Val.Text &
                                """ is not a valid directory"));
                     end if;
                  end loop;
               end if;
            end;
         end if;
      end loop;
   end Check_Excluded_Source_Dirs;

   --------------------------
   -- Check_Package_Naming --
   --------------------------

   procedure Check_Package_Naming (View : Project.View.Object) is
      procedure Check_View (View : Project.View.Object);
      --  Checks in View tree Casing, Dot_Replacement and Suffix attributes
      --  values.

      ----------------
      -- Check_View --
      ----------------

      procedure Check_View (View : Project.View.Object) is

         package Suffix_Lang_Maps is
           new Ada.Containers.Indefinite_Hashed_Maps
             (Value_Type, Language_Id, Ada.Strings.Hash, "=");

         Suffix_Lang_Map : Suffix_Lang_Maps.Map;
         --  key=suffix value; value=first language registering suffix use
         --  map used to detect/report multiple use of a suffix.

         procedure Log_Error
           (Level     : Message.Level_Value;
            Msg       : String;
            Attribute : Project.Attribute.Object);
         --  log naming package's  attribute problem at 'Attribute' source ref

         procedure Check_Casing;
         --  check casing is in expected range

         procedure Check_Dot_Replacement;
         --  check dot_replacement is not illegal

         use type Project.Attribute.Object;

         procedure Check_Illegal_Suffix
           (Attribute_Name : Attribute_Id;
            Language       : Language_Id;
            Attribute      : Project.Attribute.Object)
           with Pre => Attribute /= Project.Attribute.Undefined;
         --  check Spec_Suffix, Body_Suffix or Separate_Suffix is not illegal

         ------------------
         -- Check_Casing --
         ------------------

         procedure Check_Casing is
            Casing : Project.Attribute.Object;
         begin
            if View.Check_Attribute (PRP.Naming, PRA.Casing, Result => Casing)
              and then ACH.To_Lower (Casing.Value.Text) not in
              "lowercase" | "uppercase" | "mixedcase"
            then
               Log_Error (Message.Error, "invalid value for casing", Casing);
            end if;
         end Check_Casing;

         ---------------------------
         -- Check_Dot_Replacement --
         ---------------------------

         procedure Check_Dot_Replacement is
            Dot_Replacement : constant Project.Attribute.Object :=
                                View.Attribute
                                  (PRA.Dot_Replacement, PRP.Naming);
            Value           : constant String :=
                                Dot_Replacement.Value.Text;
            Not_OK          : Boolean := False;
            subtype Printable_ASCII is Character range '!' .. '~';
         begin
            --  It must not be empty
            --  It cannot start or end with an alphanumeric character
            --  It cannot be a single underscore
            --  It cannot start with an underscore followed by an alphanumeric
            --  It cannot contain a dot '.' unless the entire string is "."
            --  It cannot include a space or a char that is not printable ASCII

            if ACH.Is_Alphanumeric (Value (Value'First))
              or else ACH.Is_Alphanumeric (Value (Value'Last))
              or else (Value (Value'First) = '_'
                       and then (Value'Length = 1
                                 or else ACH.Is_Alphanumeric
                                   (Value (Value'First + 1))))
              or else (Value'Length > 1
                       and then ASF.Index
                         (Source => Value, Pattern => ".") > 0)
            then
               Not_OK := True;

            else
               for J in Value'Range loop
                  if not (Value (J) in Printable_ASCII) then
                     Not_OK := True;
                     exit;
                  end if;
               end loop;
            end if;

            if Not_OK then
               Log_Error
                 (Message.Error,
                  """" & Value & """ is illegal for Dot_Replacement",
                  Dot_Replacement);
            end if;
         end Check_Dot_Replacement;

         --------------------------
         -- Check_Illegal_Suffix --
         --------------------------

         procedure Check_Illegal_Suffix
           (Attribute_Name : Attribute_Id;
            Language       : Language_Id;
            Attribute      : Project.Attribute.Object)
         is
            Value    : constant Value_Type := Attribute.Value.Text;
            Dot_Repl : constant Value_Type :=
                         View.Attribute
                           (PRA.Dot_Replacement, PRP.Naming).Value.Text;
         begin
            if Value /= No_Value and then ASF.Index (Value, ".") = 0 then
               Log_Error
                 (Message.Error,
                  """" & Value & """ is illegal for "
                  & Image (Attribute_Name) & ": must have a dot",
                  Attribute);

               return;
            end if;

            --  Case of dot replacement is a single dot, and first character of
            --  suffix is also a dot.

            if Value'Length /= 0
              and then Dot_Repl'Length /= 0
              and then Dot_Repl = "."
              and then Value (Value'First) = '.'
            then
               for Index in Value'First + 1 .. Value'Last loop
                  --  If there are multiple dots in the name

                  if Value (Index) = '.' then
                     --  A letter is illegal following the initial dot

                     if ACH.Is_Letter (Value (Value'First + 1)) then
                        Log_Error
                          (Message.Error,
                           """" & Value & """ is illegal for "
                           & Image (Attribute_Name)
                           & ": ambiguous prefix when "
                           & "Dot_Replacement is a dot",
                           Attribute);
                     end if;

                     return;
                  end if;
               end loop;
            end if;

            --  detect/report multiple use of same suffix.
            --  Separate_Suffix = Body_Suffix ("Ada") is allowed.
            declare
               Associated_Lang : constant Suffix_Lang_Maps.Cursor :=
                                   Suffix_Lang_Map.Find (Value);
               Index           : constant Attribute_Index.Object :=
                                   Attribute_Index.Create (Ada_Language);
            begin
               if Suffix_Lang_Maps.Has_Element (Associated_Lang) then
                  if Suffix_Lang_Maps.Element (Associated_Lang) = Ada_Language
                    and then Attribute_Name = PRA.Separate_Suffix
                    and then View.Has_Attribute (PRA.Body_Suffix,
                                                 Pack  => PRP.Naming,
                                                 Index => Index)
                    and then View.Attribute
                      (PRA.Body_Suffix, PRP.Naming, Index).Value.Text = Value
                  then
                     return;
                  end if;

                  if Language = Suffix_Lang_Maps.Element (Associated_Lang) then
                     Log_Error
                       (Message.Error,
                          Image (Attribute_Name) & " (" & Image (Language) &
                          ") value already used for this language",
                        Attribute);
                  else
                     Log_Error
                       (Message.Error,
                        Image (Attribute_Name) & " (" & Image (Language) &
                          ") value is already used for language " &
                          Image (Suffix_Lang_Maps.Element (Associated_Lang)),
                        Attribute);
                  end if;
               else
                  Suffix_Lang_Map.Include (Value, Language);
               end if;
            end;

         end Check_Illegal_Suffix;

         ---------------
         -- Log_Error --
         ---------------

         procedure Log_Error
           (Level     : Message.Level_Value;
            Msg       : String;
            Attribute : Project.Attribute.Object)
         is
         begin
            View.Tree.Log_Messages.Append
              (Message.Create
                 (Level   => Level,
                  Sloc    => Attribute,
                  Message => Msg));
         end Log_Error;

      begin
         if View.Has_Package (PRP.Naming) then
            Check_Casing;
            Check_Dot_Replacement;

            if View.Kind /= K_Aggregate and then View.Has_Languages then
               for L of View.Languages loop
                  declare
                     Language    : constant Language_Id := +Name_Type (L.Text);
                     Index       : constant Attribute_Index.Object :=
                                     Attribute_Index.Create (Language);
                     Spec_Suffix : constant Attribute.Object :=
                                     View.Attribute
                                       (PRA.Spec_Suffix, PRP.Naming, Index);
                     Body_Suffix : constant Attribute.Object :=
                                     View.Attribute
                                       (PRA.Body_Suffix, PRP.Naming, Index);
                  begin
                     if Spec_Suffix.Is_Defined
                       and then not Spec_Suffix.Is_Default
                     then
                        Check_Illegal_Suffix
                          (PRA.Spec_Suffix,
                           Language,
                           Spec_Suffix);
                     end if;

                     if Body_Suffix.Is_Defined
                       and then not Body_Suffix.Is_Default
                     then
                        Check_Illegal_Suffix
                          (PRA.Body_Suffix,
                           Language,
                           Body_Suffix);
                     end if;
                  end;
               end loop;
            end if;

            declare
               Sep_Suffix : constant Attribute.Object :=
                              View.Attribute
                                (PRA.Separate_Suffix, PRP.Naming);
            begin
               if Sep_Suffix.Is_Defined and then not Sep_Suffix.Is_Default then
                  Check_Illegal_Suffix
                    (PRA.Separate_Suffix,
                     Ada_Language,
                     Sep_Suffix);
               end if;
            end;
         end if;
      end Check_View;

   begin
      for C in View.Tree.Iterate loop
         Check_View (Project.Tree.Element (C));
      end loop;
   end Check_Package_Naming;

   ------------------------------
   -- Check_Same_Name_Extended --
   ------------------------------

   procedure Check_Same_Name_Extended (View : Project.View.Object) is
      procedure Check_View (View : Project.View.Object);
      --  Checks in View tree (extended, aggregated, imported) that
      --  any extending list contains unique project name.

      ----------------
      -- Check_View --
      ----------------

      procedure Check_View (View : Project.View.Object) is
         OK    : Boolean;
         CN    : Containers.Name_Type_Set.Cursor;

         Names : Containers.Name_Set;
         --  set of already found extended's name.

         procedure Check_Extending (View : Project.View.Object);
         --  If View is extending, checks that extended projects list contains
         --  unique project's names.

         ---------------------
         -- Check_Extending --
         ---------------------

         procedure Check_Extending (View : Project.View.Object) is
         begin
            if View.Is_Extending then
               Names.Insert (View.Name, CN, OK);

               if not OK then
                  declare
                     Extending : constant Project.View.Object :=
                                   (if View.Is_Extended
                                    then View.Extending
                                    else View);
                  begin
                     View.Tree.Log_Messages.Append
                       (Message.Create
                          (Level   => Message.Error,
                           Sloc    => SR.Value.Create
                             (Filename => Extending.Path_Name.Value,
                              Line     => 0,
                              Column   => 0,
                              Text     => ""),
                           Message =>
                             "cannot extend a project with the same name"));
                  end;
               end if;

               Check_Extending (View.Extended_Root);
            end if;
         end Check_Extending;

         Def  : constant Const_Ref := Get_RO (View);

      begin
         Check_Extending (View);

         for V of Def.Imports loop
            Check_View (V);
         end loop;

         for V of Def.Aggregated loop
            Check_View (V);
         end loop;

      end Check_View;

   begin
      Check_View (View);
   end Check_Same_Name_Extended;

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache (Def : in out Data)
   is
   begin
      Def.Cache.Clear_Cache;
      Def.Dir_Cache := (others => <>);
   end Clear_Cache;

   -------------------
   -- Disable_Cache --
   -------------------

   procedure Disable_Cache (Def : in out Data)
   is
   begin
      Def.Cache.Disable_Cache;
   end Disable_Cache;

   ------------------
   -- Enable_Cache --
   ------------------

   procedure Enable_Cache (Def : in out Data)
   is
   begin
      Def.Cache.Enable_Cache;
   end Enable_Cache;

   -----------------------
   -- Is_Sources_Loaded --
   -----------------------

   function Is_Sources_Loaded (View : Project.View.Object) return Boolean is
   begin
      return not Get_RO (View).Sources_Map.Is_Empty;
   end Is_Sources_Loaded;

   -----------------------
   -- Source_Map_Insert --
   -----------------------

   procedure Sources_Map_Insert
     (Def : in out Data;
      Src : Project.Source.Object;
      C   : Project.Source.Set.Cursor)
   is
      Position : Simple_Name_Source.Cursor;
      Inserted : Boolean;
   begin
      Def.Sources_Map.Insert
        (Src.Path_Name.Simple_Name, C, Position, Inserted);
   end Sources_Map_Insert;

   --------------------
   -- Update_Sources --
   --------------------

   procedure Update_Sources
     (Def           : in out Data;
      View          : Project.View.Object;
      Stop_On_Error : Boolean;
      Backends      : Source_Info.Backend_Set)
   is
   begin
      Update_Sources_List (Def, View, Stop_On_Error);
      Update_Sources_Parse
        (Def, Backends);
   end Update_Sources;

   -------------------------
   -- Update_Sources_List --
   -------------------------

   procedure Update_Sources_List
     (Def           : in out Data;
      View          : Project.View.Object;
      Stop_On_Error : Boolean) is separate;

   --------------------------
   -- Update_Sources_Parse --
   --------------------------

   procedure Update_Sources_Parse
     (Def : in out Data; Backends : Source_Info.Backend_Set)
   is
      Repeat_Map  : Simple_Name_Source.Map; -- Second pass for subunits
      Position    : Simple_Name_Source.Cursor;
      Inserted    : Boolean;
      SW          : Project.Source.Object;

      procedure Insert_SW (C : Project.Source.Set.Cursor);
      --  Insert SW into Def_Sources and Def_Src_Map

      ---------------
      -- Insert_SW --
      ---------------

      procedure Insert_SW (C : Project.Source.Set.Cursor) is
         use GPR2.Unit;
         CUnits : GPR2.Project.Unit_Info.Set.Cursor;
      begin
         Def.Sources.Replace (C, SW);

         if SW.Has_Units then
            --  Check newly found separates and update Unit_Info
            for Unit of SW.Units loop
               if Unit.Kind = S_Separate then
                  CUnits := Def.Units.Find (Unit.Separate_From);
                  if GPR2.Project.Unit_Info.Set.Set.Has_Element (CUnits) then
                     declare
                        Ref : constant Unit_Info.Set.Set.Reference_Type :=
                                Def.Units.Reference (CUnits);
                        SUI : constant GPR2.Unit.Source_Unit_Identifier :=
                                (SW.Path_Name, Unit.Index);
                     begin
                        if not Ref.Separates.Contains (SUI) then
                           Ref.Update_Separates (SUI);
                        end if;
                     end;
                  end if;
               end if;

               Def.Units_Map.Include (Key (Unit), C);
            end loop;
         end if;
      end Insert_SW;

   begin
      Source_Info.Parser.Registry.Clear_Cache;
      Def.Units_Map.Clear;

      for C in Def.Sources.Iterate loop
         SW := Project.Source.Set.Element (C);

         --  If the view is extended, we will use the ALI from the extending
         --  project. We still need to call SW.Update to disambiguate
         --  Spec/Spec_Only and Body/Body_Only units.

         SW.Update
           (if Def.Extending.Was_Freed
            then Backends
            else Source_Info.No_Backends);

         if SW.Is_Parsed (No_Index)
           or else not Def.Extending.Was_Freed
           or else SW.Language /= Ada_Language
         then
            Insert_SW (C);

         else
            --  It can be subunit case in runtime krunched source names, need
            --  to repeat after all .ali files parsed.

            Repeat_Map.Insert
              (SW.Path_Name.Simple_Name, C, Position, Inserted);

            pragma Assert
              (Inserted,
               String (SW.Path_Name.Simple_Name) & " subunit duplicated");
         end if;
      end loop;

      for C of Repeat_Map loop
         SW := Project.Source.Set.Element (C);
         SW.Update (Backends);
         Insert_SW (C);
      end loop;

      --  Check unit-based interface attributes

      if not Def.Interface_Units.Is_Empty then
         for C in Def.Interface_Units.Iterate loop
            declare
               Name : constant Name_Type := Unit_Name_To_Sloc.Key (C);
            begin
               if not Def.Units_Map.Contains ('S' & To_Lower (Name))
                 and then not Def.Units_Map.Contains ('B' & To_Lower (Name))
               then
                  Def.Tree.Append_Message
                    (Message.Create
                       (Message.Error,
                        "source for interface unit '" & String (Name)
                        & "' not found",
                        Unit_Name_To_Sloc.Element (C)));
               end if;
            end;
         end loop;
      end if;
   end Update_Sources_Parse;

end GPR2.Project.Definition;
