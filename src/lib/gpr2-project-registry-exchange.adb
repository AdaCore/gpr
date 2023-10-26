--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Handling;

with GNATCOLL;
with GNATCOLL.JSON;

with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Attribute.Description;
with GPR2.Project.Registry.Pack.Description;
with GPR2.Project.View;

package body GPR2.Project.Registry.Exchange is
   use GNATCOLL.JSON;

   --  constant strings used in exchange data JSON & Textual

   ABSTRACT_QUALIFIER         : constant UTF8_String := "abstract";
   AGGREGATE                  : constant UTF8_String := "aggregate";
   AGGREGATE_LIBRARY          : constant UTF8_String := "aggregate_library";
   ATTR                       : constant UTF8_String := "attr";
   ATTRIBUTE_DEF              : constant UTF8_String := "attribute_def";
   ATTRIBUTE_DESCR            : constant UTF8_String := "attribute_descr";
   ATTRIBUTE_NAME             : constant UTF8_String := "attribute_name";
   ATTRIBUTE_REFERENCE        : constant UTF8_String := "attribute_reference";
   ATTRIBUTES                 : constant UTF8_String := "attributes";
   BUILTIN                    : constant UTF8_String := "builtin";
   CALLBACK                   : constant UTF8_String := "callback";
   CASE_INSENSITIVE_INDEX     : constant UTF8_String :=
                                  "case-insensitive index";
   CONFIGURATION_CONCATENABLE : constant UTF8_String := "config_concatenable";
   CONFIGURATION              : constant UTF8_String := "configuration";
   DEFAULT_ATTRIBUTE          : constant UTF8_String := "default";
   DEFAULT_VALUE_KIND         : constant UTF8_String := "default_value_kind";
   DESCRIPTION                : constant UTF8_String := "Description";
   EMPTY_VALUE                : constant UTF8_String := "empty_value";
   FILE_NAME_INDEX            : constant UTF8_String := "file name index";
   HAS_DEFAULT_IN             : constant UTF8_String := "has_default_in";
   INDEXED                    : constant UTF8_String := "indexed";
   INDEX_TYPE                 : constant UTF8_String := "index_type";
   INDEX_OPTIONAL             : constant UTF8_String := "index_optional";
   INHERIT_FROM_EXTENDED      : constant UTF8_String :=
                                  "inherit_from_extended";
   IS_ALLOWED_IN              : constant UTF8_String := "is_allowed_in";
   IS_TOOLCHAIN_CONFIG        : constant UTF8_String := "is_toolchain_config";
   LIBRARY                    : constant UTF8_String := "library";
   LIST                       : constant UTF8_String := "list";
   OPTIONAL_INDEX             : constant UTF8_String := "optional index";
   OTHERS_ALLOWED             : constant UTF8_String := "others allowed";
   PACKAGE_DESCR              : constant UTF8_String := "package_descr";
   PACKAGE_NAME               : constant UTF8_String := "package_name";
   PACKAGES                   : constant UTF8_String := "packages";
   PROJECT_LEVEL              : constant UTF8_String := "Project_Level";
   PROJECTS_KIND              : constant UTF8_String := "projects_kind";
   READ_ONLY                  : constant UTF8_String := "read-only";
   SINGLE                     : constant UTF8_String := "single";
   SPECIAL                    : constant UTF8_String := "special";
   STANDARD                   : constant UTF8_String := "standard";
   TYPE_DEF                   : constant UTF8_String := "type_def";
   VALUE                      : constant UTF8_String := "value";
   VALUE_CASE_INSENSITIVE     : constant UTF8_String := "value_case_sensitive";
   VALUE_IS_SET               : constant UTF8_String := "value_is_set";

   function Dummy_Callback
     (View : GPR2.Project.View.Object) return Value_Type;
   --  callback used when importing a callback based default value.

   function Dummy_Callback
     (View : GPR2.Project.View.Object) return Value_Type is
     ("WARNING: stub created by registry import. Not the real default value");

   ------------
   -- Export --
   ------------

   procedure Export
     (Included : GPR2.Containers.Package_Id_List :=
        GPR2.Project.Registry.Pack.All_Packages;
      Excluded : GPR2.Containers.Package_Id_List :=
        GPR2.Project.Registry.Pack.Predefined_Packages;
      Format   : Export_Format := K_JSON_COMPACT;
      Output : access procedure (Item : String) := Ada.Text_IO.Put'Access) is

      use GNATCOLL;
      use GPR2.Project.Registry.Attribute;

      package PRA  renames GPR2.Project.Registry.Attribute;
      package PRP  renames GPR2.Project.Registry.Pack;
      package PRAD renames GPR2.Project.Registry.Attribute.Description;
      package PRPD  renames GPR2.Project.Registry.Pack.Description;

      function Is_Included (Pack : Package_Id) return Boolean is
        (Included.Contains (Pack) and then not Excluded.Contains (Pack));
      --  return True is 'Pack' should be exported.

      procedure Append (Item : String);
      --  Append 'Item' through 'Output' callback.

      procedure Append_Line (Item : String := "");
      --  Append 'Item' + LF through 'Output' callback.

      procedure Generate_IO_Textual_Documentation;
      --  Format the attributes and package information to generate a textual
      --  output.

      procedure Generate_JSON_Documentation (Compact : Boolean);
      --  Format the attributes and package information to generate a JSON
      --  output which will be used to generate the documentation.

      ------------
      -- Append --
      ------------

      procedure Append (Item : String) is
      begin
         Output.all (Item);
      end Append;

      -----------------
      -- Append_Line --
      -----------------

      procedure Append_Line (Item : String := "") is
      begin
         Output.all (Item & ASCII.LF);
      end Append_Line;

      ---------------------------------------
      -- Generate_IO_Textual_Documentation --
      ---------------------------------------

      procedure Generate_IO_Textual_Documentation is

         procedure Display_Attributes
           (Attr_Name : Q_Attribute_Id;
            Attr_Def  : Def);
         --  Display the attributes as the same GNAT Project Manager
         --  Documentation output. Useful in order to compare the actual code
         --  attributes definitions to the existing documentation.

         procedure Generate_Package_Attributes
           (Pack : Package_Id);
         --  Look for all attributes for a given package

         ------------------------
         -- Display_Attributes --
         ------------------------

         procedure Display_Attributes
           (Attr_Name : Q_Attribute_Id;
            Attr_Def  : Def)
         is
            K_Separator : constant String := ", ";
         begin

            Append (Item => Image (Attr_Name.Attr) & ": ");

            if Attr_Def.Value = GPR2.Project.Registry.Attribute.Single
            then
               Append (Item => SINGLE);
            else
               Append (Item => LIST);
            end if;

            if Attr_Def.Builtin
            then
               Append (Item => K_Separator & READ_ONLY);
            end if;

            if Attr_Def.Index_Type /= PRA.No_Index
            then
               if Attr_Def.Index_Optional
               then
                  Append (Item => K_Separator & OPTIONAL_INDEX);
               end if;

               Append (Item => K_Separator & INDEXED);
            end if;

            case Attr_Def.Index_Type is
            when PRA.No_Index | PRA.String_Index | PRA.Env_Var_Name_Index =>
               null;

            when PRA.Unit_Index | PRA.Language_Index =>
               Append (Item => K_Separator & CASE_INSENSITIVE_INDEX);

            when PRA.File_Index |
                 PRA.FileGlob_Index |
                 PRA.FileGlob_Or_Language_Index =>

               if not GPR2.File_Names_Case_Sensitive
               then
                  Append (Item => K_Separator & CASE_INSENSITIVE_INDEX);
               end if;
            end case;

            if  Attr_Def.Index_Type = PRA.File_Index
              or else
                Attr_Def.Index_Type = PRA.FileGlob_Index
            then
               Append (Item => K_Separator & FILE_NAME_INDEX);
            end if;

            if Attr_Def.Index_Optional
            then
               Append (Item => K_Separator & OTHERS_ALLOWED);
            end if;

            if Attr_Def.Config_Concatenable
            then
               Append (Item => K_Separator & CONFIGURATION_CONCATENABLE);
            end if;

            Append_Line;
            Append_Line
              (Item => DESCRIPTION & ": "
               & PRAD.Get_Attribute_Description (Key => Attr_Name));

            Append_Line;
         end Display_Attributes;

         ---------------------------------
         -- Generate_Package_Attributes --
         ---------------------------------

         procedure Generate_Package_Attributes
           (Pack : Package_Id)
         is
            Package_Name         : constant String :=
                                     (if Pack = Project_Level_Scope
                                      then PROJECT_LEVEL
                                      else Image (Pack));
            Package_Name_Shown   : Boolean := False;
            Attribute_Definition : Def;
         begin

            --  For every attributes of a given package

            for Attr_Id of PRA.All_Attributes (Pack => Pack)
            loop

               --  Get the attribute information
               Attribute_Definition :=
                 PRA.Get (Q_Name => Attr_Id);

               --  Display attribute and package information

               if not Package_Name_Shown then
                  Append_Line (Item => Package_Name);
                  Append_Line
                    (Item => DESCRIPTION & ": "
                     & PRPD.Get_Package_Description (Key => Pack));
                  Package_Name_Shown := True;
               end if;

               Display_Attributes
                 (Attr_Name => Attr_Id,
                  Attr_Def  => Attribute_Definition);

               Append_Line;

            end loop;

         end Generate_Package_Attributes;

      begin

         --  First retrieve all attributes from the Top-Level package

         if Is_Included (Project_Level_Scope) then
            Generate_Package_Attributes (Pack => Project_Level_Scope);
         end if;

         --  Then retrieve all attributes from all registered packages

         for Pack of PRP.All_Packages
         loop
            if Is_Included (Pack) then
               Generate_Package_Attributes (Pack => Pack);
            end if;
         end loop;

      end Generate_IO_Textual_Documentation;

      ----------------------------
      -- Generate_Documentation --
      ----------------------------

      procedure Generate_JSON_Documentation (Compact : Boolean) is

         function Attribute_Object (Attr_Name  : Q_Attribute_Id;
                                    Attr_Def   : Def;
                                    Attr_Descr : String) return JSON_Value;
         --  The attribute JSON object description

         function Package_Object (Pack      : Package_Id;
                                  Attr_List : JSON_Array) return JSON_Value;
         --  The package JSON object description

         ----------------------
         -- Attribute_Object --
         ----------------------

         function Attribute_Object (Attr_Name  : Q_Attribute_Id;
                                    Attr_Def   : Def;
                                    Attr_Descr : String) return JSON_Value
         is

            function Attribute_Def_Object (Attr_Def : Def) return JSON_Value;
            --  The attribute definition JSON object description

            --------------------------
            -- Attribute_Def_Object --
            --------------------------

            function Attribute_Def_Object (Attr_Def : Def) return JSON_Value
            is
               function Array_Object (A : Allowed_In) return JSON_Value;
               --  The Allowed_In JSON object description

               function Array_Object (A : Attribute_Type) return JSON_Value;
               --  The Attribute_Type JSON object description

               function Variant_Record (VR : Default_Value) return JSON_Value;
               --  The variant record JSON object description

               ------------------
               -- Array_Object --
               ------------------

               function Array_Object (A : Allowed_In) return JSON_Value is
                  Obj : constant JSON_Value := Create_Object;
               begin
                  for Elt in A'Range loop
                     Set_Field
                       (Val        => Obj,
                        Field_Name => Ada.Characters.Handling.To_Lower
                          (Elt'Img (3 .. Elt'Img'Last)), --  To get rid of "K_"
                        Field      => A (Elt));
                  end loop;

                  return Obj;
               end Array_Object;

               function Array_Object (A : Attribute_Type) return JSON_Value is
                  A_Array : JSON_Array;
               begin
                  for S of A loop
                     Append (A_Array, Create (S));
                  end loop;
                  return Create (A_Array);
               end Array_Object;

               --------------------
               -- Variant_Record --
               --------------------

               function Variant_Record
                 (VR : Default_Value) return JSON_Value is
                  Obj : constant JSON_Value := Create_Object;
               begin

                  Set_Field
                    (Val        => Obj,
                     Field_Name => DEFAULT_VALUE_KIND,
                     Field      => Ada.Characters.Handling.To_Lower
                       (VR.Kind'Img (3 .. VR.Kind'Img'Last)));
                  --  To get rid of "D_"

                  case VR.Kind is
                  when D_Attribute_Reference =>
                     Set_Field (Val        => Obj,
                                Field_Name => ATTR,
                                Field      => Image (VR.Attr));

                  when D_Value =>
                     if Attr_Def.Index_Type =
                       GPR2.Project.Registry.Attribute.No_Index
                     then
                        Set_Field (Val => Obj,
                                   Field_Name => VALUE,
                                   Field      => VR.Values.First_Element);
                     else
                        for Elt in VR.Values.Iterate loop
                           Set_Field
                             (Val        => Obj,
                              Field_Name => Value_Map.Key (Elt),
                              Field      => Value_Map.Element (Elt));
                        end loop;
                     end if;

                  when D_Callback =>
                     Set_Field (Val        => Obj,
                                Field_Name => CALLBACK,
                                Field      => SPECIAL);

                  end case;

                  return Obj;
               end Variant_Record;

               Obj : constant JSON_Value := Create_Object;

               use type Attribute_Type;
            begin
               Set_Field (Val        => Obj,
                          Field_Name => INDEX_TYPE,
                          Field      => Attr_Def.Index_Type'Img);
               Set_Field (Val        => Obj,
                          Field_Name => INDEX_OPTIONAL,
                          Field      => Attr_Def.Index_Optional);
               Set_Field (Val        => Obj,
                          Field_Name => VALUE,
                          Field      => Attr_Def.Value'Img);
               Set_Field (Val        => Obj,
                          Field_Name => VALUE_CASE_INSENSITIVE,
                          Field      => Attr_Def.Value_Case_Sensitive);
               Set_Field (Val        => Obj,
                          Field_Name => VALUE_IS_SET,
                          Field      => Attr_Def.Value_Is_Set);
               Set_Field (Val        => Obj,
                          Field_Name => EMPTY_VALUE,
                          Field      => Attr_Def.Empty_Value'Img);
               Set_Field (Val        => Obj,
                          Field_Name => BUILTIN,
                          Field      => Attr_Def.Builtin);
               Set_Field (Val        => Obj,
                          Field_Name => IS_ALLOWED_IN,
                          Field      => Array_Object
                            (A => Attr_Def.Is_Allowed_In));
               if Attr_Def.Type_Def /= No_Attribute_Type then
                  Set_Field (Val        => Obj,
                             Field_Name => TYPE_DEF,
                             Field      => Array_Object
                               (A =>  Attr_Def.Type_Def));
               end if;
               Set_Field (Val        => Obj,
                          Field_Name => HAS_DEFAULT_IN,
                          Field      => Array_Object
                            (A =>  Attr_Def.Has_Default_In));

               if Attr_Def.Has_Default_In /= Nowhere
               then
                  Set_Field (Val        => Obj,
                             Field_Name => DEFAULT_ATTRIBUTE,
                             Field      => Variant_Record
                               (VR => Attr_Def.Default));
               end if;

               Set_Field (Val        => Obj,
                          Field_Name => IS_TOOLCHAIN_CONFIG,
                          Field      => Attr_Def.Is_Toolchain_Config);
               Set_Field (Val        => Obj,
                          Field_Name => CONFIGURATION_CONCATENABLE,
                          Field      => Attr_Def.Config_Concatenable);
               Set_Field (Val        => Obj,
                          Field_Name => INHERIT_FROM_EXTENDED,
                          Field      => Attr_Def.Inherit_From_Extended'Img);

               return Obj;
            end Attribute_Def_Object;

            Obj : constant JSON_Value := Create_Object;
         begin
            Set_Field (Val        => Obj,
                       Field_Name => ATTRIBUTE_NAME,
                       Field      => Image (Attr_Name.Attr));
            Set_Field (Val        => Obj,
                       Field_Name => ATTRIBUTE_DEF,
                       Field      => Attribute_Def_Object
                                       (Attr_Def => Attr_Def));
            Set_Field (Val        => Obj,
                       Field_Name => ATTRIBUTE_DESCR,
                       Field      => Attr_Descr);

            return Obj;
         end Attribute_Object;

         --------------------
         -- Package_Object --
         --------------------

         function Package_Object (Pack      : Package_Id;
                                  Attr_List : JSON_Array) return JSON_Value
         is

            function Projects_Kind_Object
              (Pack : Package_Id) return JSON_Value;
            --  Create a JSON value containing project kinds in which the
            --  package 'Pack' is allowed.

            --------------------------
            -- Projects_Kind_Object --
            --------------------------

            function Projects_Kind_Object
              (Pack : Package_Id) return JSON_Value is
               Obj : constant JSON_Value := Create_Object;
            begin
               if Pack /= Project_Level_Scope and then
                 GPR2.Project.Registry.Pack.Exists (Pack)
               then
                  for Kind in GPR2.Project_Kind loop
                     Set_Field
                       (Val        => Obj,
                        Field_Name => Ada.Characters.Handling.To_Lower
                          (Kind'Img (3 .. Kind'Img'Last)), -- get rid of "K_"
                        Field      => Project.Registry.Pack.Is_Allowed_In
                          (Pack, Kind));
                  end loop;
               end if;

               return Obj;
            end Projects_Kind_Object;

            Name : constant String :=
                     (if Pack = Project_Level_Scope
                      then PROJECT_LEVEL
                      else Image (Pack));
            --  'Pack' package name or PROJECT_LEVEL for project level
            --  attributes

            Obj   : constant JSON_Value := Create_Object;
            --  Created JSON object that is returned.

         begin
            Set_Field (Val        => Obj,
                       Field_Name => PACKAGE_NAME,
                       Field      => Name);
            Set_Field (Val        => Obj,
                       Field_Name => PACKAGE_DESCR,
                       Field      => PRPD.Get_Package_Description
                                       (Key => Pack));
            Set_Field (Val        => Obj,
                       Field_Name => ATTRIBUTES,
                       Field      => Attr_List);

            if Pack /= Project_Level_Scope then
               Set_Field (Val        => Obj,
                          Field_Name => PROJECTS_KIND,
                          Field      => Projects_Kind_Object (Pack));
            end if;

            return Obj;
         end Package_Object;

         J_Doc : constant JSON_Value := Create_Object;
         --  The JSON doc root

         P_Array : JSON_Array;
         --  The JSON package's array

      begin

         --  First retrieve all attributes from the Top-Level package

         if Is_Included (Project_Level_Scope) then
            declare
               Attr_Def : Def;
               --  current attribute definition data

               A_Array  : JSON_Array;
               --  The JSON attribute's array
            begin

               --  Fill Top-Level package's attributes

               for Attr_Id of PRA.All_Attributes (Pack => Project_Level_Scope)
               loop

                  --  Get the attribute information
                  Attr_Def :=
                    PRA.Get (Q_Name => Attr_Id);

                  --  Create the JSON Attributes list

                  JSON.Append
                    (Arr => A_Array,
                     Val => Attribute_Object
                       (Attr_Name  => Attr_Id,
                        Attr_Def   => Attr_Def,
                        Attr_Descr => PRAD.Get_Attribute_Description
                          (Key => Attr_Id)));

               end loop;

               --  Create the JSON package with the attribute list

               JSON.Append (Arr => P_Array,
                            Val => Package_Object
                              (Pack      => Project_Level_Scope,
                               Attr_List => A_Array));
            end;
         end if;

         --  Then retrieve all attributes from all registered packages

         for Pack of PRP.All_Packages
         loop
            if Is_Included (Pack) then
               declare
                  Attr_Def : Def;
                  --  current attribute definition data

                  A_Array  : JSON_Array;
                  --  The JSON attribute's array
               begin

                  --  Fill 'Pack' package's attributes

                  for Attr_Id of PRA.All_Attributes (Pack => Pack)
                  loop

                     --  Get the attribute information
                     Attr_Def :=
                       PRA.Get (Q_Name => Attr_Id);

                     --  Create the JSON Attributes list

                     JSON.Append
                       (Arr => A_Array,
                        Val => Attribute_Object
                          (Attr_Name  => Attr_Id,
                           Attr_Def   => Attr_Def,
                           Attr_Descr => PRAD.Get_Attribute_Description
                             (Key => Attr_Id)));

                  end loop;

                  --  Create the JSON package with the attribute list

                  JSON.Append (Arr => P_Array,
                               Val => Package_Object (Pack      => Pack,
                                                      Attr_List => A_Array));
               end;
            end if;
         end loop;

         --  Add packages to JSON document

         Set_Field (Val        => J_Doc,
                    Field_Name => PACKAGES,
                    Field      => P_Array);

         --  Output JSON document

         Append_Line
           (Item => JSON.Write (Item => J_Doc, Compact => Compact));

      end Generate_JSON_Documentation;

   begin
      case Format is
         when K_JSON_COMPACT =>
            Generate_JSON_Documentation (Compact => True);

         when K_JSON =>
            Generate_JSON_Documentation (Compact => False);

         when K_TEXT =>
            Generate_IO_Textual_Documentation;
      end case;
   end Export;

   ------------
   -- Import --
   ------------

   procedure Import (Definitions : Ada.Strings.Unbounded.Unbounded_String;
                     Included : GPR2.Containers.Package_Id_List :=
                       GPR2.Project.Registry.Pack.All_Packages;
                     Excluded : GPR2.Containers.Package_Id_List :=
                       GPR2.Project.Registry.Pack.Predefined_Packages) is

      use GPR2.Project.Registry.Attribute;

      -------------------
      --  JSON Getters --
      -------------------

      function Get_JSON_Value
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : JSON_Value := JSON_Null) return JSON_Value;

      function Get_String
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : String := "") return String;

      function Get_Bool
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : Boolean := False) return Boolean;

      function Get_Allowed_In
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : GPR2.Project.Registry.Attribute.Allowed_In :=
           GPR2.Project.Registry.Attribute.Everywhere)
         return GPR2.Project.Registry.Attribute.Allowed_In;

      function Get_Projects_Kind
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : GPR2.Project.Registry.Pack.Projects_Kind :=
           GPR2.Project.Registry.Pack.Everywhere)
         return GPR2.Project.Registry.Pack.Projects_Kind;

      function Get_Array
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : JSON_Array := Empty_Array) return JSON_Array;

      function Get_Index_Value_Type
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : Index_Value_Type := Project.Registry.Attribute.No_Index)
         return Index_Value_Type;

      function Get_Value_Kind
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : Value_Kind := Project.Registry.Attribute.Single)
         return Value_Kind;

      function Get_Empty_Value_Status
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : Empty_Value_Status := Allow)
         return Empty_Value_Status;

      function Get_Inherit_From_Extended_Type
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : Inherit_From_Extended_Type := Inherited)
         return Inherit_From_Extended_Type;

      function Get_Default_Value
        (Val     : JSON_Value;
         Default : Default_Value := No_Default_Value)
         return Default_Value;

      function Get_Attribute_Type
        (Val     : JSON_Value;
         Default : Attribute_Type := No_Attribute_Type)
         return Attribute_Type;


      procedure Import_Package (Pack : JSON_Value);
      --  Import into GPR2 registry package/attributes found in 'Pack' JSON
      --  value

      procedure Import_Attribute (Pack : Package_Id; Attribute : JSON_Value);
      --  Import into GPR2 registry for 'Pack' package the attribute defined by
      --  'Attribute' JSON value

      function Is_Included (Pack : Package_Id) return Boolean is
        ((Included.Contains (Pack) or else Included.Is_Empty)
         and then not Excluded.Contains (Pack));
      --  return True if 'Pack' should be imported as defined by 'Included' &
      --  'Excluded'

      --------------------
      -- Get_Allowed_In --
      --------------------

      function Get_Allowed_In
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : GPR2.Project.Registry.Attribute.Allowed_In :=
           GPR2.Project.Registry.Attribute.Everywhere)
         return GPR2.Project.Registry.Attribute.Allowed_In is
         Result : GPR2.Project.Registry.Attribute.Allowed_In := Default;
         V : JSON_Value;
      begin
         if Value.Kind = JSON_Object_Type and then Value.Has_Field (Field) then
            V := Value.Get (Field);
            if V.Kind = JSON_Object_Type then
               Result (K_Configuration) := Get_Bool
                 (Value   => V,
                  Field   => CONFIGURATION,
                  Default => Default (K_Configuration));
               Result (K_Abstract) := Get_Bool
                 (Value   => V,
                  Field   => ABSTRACT_QUALIFIER,
                  Default => Default (K_Abstract));
               Result (K_Standard) := Get_Bool
                 (Value   => V,
                  Field   => STANDARD,
                  Default => Default (K_Standard));
               Result (K_Library) := Get_Bool
                 (Value   => V,
                  Field   => LIBRARY,
                  Default => Default (K_Library));
               Result (K_Aggregate) := Get_Bool
                 (Value   => V,
                  Field   => AGGREGATE,
                  Default => Default (K_Aggregate));
               Result (K_Aggregate_Library) := Get_Bool
                 (Value   => V,
                  Field   => AGGREGATE_LIBRARY,
                  Default => Default (K_Aggregate_Library));

               return Result;
            else
               return Result;
            end if;
         else
            return Result;
         end if;
      end Get_Allowed_In;

      ---------------
      -- Get_Array --
      ---------------

      function Get_Array
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : JSON_Array := Empty_Array) return JSON_Array is
         V : JSON_Value;
      begin
         if Value.Kind = JSON_Object_Type and then Value.Has_Field (Field) then
            V := Value.Get (Field);
            if V.Kind = JSON_Array_Type then
               return V.Get;
            else
               return Default;
            end if;
         else
            return Default;
         end if;
      end Get_Array;

      ------------------------
      -- Get_Attribute_Type --
      ------------------------

      function Get_Attribute_Type
        (Val     : JSON_Value;
         Default : Attribute_Type := No_Attribute_Type)
         return Attribute_Type is
         A_Array : constant JSON_Array := Get_Array (Val, TYPE_DEF);
      begin
         if A_Array /= Empty_Array then
            declare
               Result : Attribute_Type;
            begin
               for Attribute_Value of A_Array loop
                  if Attribute_Value.Kind = JSON_String_Type then
                     Result.Insert (Attribute_Value.Get);
                  end if;
               end loop;
               return Result;
            end;
         end if;
         return Default;
      end Get_Attribute_Type;

      --------------
      -- Get_Bool --
      --------------

      function Get_Bool
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : Boolean := False) return Boolean is
         V : JSON_Value;
      begin
         if Value.Kind = JSON_Object_Type and then Value.Has_Field (Field) then
            V := Value.Get (Field);
            if V.Kind = JSON_Boolean_Type then
               return V.Get;
            else
               return Default;
            end if;
         else
            return Default;
         end if;
      end Get_Bool;

      -----------------------
      -- Get_Default_Value --
      -----------------------

      function Get_Default_Value
        (Val   : JSON_Value;
         Default : Default_Value := No_Default_Value)
         return Default_Value is

         Def_Value : constant JSON_Value :=
                       Get_JSON_Value (Val, DEFAULT_ATTRIBUTE);
         --  'Val' default attribute JSON value

         Kind : constant String := Get_String (Def_Value, DEFAULT_VALUE_KIND);
         --  Def_Value default value kind

         Values : Value_Map.Map;
         --  Values returned if 'Kind' is VALUE

         procedure Handle_Value (Name : UTF8_String; Value : JSON_Value);
         --  Insert in 'Values' ('Name', 'Value') if needed

         ------------------
         -- Handle_Value --
         ------------------

         procedure Handle_Value (Name : UTF8_String; Value : JSON_Value) is
         begin
            if Name /= DEFAULT_VALUE_KIND
              and then Value.Kind = JSON_String_Type
            then
               Values.Insert (Name, Value.Get);
            end if;
         end Handle_Value;

      begin
         if Kind = VALUE then
            Map_JSON_Object (Def_Value, Handle_Value'Access);
            return (D_Value, Values);
         elsif Kind = ATTRIBUTE_REFERENCE then
            return (D_Attribute_Reference,
                                  +Optional_Name_Type
                                    (Get_String (Def_Value, ATTR)));
         elsif Kind = CALLBACK then
            return ((D_Callback, Dummy_Callback'Access));
         end if;
         return Default;
      end Get_Default_Value;

      ----------------------------
      -- Get_Empty_Value_Status --
      ----------------------------

      function Get_Empty_Value_Status
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : Empty_Value_Status := Allow)
         return Empty_Value_Status is
      begin
         return Empty_Value_Status'Value
           (Get_String (Value, Field, Default'Img));
      exception
         when Constraint_Error =>
            return Default;
      end Get_Empty_Value_Status;

      --------------------------
      -- Get_Index_Value_Type --
      --------------------------

      function Get_Index_Value_Type
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : Index_Value_Type := Project.Registry.Attribute.No_Index)
         return Index_Value_Type is
      begin
         return Index_Value_Type'Value
           (Get_String (Value, Field, Default'Img));
      exception
         when Constraint_Error =>
            return Default;
      end Get_Index_Value_Type;

      ------------------------------------
      -- Get_Inherit_From_Extended_Type --
      ------------------------------------

      function Get_Inherit_From_Extended_Type
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : Inherit_From_Extended_Type := Inherited)
         return Inherit_From_Extended_Type is
      begin
         return Inherit_From_Extended_Type'Value
           (Get_String (Value, Field, Default'Img));
      exception
         when Constraint_Error =>
            return Default;
      end Get_Inherit_From_Extended_Type;

      --------------------
      -- Get_JSON_Value --
      --------------------

      function Get_JSON_Value
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : JSON_Value := JSON_Null) return JSON_Value is
         V : JSON_Value;
      begin
         if Value.Kind = JSON_Object_Type and then Value.Has_Field (Field) then
            V := Value.Get (Field);
            if V.Kind = JSON_Object_Type then
               return V;
            else
               return Default;
            end if;
         else
            return Default;
         end if;
      end Get_JSON_Value;

      -----------------------
      -- Get_Projects_Kind --
      -----------------------

      function Get_Projects_Kind
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : GPR2.Project.Registry.Pack.Projects_Kind :=
           GPR2.Project.Registry.Pack.Everywhere)
         return GPR2.Project.Registry.Pack.Projects_Kind is
         Result : GPR2.Project.Registry.Pack.Projects_Kind := Default;
         V      : JSON_Value;
      begin
         if Value.Kind = JSON_Object_Type and then Value.Has_Field (Field) then
            V := Value.Get (Field);
            if V.Kind = JSON_Object_Type then
               Result (K_Configuration) := Get_Bool
                 (Value   => V,
                  Field   => CONFIGURATION,
                  Default => Default (K_Configuration));
               Result (K_Abstract) := Get_Bool
                 (Value   => V,
                  Field   => ABSTRACT_QUALIFIER,
                  Default => Default (K_Abstract));
               Result (K_Standard) := Get_Bool
                 (Value   => V,
                  Field   => STANDARD,
                  Default => Default (K_Standard));
               Result (K_Library) := Get_Bool
                 (Value   => V,
                  Field   => LIBRARY,
                  Default => Default (K_Library));
               Result (K_Aggregate) := Get_Bool
                 (Value   => V,
                  Field   => AGGREGATE,
                  Default => Default (K_Aggregate));
               Result (K_Aggregate_Library) := Get_Bool
                 (Value   => V,
                  Field   => AGGREGATE_LIBRARY,
                  Default => Default (K_Aggregate_Library));

               return Result;
            else
               return Result;
            end if;
         else
            return Result;
         end if;
      end Get_Projects_Kind;

      ----------------
      -- Get_String --
      ----------------

      function Get_String
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : String := "") return String is
         V : JSON_Value;
      begin
         if Value.Kind = JSON_Object_Type and then Value.Has_Field (Field) then
            V := Value.Get (Field);
            if V.Kind = JSON_String_Type then
               return V.Get;
            else
               return Default;
            end if;
         else
            return Default;
         end if;
      end Get_String;

      --------------------
      -- Get_Value_Kind --
      --------------------

      function Get_Value_Kind
        (Value   : JSON_Value;
         Field   : UTF8_String;
         Default : Value_Kind := Project.Registry.Attribute.Single)
         return Value_Kind is
      begin
         return Value_Kind'Value
           (Get_String (Value, Field, Default'Img));
      exception
         when Constraint_Error =>
            return Default;
      end Get_Value_Kind;

      ----------------------
      -- Import_Attribute --
      ----------------------

      procedure Import_Attribute (Pack : Package_Id; Attribute : JSON_Value) is

         Name                  : constant String :=
                                   Get_String (Attribute, ATTRIBUTE_NAME);
         Attr_Def              : constant JSON_Value :=
                                   Get_JSON_Value (Attribute, ATTRIBUTE_DEF);
         Attr_Index_Type       : constant Index_Value_Type :=
                                   Get_Index_Value_Type
                                     (Attr_Def, INDEX_TYPE);
         Attr_Value            : constant Value_Kind :=
                                   Get_Value_Kind (Attr_Def, VALUE);
         Value_Case_Sensitive  : constant Boolean :=
                                   Get_Bool
                                     (Attr_Def, VALUE_CASE_INSENSITIVE);
         Attr_Allowed_In       : constant Allowed_In :=
                                   Get_Allowed_In (Attr_Def, IS_ALLOWED_IN);
         Type_Def              : constant Attribute_Type :=
                                   Get_Attribute_Type (Attr_Def);
         Is_Builtin            : constant Boolean :=
                                   Get_Bool (Attr_Def, BUILTIN);
         Index_Is_Optional     : constant Boolean :=
                                   Get_Bool (Attr_Def, INDEX_OPTIONAL);
         Attr_Empty_Value      : constant Empty_Value_Status :=
                                   Get_Empty_Value_Status
                                     (Attr_Def, EMPTY_VALUE);
         Default               : constant Default_Value :=
                                   Get_Default_Value (Attr_Def);
         Attr_Has_Default_In   : constant Allowed_In :=
                                   Get_Allowed_In (Attr_Def, HAS_DEFAULT_IN);
         Is_Toolchain_Conf     : constant Boolean :=
                                   Get_Bool (Attr_Def, IS_TOOLCHAIN_CONFIG);
         Config_Concatenable   : constant Boolean :=
                                   Get_Bool
                                     (Attr_Def, CONFIGURATION_CONCATENABLE);
         Inherit_Extended      : constant Inherit_From_Extended_Type :=
                                   Get_Inherit_From_Extended_Type
                                     (Attr_Def, INHERIT_FROM_EXTENDED);
         Is_Set                : constant Boolean :=
                                   Get_Bool (Attr_Def, VALUE_IS_SET);
         Descr                 : constant String :=
                                   Get_String (Attribute, ATTRIBUTE_DESCR);
      begin
         if Name /= "" then
            declare
               Q_Name : constant Q_Attribute_Id :=
                          (Pack, +GPR2.Optional_Name_Type (Name));

               package PRAD renames Project.Registry.Attribute.Description;
            begin
               if not Project.Registry.Attribute.Exists (Q_Name) then
                  begin
                     Add
                       (Name                  => Q_Name,
                        Index_Type            => Attr_Index_Type,
                        Value                 => Attr_Value,
                        Value_Case_Sensitive  => Value_Case_Sensitive,
                        Is_Allowed_In         => Attr_Allowed_In,
                        Type_Def              => Type_Def,
                        Is_Builtin            => Is_Builtin,
                        Index_Optional        => Index_Is_Optional,
                        Empty_Value           => Attr_Empty_Value,
                        Default               => Default,
                        Has_Default_In        => Attr_Has_Default_In,
                        Is_Toolchain_Config   => Is_Toolchain_Conf,
                        Config_Concatenable   => Config_Concatenable,
                        Inherit_From_Extended => Inherit_Extended,
                        Is_Set                => Is_Set);
                  exception
                        --  Handle Preconditions exception
                     when others =>
                        null;
                  end;
               end if;

               if Descr /= ""
                 and then PRAD.Get_Attribute_Description (Q_Name) = ""
               then
                  PRAD.Set_Attribute_Description
                    (Key         => Q_Name,
                     Description => Descr);
               end if;
            end;
         end if;
      end Import_Attribute;

      --------------------
      -- Import_Package --
      --------------------

      procedure Import_Package (Pack : JSON_Value) is
         Current_Package : Package_Id;
         Name            : constant String := Get_String (Pack, PACKAGE_NAME);
         Descr           : constant String := Get_String (Pack, PACKAGE_DESCR);
         package PRPD renames GPR2.Project.Registry.Pack.Description;

      begin
         if Name = "" then
            --  Ignore this package definitions
            return;
         end if;

         if Name = PROJECT_LEVEL then
            Current_Package := GPR2.Project_Level_Scope;
         else
            Current_Package := +GPR2.Optional_Name_Type (Name);
         end if;

         if not Is_Included (Current_Package) then
            --  Ignore it as it is not part of packages to import.
            return;
         end if;

         if not GPR2.Project.Registry.Pack.Exists (Current_Package)
         then
            GPR2.Project.Registry.Pack.Add
              (Name     => Current_Package,
               Projects => Get_Projects_Kind (Pack, PROJECTS_KIND));
         end if;

         if Descr /= "" and then
           PRPD.Get_Package_Description (Current_Package) = ""
         then
            PRPD.Set_Package_Description
              (Key         => Current_Package,
               Description => Descr);
         end if;

         for Attribute of Get_Array (Pack, ATTRIBUTES) loop
            Import_Attribute (Current_Package, Attribute);
         end loop;
      end Import_Package;

      Result : constant GNATCOLL.JSON.Read_Result := Read (Definitions);

   begin
      if Result.Success then
         for Pack of Get_Array (Result.Value, PACKAGES) loop
            Import_Package (Pack);
         end loop;
      else
         raise Invalid_JSON_Stream with To_String (Result.Error.Message);
      end if;
   end Import;

end GPR2.Project.Registry.Exchange;
