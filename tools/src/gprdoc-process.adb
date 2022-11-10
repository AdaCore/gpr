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

with Ada.Text_IO;
with Ada.Characters.Handling;

with GNATCOLL;
with GNATCOLL.JSON;

with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Attribute.Description;
with GPR2.Project.Registry.Pack;

procedure GPRdoc.Process (Options : GPRdoc.GPRdoc_Options) is

   use Ada;
   use GNATCOLL;
   use GNATCOLL.JSON;
   use GPR2;
   use GPR2.Project.Registry.Attribute;

   package PRA  renames GPR2.Project.Registry.Attribute;
   package PRP  renames GPR2.Project.Registry.Pack;
   package PRAD renames GPR2.Project.Registry.Attribute.Description;

   procedure Generate_IO_Textual_Documentation;
   --  Format the attributes and package information to generate a textual
   --  output.

   procedure Generate_JSON_Documentation (Compact : Boolean);
   --  Format the attributes and package information to generate a JSON output
   --  which will be used to generate the documentation.

   procedure Generate_IO_Textual_Documentation is

      procedure Display_Attributes
        (Attr_Name : Q_Attribute_Id;
         Attr_Def  : Def);
      --  Display the attributes as the same GNAT Project Manager Documentation
      --  output. Useful in order to compare the actual code attributes
      --  definitions to the existing documentation.

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

         Text_IO.Put (Item => Image (Attr_Name.Attr) & ": ");

         if Attr_Def.Value = Single
         then
            Text_IO.Put (Item => "single");
         else
            Text_IO.Put (Item => "list");
         end if;

         if Attr_Def.Builtin
         then
            Text_IO.Put (Item => K_Separator & "read-only");
         end if;

         if Attr_Def.Index_Type /= PRA.No_Index
         then
            if Attr_Def.Index_Optional
            then
               Text_IO.Put (Item => K_Separator & "optional index");
            end if;

            Text_IO.Put (Item => K_Separator & "indexed");
         end if;

         case Attr_Def.Index_Type is
            when PRA.No_Index | PRA.String_Index | PRA.Env_Var_Name_Index =>
               null;

            when PRA.Unit_Index | PRA.Language_Index =>
               Text_IO.Put (Item => K_Separator & "case-insensitive index");

            when PRA.File_Index |
                 PRA.FileGlob_Index |
                 PRA.FileGlob_Or_Language_Index =>

               if not GPR2.File_Names_Case_Sensitive
               then
                  Text_IO.Put (Item => K_Separator & "case-insensitive index");
               end if;
         end case;

         if  Attr_Def.Index_Type = PRA.File_Index
           or else
             Attr_Def.Index_Type = PRA.FileGlob_Index
         then
            Text_IO.Put (Item => K_Separator & "file name index");
         end if;

         if Attr_Def.Index_Optional
         then
            Text_IO.Put (Item => K_Separator & "others allowed");
         end if;

         if Attr_Def.Config_Concatenable
         then
            Text_IO.Put (Item => K_Separator & "configuration concatenable");
         end if;

         Text_IO.New_Line;
         Text_IO.Put_Line
           (Item => "Description: "
            & PRAD.Get_Attribute_Description (Key => Attr_Name));

         Text_IO.New_Line;
      end Display_Attributes;

      ---------------------------------
      -- Generate_Package_Attributes --
      ---------------------------------

      procedure Generate_Package_Attributes
        (Pack : Package_Id)
      is
         Package_Name         : constant String :=
                                  (if Pack = Project_Level_Scope
                                   then "Project_Level"
                                   else Image (Pack));
         Package_Name_Shown   : Boolean := False;
         Attribute_Definition : Def;
      begin

         --  For every attributes of a given package

         for Attr_Id of PRA.All_Attributes (Pack => Pack)
         loop

            --  Get the attribute informations
            Attribute_Definition :=
              PRA.Get (Q_Name => Attr_Id);

            --  Display attribute and package informations

            if not Package_Name_Shown then
               Text_IO.Put_Line (Item => Package_Name);
               Package_Name_Shown := True;
            end if;

            Display_Attributes
              (Attr_Name => Attr_Id,
               Attr_Def  => Attribute_Definition);

            Text_IO.New_Line;

         end loop;

      end Generate_Package_Attributes;

   begin

      --  First retrieve all attributes from the Top-Level package

      Generate_Package_Attributes (Pack => Project_Level_Scope);

      --  Then retrieve all attributes from all registered packages

      for Pack of PRP.All_Packages
      loop
         Generate_Package_Attributes (Pack => Pack);
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
                               Descr     : String;
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
            --  The array JSON object description

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

            --------------------
            -- Variant_Record --
            --------------------

            function Variant_Record (VR : Default_Value) return JSON_Value is
               Obj : constant JSON_Value := Create_Object;
            begin

               Set_Field
                 (Val        => Obj,
                  Field_Name => "default_value_kind",
                  Field      => Ada.Characters.Handling.To_Lower
                    (VR.Kind'Img (3 .. VR.Kind'Img'Last)));
               --  To get rid of "D_"

               case VR.Kind is
                  when D_Attribute_Reference =>
                     Set_Field (Val        => Obj,
                                Field_Name => "attr",
                                Field      => Image (VR.Attr));

                  when D_Value =>
                     if Attr_Def.Index_Type =
                       GPR2.Project.Registry.Attribute.No_Index
                     then
                        Set_Field (Val => Obj,
                                   Field_Name => "value",
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
                                Field_Name => "callback",
                                Field      => "special");

               end case;

               return Obj;
            end Variant_Record;

            Obj : constant JSON_Value := Create_Object;
         begin
            Set_Field (Val        => Obj,
                       Field_Name => "index_type",
                       Field      => Attr_Def.Index_Type'Img);
            Set_Field (Val        => Obj,
                       Field_Name => "index_optional",
                       Field      => Attr_Def.Index_Optional);
            Set_Field (Val        => Obj,
                       Field_Name => "value",
                       Field      => Attr_Def.Value'Img);
            Set_Field (Val        => Obj,
                       Field_Name => "value_case_sensitive",
                       Field      => Attr_Def.Value_Case_Sensitive);
            Set_Field (Val        => Obj,
                       Field_Name => "value_is_set",
                       Field      => Attr_Def.Value_Is_Set);
            Set_Field (Val        => Obj,
                       Field_Name => "empty_value",
                       Field      => Attr_Def.Empty_Value'Img);
            Set_Field (Val        => Obj,
                       Field_Name => "builtin",
                       Field      => Attr_Def.Builtin);
            Set_Field (Val        => Obj,
                       Field_Name => "is_allowed_in",
                       Field      => Array_Object
                         (A => Attr_Def.Is_Allowed_In));
            Set_Field (Val        => Obj,
                       Field_Name => "has_default_in",
                       Field      => Array_Object
                         (A =>  Attr_Def.Has_Default_In));

            if Attr_Def.Has_Default_In /= Nowhere
            then
               Set_Field (Val        => Obj,
                          Field_Name => "default",
                          Field      => Variant_Record
                            (VR => Attr_Def.Default));
            end if;

            Set_Field (Val        => Obj,
                       Field_Name => "is_toolchain_config",
                       Field      => Attr_Def.Is_Toolchain_Config);
            Set_Field (Val        => Obj,
                       Field_Name => "config_concatenable",
                       Field      => Attr_Def.Config_Concatenable);
            Set_Field (Val        => Obj,
                       Field_Name => "inherit_from_extended",
                       Field      => Attr_Def.Inherit_From_Extended'Img);

            return Obj;
         end Attribute_Def_Object;

         Obj : constant JSON_Value := Create_Object;
      begin
         Set_Field (Val        => Obj,
                    Field_Name => "attribute_name",
                    Field      => Image (Attr_Name.Attr));
         Set_Field (Val        => Obj,
                    Field_Name => "attribute_def",
                    Field      => Attribute_Def_Object (Attr_Def => Attr_Def));
         Set_Field (Val        => Obj,
                    Field_Name => "attribute_descr",
                    Field      => Attr_Descr);

         return Obj;
      end Attribute_Object;

      --------------------
      -- Package_Object --
      --------------------

      function Package_Object (Pack      : Package_Id;
                               Descr     : String;
                               Attr_List : JSON_Array) return JSON_Value
      is
         Package_Name : constant String :=
                          (if Pack = Project_Level_Scope
                           then "Project_Level"
                           else Image (Pack));
         Obj          : constant JSON_Value := Create_Object;
      begin
         Set_Field (Val        => Obj,
                    Field_Name => "package_name",
                    Field      => Package_Name);
         Set_Field (Val        => Obj,
                    Field_Name => "package_descr",
                    Field      => Descr);
         Set_Field (Val        => Obj,
                    Field_Name => "attributes",
                    Field      => Attr_List);

         return Obj;
      end Package_Object;

      J_Doc : constant JSON_Value := Create_Object;
      --  The JSON doc

      P_Array : JSON_Array;
   begin

      --  First retrieve all attributes from the Top-Level package
      declare
         Attr_Def : Def;
         A_Array  : JSON_Array;
      begin

         for Attr_Id of PRA.All_Attributes (Pack => Project_Level_Scope)
         loop

            --  Get the attribute informations
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
                         Descr     => "",
                         Attr_List => A_Array));
      end;

      --  Then retrieve all attributes from all registered packages

      for Pack of PRP.All_Packages
      loop
         declare
            Attr_Def : Def;
            A_Array  : JSON_Array;
         begin
            for Attr_Id of PRA.All_Attributes (Pack => Pack)
            loop

               --  Get the attribute informations
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
                                                Descr     => "",
                                                Attr_List => A_Array));
         end;
      end loop;

      --  Create the whole JSON file

      Set_Field (Val        => J_Doc,
                 Field_Name => "packages",
                 Field      => P_Array);

      Text_IO.Put_Line
        (Item => JSON.Write (Item => J_Doc, Compact => Compact));

   end Generate_JSON_Documentation;

begin

   case Options.Kind_Of_Display is
      when GPRtools.K_JSON_Compact =>
         Generate_JSON_Documentation (Compact => True);

      when GPRtools.K_JSON =>
         Generate_JSON_Documentation (Compact => False);

      when GPRtools.K_Textual_IO =>
         Generate_IO_Textual_Documentation;
   end case;

end GPRdoc.Process;
