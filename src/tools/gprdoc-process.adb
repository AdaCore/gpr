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

with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Attribute.Description;
with GPR2.Project.Registry.Pack;

procedure GPRdoc.Process is

   use Ada;
   use GPR2;
   use GPR2.Project.Registry.Attribute;

   package PRA  renames GPR2.Project.Registry.Attribute;
   package PRP  renames GPR2.Project.Registry.Pack;
   package PRAD renames GPR2.Project.Registry.Attribute.Description;

   type Display_Kind is
     (K_Official_Documentation,
      K_IO_Documentation);
   --  Used to switch between 2 types of output

   procedure Display_Attributes
     (Attr_Name : Qualified_Name;
      Attr_Def  : Def);
   --  Display the attributes as the same GNAT Project Manager Documentation
   --  output. Useful in order to compare the actual code attributes
   --  definitions to the existing documentation.

   procedure Generate_Package_Attributes
     (Kind : Display_Kind;
      Pack : Optional_Package_Id);

   procedure Generate_Documentation
     (Pack_Name : String;
      Attr_Name : Qualified_Name;
      Attr_Def  : Def);
   --  Format the attributes and package information for generating the
   --  documentation. #WIP.

   ------------------------
   -- Display_Attributes --
   ------------------------

   procedure Display_Attributes
     (Attr_Name : Qualified_Name;
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
         when PRA.No_Index | PRA.Env_Var_Name_Index =>
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

   ----------------------------
   -- Generate_Documentation --
   ----------------------------

   procedure Generate_Documentation
     (Pack_Name : String;
      Attr_Name : Qualified_Name;
      Attr_Def  : Def)
   is
   begin
      Text_IO.Put_Line
        (Item => Pack_Name & "." & Image (Attr_Name.Attr));
      Text_IO.Put_Line
        (Item => "Description: "
         & PRAD.Get_Attribute_Description (Key => Attr_Name));
      Text_IO.Put_Line (Item => "INDEX_TYPE            => "
                        & Attr_Def.Index_Type'Img);
      Text_IO.Put_Line (Item => "INDEX_OPTIONAL        => "
                        & Attr_Def.Index_Optional'Img);
      Text_IO.Put_Line (Item => "VALUE                 => "
                        & Attr_Def.Value'Img);
      Text_IO.Put_Line (Item => "VALUE_CASE_SENSITIVE  => "
                        & Attr_Def.Value_Case_Sensitive'Img);
      Text_IO.Put_Line (Item => "VALUE_IS_SET          => "
                        & Attr_Def.Value_Is_Set'Img);
      Text_IO.Put_Line (Item => "EMPTY_VALUE           => "
                        & Attr_Def.Empty_Value'Img);
      Text_IO.Put_Line (Item => "BUILTIN               => "
                        & Attr_Def.Builtin'Img);
      Text_IO.Put_Line (Item => "IS_ALLOWED_IN :");
      Text_IO.Put_Line (Item => "   CONFIGURATION     - " &
                          Attr_Def.Is_Allowed_In (K_Configuration)'Img
                       );
      Text_IO.Put_Line (Item => "   ABSTRACT          - " &
                          Attr_Def.Is_Allowed_In (K_Abstract)'Img
                       );
      Text_IO.Put_Line (Item => "   STANDARD          - " &
                          Attr_Def.Is_Allowed_In (K_Standard)'Img
                       );
      Text_IO.Put_Line (Item => "   LIBRARY           - " &
                          Attr_Def.Is_Allowed_In (K_Library)'Img
                       );
      Text_IO.Put_Line (Item => "   AGGREGATE         - " &
                          Attr_Def.Is_Allowed_In (K_Aggregate)'Img
                       );
      Text_IO.Put_Line (Item => "   AGGREGATE_LIBRARY - " &
                          Attr_Def.Is_Allowed_In (K_Aggregate_Library)'Img
                       );
      Text_IO.Put_Line (Item => "DEFAULT               => ");
      Text_IO.Put_Line (Item => "   KIND     -  " & Attr_Def.Default.Kind'Img);
      case Attr_Def.Default.Kind is
         when D_Attribute_Reference =>
            Text_IO.Put_Line (Item => "   ATTR     - " &
                                Attr_Def.Default.Attr'Img
                             );
         when D_Value =>
            Text_IO.Put_Line (Item => "   VALUES   - " &
                                Attr_Def.Default.Values'Img
                             );
         when D_Callback =>
            Text_IO.Put_Line (Item => "   CALLBACK - " &
                                Attr_Def.Default.Callback'Img
                             );
      end case;
      Text_IO.Put_Line (Item => "HAS_DEFAULT_IN        => ");
      Text_IO.Put_Line (Item => "   CONFIGURATION     - " &
                          Attr_Def.Has_Default_In (K_Configuration)'Img
                       );
      Text_IO.Put_Line (Item => "   ABSTRACT          - " &
                          Attr_Def.Has_Default_In (K_Abstract)'Img
                       );
      Text_IO.Put_Line (Item => "   STANDARD          - " &
                          Attr_Def.Has_Default_In (K_Standard)'Img
                       );
      Text_IO.Put_Line (Item => "   LIBRARY           - " &
                          Attr_Def.Has_Default_In (K_Library)'Img
                       );
      Text_IO.Put_Line (Item => "   AGGREGATE         - " &
                          Attr_Def.Has_Default_In (K_Aggregate)'Img
                       );
      Text_IO.Put_Line (Item => "   AGGREGATE_LIBRARY - " &
                          Attr_Def.Has_Default_In (K_Aggregate_Library)'Img
                       );
      Text_IO.Put_Line (Item => "IS_TOOLCHAIN_CONFIG   => "
                        & Attr_Def.Is_Toolchain_Config'Img);
      Text_IO.Put_Line (Item => "CONFIG_CONCATENABLE   => "
                        & Attr_Def.Config_Concatenable'Img);
      Text_IO.Put_Line (Item => "INHERIT_FROM_EXTENDED => "
                        & Attr_Def.Inherit_From_Extended'Img);
      Text_IO.New_Line;

   end Generate_Documentation;

   ---------------------------------
   -- Generate_Package_Attributes --
   ---------------------------------

   procedure Generate_Package_Attributes
     (Kind : Display_Kind;
      Pack : Optional_Package_Id)
   is
      Package_Name         : constant String
        := (if Pack /= No_Package then Image (Pack) else "Project_Level");
      Package_Name_Shown : Boolean := False;
      Attribute_Name       : Qualified_Name;
      Attribute_Definition : Def;
   begin

      --  For every attributes of a given package.

      for Attr_Id of PRA.All_Attributes (Pack => Pack)
      loop

         --  Get the attribute information.

         Attribute_Name       := Create (Name => Attr_Id, Pack => Pack);
         Attribute_Definition := PRA.Get (Q_Name => Attribute_Name);

         --  Display mode

         case Kind is
            when K_Official_Documentation =>

               Generate_Documentation
                 (Pack_Name   => Package_Name,
                  Attr_Name => Attribute_Name,
                  Attr_Def  => Attribute_Definition);

            when K_IO_Documentation =>

               if not Package_Name_Shown then
                  Text_IO.Put_Line (Item => Package_Name);
                  Package_Name_Shown := True;
               end if;

               Display_Attributes
                 (Attr_Name => Attribute_Name,
                  Attr_Def  => Attribute_Definition);
         end case;

      end loop;

      Text_IO.New_Line;

   end Generate_Package_Attributes;

   K_Kind_Of_Display : constant Display_Kind := K_Official_Documentation;

begin

   --  First retrieve all attributes from the Top-Level package

   Generate_Package_Attributes (Kind => K_Kind_Of_Display,
                                Pack => No_Package);

   --  Then retrieve all attributes from all registered packages

   for Pack of PRP.All_Packages
   loop
      Generate_Package_Attributes (Kind => K_Kind_Of_Display,
                                   Pack => Pack);
   end loop;

end GPRdoc.Process;
