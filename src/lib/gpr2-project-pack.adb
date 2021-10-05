------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2021, AdaCore                      --
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

with GPR2.Project.Definition;

package body GPR2.Project.Pack is

   procedure Set_Default_Attributes
     (Self : in out Object; VDD : Definition.Data);
   --  Set default attributes for the package

   ---------------
   -- Attribute --
   ---------------

   function Attribute
     (Self  : Object;
      Name  : Attribute_Id;
      Index : Attribute_Index.Object := Attribute_Index.Undefined)
      return Project.Attribute.Object is
   begin
      return Self.Attrs.Element (Name, Index);
   end Attribute;

   ----------------
   -- Attributes --
   ----------------

   function Attributes
     (Self  : Object;
      Name  : Optional_Attribute_Id := No_Attribute;
      Index : Attribute_Index.Object := Attribute_Index.Undefined)
      return Project.Attribute.Set.Object is
   begin
      return Self.Attrs.Filter (Name, Index);
   end Attributes;

   -----------------
   -- Body_Suffix --
   -----------------

   function Body_Suffix
     (Self     : Object;
      Language : Language_Id) return Project.Attribute.Object
   is
   begin
      return Self.Attribute
        (Registry.Attribute.Body_Suffix, Attribute_Index.Create (Language));
   end Body_Suffix;

   ---------------------
   -- Check_Attribute --
   ---------------------

   function Check_Attribute
     (Self   : Object;
      Name   : Attribute_Id;
      Index  : Attribute_Index.Object := Attribute_Index.Undefined;
      At_Pos : Unit_Index := No_Index;
      Result : out GPR2.Project.Attribute.Object) return Boolean is
   begin
      Result := Self.Attrs.Element (Name, Index, At_Pos);
      return Result.Is_Defined;
   end Check_Attribute;

   ------------
   -- Create --
   ------------

   function Create
     (Name       : Source_Reference.Pack.Object;
      Attributes : Project.Attribute.Set.Object;
      Variables  : Project.Variable.Set.Object) return Object is
   begin
      return Object'
        (Source_Reference.Object (Name)
         with Name.Id, Attributes, Variables);
   end Create;

   --------------------
   -- Has_Attributes --
   --------------------

   function Has_Attributes
     (Self  : Object;
      Name  : Optional_Attribute_Id := No_Attribute;
      Index : Attribute_Index.Object := Attribute_Index.Undefined)
      return Boolean is
   begin
      if Name = No_Attribute and then not Index.Is_Defined then
         return not Self.Attrs.Is_Empty;
      else
         return Self.Attrs.Contains (Name, Index);
      end if;
   end Has_Attributes;

   -------------------
   -- Has_Variables --
   -------------------

   function Has_Variables
     (Self : Object;
      Name : Optional_Name_Type := No_Name) return Boolean is
   begin
      if Name = No_Name then
         return not Self.Vars.Is_Empty;
      else
         return Self.Vars.Contains (Name);
      end if;
   end Has_Variables;

   --------------------
   -- Implementation --
   --------------------

   function Implementation
     (Self : Object;
      Unit : Value_Type) return Project.Attribute.Object is
   begin
      return Self.Attribute
        (Registry.Attribute.Body_N, Attribute_Index.Create (Unit));
   end Implementation;

   ----------
   -- Name --
   ----------

   function Name (Self : Object) return Package_Id is
   begin
      return Self.Name;
   end Name;

   ---------------------
   -- Separate_Suffix --
   ---------------------

   function Separate_Suffix
     (Self : Object) return Project.Attribute.Object
   is
   begin
      return Self.Attribute
        (Registry.Attribute.Separate_Suffix);
   end Separate_Suffix;

   ----------------------------
   -- Set_Default_Attributes --
   ----------------------------

   procedure Set_Default_Attributes
     (Self : in out Object; VDD : Definition.Data) is
   begin
      Definition.Set_Defaults
        (Self.Attrs, VDD, Self.Name);
   end Set_Default_Attributes;

   -----------------
   -- Spec_Suffix --
   -----------------

   function Spec_Suffix
     (Self     : Object;
      Language : Language_Id) return Project.Attribute.Object
   is
   begin
      return Self.Attribute
        (Registry.Attribute.Spec_Suffix, Attribute_Index.Create (Language));
   end Spec_Suffix;

   -------------------
   -- Specification --
   -------------------

   function Specification
     (Self : Object;
      Unit : Value_Type) return Project.Attribute.Object is
   begin
      return Self.Attribute
        (Registry.Attribute.Spec, Attribute_Index.Create (Unit));
   end Specification;

   --------------
   -- Variable --
   --------------

   function Variable
     (Self : Object; Name : Name_Type) return Project.Variable.Object is
   begin
      return Self.Vars (Name);
   end Variable;

   ---------------
   -- Variables --
   ---------------

   function Variables (Self : Object) return Project.Variable.Set.Object is
   begin
      return Self.Vars;
   end Variables;

begin
   Definition.Set_Pack_Default_Attributes := Set_Default_Attributes'Access;
end GPR2.Project.Pack;
