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

package body GPR2.Project.Pack is

   ---------------
   -- Attribute --
   ---------------

   function Attribute
     (Self  : Object;
      Name  : Name_Type;
      Index : Value_Type := "") return Project.Attribute.Object is
   begin
      return Self.Attributes.Element (Name, Index);
   end Attribute;

   ----------------
   -- Attributes --
   ----------------

   function Attributes
     (Self  : Object;
      Name  : Optional_Name_Type := "";
      Index : Value_Type := "") return Project.Attribute.Set.Object is
   begin
      return Self.Attrs.Filter (Name, Index);
   end Attributes;

   -----------------
   -- Body_Suffix --
   -----------------

   function Body_Suffix
     (Self     : Object;
      Language : Name_Type) return Project.Attribute.Object
   is
      Lang : constant Value_Type := Value_Type (Language);
   begin
      if Self.Has_Attributes (Registry.Attribute.Body_Suffix, Lang) then
         return Self.Attribute (Registry.Attribute.Body_Suffix, Lang);

      elsif Self.Has_Attributes
        (Registry.Attribute.Implementation_Suffix, Lang)
      then
         return Self.Attribute
           (Registry.Attribute.Implementation_Suffix, Lang);

      else
         return Project.Attribute.Undefined;
      end if;
   end Body_Suffix;

   ------------
   -- Create --
   ------------

   function Create
     (Name       : Source_Reference.Identifier.Object;
      Attributes : Project.Attribute.Set.Object;
      Variables  : Project.Variable.Set.Object) return Object
   is
   begin
      return Object'
        (Source_Reference.Object (Name)
         with To_Unbounded_String (String (Name.Text)), Attributes, Variables);
   end Create;

   --------------------
   -- Has_Attributes --
   --------------------

   function Has_Attributes
     (Self  : Object;
      Name  : Optional_Name_Type := "";
      Index : Value_Type := "") return Boolean is
   begin
      if Name = No_Name and then Index = No_Value then
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
      Name : Optional_Name_Type := "") return Boolean is
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
      if Self.Has_Attributes (Registry.Attribute.Body_N, Unit) then
         return Self.Attribute (Registry.Attribute.Body_N, Unit);

      elsif Self.Has_Attributes (Registry.Attribute.Implementation, Unit) then
         return Self.Attribute (Registry.Attribute.Implementation, Unit);

      else
         return Project.Attribute.Undefined;
      end if;
   end Implementation;

   ----------
   -- Name --
   ----------

   function Name (Self : Object) return Name_Type is
   begin
      return Name_Type (To_String (Self.Name));
   end Name;

   ---------------------
   -- Separate_Suffix --
   ---------------------

   function Separate_Suffix
     (Self     : Object;
      Language : Name_Type) return Project.Attribute.Object
   is
      Lang : constant Value_Type := Value_Type (Language);
   begin
      if Self.Has_Attributes
        (Registry.Attribute.Separate_Suffix, Lang)
      then
         return Self.Attribute (Registry.Attribute.Separate_Suffix, Lang);

      else
         return Project.Attribute.Undefined;
      end if;
   end Separate_Suffix;

   -----------------
   -- Spec_Suffix --
   -----------------

   function Spec_Suffix
     (Self     : Object;
      Language : Name_Type) return Project.Attribute.Object
   is
      Lang : constant Value_Type := Value_Type (Language);
   begin
      if Self.Has_Attributes (Registry.Attribute.Spec_Suffix, Lang) then
         return Self.Attribute (Registry.Attribute.Spec_Suffix, Lang);

      elsif Self.Has_Attributes
        (Registry.Attribute.Specification_Suffix, Lang)
      then
         return Self.Attribute
           (Registry.Attribute.Specification_Suffix, Lang);

      else
         return Project.Attribute.Undefined;
      end if;
   end Spec_Suffix;

   -------------------
   -- Specification --
   -------------------

   function Specification
     (Self : Object;
      Unit : Value_Type) return Project.Attribute.Object is
   begin
      if Self.Has_Attributes (Registry.Attribute.Spec, Unit) then
         return Self.Attribute (Registry.Attribute.Spec, Unit);

      elsif Self.Has_Attributes (Registry.Attribute.Specification, Unit) then
         return Self.Attribute (Registry.Attribute.Specification, Unit);

      else
         return Project.Attribute.Undefined;
      end if;
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

end GPR2.Project.Pack;
