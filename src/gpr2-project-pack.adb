------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2016, Free Software Foundation, Inc.            --
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

with GPR2.Project.Registry.Attribute;

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
     (Name       : Name_Type;
      Attributes : Project.Attribute.Set.Object;
      Sloc       : Source_Reference.Object) return Object is
   begin
      return Object'
        (Sloc with To_Unbounded_String (String (Name)), Attributes);
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

end GPR2.Project.Pack;
