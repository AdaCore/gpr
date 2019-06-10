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

with GPR2.Project.Definition;

package body GPR2.Project.Pack is

   procedure Set_Default_Attributes
     (Self : in out Object; Kind : Project_Kind);
   --  Set default attributes for the package

   ---------------
   -- Attribute --
   ---------------

   function Attribute
     (Self  : Object;
      Name  : Name_Type;
      Index : Value_Type := No_Value) return Project.Attribute.Object is
   begin
      return Self.Attrs.Element (Name, Index);
   end Attribute;

   ----------------
   -- Attributes --
   ----------------

   function Attributes
     (Self  : Object;
      Name  : Optional_Name_Type := No_Name;
      Index : Value_Type := No_Value) return Project.Attribute.Set.Object
   is
      Result  : Project.Attribute.Set.Object :=
                  Self.Attrs.Filter (Name, Index);
      Default : Project.Attribute.Object;
   begin
      if Name /= No_Name and then Result.Length = 0
        and then Self.Check_Attribute (Name, Index, Default)
      then
         Result.Insert (Default);
      end if;

      return Result;
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
      return Self.Attribute (Registry.Attribute.Body_Suffix, Lang);
   end Body_Suffix;

   ---------------------
   -- Check_Attribute --
   ---------------------

   function Check_Attribute
     (Self   : Object;
      Name   : Name_Type;
      Index  : Value_Type := No_Value;
      Result : out Project.Attribute.Object) return Boolean is
   begin
      Result := Self.Attrs.Element (Name, Index);
      return Result.Is_Defined;
   end Check_Attribute;

   ------------
   -- Create --
   ------------

   function Create
     (Name       : Source_Reference.Identifier.Object;
      Attributes : Project.Attribute.Set.Object;
      Variables  : Project.Variable.Set.Object) return Object is
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
      Name  : Optional_Name_Type := No_Name;
      Index : Value_Type := No_Value) return Boolean is
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
      return Self.Attribute (Registry.Attribute.Body_N, Unit);
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
      return Self.Attribute (Registry.Attribute.Separate_Suffix, Lang);
   end Separate_Suffix;

   ----------------------------
   -- Set_Default_Attributes --
   ----------------------------

   procedure Set_Default_Attributes
     (Self : in out Object; Kind : Project_Kind) is
   begin
      Definition.Set_Defaults
        (Self.Attrs, Kind, Name_Type (To_String (Self.Name)));
   end Set_Default_Attributes;

   -----------------
   -- Spec_Suffix --
   -----------------

   function Spec_Suffix
     (Self     : Object;
      Language : Name_Type) return Project.Attribute.Object
   is
      Lang : constant Value_Type := Value_Type (Language);
   begin
      return Self.Attribute (Registry.Attribute.Spec_Suffix, Lang);
   end Spec_Suffix;

   -------------------
   -- Specification --
   -------------------

   function Specification
     (Self : Object;
      Unit : Value_Type) return Project.Attribute.Object is
   begin
      return Self.Attribute (Registry.Attribute.Spec, Unit);
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
