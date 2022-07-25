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

--  This package record the relation between unit name and the different part
--  composing it. From a unit name we can retrieve the source where the spec,
--  the body or separates are to be found.

with GPR2.Unit;

package GPR2.Project.Unit_Info is

   type Object is tagged private;

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Create
     (Name      : Name_Type;
      Spec      : Unit.Source_Unit_Identifier := Unit.Undefined_Id;
      Main_Body : Unit.Source_Unit_Identifier := Unit.Undefined_Id;
      Separates : Unit.Source_Unit_Vectors.Vector :=
                    Unit.Source_Unit_Vectors.Empty_Vector) return Object;
   --  Constructor for a Unit object

   function Name (Self : Object) return Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the unit name

   function Has_Spec (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if a spec is defined for this unit

   function Spec (Self : Object) return Unit.Source_Unit_Identifier
     with Pre => Self.Is_Defined;
   --  Returns the Spec

   function Has_Body (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if a body is defined for this unit

   function Main_Body (Self : Object) return Unit.Source_Unit_Identifier
     with Pre => Self.Is_Defined;
   --  Returns the Body

   function Separates (Self : Object) return Unit.Source_Unit_Vectors.Vector
     with Pre => Self.Is_Defined;
   --  Returns all separates

   procedure Update_Name
     (Self : in out Object; Name : Name_Type)
     with Pre => Self.Is_Defined;
   --  Sets unit spec

   procedure Update_Spec
     (Self : in out Object; Source : Unit.Source_Unit_Identifier)
     with Pre => Self.Is_Defined and then Source.Source.Is_Defined;
   --  Sets unit spec

   procedure Update_Body
     (Self : in out Object; Source : Unit.Source_Unit_Identifier)
     with Pre => Self.Is_Defined and then Source.Source.Is_Defined;
   --  Sets unit body

   procedure Remove_Body (Self : in out Object)
     with Pre => Self.Is_Defined;
   --  Sets unit body

   procedure Update_Separates
     (Self : in out Object; Source : Unit.Source_Unit_Identifier)
     with Pre => Self.Is_Defined and then Source.Source.Is_Defined;
   --  Appends separate

private

   use GPR2.Unit;

   type Object is tagged record
      Name      : Unbounded_String;
      Spec      : Source_Unit_Identifier;
      Main_Body : Source_Unit_Identifier;
      Separates : Source_Unit_Vectors.Vector;
   end record;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Has_Spec (Self : Object) return Boolean is
     (Self.Spec.Source.Is_Defined);

   function Has_Body (Self : Object) return Boolean is
     (Self.Main_Body.Source.Is_Defined);

   function Spec (Self : Object) return Source_Unit_Identifier is (Self.Spec);

   function Main_Body (Self : Object) return Source_Unit_Identifier is
     (Self.Main_Body);

   function Separates
     (Self : Object) return Source_Unit_Vectors.Vector is (Self.Separates);

   function Name (Self : Object) return Name_Type is
     (Name_Type (To_String (Self.Name)));

end GPR2.Project.Unit_Info;
