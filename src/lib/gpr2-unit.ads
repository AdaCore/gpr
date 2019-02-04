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

--  This package represents a unit object. This is useful for unit-based
--  language like Ada. Note that we associate a spec with multiple bodies
--  as we can have a main body and a set of separate source.

with GPR2.Project.Source.Set;

package GPR2.Unit is

   use type Project.Source.Object;

   type Object is tagged private;

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Create
     (Spec   : Project.Source.Object;
      Bodies : Project.Source.Set.Object) return Object;
   --  Constructor for a Unit object

   function Has_Spec (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if a spec is defined for this unit

   function Spec (Self : Object) return Project.Source.Object
     with Pre => Self.Is_Defined;
   --  Returns the Spec

   function Bodies (Self : Object) return Project.Source.Set.Object
     with Pre => Self.Is_Defined;
   --  Returns all bodies

   procedure Update_Spec
     (Self : in out Object; Source : Project.Source.Object)
     with Pre => Source.Is_Defined;
   --  Sets unit spec

   procedure Update_Bodies
     (Self : in out Object; Source : Project.Source.Object)
     with Pre => Source.Is_Defined;
   --  Sets or appends unit body

   function Is_Interface (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the unit is an interface

private

   type Object is tagged record
      Spec   : Project.Source.Object;
      Bodies : Project.Source.Set.Object;
   end record;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Has_Spec (Self : Object) return Boolean is
     (Self.Spec.Is_Defined);

   function Spec
     (Self : Object) return Project.Source.Object is (Self.Spec);

   function Bodies
     (Self : Object) return Project.Source.Set.Object is (Self.Bodies);

   function Is_Interface (Self : Object) return Boolean is
     (Self.Spec.Is_Interface);

end GPR2.Unit;
