------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2017-2018, Free Software Foundation, Inc.          --
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

--  This package represents a unit object. This is useful for unit-based
--  language like Ada. Note that we associate a spec with multiple bodies
--  as we can have a main body and a set of separate source.

with GPR2.Project.Source.Set;

package GPR2.Unit is

   use type Project.Source.Object;

   type Object is tagged private;

   Undefined : constant Object;

   function Create
     (Spec   : Project.Source.Object;
      Bodies : Project.Source.Set.Object) return Object;
   --  Constructor for a Unit object

   function Has_Spec (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if a spec is defined for this unit

   function Spec (Self : Object) return Project.Source.Object
     with Pre => Self /= Undefined;
   --  Returns the Spec

   function Bodies (Self : Object) return Project.Source.Set.Object
     with Pre => Self /= Undefined;
   --  Returns all bodies

   procedure Update_Spec
     (Self : in out Object; Source : Project.Source.Object)
     with Pre => Source /= Project.Source.Undefined;
   --  Sets unit spec

   procedure Update_Bodies
     (Self : in out Object; Source : Project.Source.Object)
     with Pre => Source /= Project.Source.Undefined;
   --  Sets or appends unit body

   function Is_Interface (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if the unit is an interface

private

   type Object is tagged record
      Spec   : Project.Source.Object;
      Bodies : Project.Source.Set.Object;
   end record;

   Undefined : constant Object :=
                 (Spec   => <>,
                  Bodies => <>);

   function Has_Spec (Self : Object) return Boolean is
     (Self.Spec /= Project.Source.Undefined);

   function Spec
     (Self : Object) return Project.Source.Object is (Self.Spec);

   function Bodies
     (Self : Object) return Project.Source.Set.Object is (Self.Bodies);

   function Is_Interface (Self : Object) return Boolean is
     (Self.Spec.Is_Interface);

end GPR2.Unit;
