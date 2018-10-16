------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2018, Free Software Foundation, Inc.          --
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

with GPR2.Project.View;
with GPR2.Source;

limited with GPR2.Project.Source.Artifact;
limited with GPR2.Project.Source.Set;

package GPR2.Project.Source is

   use type GPR2.Source.Object;
   use type GPR2.Project.View.Object;

   type Object is tagged private;

   subtype Source_Object is Object;

   Undefined : constant Object;

   function "<" (Left, Right : Object) return Boolean;
   overriding function "=" (Left, Right : Object) return Boolean;

   function Create
     (Source               : GPR2.Source.Object;
      View                 : Project.View.Object;
      Is_Interface         : Boolean;
      Has_Naming_Exception : Boolean) return Object
     with Pre => Source /= GPR2.Source.Undefined
                 and then View /= GPR2.Project.View.Undefined;
   --  Constructor for Object

   function View (Self : Object) return Project.View.Object
     with Pre => Self /= Undefined;
   --  The view the source is in

   function Source (Self : Object) return GPR2.Source.Object;
   --  The source object

   function Has_Other_Part (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if an other part exists for this project source

   function Other_Part (Self : Object) return Object
     with Pre  => Self /= Undefined and then Self.Has_Other_Part,
          Post => Other_Part'Result /= Undefined;
   --  Returns the other part for this project source

   function Is_Interface (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if Self is part of the project view interface

   function Has_Naming_Exception (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns whether the source comes from a naming exception

   function Artifacts (Self : Object) return Artifact.Object
     with Pre => Self /= Undefined;
   --  Returns the source artifacts for this source. Note that the returned
   --  Source artifacts may not exist if the compilation has not yet been
   --  done/finished.

   type Dependency is (Direct, Unit, Closure);
   --  Direct  : the dependencies from the source withed units.
   --  Unit    : the dependencies from the spec/body for this source.
   --  Closure : the full dependencies for this sources and all withed sources
   --            recursively.

   function Dependencies
     (Self : Object;
      Mode : Dependency := Direct) return GPR2.Project.Source.Set.Object
     with Pre => Self /= Undefined;
   --  Returns the dependencies for this given source

   procedure Release (Self : in out Object)
     with Pre => Self /= Undefined;
   --  Releases the project source

private

   type Object is tagged record
      Source               : GPR2.Source.Object;
      View                 : Project.View.Object;
      Is_Interface         : Boolean;
      Has_Naming_Exception : Boolean;
   end record;

   Undefined : constant Object :=
                 (Source               => GPR2.Source.Undefined,
                  View                 => Project.View.Undefined,
                  Is_Interface         => False,
                  Has_Naming_Exception => False);

   function "<" (Left, Right : Object) return Boolean is
     (Left.Source < Right.Source);

   overriding function "=" (Left, Right : Object) return Boolean is
     (Left.Source = Right.Source);

   function Is_Interface (Self : Object) return Boolean is
     (Self.Is_Interface);

   function Has_Naming_Exception (Self : Object) return Boolean is
     (Self.Has_Naming_Exception);

end GPR2.Project.Source;
