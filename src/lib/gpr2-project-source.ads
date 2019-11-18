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

with GPR2.Project.View;
with GPR2.Source;

limited with GPR2.Project.Source.Artifact;
limited with GPR2.Project.Source.Set;

package GPR2.Project.Source is

   use type GPR2.Source.Object;

   type Object is tagged private;

   subtype Source_Object is Object;

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function "<" (Left, Right : Object) return Boolean;

   overriding function "=" (Left, Right : Object) return Boolean;

   function Create
     (Source               : GPR2.Source.Object;
      View                 : Project.View.Object;
      Is_Interface         : Boolean;
      Has_Naming_Exception : Boolean;
      Is_Compilable        : Boolean;
      Aggregated           : Boolean := False) return Object
     with Pre => Source.Is_Defined
                 and then View.Is_Defined
                 and then (not Aggregated
                           or else View.Is_Aggregated_In_Library);
   --  Constructor for Object. View is where the source is defined (found from
   --  View Source_Dirs) and Extending_View is the optional view from which the
   --  project source is extended. That is, if Extending_View is defined then
   --  this source is coming from an extended project for View.

   function View (Self : Object) return Project.View.Object
     with Pre  => Self.Is_Defined,
          Post => View'Result.Is_Defined;
   --  The view the source is in

   function Is_Aggregated (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the source is taken into aggregating library source set
   --  from the aggregated project.

   function Is_Compilable (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the source is compilable, meaning that a compiler is
   --  defined for this language.

   function Source (Self : Object) return GPR2.Source.Object;
   --  The source object

   function Is_Interface (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self is part of the project view interface

   function Has_Naming_Exception (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns whether the source comes from a naming exception

   function Has_Extending_View (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self has an extending view defined

   function Extending_View (Self : Object) return Project.View.Object
     with Pre => Self.Is_Defined and then Self.Has_Extending_View;
   --  Returns the extending view

   function Has_Aggregating_View (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self has an aggregating view defined, that is source
   --  is part of an aggregate library.

   function Aggregating_View (Self : Object) return Project.View.Object
     with Pre  => Self.Is_Defined and then Self.Has_Aggregating_View,
          Post => Aggregating_View'Result.Kind = K_Aggregate_Library;
   --  Returns the aggregating view

   function Is_Main (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.View.Has_Mains;
   --  Returns whether the source is the main file to create executable

   function Artifacts (Self : Object) return Artifact.Object
     with Pre => Self.Is_Defined;
   --  Returns the source artifacts for this source. Note that the returned
   --  Source artifacts may not exist if the compilation has not yet been
   --  done/finished.

   procedure Release (Self : in out Object)
     with Pre => Self.Is_Defined;
   --  Releases the project source

   --
   --  The following routines only make sense if Has_Units is True
   --

   type Dependency is (Direct, Unit, Closure);
   --  Direct  : the dependencies from the source withed units.
   --  Unit    : the dependencies from the spec/body for this source.
   --  Closure : the full dependencies for this sources and all withed sources
   --            recursively.

   function Dependencies
     (Self : Object;
      Mode : Dependency := Direct) return GPR2.Project.Source.Set.Object
     with Pre => Self.Is_Defined and then Self.Source.Has_Units;
   --  Returns the dependencies for this given source

   --
   --  The following routines may be used for both unit-based and
   --  non-unit-based sources. In the latter case, Index is not used.
   --

   function Has_Other_Part (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if an other part exists for this project source

   function Other_Part (Self : Object) return Object
     with Pre => Self.Is_Defined and then Self.Has_Other_Part,
          Post => Other_Part'Result.Is_Defined;
   --  Returns the other part for this project source

   function Separate_From (Self : Object) return Object
     with Pre => Self.Is_Defined and then Self.Source.Kind = S_Separate;
   --  Returns the separate for Self

private

   type Object is tagged record
      Source : GPR2.Source.Object;
      View   : Project.Weak_Reference;
      --  Use weak reference to View to avoid reference cycle between Source
      --  and its View. Otherwise we've got memory leak after release view and
      --  valgrind detected mess in memory deallocations at the process exit.

      Is_Interface         : Boolean := False;
      Has_Naming_Exception : Boolean := False;
      Is_Compilable        : Boolean := False;
      Aggregated           : Boolean := False;
   end record;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Is_Aggregated (Self : Object) return Boolean is
     (Self.Aggregated);

   function "<" (Left, Right : Object) return Boolean is
     (Left.Source < Right.Source);

   overriding function "=" (Left, Right : Object) return Boolean is
     (Left.Source = Right.Source);

   function Is_Interface (Self : Object) return Boolean is
     (Self.Is_Interface);

   function Has_Naming_Exception (Self : Object) return Boolean is
     (Self.Has_Naming_Exception);

end GPR2.Project.Source;
