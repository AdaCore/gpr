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

with GPR2.Unit;
with GPR2.Project.View;
with GPR2.Source;
with GPR2.Source_Info;

limited with GPR2.Project.Source.Artifact;
limited with GPR2.Project.Source.Set;

package GPR2.Project.Source is

   use type GPR2.Source.Object;
   use type GPR2.Unit.Library_Unit_Type;

   type Naming_Exception_Kind is (No, Yes, Multi_Unit);

   subtype Naming_Exception_Value is
     Naming_Exception_Kind range Yes .. Multi_Unit;

   type Object is tagged private;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function "<" (Left, Right : Object) return Boolean;

   overriding function "=" (Left, Right : Object) return Boolean;

   function Create
     (Source           : GPR2.Source.Object;
      View             : Project.View.Object;
      Is_Interface     : Boolean;
      Naming_Exception : Naming_Exception_Kind;
      Is_Compilable    : Boolean;
      Aggregated       : Boolean := False) return Object
     with Pre => Source.Is_Defined
                 and then View.Is_Defined
                 and then (not Aggregated
                           or else View.Is_Aggregated_In_Library);
   --  Constructor for Object. View is where the source is defined (found from
   --  View Source_Dirs) and Extending_View is the optional view from which the
   --  project source is extended. That is, if Extending_View is defined then
   --  this source is coming from an extended project for View.

   function Path_Name (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Returns the source path-name

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

   function Naming_Exception (Self : Object) return Naming_Exception_Kind
     with Pre  => Self.Is_Defined,
          Post => Self.Has_Naming_Exception
                  or else Naming_Exception'Result = No;
   --  Returns whether the source comes from a naming exception

   function Has_Aggregating_View (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self has an aggregating view defined, that is source
   --  is part of an aggregate library.

   function Aggregating_View (Self : Object) return Project.View.Object
     with Pre  => Self.Is_Defined and then Self.Has_Aggregating_View,
          Post => Aggregating_View'Result.Kind = K_Aggregate_Library;
   --  Returns the aggregating view

   function Is_Main (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns whether the source is the main file to create executable

   function Artifacts
     (Self : Object; Force_Spec : Boolean := False) return Artifact.Object
     with Pre => Self.Is_Defined;
   --  Returns the source artifacts for this source. Note that the returned
   --  Source artifacts may not exist if the compilation has not yet been
   --  done/finished.
   --  If Flag Force_Spec is True than the artifact object created like the
   --  spec does not have a body. This mode is needed for gprinstall -m option.

   --
   --  The following routines only make sense if Has_Units is True
   --

   function Dependencies
     (Self    : Object;
      Closure : Boolean := False) return GPR2.Project.Source.Set.Object
     with Pre => Self.Is_Defined and then Self.Source.Has_Units;
   --  Returns the source files on which the current source file depends
   --  (potentially transitively).

   procedure Dependencies
     (Self     : Object;
      For_Each : not null access procedure
                   (Source : GPR2.Project.Source.Object);
      Closure  : Boolean := False);
   --  Call For_Each routine for each dependency source

   --
   --  The following routines may be used for both unit-based and
   --  non-unit-based sources. In the latter case, Index is not used.
   --

   function Has_Other_Part
     (Self  : Object;
      Index : Source_Info.Unit_Index := 1) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if an other part exists for this project source's unit at
   --  the given index.

   function Other_Part
     (Self  : Object;
      Index : Source_Info.Unit_Index := 1) return Object
     with Pre  => Self.Is_Defined and then Self.Has_Other_Part (Index),
          Post => Other_Part'Result.Is_Defined;
   --  Returns the project's source containing the other part for this project
   --  source's unit at the given index.

   function Separate_From
     (Self  : Object;
      Index : Source_Info.Unit_Index := 1) return Object
     with Pre => Self.Is_Defined
                 and then Self.Source.Kind = GPR2.Unit.S_Separate;
   --  Returns the project's source containing the separate for Self's unit at
   --  the given index.

   procedure Update (Self : in out Object);
   --  Ensure that the project source is parsed/updated if needed

private

   type Object is tagged record
      Source : GPR2.Source.Object;
      View   : Project.Weak_Reference;
      --  Use weak reference to View to avoid reference cycle between Source
      --  and its View. Otherwise we've got memory leak after release view and
      --  valgrind detected mess in memory deallocations at the process exit.

      Is_Interface     : Boolean               := False;
      Naming_Exception : Naming_Exception_Kind := No;
      Is_Compilable    : Boolean               := False;
      Aggregated       : Boolean               := False;
      Inherited        : Boolean               := False;
      --  From extended project
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
     (Self.Naming_Exception in Naming_Exception_Value);

   function Naming_Exception (Self : Object) return Naming_Exception_Kind is
     (Self.Naming_Exception);

   function Path_Name (Self : Object) return GPR2.Path_Name.Object is
     (Self.Source.Path_Name);

end GPR2.Project.Source;
