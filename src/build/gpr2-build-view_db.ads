--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  limited with GPR2.Build.Compilation_Input.Sets;
with GPR2.Build.Source;
limited with GPR2.Build.Source.Sets;
limited with GPR2.Build.Tree_Db;
with GPR2.Build.Compilation_Unit.Maps;
with GPR2.Log;
with GPR2.Project.View;

private with GPR2.Build.View_Tables;

package GPR2.Build.View_Db is

   type Object is tagged private;

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;

   procedure Update
     (Self     : Object;
      Messages : in out GPR2.Log.Object)
     with Pre => Self.Is_Defined;
   --  Update the list of objects and dependency files found in the object
   --  directory and adjust the internal values.

   function Sources
     (Self   : Object;
      Sorted : Boolean := False) return GPR2.Build.Source.Sets.Object
     with Pre => Self.Is_Defined and then Self.Source_Option > No_Source;
   --  Returns an iterator on the source set. If Sorted is set, the result is
   --  alphabetically sorted (but the operation is slower).

   function Has_Source
     (Self     : Object;
      Basename : Simple_Name) return Boolean
     with Pre => Self.Is_Defined and then Self.Source_Option > No_Source;
   --  Check if Basename is a source for the view.

   function Source
     (Self     : Object;
      Basename : Simple_Name) return GPR2.Build.Source.Object
     with Pre  => Self.Is_Defined and then Self.Source_Option > No_Source,
          Post => Self.Has_Source (Basename) = Source'Result.Is_Defined;
   --  Get a source info object for the view source with simple name Basename.
   --
   --  Note that only sources owned by the view (present in its source
   --  directory, inherited via project extension, or aggregated in an
   --  aggregate library are returned, but not the sources visible via simple
   --  withed or limited withed projects.
   --
   --  This contrasts with the "Visible_Source" primitive in this regard.

   type Source_Context is record
      Owning_View   : GPR2.Project.View.Object;
      Source        : GPR2.Build.Source.Object;
   end record;

   No_Context : constant Source_Context := (others => <>);

   function Visible_Source
     (Self     : Object;
      Basename : Simple_Name) return Source_Context
     with Pre  => Self.Is_Defined and then Self.Source_Option > No_Source;
   --  Get a source from its simple name, that is visible for a given view's
   --  sources (so project's own sources and all its withed projects).

   function Visible_Sources
     (Self : Object) return GPR2.Build.Source.Sets.Object
     with Pre => Self.Is_Defined and then Self.Source_Option > No_Source;
   --  Get the complete list of visible sources: so sources owned by the view
   --  but also all sources made visible by withed or limited withed views.

   function Has_Compilation_Unit
     (Self : Object;
      Name : Name_Type) return Boolean
     with Pre => Self.Is_Defined
                   and then Self.View.Is_Namespace_Root
                   and then Self.Source_Option >= Sources_Units;
   --  Whether the compilation unit is defined in the namespace

   function Compilation_Unit
     (Self : Object;
      Name : Name_Type) return Build.Compilation_Unit.Object
     with Pre => Self.Has_Compilation_Unit (Name);
   --  Return the compilation unit named "Name".

   function Compilation_Units
     (Self : Object) return Build.Compilation_Unit.Maps.Map
     with Pre => Self.Is_Defined
                   and then Self.View.Is_Namespace_Root
                   and then Self.Source_Option >= Sources_Units;

   function Own_Unit
     (Self : Object;
      Name : Name_Type) return Build.Compilation_Unit.Object
     with Pre => Self.Is_Defined and then Self.Source_Option >= Sources_Units;

   function View (Self : Object) return GPR2.Project.View.Object
     with Pre => Self.Is_Defined;

   function View_Base_For
     (Self : Object;
      View : Project.View.Object) return Object
     with Pre => Self.Is_Defined;
   --  Retrieve the build database for View

   function Tree_Db (Self : Object) return access GPR2.Build.Tree_Db.Object
     with Pre => Self.Is_Defined;

   function Source_Option (Self : Object) return Optional_Source_Info_Option
     with Pre => Self.Is_Defined;

   function "<" (L, R : Object) return Boolean;

private

   use type Project.View.Object;

   type Object is new View_Tables.Data_Refs.Ref with null record;

   Undefined : constant Object :=
                 Object'(View_Tables.Data_Refs.Null_Ref with null record);

   function Is_Defined (Self : Object) return Boolean is
      (not Self.Is_Null);

   function View (Self : Object) return GPR2.Project.View.Object is
     (Self.Get.View);

   function "<" (L, R : Object) return Boolean is
      (L.Get.View < R.Get.View);

end GPR2.Build.View_Db;
