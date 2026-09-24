--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;

with GPR2.Build.Compilation_Unit;

package GPR2.Build.Actions.Thread.Lib_Copy is

   type Object is new GPR2.Build.Actions.Thread.Object with private;
   --  Copy to the library directories the ALI files, and the interface
   --  sources when the view has a Library_Src_Dir. What to copy is registered
   --  by Add_Interface_Unit.

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;

   type Lib_Copy_Id (<>) is new Actions.Action_Id with private;

   function Create (Ctxt : GPR2.Project.View.Object) return Lib_Copy_Id
   with Pre => Ctxt.Is_Defined and then Ctxt.Is_Library;
   --  Id of the library copy action of Ctxt, to look it up in the tree

   function Needed_For_View (Ctxt : GPR2.Project.View.Object) return Boolean
   with Pre => Ctxt.Is_Defined and then Ctxt.Is_Library;
   --  Return True if the library copy action is needed for the given view.
   --  This is the case when the view is a library and at least one of the
   --  following conditions is true:
   --  - the library is standalone and has a non-empty interface
   --  - the library is not standalone and has at least one unit in its
   --    interface (either directly or through aggregation) or one source
   --    to copy.

   procedure Initialize
     (Self : in out Object;
      Ctxt : GPR2.Project.View.Object)
   with Pre => Ctxt.Is_Defined and then Ctxt.Is_Library;

   function Add_Interface_Unit
     (Self            : in out Object;
      CU              : GPR2.Build.Compilation_Unit.Object;
      Dependency_File : Path_Name.Object) return Boolean
   with Pre => Self.Is_Defined and then Self.View.Is_Defined;
   --  Register what to copy for CU: Dependency_File, and the unit's sources
   --  when the view has a Library_Src_Dir. Called by the action that holds
   --  the compile action of CU. Idempotent.
   --  Returns False if a destination is already produced by another action.

   overriding
   function Execute
     (Self   : in out Object;
      Stdout : in out Unbounded_String;
      Stderr : in out Unbounded_String) return Integer
   with Pre => Self.Is_Defined;

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding
   function UID (Self : Object) return Action_Id'Class;

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object
   with Pre => Self.Is_Defined;

private

   type Lib_Copy_Id is new Actions.Action_Id with record
      Ctxt : GPR2.Project.View.Object;
   end record;

   overriding
   function View (Self : Lib_Copy_Id) return GPR2.Project.View.Object
   is (Self.Ctxt);

   overriding
   function Action_Class (Self : Lib_Copy_Id) return Value_Type
   is ("Library-Files-Copy");

   overriding
   function Language (Self : Lib_Copy_Id) return Language_Id
   is (No_Language);

   overriding
   function Action_Parameter (Self : Lib_Copy_Id) return Value_Type
   is (Value_Type (Self.Ctxt.Name));

   function Create (Ctxt : GPR2.Project.View.Object) return Lib_Copy_Id
   is (Lib_Copy_Id'(Ctxt => Ctxt));

   type Copy_Entry is record
      From   : Path_Name.Object;
      To     : Path_Name.Object;
      Unit   : Compilation_Unit.Object;
      --  The unit From belongs to
      Kind   : Unit_Kind := S_Spec;
      --  Which part of Unit From is, for the source entries
      Is_Ali : Boolean := False;
      --  ALIs are registered as files, sources as source files
      Add_SL : Boolean := False;
      --  Add the SL flag to the ALI's P line, so that a standalone library's
      --  units are not elaborated twice
      Skip   : Boolean := False;
      --  Set by Pre_Execution on the parts that need not be copied
   end record;

   package Copy_Entry_Vectors is new
     Ada.Containers.Vectors (Positive, Copy_Entry);

   package Filename_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (Filename_Type);

   type Object is new GPR2.Build.Actions.Thread.Object with record
      Copies   : Copy_Entry_Vectors.Vector;
      Alis     : Filename_Sets.Set;
      --  The ALIs already in Copies: a unit can be registered twice, and
      --  scanning Copies for each is quadratic in the interface size
      Lib_Name : Unbounded_String;
      --  Execute runs in its own task and can query neither the tree database
      --  nor the view, so everything it needs is stored here
   end record;

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean);

   overriding
   function Pre_Execution (Self : in out Object) return Boolean;

   overriding
   function Extended (Self : Object) return Object
   is (raise Internal_Error with "This action is not extending");

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object
   is (Self.Ctxt.Object_Directory);

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean
   is (Self /= Undefined);

end GPR2.Build.Actions.Thread.Lib_Copy;
