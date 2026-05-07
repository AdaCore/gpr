--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--
with GPR2.Build.Compilation_Unit;
with GPR2.Build.Compilation_Unit.Maps;

package GPR2.Build.Actions.Thread.Lib_Copy is

   type Object is new GPR2.Build.Actions.Thread.Object with private;
   --  Base type for actions executed as Ada threads

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;

   type Lib_Copy_Id (<>) is new Actions.Action_Id with private;

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

   overriding
   function Execute
     (Self   : in out Object;
      Stdout : in out Unbounded_String;
      Stderr : in out Unbounded_String) return Integer
   with Pre => Self.Is_Defined;

   overriding
   function On_Static_Completion (Self : in out Object) return Boolean;

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding
   function UID (Self : Object) return Action_Id'Class;

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object
   with Pre => Self.Is_Defined;

   procedure Add_Unit_To_Lib_Interface
     (Self             : in out Object;
      Compilation_Unit : GPR2.Build.Compilation_Unit.Object)
   with Pre => Self.Is_Defined and then
     Self.View.Is_Defined and then Self.View.Is_Library_Standalone;

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

   type Object is new GPR2.Build.Actions.Thread.Object with record
      Extended_Interface : Compilation_Unit.Maps.Map;
   end record;

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean);

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
