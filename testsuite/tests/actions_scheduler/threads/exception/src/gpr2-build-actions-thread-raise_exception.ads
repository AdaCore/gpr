--
--  Copyright (C) 2024-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Command_Line;
with GPR2.Build.Signature;
with GPR2.Build.Source;
with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;

private with GPR2.View_Ids;

package GPR2.Build.Actions.Thread.Raise_Exception is

   package PRA renames GPR2.Project.Registry.Attribute;

   type Raise_Exception_Id (<>) is new Actions.Action_Id with private;

   type Object is new Actions.Thread.Object with private;
   --  Action that raises an exception during execution.

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class;

   procedure Initialize
     (Self       : in out Object;
      Ctxt       : GPR2.Project.View.Object);

   overriding
   function View (Self : Object) return GPR2.Project.View.Object;

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean);

   overriding
   function Execute
     (Self   : in out Object;
      Stdout : in out Unbounded_String;
      Stderr : in out Unbounded_String) return Integer;

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object;

   overriding
   function Extended (Self : Object) return Object
   is (raise Constraint_Error with "Unexpected call to Extended");

private

   use type GPR2.View_Ids.View_Id;

   type Raise_Exception_Id is new Actions.Action_Id with record
      Ctxt  : GPR2.Project.View.Object;
   end record;

   overriding
   function View (Self : Raise_Exception_Id) return Project.View.Object
   is (Self.Ctxt);

   overriding
   function Action_Class (Self : Raise_Exception_Id) return Value_Type
   is ("Raise Exception");

   overriding
   function Language (Self : Raise_Exception_Id) return Language_Id
   is (No_Language);

   overriding
   function Action_Parameter (Self : Raise_Exception_Id) return Value_Type
   is (Value_Type (Self.Ctxt.Path_Name.Simple_Name));

   type Object is new Actions.Thread.Object with null record;

   overriding
   function View (Self : Object) return GPR2.Project.View.Object
   is (Self.Ctxt);

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object
   is (Path_Name.Create_Directory ("."));

end GPR2.Build.Actions.Thread.Raise_Exception;
