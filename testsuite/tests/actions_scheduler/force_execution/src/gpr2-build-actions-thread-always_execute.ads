--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Tree_Db;
with GPR2.Path_Name;

package GPR2.Build.Actions.Thread.Always_Execute is

   type Always_Execute_Id (<>) is new Actions.Action_Id with private;

   type Object is new Actions.Thread.Object with private;
   --  Action that always executes regardless of its signature, used to test
   --  the Force_Execution primitive.

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class;

   procedure Initialize
     (Self : in out Object;
      Ctxt : GPR2.Project.View.Object);

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
   function Force_Execution (Self : Object) return Boolean;

   overriding
   function Extended (Self : Object) return Object is
     (raise Constraint_Error with "Unexpected call to Extended");

private

   type Always_Execute_Id is new Actions.Action_Id with record
      Ctxt : GPR2.Project.View.Object;
   end record;

   overriding
   function View (Self : Always_Execute_Id) return Project.View.Object
   is (Self.Ctxt);

   overriding
   function Action_Class (Self : Always_Execute_Id) return Value_Type
   is ("Always-Execute");

   overriding
   function Language (Self : Always_Execute_Id) return Language_Id
   is (No_Language);

   overriding
   function Action_Parameter (Self : Always_Execute_Id) return Value_Type
   is ("");

   type Object is new Actions.Thread.Object with null record;

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object
   is (Self.Ctxt.Object_Directory);

   overriding
   function Force_Execution (Self : Object) return Boolean
   is (True);

end GPR2.Build.Actions.Thread.Always_Execute;
