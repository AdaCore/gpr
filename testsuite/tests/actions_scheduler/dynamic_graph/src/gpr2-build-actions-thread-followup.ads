--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Tree_Db;
with GPR2.Path_Name;

package GPR2.Build.Actions.Thread.Followup is

   type Followup_Id (<>) is new Actions.Action_Id with private;

   type Object is new Actions.Thread.Object with private;
   --  Action added dynamically by Trigger.Post_Execution to verify that the
   --  scheduler re-enters the graph after a successful completion.

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
   function Extended (Self : Object) return Object is
     (raise Constraint_Error with "Unexpected call to Extended");

private

   type Followup_Id is new Actions.Action_Id with record
      Ctxt : GPR2.Project.View.Object;
   end record;

   overriding
   function View (Self : Followup_Id) return Project.View.Object
   is (Self.Ctxt);

   overriding
   function Action_Class (Self : Followup_Id) return Value_Type
   is ("Followup");

   overriding
   function Language (Self : Followup_Id) return Language_Id
   is (No_Language);

   overriding
   function Action_Parameter (Self : Followup_Id) return Value_Type
   is (Value_Type (Self.Ctxt.Name));

   type Object is new Actions.Thread.Object with null record;

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object
   is (Self.Ctxt.Object_Directory);

end GPR2.Build.Actions.Thread.Followup;
