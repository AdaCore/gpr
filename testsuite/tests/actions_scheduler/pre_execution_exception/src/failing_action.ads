--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GPR2.Build.Actions.Thread;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;
with GPR2.Project.View;

package Failing_Action is
   use GPR2;
   use GPR2.Build;

   type Failure_Kind is (Scheduler_Error, Unexpected_Exception);

   type Object is new Actions.Thread.Object with private;

   procedure Initialize
     (Self : in out Object;
      View : Project.View.Object;
      Kind : Failure_Kind);

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object;

   overriding function On_Tree_Insertion
     (Self : Object; Db : in out Tree_Db.Object) return Boolean;

   overriding procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean);

   overriding function Pre_Execution (Self : in out Object) return Boolean;

   overriding function Execute
     (Self   : in out Object;
      Stdout : in out Unbounded_String;
      Stderr : in out Unbounded_String) return Integer;

   overriding function Extended (Self : Object) return Object
   is (raise Program_Error with "unexpected call to Extended");

private

   type Object is new Actions.Thread.Object with record
      Kind : Failure_Kind;
   end record;

   type Action_Id is new Actions.Action_Id with record
      Ctxt : Project.View.Object;
   end record;

   overriding function View (Self : Action_Id) return Project.View.Object
   is (Self.Ctxt);

   overriding function Action_Class (Self : Action_Id) return Value_Type
   is ("Pre Execution Exception");

   overriding function Language (Self : Action_Id) return Language_Id
   is (No_Language);

   overriding function Action_Parameter (Self : Action_Id) return Value_Type
   is (Value_Type (Self.Ctxt.Name));

end Failing_Action;
