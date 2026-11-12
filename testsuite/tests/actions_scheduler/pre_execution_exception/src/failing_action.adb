--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions_Scheduler;

package body Failing_Action is

   overriding procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean) is
   begin
      null;
   end Compute_Signature;

   overriding function Execute
     (Self   : in out Object;
      Stdout : in out Unbounded_String;
      Stderr : in out Unbounded_String) return Integer
   is (raise Program_Error with "action must not execute after Pre_Execution");

   procedure Initialize
     (Self : in out Object;
      View : Project.View.Object;
      Kind : Failure_Kind) is
   begin
      Self.Set_View (View);
      Self.Kind := Kind;
   end Initialize;

   overriding function On_Tree_Insertion
     (Self : Object; Db : in out Tree_Db.Object) return Boolean is
   begin
      return True;
   end On_Tree_Insertion;

   overriding function Pre_Execution (Self : in out Object) return Boolean is
   begin
      return
        (case Self.Kind is
            when Scheduler_Error =>
              raise Actions_Scheduler.Actions_Scheduler_Error
                with "scheduler exception from Pre_Execution",
            when Unexpected_Exception =>
              raise Constraint_Error
                with "unexpected exception from Pre_Execution");
   end Pre_Execution;

   overriding function UID (Self : Object) return Actions.Action_Id'Class
   is (Action_Id'(Ctxt => Self.View));

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object
   is (Self.View.Dir_Name);

end Failing_Action;
