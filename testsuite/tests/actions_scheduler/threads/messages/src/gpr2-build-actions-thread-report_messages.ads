--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Signature;
with GPR2.Path_Name;

private with GPR2.View_Ids;

package GPR2.Build.Actions.Thread.Report_Messages is

   type Report_Messages_Id (<>) is new Actions.Action_Id with private;

   type Object is new Actions.Thread.Object with private;
   --  Action that reports diagnostics from the task it is executed in, by
   --  writing them to its standard error output like a process action would,
   --  rather than through the tree's reporter, which it must not touch from
   --  there.

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class;

   procedure Initialize
     (Self     : in out Object;
      Ctxt     : GPR2.Project.View.Object;
      Ret_Code : Integer := 0);
   --  Ret_Code is what Execute returns, so that the output can be checked
   --  both on the successful and on the failing path.

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

   type Report_Messages_Id is new Actions.Action_Id with record
      Ctxt : GPR2.Project.View.Object;
   end record;

   overriding
   function View (Self : Report_Messages_Id) return Project.View.Object
   is (Self.Ctxt);

   overriding
   function Action_Class (Self : Report_Messages_Id) return Value_Type
   is ("Report Messages");

   overriding
   function Language (Self : Report_Messages_Id) return Language_Id
   is (No_Language);

   overriding
   function Action_Parameter (Self : Report_Messages_Id) return Value_Type
   is (Value_Type (Self.Ctxt.Path_Name.Simple_Name));

   type Object is new Actions.Thread.Object with record
      Ret_Code : Integer := 0;
   end record;

   overriding
   function View (Self : Object) return GPR2.Project.View.Object
   is (Self.Ctxt);

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object
   is (Path_Name.Create_Directory ("."));

end GPR2.Build.Actions.Thread.Report_Messages;
