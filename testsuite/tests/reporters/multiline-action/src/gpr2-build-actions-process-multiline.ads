--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Command_Line;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;
with GPR2.Project.View;

package GPR2.Build.Actions.Process.Multiline is

   type Multiline_Id (<>) is new Actions.Action_Id with private;

   type Object is new Actions.Process.Object with private;

   procedure Initialize
     (Self       : in out Object;
      View       : GPR2.Project.View.Object;
      Executable : GPR2.Path_Name.Object);

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class;

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding
   procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean);

private

   type Multiline_Id is new Actions.Action_Id with record
      Ctxt : GPR2.Project.View.Object;
   end record;

   overriding
   function View (Self : Multiline_Id) return Project.View.Object
   is (Self.Ctxt);

   overriding
   function Action_Class (Self : Multiline_Id) return Value_Type
   is ("Multiline");

   overriding
   function Language (Self : Multiline_Id) return Language_Id
   is (No_Language);

   overriding
   function Action_Parameter (Self : Multiline_Id) return Value_Type
   is ("multiline");

   type Object is new Actions.Process.Object with record
      Executable : GPR2.Path_Name.Object;
   end record;

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object
   is (Path_Name.Create_Directory ("."));

   overriding
   function Extended (Self : Object) return Object
   is (raise Constraint_Error with "This action is not extending");

end GPR2.Build.Actions.Process.Multiline;
