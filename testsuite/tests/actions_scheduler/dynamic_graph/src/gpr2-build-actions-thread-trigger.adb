--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Text_IO;
with GPR2.Build.Actions.Thread.Followup;
with GPR2.Build.Artifacts.Files;

package body GPR2.Build.Actions.Thread.Trigger is

   function Output_File (Self : Object) return Artifacts.Files.Object is
     (Artifacts.Files.Create
       (Self.Ctxt.Object_Directory.Compose ("trigger.out")));

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean) is
   begin
      if not Self.Signature.Add_Output (Output_File (Self), Check_Checksums)
      then
         return;
      end if;
   end Compute_Signature;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Self   : in out Object;
      Stdout : in out Unbounded_String;
      Stderr : in out Unbounded_String) return Integer
   is
      pragma Unreferenced (Self, Stdout, Stderr);
   begin
      Ada.Text_IO.Put_Line ("Trigger executed");
      return 0;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Object;
      Ctxt : GPR2.Project.View.Object) is
   begin
      Self.Ctxt := Ctxt;
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean is
   begin
      return Db.Add_Output (Self.UID, Output_File (Self));
   end On_Tree_Insertion;

   --------------------
   -- Post_Execution --
   --------------------

   overriding
   function Post_Execution
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : Unbounded_String := Null_Unbounded_String;
      Stderr : Unbounded_String := Null_Unbounded_String) return Boolean
   is
      pragma Unreferenced (Stdout, Stderr);
      F : Followup.Object;
   begin
      if Status = Success then
         F.Initialize (Self.Ctxt);
         return Self.Tree.Add_Action (F);
      end if;
      return True;
   end Post_Execution;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class is
      Result : constant Trigger_Id := (Ctxt => Self.Ctxt);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Thread.Trigger;
