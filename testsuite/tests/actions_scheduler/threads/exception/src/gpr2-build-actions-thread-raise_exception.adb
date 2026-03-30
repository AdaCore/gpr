--
--  Copyright (C) 2024-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;

package body GPR2.Build.Actions.Thread.Raise_Exception is

   function Output_File return Artifacts.Files.Object;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean)
   is
   begin
      if not Self.Signature.Add_Output
               (Output_File, Check_Checksums)
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
   begin
      raise Constraint_Error with "test exception raised by action";
      return 0;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : in out Object;
      Ctxt       : GPR2.Project.View.Object) is
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
      return Db.Add_Output (Self.UID, Output_File);
   end On_Tree_Insertion;

   -----------------
   -- Output_File --
   -----------------

   function Output_File return Artifacts.Files.Object is
   begin
      return
        Artifacts.Files.Create
          (GPR2.Path_Name.Create_Directory (".").Compose ("output.txt"));
   end Output_File;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class is
      Result : constant Raise_Exception_Id :=
        (Ctxt => Self.Ctxt);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Thread.Raise_Exception;
