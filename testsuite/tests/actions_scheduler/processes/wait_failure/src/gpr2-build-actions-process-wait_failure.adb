--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;

package body GPR2.Build.Actions.Process.Wait_Failure is

   function Output_File return Artifacts.Files.Object;

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding
   procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean) is
   begin
      Cmd_Line.Add_Argument ("./child");
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean) is
   begin
      if not Self.Signature.Add_Output (Output_File, Check_Checksums) then
         return;
      end if;
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Object; Ctxt : GPR2.Project.View.Object) is
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
      Result : constant Wait_Failure_Id := (Ctxt => Self.Ctxt);
   begin
      return Result;
   end UID;

   -----------------------
   -- Working_Directory --
   -----------------------

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object is
   begin
      return Path_Name.Create_Directory (".");
   end Working_Directory;

end GPR2.Build.Actions.Process.Wait_Failure;
