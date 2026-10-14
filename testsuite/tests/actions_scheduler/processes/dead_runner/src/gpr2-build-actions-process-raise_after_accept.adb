--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Task_Attributes;
with Ada.Task_Identification;

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;

package body GPR2.Build.Actions.Process.Raise_After_Accept is

   package Copy_Failure is new Ada.Task_Attributes (Boolean, False);

   function Output_File return Artifacts.Files.Object;

   ------------
   -- Adjust --
   ------------

   overriding
   procedure Adjust (Self : in out Copy_Trigger) is
   begin
      if Copy_Failure.Value then
         raise Constraint_Error with "test exception after Execute";
      end if;
   end Adjust;

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
      --  Any command will do: the launch never happens

      Cmd_Line.Add_Argument ("echo");
      Cmd_Line.Add_Argument ("unreachable");
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
      use Ada.Task_Identification;

      Result : constant Raise_After_Accept_Id := (Ctxt => Self.Ctxt);
   begin
      if Current_Task /= Environment_Task then
         --  Arm the next action copy, after the rendezvous returns.

         Copy_Failure.Set_Value (True);
      end if;

      return Result;
   end UID;

end GPR2.Build.Actions.Process.Raise_After_Accept;
