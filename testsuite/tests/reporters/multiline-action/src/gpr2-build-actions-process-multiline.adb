--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.Actions.Process.Multiline is

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding
   procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean)
   is
      pragma Unreferenced (Slot, Signature_Only);
   begin
      --  Spawn the custom Ada executable. It uses Ada.Text_IO.Put_Line to
      --  print several lines, so the line terminators are produced by the
      --  Text_IO text layer (bare LF on Unix, CR/LF on Windows).
      Cmd_Line.Set_Driver (Self.Executable.String_Value);
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean) is
   begin
      --  This action produces no file: use its UID artifact as output so the
      --  signature is non-empty (an empty signature is not writable).
      if not Self.Signature.Add_Output (Self.UID_Artifact, Check_Checksums)
      then
         return;
      end if;
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : in out Object;
      View       : GPR2.Project.View.Object;
      Executable : GPR2.Path_Name.Object) is
   begin
      Self.Ctxt       := View;
      Self.Executable := Executable;
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      return Db.Add_Output (UID, Self.UID_Artifact);
   end On_Tree_Insertion;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class is
      Result : constant Multiline_Id := (Ctxt => Self.Ctxt);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Process.Multiline;
