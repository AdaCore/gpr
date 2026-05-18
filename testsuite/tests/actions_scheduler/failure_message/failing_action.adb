--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body Failing_Action is

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean)
   is
   begin
      null;
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean)
   is
   begin
      null;
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Object;
      Ctxt    : GPR2.Project.View.Object;
      Message : String := "custom error: failing action could not complete")
   is
   begin
      Self.Set_View (Ctxt);
      Self.Msg := Ada.Strings.Unbounded.To_Unbounded_String (Message);
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding function On_Tree_Insertion
     (Self : Object;
      Db   : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
   begin
      return True;
   end On_Tree_Insertion;

   -------------------
   -- Pre_Execution --
   -------------------

   overriding function Pre_Execution
     (Self : in out Object) return Boolean
   is
   begin
      return False;
   end Pre_Execution;

   ---------------------
   -- Failure_Message --
   ---------------------

   overriding function Failure_Message
     (Self : Object) return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Self.Msg);
   end Failure_Message;

   ---------
   -- UID --
   ---------

   overriding function UID
     (Self : Object) return GPR2.Build.Actions.Action_Id'Class
   is
      Result : constant Failing_Action_Id := (Ctxt => Self.View);
   begin
      return Result;
   end UID;

end Failing_Action;
