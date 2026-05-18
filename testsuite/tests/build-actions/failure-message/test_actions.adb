--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Test_Actions is

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding procedure Compute_Command
     (Self           : in out Default_Msg_Action;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean)
   is
   begin
      null;
   end Compute_Command;

   overriding procedure Compute_Command
     (Self           : in out Custom_Msg_Action;
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
     (Self : in out Default_Msg_Action; Check_Checksums : Boolean)
   is
   begin
      null;
   end Compute_Signature;

   overriding procedure Compute_Signature
     (Self : in out Custom_Msg_Action; Check_Checksums : Boolean)
   is
   begin
      null;
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Default_Msg_Action;
      Ctxt : GPR2.Project.View.Object)
   is
   begin
      Self.Set_View (Ctxt);
   end Initialize;

   procedure Initialize
     (Self : in out Custom_Msg_Action;
      Ctxt : GPR2.Project.View.Object)
   is
   begin
      Self.Set_View (Ctxt);
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding function On_Tree_Insertion
     (Self : Default_Msg_Action;
      Db   : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
   begin
      return True;
   end On_Tree_Insertion;

   overriding function On_Tree_Insertion
     (Self : Custom_Msg_Action;
      Db   : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
   begin
      return True;
   end On_Tree_Insertion;

   ---------------------
   -- Failure_Message --
   ---------------------

   overriding function Failure_Message
     (Self : Custom_Msg_Action) return String
   is
   begin
      return "custom failure: action did not succeed";
   end Failure_Message;

   ---------
   -- UID --
   ---------

   overriding function UID
     (Self : Default_Msg_Action)
      return GPR2.Build.Actions.Action_Id'Class
   is
      Result : constant Test_Action_Id :=
        (Ctxt         => Self.View,
         Action_Class => To_Unbounded_String ("Default Action"));
   begin
      return Result;
   end UID;

   overriding function UID
     (Self : Custom_Msg_Action)
      return GPR2.Build.Actions.Action_Id'Class
   is
      Result : constant Test_Action_Id :=
        (Ctxt         => Self.View,
         Action_Class => To_Unbounded_String ("Custom Action"));
   begin
      return Result;
   end UID;

end Test_Actions;
