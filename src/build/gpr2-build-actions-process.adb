--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Tree_Db;
with GPR2.Build.Artifacts.Key_Value;
with GPR2.Build.Signature;
with GPR2.Path_Name;
with GPR2.Project.View;


package body GPR2.Build.Actions.Process is

   Command_Line_Key : constant String := "command_line";

   --------------------
   -- Load_Signature --
   --------------------

   overriding
   procedure Load_Signature
     (Self : in out Object; Check_Checksums : Boolean := True)
   is
      Db_File  : constant GPR2.Path_Name.Object :=
        Self.Tree.Db_Filename_Path (Object'Class (Self).UID, True);
      Cmd_Line : GPR2.Build.Command_Line.Object;
      Ign      : Boolean
      with Unreferenced;

   begin
      if not Db_File.Is_Defined then
         Self.Signature.Clear;

         return;
      end if;

      Self.Signature := Build.Signature.Load (Db_File, Self.View);

      Object'Class (Self).Compute_Signature (Check_Checksums);

      if Self.Signature.Was_Saved then
         --  The signature hasn't been invalidated for now, so the last
         --  element to check is its command line
         Cmd_Line :=
           GPR2.Build.Command_Line.Create
             (Object'Class (Self).Working_Directory, Self.Ctxt.Context);
         Object'Class (Self).Compute_Command
           (1, Cmd_Line, Signature_Only => True);

         Ign :=
           Self.Signature.Add_Input
             (Artifacts.Key_Value.Create
                (Command_Line_Key, Cmd_Line.Signature),
              Check_Checksums);
      end if;

   exception
      when others =>
         Self.Signature.Clear;
   end Load_Signature;

   -------------------------
   -- Update_Command_Line --
   -------------------------

   procedure Update_Command_Line (Self : in out Object'Class; Slot : Positive)
   is
      Ign : Boolean
      with Unreferenced;
   begin
      Self.Cmd_Line :=
        GPR2.Build.Command_Line.Create
          (Self.Working_Directory, Self.Ctxt.Context);
      Self.Compute_Command (Slot, Self.Cmd_Line, False);
      Self.Compute_Response_Files (Self.Cmd_Line);

      if Self.Cmd_Line.Total_Length = 0 and then not Self.Deactivated then
         raise Action_Error;
      end if;
   end Update_Command_Line;

   ---------------------
   -- Write_Signature --
   ---------------------

   overriding
   function Write_Signature
     (Self   : in out Object;
      Stdout : Unbounded_String;
      Stderr : Unbounded_String) return Boolean
   is
      UID : constant Action_Id'Class := Object'Class (Self).UID;
      Ign : Boolean
      with Unreferenced;
   begin
      --  Ensure the inputs and outputs are up-to-date after the action is
      --  executed: the list of presumed inputs and outputs may need to be
      --  refined after the fact.

      Self.Signature.Clear;
      Object'Class (Self).Compute_Signature (Check_Checksums => False);

      if Self.Signature.Is_Empty then
         return False;
      end if;

      Ign :=
        Self.Signature.Add_Input
          (Artifacts.Key_Value.Create
             (Command_Line_Key, Self.Cmd_Line.Signature));

      Self.Signature.Add_Console_Output (Stdout, Stderr);

      Self.Signature.Store (Self.Tree.Db_Filename_Path (UID, False));

      return True;
   end Write_Signature;

end GPR2.Build.Actions.Process;
