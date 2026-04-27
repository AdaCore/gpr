--  Copyright (C) 2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception

with Ada.Text_IO;
with GPR2.Build.Signature;

package body File_Action is
   overriding procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean)
   is
      Sig : GPR2.Build.Signature.Object := Self.Signature;
   begin
      if not Sig.Add_Input (Input, Check_Checksums)
        or else not Sig.Add_Output (Copy, Check_Checksums)
        or else not Sig.Add_Output (Marker, Check_Checksums)
      then
         Self.Set_Signature (Sig);
         return;
      end if;

      Self.Set_Signature (Sig);
   end Compute_Signature;

   overriding function Execute
     (Self   : in out Object;
      Stdout : in out Ada.Strings.Unbounded.Unbounded_String;
      Stderr : in out Ada.Strings.Unbounded.Unbounded_String) return Integer
   is
      pragma Unreferenced (Self, Stdout, Stderr);
      Source : Ada.Text_IO.File_Type;
      Dest   : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (Source, Ada.Text_IO.In_File, Input.Path.String_Value);
      declare
         Content : constant String := Ada.Text_IO.Get_Line (Source);
      begin
         Ada.Text_IO.Close (Source);
         Ada.Text_IO.Create
           (Dest, Ada.Text_IO.Out_File, Copy.Path.String_Value);
         Ada.Text_IO.Put_Line (Dest, Content);
         Ada.Text_IO.Close (Dest);
         Ada.Text_IO.Create
           (Dest, Ada.Text_IO.Out_File, Marker.Path.String_Value);
         Ada.Text_IO.Put_Line (Dest, "processed");
         Ada.Text_IO.Close (Dest);
      end;
      return 0;
   end Execute;

   procedure Initialize
     (Self : in out Object; View : GPR2.Project.View.Object) is
   begin
      Self.Set_View (View);
   end Initialize;

   overriding function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean is
   begin
      Db.Add_Input (Self.UID, Input);
      return Db.Add_Output (Self.UID, Copy)
        and then Db.Add_Output (Self.UID, Marker);
   end On_Tree_Insertion;

   overriding function UID
     (Self : Object) return GPR2.Build.Actions.Action_Id'Class is
   begin
      return File_Action_Id'(Ctxt => Self.View);
   end UID;
end File_Action;
