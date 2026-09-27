--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Text_IO;

with GPR2.Build.Artifacts.Files;
with GPR2.Path_Name;

package body GPR2.Build.Actions.Thread.Always_Execute is

   function Output_Name (Self : Object) return Filename_Type is
     (Filename_Type
        ("always_execute" & Self.Index'Image (2 .. Self.Index'Image'Last)
         & ".out"));

   function Output_File (Self : Object) return Artifacts.Files.Object is
     (Artifacts.Files.Create
       (Self.Ctxt.Object_Directory.Compose (Output_Name (Self))));

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean) is
   begin
      if not Self.Signature.Add_Input
        (Artifacts.Files.Create (Self.Ctxt.Dir_Name.Compose ("foo.ads")),
         Check_Checksums)
      then
         return;
      end if;

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
      pragma Unreferenced (Stdout, Stderr);
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Put_Line ("Executed");

      Ada.Text_IO.Create
        (File, Ada.Text_IO.Out_File,
         Output_File (Self).Path.String_Value);
      Ada.Text_IO.Put_Line (File, "done");
      Ada.Text_IO.Close (File);

      return 0;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : in out Object;
      Ctxt  : GPR2.Project.View.Object;
      Index : Positive;
      Force : Boolean := True) is
   begin
      Self.Ctxt  := Ctxt;
      Self.Index := Index;
      Self.Force := Force;
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean is
   begin
      Db.Add_Input
        (Self.UID,
         Artifacts.Files.Create (Self.Ctxt.Dir_Name.Compose ("foo.ads")));
      return Db.Add_Output (Self.UID, Output_File (Self));
   end On_Tree_Insertion;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class is
      Result : constant Always_Execute_Id :=
                 (Ctxt => Self.Ctxt, Index => Self.Index);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Thread.Always_Execute;
