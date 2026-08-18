--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Text_IO;

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;

package body GPR2.Build.Actions.Thread.Report_Messages is

   function Output_File return Artifacts.Files.Object;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean)
   is
   begin
      if not Self.Signature.Add_Output (Output_File, Check_Checksums) then
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
      F : Ada.Text_IO.File_Type;

   begin
      --  Create the expected output so that the signature can be written

      Ada.Text_IO.Create
        (F, Ada.Text_IO.Out_File, String (Output_File.Path.Simple_Name));
      Ada.Text_IO.Put_Line (F, "done");
      Ada.Text_IO.Close (F);

      --  This runs in a dedicated task: report through the standard error
      --  output, like a process action would, and not through the tree's
      --  reporter, which belongs to the main task.

      Append (Stderr, "first line reported from the action thread" & ASCII.LF);
      Append (Stderr, "second line reported from the action thread");

      return Self.Ret_Code;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Object;
      Ctxt     : GPR2.Project.View.Object;
      Ret_Code : Integer := 0) is
   begin
      Self.Ctxt     := Ctxt;
      Self.Ret_Code := Ret_Code;
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
      Result : constant Report_Messages_Id := (Ctxt => Self.Ctxt);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Thread.Report_Messages;
