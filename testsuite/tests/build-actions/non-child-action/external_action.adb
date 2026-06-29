--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Signature;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;

package body External_Action is

   function Output_File (Self : Object) return GPR2.Build.Artifacts.Files.Object;
   --  Return the output artifact: <view_name>.txt in the current directory.

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature
     (Self            : in out Object;
      Check_Checksums : Boolean)
   is
      --  External_Action is not a child of GPR2.Build.Actions, so the
      --  Signature field is private. Use the getter/setter pair to modify
      --  the signature without direct field access.
      Sig : GPR2.Build.Signature.Object := Self.Signature;
   begin
      if not Sig.Add_Output (Output_File (Self), Check_Checksums) then
         return;
      end if;
      Self.Set_Signature (Sig);
   end Compute_Signature;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self   : in out Object;
      Stdout : in out Unbounded_String;
      Stderr : in out Unbounded_String) return Integer
   is
      pragma Unreferenced (Stdout);
      File_Name : constant String :=
        String (Output_File (Self).Path.Simple_Name);
      F         : File_Type;
   begin
      Create (F, Out_File, File_Name);
      Put_Line (F, String (Self.View.Name));
      Close (F);
      return 0;
   exception
      when E : others =>
         Append (Stderr, Ada.Exceptions.Exception_Message (E));
         return 1;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Object;
      Ctxt : GPR2.Project.View.Object) is
   begin
      Self.Set_View (Ctxt);
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding function On_Tree_Insertion
     (Self : Object;
      Db   : in out GPR2.Build.Tree_Db.Object) return Boolean is
   begin
      return Db.Add_Output (Self.UID, Output_File (Self));
   end On_Tree_Insertion;

   -----------------
   -- Output_File --
   -----------------

   function Output_File (Self : Object) return GPR2.Build.Artifacts.Files.Object is
   begin
      return GPR2.Build.Artifacts.Files.Create
        (GPR2.Path_Name.Create_Directory (".").Compose
           (GPR2.Filename_Type (String (Self.View.Name) & ".txt")));
   end Output_File;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return GPR2.Build.Actions.Action_Id'Class is
      Result : constant External_Action_Id := (Ctxt => Self.View);
   begin
      return Result;
   end UID;

end External_Action;
