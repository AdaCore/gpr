--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.View.Set;
with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.OS.Stat;

package body GPR2.Build.Actions.Thread.Write_File is

   function Output_File (Index : Integer) return Artifacts.Files.Object;
   --  Return output file "<index>.txt"

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean)
   is
      Art : Artifacts.Files.Object;
   begin
      for Art of Self.Tree.Inputs (Self.UID) loop
         if not Self.Signature.Add_Input (Art, Check_Checksums) then
            return;
         end if;
      end loop;

      if not Self.Signature.Add_Output
               (Output_File (Self.Index), Check_Checksums)
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

      File_Name    : constant String :=
        String (Output_File (Self.Index).Path.Simple_Name);
      File_Content : constant String :=
        Ada.Strings.Fixed.Trim (Self.Index'Img, Both);
      F            : File_Type;
   begin
      if Self.With_Wait > 0 then
         loop
         if Output_File (Self.With_Wait).Path.Exists then
            exit;
         end if;
         delay 0.1;
         end loop;
      end if;

      Create (F, Out_File, File_Name);
      Put_Line (F, File_Content);
      Close (F);

      if Self.Ret_Code /= 0 then
         Ada.Text_IO.Put_Line
           (Standard_Error,
            "File '"
            & File_Name
            & "' has been created, but return code is"
            & Self.Ret_Code'Img);
         return 1;
      end if;

      return 0;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : in out Object;
      Ctxt       : GPR2.Project.View.Object;
      Index      : Integer;
      Executable : GPR2.Path_Name.Object;
      Ret_Code   : Integer := 0;
      With_Deps  : Boolean := True;
      With_Wait  : Natural := 0) is
   begin
      Self.Ctxt := Ctxt;
      Self.Index := Index;
      Self.Ret_Code := Ret_Code;
      Self.With_Deps := With_Deps;
      Self.Executable := Executable;
      Self.With_Wait := With_Wait;
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean is
   begin
      if Self.With_Deps and then Self.Index > 1 then
         Db.Add_Input (Self.UID, Output_File (Self.Index - 1), True);
      end if;

      return Db.Add_Output (Self.UID, Output_File (Self.Index));
   end On_Tree_Insertion;

   -----------------
   -- Output_File --
   -----------------

   function Output_File (Index : Integer) return Artifacts.Files.Object is
      Idx : constant String := Index'Image;
   begin
      return
        Artifacts.Files.Create
          (GPR2.Path_Name.Create_Directory (".").Compose
             (Filename_Type (Idx (Idx'First + 1 .. Idx'Last)) & ".txt"));
   end Output_File;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class is
      Result : constant Write_File_Id :=
        (Ctxt => Self.Ctxt, Index => Self.Index);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Thread.Write_File;
