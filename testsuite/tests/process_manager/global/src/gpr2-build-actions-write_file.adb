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

package body GPR2.Build.Actions.Write_File is

   function Output_File (Index : Integer) return Artifacts.Files.Object;
   --  Return output file "<index>.txt"

   -------------
   -- Command --
   -------------

   overriding procedure Compute_Command
     (Self     : in out Object;
      Slot     : Positive;
      Cmd_Line : in out GPR2.Build.Command_Line.Object) is
   begin
      Cmd_Line.Add_Argument (Self.Executable.String_Value);
      Cmd_Line.Add_Argument (Ada.Strings.Fixed.Trim (Self.Ret_Code'Img, Both));
      Cmd_Line.Add_Argument (String (Output_File (Self.Index).Path.Simple_Name));
      Cmd_Line.Add_Argument (Ada.Strings.Fixed.Trim (Self.Index'Img, Both));

      if Self.With_Wait > 0 then
         Cmd_Line.Add_Argument (String (Output_File (Self.With_Wait).Path.Simple_Name));
      end if;
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature
     (Self      : in out Object;
      Load_Mode : Boolean)
   is
      Art : Artifacts.Files.Object;
   begin
      for Art of Self.Tree.Inputs (Self.UID) loop
         if not Self.Signature.Add_Input (Art) and then Load_Mode then
            return;
         end if;
      end loop;

      if not Self.Signature.Add_Output (Output_File (Self.Index))
        and then Load_Mode
      then
         return;
      end if;
   end Compute_Signature;

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
     With_Wait  : Natural := 0)
   is
   begin
      Self.Ctxt       := Ctxt;
      Self.Index      := Index;
      Self.Ret_Code   := Ret_Code;
      Self.With_Deps  := With_Deps;
      Self.Executable := Executable;
      Self.With_Wait  := With_Wait;
      Self.Traces     := Create ("ACTION_WRITE_FILE");
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding function On_Tree_Insertion
     (Self : Object;
      Db   : in out GPR2.Build.Tree_Db.Object) return Boolean is
   begin
      if Self.With_Deps and then Self.Index > 1 then
         Db.Add_Input
            (Self.UID,
             Output_File (Self.Index - 1),
             True);
      end if;

      return Db.Add_Output
        (Self.UID, Output_File (Self.Index));
   end On_Tree_Insertion;

   -----------------
   -- Output_File --
   -----------------

   function Output_File (Index : Integer) return Artifacts.Files.Object
   is
      Idx : constant String := Index'Image;
   begin
      return Artifacts.Files.Create
        (GPR2.Path_Name.Create_Directory (".").Compose
          (Filename_Type (Idx (Idx'First + 1 .. Idx'Last)) & ".txt"));
   end Output_File;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      Result : constant Write_File_Id := (Ctxt     => Self.Ctxt,
                                          Index    => Self.Index);
   begin
      return Result;
   end UID;
end GPR2.Build.Actions.Write_File;
