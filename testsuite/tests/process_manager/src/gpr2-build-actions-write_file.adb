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

   function Output_File (Index : Integer) return GPR2.Path_Name.Object;
   --  Return output file "<index>.txt"

   -------------
   -- Command --
   -------------

   overriding function Command (Self : Object)
     return GNATCOLL.OS.Process.Argument_List
   is
      Args : GNATCOLL.OS.Process.Argument_List;
   begin
      Args.Append (Self.Executable.String_Value);
      Args.Append (Ada.Strings.Fixed.Trim (Self.Ret_Code'Img, Both));
      Args.Append (Output_File (Self.Index).String_Value);
      Args.Append (Ada.Strings.Fixed.Trim (Self.Index'Img, Both));

      if Self.With_Wait > 0 then
         Args.Append (String (Output_File (Self.With_Wait).Simple_Name));
      end if;

      return Args;
   end Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature (Self : in out Object) is
      use GPR2.Build.Signature;
      Art : Artifacts.Files.Object;
   begin
      Self.Signature.Clear;

      Art := Artifacts.Files.Create (Output_File (Self.Index));
      Self.Signature.Update_Artifact (Art.UID, Art.Image, Art.Checksum);

      Self.Signature.Store
        (Self.Tree.Db_Filename_Path (Object'Class (Self).UID));
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

   overriding procedure On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object;
      Messages : in out GPR2.Log.Object)
   is
   begin
      if Self.With_Deps and then Self.Index > 1 then
         Db.Add_Input
            (Self.UID,
             Artifacts.Files.Create (Output_File (Self.Index - 1)),
             True);
      end if;

      Db.Add_Output
        (Self.UID,
         Artifacts.Files.Create (Output_File (Self.Index)),
         Messages);
   end On_Tree_Insertion;

   -----------------
   -- Output_File --
   -----------------

   function Output_File (Index : Integer) return GPR2.Path_Name.Object
   is
   begin
      return
        GPR2.Path_Name.Create_File
          (Filename_Type (Ada.Strings.Fixed.Trim (Index'Img, Both) & ".txt"));
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
