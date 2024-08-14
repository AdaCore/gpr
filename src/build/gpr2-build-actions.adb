--
--  Copyright (C) 2023-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.OS.FS;
with GNATCOLL.OS.FSUtil;

with GPR2.Build.Tree_Db;
with GPR2.Build.Artifacts.Files;
with GPR2.Utils.Hash;

package body GPR2.Build.Actions is

   ------------
   -- Attach --
   ------------

   procedure Attach (Self : in out Object;
                     Db   : in out GPR2.Build.Tree_Db.Object)
   is
   begin
      Self.Tree := Db.Ref;
   end Attach;

   ------------------------
   -- Cleanup_Temp_Files --
   ------------------------

   procedure Cleanup_Temp_Files
     (Self : in out Object'Class;
      Scope : Temp_File_Scope)
   is
      use GNATCOLL.OS.FSUtil;
   begin
      if Scope = Global then
         Self.Tree.Clear_Temp_Files;
      else
         for F of Self.Tmp_Files loop
            if not Remove_File
              (Self.View.Object_Directory.Compose (F).String_Value)
            then
               Self.Traces.Trace
                 ("error: could not remove temp file " & String (F) & " in " &
                    Self.View.Object_Directory.String_Value);
            end if;
         end loop;
      end if;

      Self.Tmp_Files.Clear;
   end Cleanup_Temp_Files;

   -----------------------
   -- Compare_Signature --
   -----------------------

   procedure Compare_Signature (Self : in out Object)
   is
      use Build.Signature;
      use Utils.Hash;

      UID     : constant Actions.Action_Id'Class
                  := Actions.Object'Class (Self).UID;
      Db_File : constant GPR2.Path_Name.Object :=
                  Self.Tree.Db_Filename_Path (UID);
   begin
      Self.Signature := Load (Db_File);

      if Self.Signature.Coherent then
         Self.Signature.Set_Valid_State (True);
      else
         Self.Signature.Set_Valid_State (False);

         return;
      end if;

      for Artifact_Sign of Self.Signature.Artifacts_Signatures loop
         declare
            Artifact : constant GPR2.Build.Artifacts.Files.Object :=
                         GPR2.Build.Artifacts.Files.Create
                           (GPR2.Path_Name.Create
                              (Name      =>
                                 Filename_Type
                                  (UB.To_String (Artifact_Sign.Path)),
                               Path_Name =>
                                 Filename_Type
                                  (UB.To_String (Artifact_Sign.Path))));
         begin
            if Artifact.Path.Exists then
               if Artifact.Checksum /= Artifact_Sign.Checksum then
                  Self.Signature.Set_Valid_State (False);
               end if;
            else
               Self.Signature.Set_Valid_State (False);
            end if;
         end;
      end loop;
   end Compare_Signature;

   -----------------------
   -- Compute_Signature --
   -----------------------

   procedure Compute_Signature (Self : in out Object)
   is
      UID : constant Action_Id'Class := Object'Class (Self).UID;
   begin
      Self.Signature.Clear;

      for Input of Self.Tree.Inputs (UID) loop
         Self.Signature.Update_Artifact (Input);
      end loop;

      for Output of Self.Tree.Outputs (UID) loop
         Self.Signature.Update_Artifact (Output);
      end loop;

      --  ??? Should be done in Signature.Store
      Self.Signature.Set_Valid_State (True);

      Self.Signature.Store (Self.Tree.Db_Filename_Path (UID));
   end Compute_Signature;

   -----------------------------
   -- Get_Or_Create_Temp_File --
   -----------------------------

   function Get_Or_Create_Temp_File
     (Self    : in out Object'Class;
      Purpose : Filename_Type;
      Scope   : Temp_File_Scope) return Tree_Db.Temp_File
   is
   begin
      if Scope = Global then
         return Self.Tree.Get_Or_Create_Temp_File
           (Self.View, Purpose);
      else
         declare
            --  ??? Naive implementation as first try
            BN   : constant Filename_Type :=
                     "." & Self.UID.Db_Filename & "-" & Purpose & ".tmp";
            Dest : constant GPR2.Path_Name.Object :=
                     Self.View.Object_Directory.Compose (BN);
            FD   : GNATCOLL.OS.FS.File_Descriptor;
            use GNATCOLL.OS.FS;

         begin
            if Self.Tmp_Files.Contains (BN) then
               FD := Null_FD;
            else
               FD := GNATCOLL.OS.FS.Open
                 (Dest.String_Value,
                  GNATCOLL.OS.FS.Write_Mode);
               pragma Assert (FD /= Null_FD and then FD /= Invalid_FD,
                              "could not create " & Dest.String_Value);
               Self.Tmp_Files.Insert (BN);
            end if;

            return (Path_Len => BN'Length,
                    FD       => FD,
                    Path     => BN);
         end;
      end if;
   end Get_Or_Create_Temp_File;


end GPR2.Build.Actions;
