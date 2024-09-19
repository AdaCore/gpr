--
--  Copyright (C) 2023-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.OS.FS;
with GNATCOLL.OS.FSUtil;

with GPR2.Build.Tree_Db;

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
   -- Compute_Signature --
   -----------------------

   procedure Compute_Signature
     (Self   : in out Object;
      Stdout : Unbounded_String;
      Stderr : Unbounded_String)
   is
      UID : constant Action_Id'Class := Object'Class (Self).UID;
   begin
      Self.Signature.Clear;

      for Input of Self.Tree.Inputs (UID) loop
         Self.Signature.Add_Artifact (Input);
      end loop;

      for Output of Self.Tree.Outputs (UID) loop
         Self.Signature.Add_Artifact (Output);
      end loop;

      Self.Signature.Add_Output (Stdout, Stderr);

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

   --------------------
   -- Load_Signature --
   --------------------

   procedure Load_Signature (Self : in out Object'Class)
   is
      Db_File : constant GPR2.Path_Name.Object :=
                  Self.View.Object_Directory.Compose (Self.UID.Db_Filename);
      Found   : Boolean;
   begin
      if Db_File.Exists then
         Self.Signature := Build.Signature.Load (Db_File);
         Found := True;

         --  ??? We also need to check the extended DB file somehow
      end if;

      if not Found then
         Self.Signature.Clear;
      end if;
   end Load_Signature;


end GPR2.Build.Actions;
