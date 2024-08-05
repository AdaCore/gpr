--
--  Copyright (C) 2023-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Tree_Db;
with GPR2.Build.Artifacts.Files;
with GPR2.Message;
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

   -----------------------
   -- Compare_Signature --
   -----------------------

   procedure Compare_Signature
     (Self     : in out Object;
      Messages : in out GPR2.Log.Object)
   is
      use Build.Signature;
      use Utils.Hash;

      UID     : constant Actions.Action_Id'Class
                  := Actions.Object'Class (Self).UID;
      Db_File : constant GPR2.Path_Name.Object :=
                  Self.Tree.Db_Filename_Path (UID);
   begin
      Self.Signature := Load (Db_File, Messages);

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
                  Messages.Append
                    (Message.Create
                      (Message.Information,
                       "not up-to-date",
                       Artifact.SLOC));
               end if;
            else
               Self.Signature.Set_Valid_State (False);
               Messages.Append
                 (Message.Create
                    (Message.Information,
                     "does not exist ",
                     Artifact.SLOC));
            end if;
         end;
      end loop;

   end Compare_Signature;

end GPR2.Build.Actions;
