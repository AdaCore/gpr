--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Message;
with GPR2.Source_Reference;

package body GPR2.Build.Signature is

   -----------------------
   -- Artifact_Checksum --
   -----------------------

   function Artifact_Checksum
     (Self : Object; Id : B3_Hash_Digest) return Utils.Hash.Hash_Digest
   is
      Checksum : Utils.Hash.Hash_Digest := (others => 'X');
   begin
      if Self.Artifacts.Contains (Id) then
         Checksum := Self.Artifacts.Element (Id).Checksum;
      end if;

      return Checksum;
   end Artifact_Checksum;

   ----------
   -- Load --
   ----------

   function Load
     (Db_File  : Path_Name.Object;
      Messages : in out GPR2.Log.Object) return Object
   is
      File        : File_Type;
      JSON_Result : Read_Result;
      Signature   : Object;
   begin
      if Exists (String (Db_File.Value)) then
         Text_IO.Open
           (File => File,
            Mode => In_File,
            Name => String (Db_File.Value));

         begin
            JSON_Result := Read (Get_Line (File));
            Close (File);

            if JSON_Result.Success then
               if Has_Field (JSON_Result.Value, TEXT_ARTIFACTS)
                 and then
                   (Kind (Get (Val => JSON_Result.Value,
                               Field => TEXT_ARTIFACTS))
                    = JSON_Array_Type)
               then
                  declare
                     List : constant JSON_Array :=
                              Get (JSON_Result.Value, TEXT_ARTIFACTS);
                  begin
                     for Index in Array_First (List) .. Length (List) loop
                        declare
                           Element  : constant JSON_Value  :=
                                        Array_Element (List, Index);
                        begin
                           if (Has_Field (Element, TEXT_ID)
                               and then
                                 (Kind (Get (Val => Element, Field => TEXT_ID))
                                  = JSON_String_Type))
                             and then
                               (Has_Field (Element, TEXT_PLAIN_ID)
                                and then
                                  (Kind (Get (Val => Element,
                                              Field => TEXT_PLAIN_ID))
                                   = JSON_String_Type))
                             and then
                               (Has_Field (Element, TEXT_CHECKSUM)
                                and then
                                  (Kind (Get (Val => Element,
                                              Field => TEXT_CHECKSUM))
                                   = JSON_String_Type))
                           then
                              declare
                                 Id       : constant String :=
                                              Get (Element, TEXT_ID);
                                 Id_Txt   : constant String  :=
                                              Get (Element, TEXT_PLAIN_ID);
                                 Chck_Txt : constant String  :=
                                              Get (Element, TEXT_CHECKSUM);
                                 Item     : Artifact_Signature;

                                 Position : Artifact_Maps.Cursor;
                                 Inserted : Boolean := False;
                              begin
                                 if Chck_Txt'Length = Item.Checksum'Length then
                                    Item.Checksum :=
                                      Utils.Hash.Hash_Digest (Chck_Txt);
                                 end if;

                                 Item.Path_Length := Id_Txt'Length;
                                 Item.Path
                                   (Item.Path'First ..
                                      Item.Path'First + Item.Path_Length - 1)
                                     := Id_Txt;

                                 if Id'Length = B3_Hash_Digest'Length then
                                    Signature.Artifacts.Insert
                                      (Key       => B3_Hash_Digest (Id),
                                       New_Item  => Item,
                                       Position  => Position,
                                       Inserted  => Inserted);
                                 end if;

                                 if not Inserted then
                                    Signature.Coherent := False;
                                    Messages.Append
                                      (Message.Create
                                         (Message.Warning,
                                          "duplicated artifact or invalid "
                                          & "artifact id",
                                          Source_Reference.Create
                                            (Db_File.Value, 0, 0)));

                                    exit;
                                 end if;
                              end;
                           else
                              Signature.Coherent := False;
                              Messages.Append
                                (Message.Create
                                   (Message.Warning,
                                    "missing mandatory artifact field in"
                                    & " artifacts list",
                                    Source_Reference.Create
                                      (Db_File.Value, 0, 0)));

                              exit;
                           end if;
                        end;
                     end loop;
                  end;
               else
                  Signature.Coherent := False;
                  Messages.Append
                    (Message.Create
                       (Message.Warning,
                        "missing mandatory artifact list field",
                        Source_Reference.Create
                          (Db_File.Value, 0, 0)));
               end if;
            else
               Signature.Coherent := False;
               Messages.Append
                 (Message.Create
                    (Message.Warning,
                     To_String (JSON_Result.Error.Message),
                     Source_Reference.Create
                       (Db_File.Value,
                        JSON_Result.Error.Line,
                        JSON_Result.Error.Column)));
            end if;
         exception
            when End_Error =>
               Signature.Coherent := False;
               Messages.Append
                 (Message.Create
                    (Message.Warning,
                     "cannot be loaded",
                     Source_Reference.Create
                       (Db_File.Value, 0, 0)));
               Close (File);
         end;
      else
         Signature.Coherent := False;
         Messages.Append
           (Message.Create
              (Message.Warning,
               "file does not exist",
               Source_Reference.Create
                 (Db_File.Value, 0, 0)));
      end if;

      return Signature;
   end Load;

   ---------------------
   -- Set_Valid_State --
   ---------------------

   procedure Set_Valid_State (Self : in out Object; Valid : Boolean) is
   begin
      Self.Valid := Valid;
   end Set_Valid_State;

   -----------
   -- Store --
   -----------

   procedure Store (Self : Object; Db_File : Path_Name.Object) is
      File : File_Type;
      JSON : constant JSON_Value := Create_Object;
      List : JSON_Array;

      procedure Create_Artifact_Element (Position : Artifact_Maps.Cursor);

      procedure Create_Artifact_Element (Position : Artifact_Maps.Cursor) is
         Artifact : constant JSON_Value := Create_Object;
         Item     : constant Artifact_Signature :=
                      Artifact_Maps.Element (Position);
      begin
         Set_Field (Val        => Artifact,
                    Field_Name => TEXT_ID,
                    Field      => String (Artifact_Maps.Key (Position)));
         Set_Field (Val        => Artifact,
                    Field_Name => TEXT_PLAIN_ID,
                    Field      => Item.Path
                      (Item.Path'First ..
                         Item.Path'First + Item.Path_Length - 1));
         Set_Field (Val        => Artifact,
                    Field_Name => TEXT_CHECKSUM,
                    Field      => String (Item.Checksum));
         Append (Arr => List, Val => Artifact);
      end Create_Artifact_Element;
   begin
      Self.Artifacts.Iterate (Create_Artifact_Element'Access);

      Set_Field (Val        => JSON,
                 Field_Name => TEXT_ARTIFACTS,
                 Field      => List);

      if Exists (String (Db_File.Value)) then
         Open (File, Out_File, String (Db_File.Value));
         Put_Line (File, Write (JSON) & ASCII.CR & ASCII.LF);
         Close (File);
      elsif Exists (String (Db_File.Containing_Directory.Value)) then
         Create (File, Out_File, String (Db_File.Value));
         Put_Line (File, Write (JSON) & ASCII.CR & ASCII.LF);
         Close (File);
      end if;
   end Store;

   ---------------------
   -- Update_Artifact --
   ---------------------

   procedure Update_Artifact
     (Self         : in out Object;
      Id           : B3_Hash_Digest;
      Plain_Id     : String;
      Checksum     : Utils.Hash.Hash_Digest)
   is
      Item : Artifact_Signature;
   begin
      Item.Path_Length := Plain_Id'Length;
      Item.Path
        (Item.Path'First ..
           Item.Path'First + Item.Path_Length - 1)
          := Plain_Id;
      Item.Checksum := Checksum;

      if Self.Artifacts.Contains (Id) then
         Self.Artifacts.Replace (Id, Item);
      else
         Self.Artifacts.Insert (Id, Item);
      end if;
   end Update_Artifact;

end GPR2.Build.Signature;