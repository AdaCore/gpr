--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

package body GPR2.Build.Signature is

   ------------------
   -- Add_Artifact --
   ------------------

   procedure Add_Artifact
     (Self : in out Object;
      Art  : Artifacts.Object'Class)
   is
   begin
      Self.Artifacts.Include (Art, Art.Checksum);
   end Add_Artifact;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Object) is
   begin
      Self := (others => <>);
   end Clear;

   ----------
   -- Load --
   ----------

   function Load (Db_File : Path_Name.Object) return Object
   is
      File        : File_Type;
      JSON_Result : Read_Result;
      Signature   : Object;

      procedure Extract
        (Key   : UTF8_String;
         Value : JSON_Value);

      -------------
      -- Extract --
      -------------

      procedure Extract
        (Key   : UTF8_String;
         Value : JSON_Value)
      is
         A      : constant Artifacts.Object'Class :=
                    Artifacts.Unserialize (Key);
         Digest : constant UTF8_String := Get (Value);
      begin
         Signature.Artifacts.Insert (A, Hash_Digest (Digest));
      end Extract;
   begin
      Signature.Clear;

      Text_IO.Open
        (File => File,
         Mode => In_File,
         Name => String (Db_File.Value));

      begin
         JSON_Result := Read (Get_Line (File));
         Close (File);
      exception
         when End_Error =>
            Close (File);
      end;

      if not JSON_Result.Success then
         return Signature;
      end if;

      if Has_Field (JSON_Result.Value, TEXT_SIGNATURE)
        and then (Kind (Get (Val   => JSON_Result.Value,
                             Field => TEXT_SIGNATURE))
                  = JSON_Object_Type)
      then
         declare
            List : constant JSON_Value :=
                     Get (JSON_Result.Value, TEXT_SIGNATURE);
         begin
            Map_JSON_Object (List, Extract'Access);
         end;
      end if;

      return Signature;

   exception
      when others =>
         Signature.Clear;
         return Signature;
   end Load;

   -----------
   -- Store --
   -----------

   procedure Store (Self : in out Object; Db_File : Path_Name.Object) is
      File : File_Type;
      JSON : constant JSON_Value := Create_Object;
      List : constant JSON_Value := Create_Object;

      procedure Create_Artifact_Element (Position : Artifact_Maps.Cursor);

      procedure Create_Artifact_Element (Position : Artifact_Maps.Cursor) is
         Key : constant Artifacts.Object'Class :=
                 Artifact_Maps.Key (Position);
      begin
         Set_Field (Val        => List,
                    Field_Name => Artifacts.Serialize (Key),
                    Field      => String (Key.Checksum));
      end Create_Artifact_Element;
   begin
      Self.Artifacts.Iterate (Create_Artifact_Element'Access);

      Set_Field (Val        => JSON,
                 Field_Name => TEXT_SIGNATURE,
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

   -----------
   -- Valid --
   -----------

   function Valid (Self : Object) return Boolean is
   begin
      if Self.Artifacts.Is_Empty then
         return False;
      end if;

      for C in Self.Artifacts.Iterate loop
         declare
            Art    : constant Artifacts.Object'Class :=
                       Artifact_Maps.Key (C);
            Digest : constant Hash_Digest :=
                       Artifact_Maps.Element (C);
         begin
            if Art.Checksum /= Digest then
               return False;
            end if;
         end;
      end loop;

      return True;
   end Valid;

end GPR2.Build.Signature;
