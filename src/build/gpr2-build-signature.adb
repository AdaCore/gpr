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
      Art  : Artifacts.Object'Class) is
   begin
      Self.Artifacts.Include (Art, Art.Checksum);
   end Add_Artifact;

   ----------------
   -- Add_Output --
   ----------------

   procedure Add_Output
     (Self   : in out Object;
      Stdout : UB.Unbounded_String;
      Stderr : UB.Unbounded_String) is
   begin
      Self.Stdout := Stdout;
      Self.Stderr := Stderr;
   end Add_Output;

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

      declare
         List : constant JSON_Array :=
                  Get (JSON_Result.Value, TEXT_SIGNATURE);
      begin
         for Obj of List loop
            declare
               Art_Uri : constant UTF8_String := Get (Obj, TEXT_URI);
               Art_Chk : constant UTF8_String := Get (Obj, TEXT_CHECKSUM);
            begin
               Signature.Artifacts.Include
                 (Artifacts.From_Uri (Art_Uri), Hash_Digest (Art_Chk));
            end;
         end loop;
      end;

      Signature.Stdout := Get (JSON_Result.Value, TEXT_STDOUT);
      Signature.Stderr := Get (JSON_Result.Value, TEXT_STDERR);

      JSON_Result.Value.Finalize;

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
      List : JSON_Array;

      procedure Create_Artifact_Element (Position : Artifact_Maps.Cursor);

      procedure Create_Artifact_Element (Position : Artifact_Maps.Cursor) is
         Art : constant Artifacts.Object'Class :=
                 Artifact_Maps.Key (Position);
         Val : constant JSON_Value := Create_Object;
      begin
         Set_Field (Val => Val,
                    Field_Name => TEXT_URI,
                    Field      => Artifacts.To_Uri (Art));
         Set_Field (Val => Val,
                    Field_Name => TEXT_CHECKSUM,
                    Field      => String (Art.Checksum));
         Append (List, Val);
      end Create_Artifact_Element;
   begin
      Self.Artifacts.Iterate (Create_Artifact_Element'Access);

      Set_Field (Val        => JSON,
                 Field_Name => TEXT_SIGNATURE,
                 Field      => List);
      Set_Field (Val        => JSON,
                 Field_Name => TEXT_STDOUT,
                 Field      => Self.Stdout);
      Set_Field (Val        => JSON,
                 Field_Name => TEXT_STDERR,
                 Field      => Self.Stderr);

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
