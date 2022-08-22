--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded;

with GPR2.Message;
with Gpr_Parser_Support.Diagnostics;

with Gpr_Parser_Support.Text;
with Gpr_Parser_Support.Slocs;

package body GPR2.File_Readers is

   type Reader is new
     Gpr_Parser_Support.File_Readers.File_Reader_Interface with
      record
         File_Reader : File_Reader_Reference;
      end record;
   --  Adapter used to convert File_Reader_Reference to
   --  Gpr_Parser_Support.File_Readers package's File_Reader_Reference.

   overriding procedure Read
     (Self        : Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Gpr_Parser_Support.File_Readers.Decoded_File_Contents;
      Diagnostics : in out Gpr_Parser_Support.Diagnostics.
        Diagnostics_Vectors.Vector);

   overriding procedure Release (Self : in out Reader) is null;

   -------------
   -- Convert --
   -------------

   function Convert
     (File_Reader : File_Reader_Reference)
      return Gpr_Parser_Support.File_Readers.File_Reader_Reference is
      use GPR2.File_Readers.File_Reader_References;
   begin
      if File_Reader = No_File_Reader_Reference then
         return Gpr_Parser_Support.File_Readers.No_File_Reader_Reference;
      else
         declare
            Gpr_Parser_Reader : constant Reader :=
              (File_Reader => File_Reader);
         begin
            return Gpr_Parser_Support.File_Readers.Create_File_Reader_Reference
              (Gpr_Parser_Reader);
         end;
      end if;
   end Convert;

   ----------------------------------
   -- Create_File_Reader_Reference --
   ----------------------------------

   function Create_File_Reader_Reference
     (File_Reader : File_Reader_Interface'Class) return File_Reader_Reference
   is
   begin
      return Result : File_Reader_Reference do
         Result.Set (File_Reader);
      end return;
   end Create_File_Reader_Reference;

   ----------------
   -- Do_Release --
   ----------------

   procedure Do_Release (Self : in out File_Reader_Interface'Class) is
   begin
      Self.Release;
   end Do_Release;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self        : Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Gpr_Parser_Support.File_Readers.Decoded_File_Contents;
      Diagnostics : in out Gpr_Parser_Support.Diagnostics.
        Diagnostics_Vectors.Vector) is
      C : Decoded_File_Contents;
      D : GPR2.Log.Object;
   begin

      Read
        (Self        => Self.File_Reader.Get,
         Filename    => Filename,
         Charset     => Charset,
         Read_BOM    => Read_BOM,
         Contents    => C,
         Diagnostics => D);

      --  Fill Contents

      Contents.Buffer := Gpr_Parser_Support.Text.Text_Access (C.Buffer);
      Contents.First := C.First;
      Contents.Last := C.Last;

      --  Fill Diagnostics

      for E of D loop
         declare
            Diagnostic : Gpr_Parser_Support.Diagnostics.Diagnostic;
         begin
            Diagnostic.Sloc_Range.Start_Line :=
              Gpr_Parser_Support.Slocs.Line_Number (E.Sloc.Line);
            Diagnostic.Sloc_Range.End_Line :=
              Diagnostic.Sloc_Range.Start_Line;
            Diagnostic.Sloc_Range.Start_Column :=
              Gpr_Parser_Support.Slocs.Column_Number (E.Sloc.Column);
            Diagnostic.Sloc_Range.End_Column :=
              Diagnostic.Sloc_Range.Start_Column;
            Diagnostic.Message :=
              Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String
                (Ada.Characters.Conversions.To_Wide_Wide_String (E.Message));
            Diagnostics.Append (Diagnostic);
         end;
      end loop;

   end Read;

end GPR2.File_Readers;
