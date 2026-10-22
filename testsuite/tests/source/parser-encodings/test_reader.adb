with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GPR2.Message;
with GPR2.Source_Reference;

package body Test_Reader is

   ------------
   -- Create --
   ------------

   function Create (Error : Boolean := False)
                    return GPR2.File_Readers.File_Reader_Reference is
   begin
      return GPR2.File_Readers.Create_File_Reader_Reference
        (Reader'(Report_Error => Error));
   end Create;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self        : Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out GPR2.File_Readers.Decoded_File_Contents;
      Diagnostics : in out GPR2.Log.Object)
   is
      use Ada.Streams, Ada.Streams.Stream_IO;
      pragma Unreferenced (Charset, Read_BOM);
   begin
      if Self.Report_Error then
         Diagnostics.Append
           (GPR2.Message.Create
              (GPR2.Message.Critical_Error, "reader error",
               GPR2.Source_Reference.Undefined));
         return;
      end if;

      declare
         Size  : constant Natural := Natural (Ada.Directories.Size (Filename));
         File  : File_Type;
         Raw   : Stream_Element_Array (1 .. Stream_Element_Offset (Size));
         Last  : Stream_Element_Offset;
         Bytes : String (1 .. Size);
      begin
         Open (File, In_File, Filename);
         Read (File, Raw, Last);
         Close (File);
         for J in 1 .. Last loop
            Bytes (Natural (J)) := Character'Val (Raw (J));
         end loop;
         declare
            Decoded : constant Wide_Wide_String :=
              Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode
                (Bytes (1 .. Natural (Last)));
         begin
            Contents.Buffer :=
              new Wide_Wide_String'(5 .. Decoded'Length + 12 => '!');
            Contents.First := 7;
            Contents.Last := 6 + Decoded'Length;
            Contents.Buffer (Contents.First .. Contents.Last) := Decoded;
         end;
      end;
   end Read;
end Test_Reader;
