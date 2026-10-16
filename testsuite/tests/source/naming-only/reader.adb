with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Wide_Wide_Text_IO;

package body Reader is
   overriding procedure Read
     (Self        : Object;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out GPR2.File_Readers.Decoded_File_Contents;
      Diagnostics : in out GPR2.Log.Object)
   is
      pragma Unreferenced (Self, Charset, Read_BOM, Diagnostics);
      use Ada.Strings.Wide_Wide_Unbounded;
      use Ada.Wide_Wide_Text_IO;
      File : File_Type;
      Text : Unbounded_Wide_Wide_String;
   begin
      if Filename'Length >= 4
        and then Filename (Filename'Last - 3 .. Filename'Last) in
                   ".ads" | ".adb"
      then
         Source_Reads := Source_Reads + 1;
      end if;
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Append (Text, Get_Line (File));
         Append (Text, Wide_Wide_Character'Val (10));
      end loop;
      Close (File);
      Contents :=
        (Buffer => new Wide_Wide_String'(To_Wide_Wide_String (Text)),
         First  => 1,
         Last   => Length (Text));
   end Read;
end Reader;
