with Ada.IO_Exceptions;

package body Test_Reader is

   ------------
   -- Create --
   ------------

   function Create (Failure : Failure_Kind)
                    return GPR2.File_Readers.File_Reader_Reference is
   begin
      return GPR2.File_Readers.Create_File_Reader_Reference
        (Reader'(Failure => Failure));
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
      pragma Unreferenced (Filename, Charset, Read_BOM, Diagnostics);
   begin
      Contents :=
        (Buffer => new Wide_Wide_String'
           (1 => Wide_Wide_Character'Val (16#110000#)),
         First => 1,
         Last  => 1);

      case Self.Failure is
         when Use_Failure =>
            raise Ada.IO_Exceptions.Use_Error;
         when Device_Failure =>
            raise Ada.IO_Exceptions.Device_Error;
         when Program_Failure =>
            raise Program_Error;
         when Bad_Encoding =>
            null;
      end case;
   end Read;
end Test_Reader;
