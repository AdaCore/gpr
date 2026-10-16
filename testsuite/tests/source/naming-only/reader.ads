with GPR2.File_Readers;
with GPR2.Log;

package Reader is
   Source_Reads : Natural := 0;

   type Object is new GPR2.File_Readers.File_Reader_Interface with null record;

   overriding procedure Read
     (Self        : Object;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out GPR2.File_Readers.Decoded_File_Contents;
      Diagnostics : in out GPR2.Log.Object);

   overriding procedure Release (Self : in out Object) is null;
end Reader;
