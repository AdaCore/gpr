with GPR2.File_Readers;
with GPR2.Log;

package Test_Reader is
   type Failure_Kind is
     (Use_Failure, Device_Failure, Program_Failure, Bad_Encoding);

   function Create (Failure : Failure_Kind)
                    return GPR2.File_Readers.File_Reader_Reference;
private
   type Reader is new GPR2.File_Readers.File_Reader_Interface with record
      Failure : Failure_Kind;
   end record;

   overriding procedure Read
     (Self        : Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out GPR2.File_Readers.Decoded_File_Contents;
      Diagnostics : in out GPR2.Log.Object);

   overriding procedure Release (Self : in out Reader) is null;
end Test_Reader;
