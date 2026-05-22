with Ada.Containers.Indefinite_Ordered_Maps;

with GPR2.File_Readers;
with GPR2.Log;
with GPR2.Path_Name; use GPR2.Path_Name;

package My_File_Reader is

   package Filename_Content_Map_Package is
     new Ada.Containers.Indefinite_Ordered_Maps
       (GPR2.Path_Name.Object, Wide_Wide_String);

   type Reader is new GPR2.File_Readers.File_Reader_Interface with private;

   overriding procedure Read
     (Self        : Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out GPR2.File_Readers.Decoded_File_Contents;
      Diagnostics : in out GPR2.Log.Object);

   overriding procedure Release (Self : in out Reader) is null;

   procedure Add (Filename : String; Content : Wide_Wide_String);

   function Reference return GPR2.File_Readers.File_Reader_Reference;

private

   type Reader is new GPR2.File_Readers.File_Reader_Interface with
      record
         Map : Filename_Content_Map_Package.Map;
      end record;

end My_File_Reader;