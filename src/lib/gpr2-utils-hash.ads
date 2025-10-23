--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.File_Indexes;

with GPR2.Path_Name;

package GPR2.Utils.Hash is

   type Object is tagged private;

   subtype Hash_Digest is GNATCOLL.File_Indexes.File_Index_Digest;
   No_Digest : constant Hash_Digest;

   function Hash_File
     (Self         : in out Object;
      Path         : Filename_Type;
      Mark_Trusted : Boolean) return Hash_Digest;
   --  Return the hash digest for the file located at Path.
   --  If Mark_Trusted is set, and the hash is actually performed, then
   --  the result is marked as trusted in the file index, and won't
   --  recomputed unless the file attributes change.
   --  A file index is not trusted when its modification time is less than
   --  a second in the past: due to some filesystem limitation the timestamp
   --  is less than a second, so the file may be modified between two calls
   --  without its attributes being changed.

   function Hash_Content (Cnt : String) return Hash_Digest;

   function Load (Path : Path_Name.Object) return Object;

   procedure Save (Self : Object;
                   Path : Path_Name.Object);

private

   type Object is tagged record
      File_Index : GNATCOLL.File_Indexes.File_Index;
   end record;

   No_Digest : constant Hash_Digest := (others => ' ');

   function Hash_File
     (Self         : in out Object;
      Path         : Filename_Type;
      Mark_Trusted : Boolean) return Hash_Digest
   is (GNATCOLL.File_Indexes.Hash
       (Self.File_Index, String (Path), Trust_Cache => Mark_Trusted));

end GPR2.Utils.Hash;
