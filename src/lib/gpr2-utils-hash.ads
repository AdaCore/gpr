--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.File_Indexes;

with GPR2.Path_Name;

package GPR2.Utils.Hash is

   type Object is tagged private;

   --  function Load (Root_View : GPR2.Project.View.Object) return Object;
   --  procedure Save (Self : Object);

   subtype Hash_Digest is GNATCOLL.File_Indexes.File_Index_Digest;
   No_Digest : constant Hash_Digest;

   function Hash_File
      (Self : in out Object;
       Path : Filename_Type) return Hash_Digest;

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
     (Self : in out Object; Path : Filename_Type) return Hash_Digest
   is (GNATCOLL.File_Indexes.Hash
       (Self.File_Index, String (Path), Trust_Cache => True));

end GPR2.Utils.Hash;
