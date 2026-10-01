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
     (Self        : in out Object;
      Path        : Filename_Type;
      Force_Cache : Boolean) return Hash_Digest;
   --  Return the hash digest for the file located at Path.
   --  If Force_Cache is set, forces the hash re-computation in the file index.
   --  This allow to ensure an artifact is always updated in the file index
   --  (since it has been re-generated) independently of if the stat of the
   --  artifact file is the same of the stored one and whether we trust what is
   --  inside the cache or not.

   procedure Start_Generation (Self : in out Object);
   --  Open a new file-index generation: each file is then stat'ed once, and
   --  later queries for it reuse that result.

   function Generations (Self : Object) return Natural;
   --  Number of generations opened so far. The scheduler opens one per
   --  executed action, so that a file modified by an action is stat'ed
   --  again for the next one.

   function Hash_Content (Cnt : String) return Hash_Digest;

   function Load (Path : Path_Name.Object) return Object;

   procedure Save (Self : Object;
                   Path : Path_Name.Object);

private

   type Object is tagged record
      File_Index : GNATCOLL.File_Indexes.File_Index;
      Gen_Count  : Natural := 0;
   end record;

   function Generations (Self : Object) return Natural is (Self.Gen_Count);

   No_Digest : constant Hash_Digest := (others => ' ');

   function Hash_File
     (Self        : in out Object;
      Path        : Filename_Type;
      Force_Cache : Boolean) return Hash_Digest
   is (GNATCOLL.File_Indexes.Hash
       (Self.File_Index, String (Path),
        Force_Cache         => Force_Cache,
        Path_Is_Normalized  => True));
   --  Path_Name.Value is already normalized.

end GPR2.Utils.Hash;
