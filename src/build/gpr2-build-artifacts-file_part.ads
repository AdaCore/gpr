--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Path_Name;
with GPR2.Build.Artifacts.Files;

package GPR2.Build.Artifacts.File_Part is

   type Object is new Files.Object with private;

   Undefined : constant Object;

   overriding function Create (Path : GPR2.Path_Name.Object) return Object;
   overriding function Create (Path : Filename_Type) return Object;

   function Create (Path  : GPR2.Path_Name.Object;
                    Index : GPR2.Unit_Index) return Object;

   overriding function Image (Self : Object) return String;

   function Index (Self : Object) return Unit_Index;

   overriding function "<" (L, R : Object) return Boolean;

   overriding function Hash (Self : Object) return Ada.Containers.Hash_Type;

   overriding procedure Unserialize
     (S   : String;
      Val : out Object);

private

   use type GPR2.Path_Name.Object;

   type Object is new Files.Object with record
      Index : Unit_Index := No_Index;
   end record;

   overriding function Protocol (Self : Object) return String is
     ("indexed_file");

   overriding function "<" (L, R : Object) return Boolean is
     (if L.Path = R.Path
      then L.Index < R.Index
      else L.Path < R.Path);

   Undefined : constant Object := (Files.Undefined with Index => No_Index);

   function Suffix (Index : Unit_Index) return String is
     (if Index = No_Index then "" else '@' & Index'Image);

   overriding function Create (Path : GPR2.Path_Name.Object) return Object is
     (Files.Create (Path) with Index => No_Index);

   overriding function Create (Path : Filename_Type) return Object is
     (Files.Create (Path) with Index => No_Index);

   function Create (Path  : GPR2.Path_Name.Object;
                    Index : GPR2.Unit_Index) return Object is
     (Files.Create (Path) with Index => Index);

   overriding function Hash (Self : Object) return Ada.Containers.Hash_Type is
     (Hash (Self.Path.Value & Filename_Optional (Suffix (Self.Index))));

   overriding function Image (Self : Object) return String is
     (Self.Path.String_Value & Suffix (Self.Index));

   function Index (Self : Object) return Unit_Index is
      (Self.Index);

end GPR2.Build.Artifacts.File_Part;
