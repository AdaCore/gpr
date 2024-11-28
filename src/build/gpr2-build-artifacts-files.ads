--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Path_Name;
with GPR2.Utils.Hash;


package GPR2.Build.Artifacts.Files is

   type Object is new Artifacts.Object with private;

   Undefined : constant Object;

   overriding function Is_Defined (Self : Object) return Boolean;

   function Create (Path : GPR2.Path_Name.Object) return Object with Inline;
   function Create (Path : Filename_Type) return Object with Inline;

   overriding procedure Unserialize
     (S   : String;
      Val : out Object);

   overriding function Image (Self : Object) return String;

   function Path (Self : Object) return GPR2.Path_Name.Object;

   overriding function SLOC
     (Self : Object) return Source_Reference.Object'Class;

   overriding function Checksum
     (Self : Object) return Utils.Hash.Hash_Digest;

   overriding function Hash (Self : Object) return Ada.Containers.Hash_Type;

private

   use type GPR2.Path_Name.Object;

   type Object is new Artifacts.Object with record
      Path : Path_Name.Object;
   end record;

   overriding function Protocol (Self : Object) return String is
     ("file");

   overriding function "<" (L, R : Object) return Boolean is
      (L.Path < R.Path);

   Undefined : constant Object := (Path => Path_Name.Undefined);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Create (Path : GPR2.Path_Name.Object) return Object is
     (Path => Path);

   function Create (Path : Filename_Type) return Object is
     (Path => Path_Name.Create_File (Path));

   overriding function Checksum
     (Self : Object) return Utils.Hash.Hash_Digest
   is (Utils.Hash.Hash (Self.Path.Value));

   function Path (Self : Object) return GPR2.Path_Name.Object is
     (Self.Path);

   overriding function Image (Self : Object) return String is
     (Self.Path.String_Value);

   overriding function SLOC
     (Self : Object) return Source_Reference.Object'Class is
      (Source_Reference.Create (Self.Path.Value, 0, 0));

   overriding function Hash (Self : Object) return Ada.Containers.Hash_Type is
     (Hash (Self.Path.Value));

end GPR2.Build.Artifacts.Files;
