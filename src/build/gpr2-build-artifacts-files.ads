--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Path_Name;

package GPR2.Build.Artifacts.Files is

   type Object is new Artifacts.Object with private;

   Undefined : constant Object;

   overriding function Is_Defined (Self : Object) return Boolean;

   function Create (Path : GPR2.Path_Name.Object) return Object with Inline;
   function Create (Path : Filename_Type) return Object with Inline;

   overriding function Serialize (Self : Object) return String;

   overriding procedure Unserialize
     (Val  : out Object;
      Repr : String;
      Chk  : String;
      Ctxt : GPR2.Project.View.Object);

   function Path (Self : Object) return GPR2.Path_Name.Object;

   overriding
   function Checksum
     (Self : Object; Hash : in out Utils.Hash.Object) return String;

   function Checksum
     (Self        : Object;
      Hash        : in out Utils.Hash.Object;
      Force_Cache : Boolean) return Utils.Hash.Hash_Digest;

   overriding function Hash (Self : Object) return Ada.Containers.Hash_Type;

   overriding function Image (Self : Object) return String;

private

   use type GPR2.Path_Name.Object;

   type Object is new Artifacts.Object with record
      Path : Path_Name.Object;
   end record;

   overriding function Protocol (Self : Object) return String is
     ("file");

   overriding function "=" (L, R : Object) return Boolean is
      (L.Path = R.Path);

   overriding function "<" (L, R : Object) return Boolean is
      (L.Path < R.Path);

   Undefined : constant Object := (others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Create (Path : GPR2.Path_Name.Object) return Object
   is (Path => Path);

   function Create (Path : Filename_Type) return Object
   is (Path => Path_Name.Create_File (Path));

   overriding
   function Checksum
     (Self : Object; Hash : in out Utils.Hash.Object) return String
   is (Hash.Hash_File (Self.Path.Value, False));

   function Checksum
     (Self        : Object;
      Hash        : in out Utils.Hash.Object;
      Force_Cache : Boolean) return Utils.Hash.Hash_Digest
   is (Hash.Hash_File (Self.Path.Value, Force_Cache));

   function Path (Self : Object) return GPR2.Path_Name.Object is
     (Self.Path);

   overriding function Serialize (Self : Object) return String is
     (Self.Path.String_Value);

   overriding function Hash (Self : Object) return Ada.Containers.Hash_Type is
     (Self.Path.Hash);

   overriding function Image (Self : Object) return String is
      (String (Self.Path.Simple_Name));

end GPR2.Build.Artifacts.Files;
