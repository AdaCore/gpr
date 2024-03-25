--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Path_Name;

package GPR2.Build.Artifacts.Files is

   type Object is new Artifacts.Object with private;

   function Create (Path : GPR2.Path_Name.Object) return Object with Inline;

   overriding function Image (Self : Object) return String;

   overriding function SLOC
     (Self : Object) return Source_Reference.Object'Class;

   overriding function Checksum
     (Self : Object) return Utils.Hash.Hash_Digest;

   function Path (Self : Object) return GPR2.Path_Name.Object;

   overriding function "<" (L, R : Object) return Boolean;

   overriding function Hash (Self : Object) return Ada.Containers.Hash_Type;

   overriding function UID (Self : Object) return B3_Hash_Digest;

private

   type Object is new Artifacts.Object with record
      UID  : B3_Hash_Digest;
      Path : GPR2.Path_Name.Object;
   end record;

   overriding function Image (Self : Object) return String is
     (Self.Path.String_Value);

   overriding function SLOC
     (Self : Object) return Source_Reference.Object'Class is
      (Source_Reference.Create (Self.Path.Value, 0, 0));

   function Path (Self : Object) return GPR2.Path_Name.Object is
     (Self.Path);

   overriding function Hash (Self : Object) return Ada.Containers.Hash_Type is
     (Hash (Self.Path.Value));

   overriding function "<" (L, R : Object) return Boolean is
     (L.Path.Value < R.Path.Value);

   overriding function UID (Self : Object) return B3_Hash_Digest is
      (Self.UID);

end GPR2.Build.Artifacts.Files;
