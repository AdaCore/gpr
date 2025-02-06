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

   function Create (Path : GPR2.Path_Name.Object;
                    View : GPR2.Project.View.Object) return Object with Inline;
   function Create (Path : Filename_Type;
                    View : GPR2.Project.View.Object) return Object with Inline;

   overriding function Serialize (Self : Object) return String;

   overriding procedure Unserialize
     (Val  : out Object;
      Repr : String;
      Chk  : String;
      Ctxt : GPR2.Project.View.Object);

   function Path (Self : Object) return GPR2.Path_Name.Object;

   overriding function Checksum (Self : Object) return String;

   overriding function Hash (Self : Object) return Ada.Containers.Hash_Type;

private

   use type GPR2.Path_Name.Object;

   type Object is new Artifacts.Object with record
      Path : Path_Name.Object;
      Ctxt : GPR2.Project.View.Object;
   end record;

   overriding function View (Self : Object) return GPR2.Project.View.Object is
     (Self.Ctxt);

   overriding function Protocol (Self : Object) return String is
     ("file");

   overriding function "<" (L, R : Object) return Boolean is
      (L.Path.Value < R.Path.Value);

   Undefined : constant Object := (others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Create (Path : GPR2.Path_Name.Object;
                    View : GPR2.Project.View.Object) return Object
   is (Path => Path,
       Ctxt => View);

   function Create (Path : Filename_Type;
                    View : GPR2.Project.View.Object) return Object
   is (Path => Path_Name.Create_File (Path),
       Ctxt => View);

   overriding function Checksum (Self : Object) return String
   is (Utils.Hash.Hash_File (Self.Path.Value));

   function Path (Self : Object) return GPR2.Path_Name.Object is
     (Self.Path);

   overriding function Serialize (Self : Object) return String is
     (Self.Path.String_Value);

   overriding function Hash (Self : Object) return Ada.Containers.Hash_Type is
     (Hash (Self.Path.Value));

end GPR2.Build.Artifacts.Files;
