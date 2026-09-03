--
--  Copyright (C) 2024-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.Files;
with GPR2.Path_Name;

package GPR2.Build.Artifacts.Library is

   type Object is new GPR2.Build.Artifacts.Files.Object with private;

   function Create_Static (Path : GPR2.Path_Name.Object) return Object;
   --  Create a static library artifact

   function Create_Shared
     (Path      : GPR2.Path_Name.Object;
      Link_Name : Value_Type) return Object
   with Pre => Link_Name /= "";
   --  Create a shared library artifact. -L and -l options are obtained from
   --  Path and Link_Name.

   overriding function Create
     (Path : GPR2.Path_Name.Object) return Object;

   overriding function Create
     (Path : Filename_Type) return Object;
   --  Create a static library artifact with no extra link options, so the
   --  same as Create_Static left at its default.

   overriding function Protocol (Self : Object) return String is
     ("library");

   function Is_Static (Self : Object) return Boolean;

   function Link_Name (Self : Object) return Value_Type;
   --  The name to give the linker's -l switch to link against this library:
   --  the file name stripped of its prefix, of its extension and of any
   --  version suffix.
   --
   --  Carried by the artifact rather than recomputed from the project file by
   --  whoever links, because the two do not always agree: a Cargo library is
   --  named after its crate and lives in Cargo's target directory, not under
   --  Library_Name in Library_Dir. Empty for static libraries, which are
   --  passed to the linker by path and need no such name.

   Undefined : constant Object;

private

   type Object is new GPR2.Build.Artifacts.Files.Object with record
      Lib_Static    : Boolean := False;
      Lib_Link_Name : Unbounded_String;
   end record;

   --  A library artifact is identified by its path only: the extra
   --  information carried by the record describes the library, it does not
   --  discriminate it. Two artifacts denoting the same file must compare
   --  equal whatever the Is_Static and Link_Name values they were created
   --  with, so equality is explicitly the one of the parent type.
   --
   --  Ada implicitly declares "=" for every type, including record
   --  extensions, and that declaration overrides the inherited one: for an
   --  extension it composes the parent's "=" with the equality of the
   --  components added by the extension part. Without this override,
   --  Lib_Static and Lib_Link_Name would thus silently take part in the
   --  comparison. Hash and "<" are plain primitives, so they are
   --  inherited from Files unchanged and stay path based: this operator must
   --  remain consistent with them.

   overriding function "=" (L, R : Object) return Boolean
   is (Files."=" (Files.Object (L), Files.Object (R)));

   function Create_Static (Path : GPR2.Path_Name.Object) return Object
   is (Files.Create (Path) with Lib_Static => True, Lib_Link_Name => <>);

   overriding function Create
     (Path : GPR2.Path_Name.Object) return Object
   is (Files.Create (Path) with Lib_Static => True,
       Lib_Link_Name => <>);

   overriding function Create
     (Path : Filename_Type) return Object
   is (Files.Create (Path) with Lib_Static => True,
       Lib_Link_Name => <>);

   function Create_Shared
     (Path      : GPR2.Path_Name.Object;
      Link_Name : Value_Type) return Object
   is (Files.Create (Path) with Lib_Static => False,
       Lib_Link_Name => To_Unbounded_String (String (Link_Name)));

   function Is_Static (Self : Object) return Boolean
   is (Self.Lib_Static);

   function Link_Name (Self : Object) return Value_Type
   is (Value_Type (To_String (Self.Lib_Link_Name)));

   Undefined : constant Object :=
     (Files.Undefined with Lib_Static => False,
      Lib_Link_Name => <>);

end GPR2.Build.Artifacts.Library;
