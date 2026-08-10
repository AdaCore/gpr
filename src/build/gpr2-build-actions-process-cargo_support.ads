--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  Shared helpers for Cargo/Rust support: the mapping from GPR target to Rust
--  target triple, cargo driver resolution, and the extra link options a given
--  triple requires.

package GPR2.Build.Actions.Process.Cargo_Support is

   type Cargo_Lib_Kind is
     (Static_Library, Shared_Library, Unsupported, Ambiguous);
   --  The kind of library a Cargo target produces, as far as GPR2 is
   --  concerned.
   --
   --  Cargo names things differently from GPR: what GPR calls a static
   --  library Cargo calls the `staticlib` crate type, and what GPR calls a
   --  shared library Cargo calls `cdylib`. Those two are the only ones that
   --  produce a system library linkable from non-Rust code.
   --
   --  @enum Static_Library A `staticlib`, linked by path
   --  @enum Shared_Library A `cdylib`, linked by `-L` and `-l`
   --  @enum Unsupported Anything else. `rlib`, the default when a crate
   --     declares no crate-type, is a format private to rustc, and `dylib` is
   --     a shared library exposing Rust's own unstable ABI, meant for
   --     Rust-to-Rust linking
   --  @enum Ambiguous Both a static and a shared library at once, which a GPR
   --     view has no room for

   function To_Cargo_Lib_Kind
     (Cargo_Lib_Types : GPR2.Containers.Value_List) return Cargo_Lib_Kind;
   --  Classify the crate-type list of a Cargo library target.
   --
   --  Crate types GPR2 cannot link, an `rlib` beside a `cdylib` for instance,
   --  do not make the list ambiguous: they are simply not GPR's business.
   --
   --  @param Cargo_Lib_Types The crate types the library target declares
   --  @return Unsupported when the list holds none GPR2 can link against,
   --     Ambiguous when it holds both, and otherwise the one it holds

   function Image (Kind : Cargo_Lib_Kind) return String;
   --  @param Kind The kind to name
   --  @return The Cargo crate type Kind stands for, for use in messages

   function Library_File_Name
     (View : GPR2.Project.View.Object;
      Kind : Cargo_Lib_Kind) return Simple_Name
   with Pre => View.Is_Defined
               and then View.Is_Library
               and then Kind in Static_Library | Shared_Library;
   --  The name of the file Cargo produces for View's library target.
   --
   --  @param View The library view Cargo builds for
   --  @param Kind The kind of library it produces
   --  @return The simple name of the file

   function Root_Directory
     (View : GPR2.Project.View.Object) return GPR2.Path_Name.Object;
   --  The directory `Cargo.Root` names for View.
   --
   --  @param View The Rust view
   --  @return The directory, always absolute

   function Manifest
     (View : GPR2.Project.View.Object) return GPR2.Path_Name.Object;
   --  @param View The Rust view
   --  @return The Cargo.toml of View's root directory

   function Driver (View : GPR2.Project.View.Object) return String;
   --  @param View The Rust view to build
   --  @return The cargo driver to build View with: the configured Rust
   --     compiler if any, otherwise the `Compiler.Driver ("Rust")` attribute,
   --     and the empty string when no driver is defined

   function Extra_Link_Options
     (Triple : String) return GPR2.Containers.Value_List;
   --  @param Triple The Rust target triple the library was built for
   --  @return The extra linker options a static library built for Triple
   --     requires, `-pthread` or the MinGW import libraries for instance.
   --     Empty when Triple is unknown or needs nothing special

   function Is_Compatible
     (GPR_Target : Name_Type; Triple : String) return Boolean;
   --  @param GPR_Target The GPR target the tree is built for
   --  @param Triple The Rust triple to check against it
   --  @return Whether Triple is one of the Rust triples known for GPR_Target

   function Rust_Triple (View : GPR2.Project.View.Object) return String;
   --  @param View The Rust view to build
   --  @return The Rust target triple to build View for: the
   --     `Cargo.Rust_Target` attribute if defined, otherwise the default
   --     triple mapped from the GPR target, and the empty string when that
   --     target has no known Rust mapping

end GPR2.Build.Actions.Process.Cargo_Support;
