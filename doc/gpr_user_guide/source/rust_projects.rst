.. index:: Rust, Cargo

.. _Rust_Projects:

*************
Rust projects
*************

GPR can build Rust code by delegating the work to Cargo. A project whose
language is ``Rust`` indicates where the Cargo manifest lives and what the
crate produces. GPRbuild runs ``cargo`` at the right point of the build. What
Cargo produces is then treated like any other artifact of the tree.

An Ada executable can therefore link a Rust library, and a Rust crate can
link a library built from a GPR project. One ``gprbuild`` invocation covers
the whole tree.

GPR never compiles Rust itself. It runs Cargo and records what Cargo
produced, so that the rest of the tree can depend on it.


.. index:: Cargo package, Cargo.Root

Declaring a Rust project
========================

Everything in this section applies to any Rust project, whether it produces a
library or executables.

A Rust project declares ``Rust`` as its language and names the directory that
holds ``Cargo.toml`` through the ``Root`` attribute of the ``Cargo`` package:

.. code-block:: gpr

   project Hello_From_Rust is
      for Languages   use ("Rust");
      for Source_Dirs use ("rust/src");

      package Cargo is
         for Root use Project'Project_Dir & "rust";
      end Cargo;
   end Hello_From_Rust;

``Cargo.Root``
  The directory containing the ``Cargo.toml`` of this project. A relative
  value is interpreted from the project's own directory. Defaults to the
  project directory. It may name a member of a Cargo workspace, in which case
  everything below applies to that member.

``Source_Dirs``
  The Rust sources, as for any other language. Cargo decides what to compile.
  GPR needs the sources to resolve the ``Main`` attribute, and to report the
  project's contents to tools such as GPRls.

The ``.rs`` suffix is the default body suffix for Rust, so a ``Naming``
package is only needed if your sources use another one.

.. warning::

   A project that declares ``Rust`` cannot declare another language. Cargo
   drives the whole build of the crate, so it cannot produce the artifacts of
   a second language. Mixing them is rejected when the tree is loaded.

   Mixed-language systems are built by giving each language its own project
   and importing one from the other, which is the subject of
   :ref:`Mixing_Ada_And_Rust` below.

   The ``Cargo`` package is also not allowed in an aggregate or aggregate
   library project. Aggregated Rust projects are supported; the aggregate
   itself simply cannot carry the Cargo settings.


.. index:: Rust target triple, Cargo.Rust_Target

Target triples
--------------

Cargo is always given an explicit target triple. GPR derives it from the
canonical GPR target of the tree:

.. list-table::
   :header-rows: 1
   :widths: 30 45

   * - Canonical GPR target
     - Rust triples
   * - ``aarch64-elf``
     - ``aarch64-unknown-none``
   * - ``aarch64-linux``
     - ``aarch64-unknown-linux-gnu``
   * - ``aarch64-qnx``
     - ``aarch64-unknown-nto-qnx800``
   * - ``aarch64-vx7r2``
     - | ``aarch64-wrs-vxworks-rtp`` (default)
       | ``aarch64-wrs-vxworks-dkm``
   * - ``arm-elf``
     - ``armv7r-none-eabihf``
   * - ``x86_64-linux``
     - ``x86_64-unknown-linux-gnu``
   * - ``x86_64-windows``
     - ``x86_64-pc-windows-gnu``

Without ``Cargo.Rust_Target``, GPR uses the row's default. Set the attribute
to pick another triple from the same row, for instance a VxWorks DKM instead
of the default RTP:

.. code-block:: gpr

   package Cargo is
      for Root        use Project'Project_Dir & "rust";
      for Rust_Target use "x86_64-wrs-vxworks-dkm";
   end Cargo;

.. warning::

   The table is built into GPR and cannot be extended.
   ``Cargo.Rust_Target`` may only name a triple from the row of the current
   GPR target. A GPR target the table does not list cannot build Rust at all.

   The table will eventually be moved to the knowledge base.


.. index:: Cargo.Profile

Build profile
-------------

``Cargo.Profile`` selects the Cargo profile, either ``"release"`` (the
default) or ``"dev"``:

.. code-block:: gpr

   package Cargo is
      for Root    use Project'Project_Dir & "rust";
      for Profile use "dev";
   end Cargo;

.. note::

   The naming is Cargo's own. The ``dev`` profile writes to a ``debug``
   directory, so a ``dev`` build lands in
   ``<cargo target directory>/<triple>/debug``.


.. index:: Rust; artifacts

Where the artifacts are
-----------------------

Cargo owns its output layout, and GPR does not move what it produces. Both
libraries and executables stay in the target directory Cargo reports, and GPR
looks for them wherever that is. It is normally ``target`` beside the
manifest, and the workspace root's for a workspace member:

.. code-block:: text

   <cargo target directory>/<triple>/<release|debug>/

Cargo also decides for itself what needs recompiling. GPRbuild therefore runs
``cargo build`` on every invocation, and lets Cargo report that everything is
up to date.

.. note::

   GPR attempts no up-to-date check of its own for a Rust project, so the cost
   of deciding that nothing changed is Cargo's. GPRbuild does track two
   things: the manifest, and the libraries linked into the crate. A change to
   either makes the crate stale for the rest of the tree. An executable
   linking it is relinked, even if no Rust source changed.


.. index:: Rust library project

Rust libraries
==============

A Rust crate becomes a library project when its manifest declares a
``staticlib`` or a ``cdylib`` crate type. Those are the two Cargo produces
that are linkable from other languages.

GPRbuild checks the project file against the manifest, and rejects the library
project if any of the following does not hold:

* The crate declares a library target, of type ``staticlib`` or ``cdylib``.
  No other type will do.

* It declares only one of the two. A manifest declaring both is rejected
  rather than disambiguated by ``Library_Kind``.

* ``Library_Name`` is the name of the library crate, spelled the way Rust
  spells it. Cargo normalizes dashes to underscores, so a crate named
  ``hello-from-rust`` is declared as ``for Library_Name use
  "hello_from_rust";``.

* ``Library_Kind`` matches the crate type: ``static``, the default, for
  ``staticlib``, and ``dynamic`` or ``relocatable`` for ``cdylib``.

* ``Library_Version`` is not set. It is not supported, so a ``cdylib`` gets no
  versioned soname from GPR.

A complete example
------------------

A shared Rust library:

.. code-block:: none

   greeter/
   ├── greeter.gpr
   └── rust/
       ├── Cargo.toml
       └── src/
           └── lib.rs

``greeter.gpr``, where ``Library_Kind`` is ``dynamic`` because the crate is a
``cdylib``:

.. code-block:: gpr

   library project Greeter is
      for Languages    use ("Rust");
      for Library_Name use "greeter";
      for Library_Kind use "dynamic";
      for Library_Dir  use "lib";
      for Source_Dirs  use ("rust/src");

      package Cargo is
         for Root use Project'Project_Dir & "rust";
      end Cargo;
   end Greeter;

``rust/Cargo.toml``, whose crate name matches ``Library_Name``:

.. code-block:: toml

   [package]
   name = "greeter"
   version = "0.1.0"
   edition = "2024"

   [lib]
   crate-type = ["cdylib"]

``rust/src/lib.rs``, exporting a C-callable symbol:

.. code-block:: rust

   #[unsafe(no_mangle)]
   pub extern "C" fn greet() {
       println!("Hello from Rust!");
   }

Building the project runs Cargo and leaves the shared library where Cargo put
it, under the name GPR derives from ``Library_Name``:

.. code-block:: shell

   $ gprbuild -p -P greeter.gpr
   $ ls rust/target/x86_64-unknown-linux-gnu/release/libgreeter.so
   rust/target/x86_64-unknown-linux-gnu/release/libgreeter.so

.. note::

   ``Library_Dir`` is still required of a library project, as the tree will
   not load without it, but nothing is written there. The ``lib/`` of this
   example stays empty: the library stays where Cargo put it, and that is the
   file GPR hands to the linker. Importing projects are unaffected, since they
   never name the path themselves.

On its own a library project only produces that file. Linking it into an
executable is the subject of :ref:`Mixing_Ada_And_Rust` below.


.. index:: Rust executable

Rust executables
================

A standard (non-library) Rust project produces the binaries its manifest
declares. With no ``Main`` attribute, every binary of the package is built.
``Main`` restricts that to the binaries built from the named sources:

.. code-block:: gpr

   project Codec is
      for Languages   use ("Rust");
      for Source_Dirs use ("rust/src");
      for Main        use ("encode.rs", "decode.rs");

      package Cargo is
         for Root use Project'Project_Dir & "rust";
      end Cargo;
   end Codec;

matching a manifest that declares:

.. code-block:: toml

   [[bin]]
   name = "encode"
   path = "src/encode.rs"

   [[bin]]
   name = "decode"
   path = "src/decode.rs"

Each value names the Rust *source* of a ``[[bin]]`` target, not the binary
name. That source must be found in ``Source_Dirs`` for GPR to resolve it.
Cargo is then asked for the matching binaries only.

.. note::

   It is an error to name a source that no binary is built from, and an
   error for the manifest to declare no binary at all.

Mains given on the ``gprbuild`` command line work the same way. As for the
other languages, they restrict the build to the projects that own them. A Rust
project that owns none of the requested mains is left alone, rather than
building every binary of its manifest.


A complete example
------------------

A standalone Rust executable, built entirely by Cargo but driven by GPRbuild:

.. code-block:: none

   hello_rust/
   ├── hello_from_rust.gpr
   └── rust/
       ├── Cargo.toml
       └── src/
           └── main.rs

``hello_from_rust.gpr``:

.. code-block:: gpr

   project Hello_From_Rust is
      for Languages   use ("Rust");
      for Source_Dirs use ("rust/src");

      package Cargo is
         for Root use Project'Project_Dir & "rust";
      end Cargo;
   end Hello_From_Rust;

``rust/Cargo.toml``:

.. code-block:: toml

   [package]
   name = "hello_from_rust"
   version = "0.1.0"
   edition = "2024"

``rust/src/main.rs``:

.. code-block:: rust

   fn main() {
       println!("hello from Rust!");
   }

The manifest declares no ``[[bin]]``, so Cargo builds one binary named after
the package, from ``src/main.rs``:

.. code-block:: shell

   $ gprbuild -P hello_from_rust.gpr
   $ ./rust/target/x86_64-unknown-linux-gnu/release/hello_from_rust
   hello from Rust!

.. note::

   The project declares no ``Exec_Dir``, and declaring one would change
   nothing. Cargo decides where the binary goes, and GPR does not move it.

.. index:: Rust; mixing with Ada

.. _Mixing_Ada_And_Rust:

Mixing Ada and Rust
===================

A Rust project cannot hold Ada sources, so the two languages meet through
project imports. Both directions work, and each is described below.


Using a Rust library from Ada
-----------------------------

Importing a Rust library project from an Ada one is all it takes to link
against it:

.. code-block:: gpr

   with "greeter.gpr";

   project Main is
      for Languages use ("Ada");
      for Main      use ("main.adb");
   end Main;

``gprbuild -P main.gpr`` then builds the Rust library through Cargo and links
it into the Ada executable. A Rust static library needs extra system libraries
at the final link, and GPR adds them for you. They also reach the link when
the Rust project is imported indirectly, through another library.

A complete example, an Ada executable calling into a Rust static library:

.. code-block:: none

   ada_calls_rust/
   ├── hello_from_ada.gpr
   ├── hello_from_rust.gpr
   ├── src/
   │   └── hello_from_ada.adb
   └── rust/
       ├── Cargo.toml
       └── src/
           └── lib.rs

``hello_from_rust.gpr``, the library project:

.. code-block:: gpr

   library project Hello_From_Rust is
      for Languages    use ("Rust");
      for Library_Name use "hello_from_rust";
      for Library_Dir  use "lib";
      for Source_Dirs  use ("rust/src");

      package Cargo is
         for Root use Project'Project_Dir & "rust";
      end Cargo;
   end Hello_From_Rust;

``rust/Cargo.toml``, whose crate name matches ``Library_Name`` and whose crate
type matches the default ``Library_Kind`` of ``static``:

.. code-block:: toml

   [package]
   name = "hello_from_rust"
   version = "0.1.0"
   edition = "2024"

   [lib]
   crate-type = ["staticlib"]

``rust/src/lib.rs``, exporting a C-callable symbol:

.. code-block:: rust

   #[unsafe(no_mangle)]
   pub extern "C" fn hello_from_rust() {
       println!("hello from Rust!");
   }

``hello_from_ada.gpr``, which only has to import the library project:

.. code-block:: gpr

   with "hello_from_rust";

   project Hello_From_Ada is
      for Languages   use ("Ada");
      for Source_Dirs use ("src");
      for Object_Dir  use "obj";
      for Exec_Dir    use ".";
      for Main        use ("hello_from_ada.adb");
   end Hello_From_Ada;

``src/hello_from_ada.adb``, importing the symbol with C convention:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Hello_From_Ada is
      procedure Hello_From_Rust with Import, Convention => C;
   begin
      Put_Line ("Hello from Ada!");
      Hello_From_Rust;
   end Hello_From_Ada;

One invocation builds both (``-p`` creates the missing ``obj/`` and ``lib/``
directories):

.. code-block:: shell

   $ gprbuild -p -P hello_from_ada.gpr
   $ ./hello_from_ada
   Hello from Ada!
   hello from Rust!

Nothing in ``hello_from_ada.gpr`` mentions Rust or Cargo: the link switches
the Rust static library needs are added by GPR.


Using an Ada library from Rust
------------------------------

A Rust project may import library projects written in other languages. GPR
tells Cargo where each library is and what to link against. For a shared
library, it also indicates where to find it at run time. Whatever linker
options the imported library recorded for its own link are passed on too.

.. code-block:: gpr

   with "math_lib.gpr";

   project Main is
      for Languages   use ("Rust");
      for Source_Dirs use ("rust/src");

      package Cargo is
         for Root use Project'Project_Dir & "rust";
      end Cargo;
   end Main;

.. note::

   An imported Ada library must be encapsulated
   (``for Library_Standalone use "encapsulated";``) so that it carries the
   elaboration code and the Ada runtime with it. One that is not is
   rejected.

A complete example, a Rust executable calling into an encapsulated Ada
library:

.. code-block:: none

   rust_calls_ada/
   ├── main.gpr
   ├── math_lib.gpr
   ├── src_mathlib/
   │   ├── math_lib.ads
   │   └── math_lib.adb
   └── rust/
       ├── Cargo.toml
       └── src/
           └── main.rs

``math_lib.gpr``. ``Library_Auto_Init`` runs the elaboration when the library
is loaded, so the Rust side has nothing to call first:

.. code-block:: gpr

   project Math_Lib is
      for Languages         use ("Ada");
      for Source_Dirs       use ("src_mathlib");
      for Object_Dir        use "obj";
      for Library_Name      use "mathlib";
      for Library_Dir       use "lib";
      for Library_Kind      use "dynamic";
      for Library_Interface use ("Math_Lib");
      for Library_Standalone use "encapsulated";
      for Library_Auto_Init use "true";
   end Math_Lib;

``src_mathlib/math_lib.ads``, exporting a C-callable subprogram:

.. code-block:: ada

   with Interfaces.C; use Interfaces.C;

   package Math_Lib is
      function Add (A, B : int) return int;
      pragma Export (C, Add, "ada_add");
   end Math_Lib;

``src_mathlib/math_lib.adb``:

.. code-block:: ada

   package body Math_Lib is
      function Add (A, B : int) return int is
      begin
         return A + B;
      end Add;
   end Math_Lib;

``main.gpr``, which imports the Ada library and is otherwise an ordinary Rust
project:

.. code-block:: gpr

   with "math_lib";

   project Main is
      for Languages   use ("Rust");
      for Source_Dirs use ("rust/src");

      package Cargo is
         for Root use Project'Project_Dir & "rust";
      end Cargo;
   end Main;

``rust/Cargo.toml``:

.. code-block:: toml

   [package]
   name = "main_from_rust"
   version = "0.1.0"
   edition = "2024"

``rust/src/main.rs``, declaring the Ada subprogram as an external C function:

.. code-block:: rust

   use std::os::raw::c_int;

   unsafe extern "C" {
       fn ada_add(a: c_int, b: c_int) -> c_int;
   }

   fn main() {
       let result = unsafe { ada_add(10, 32) };
       println!("Calling encapsulated Ada library from Rust!");
       println!("10 + 32 = {}", result);
   }

Building ``main.gpr`` builds the Ada library first, then hands it to Cargo:

.. code-block:: shell

   $ gprbuild -p -P main.gpr
   $ ./rust/target/x86_64-unknown-linux-gnu/release/main_from_rust
   Calling encapsulated Ada library from Rust!
   10 + 32 = 42

Nothing in ``Cargo.toml`` mentions the Ada library. GPR tells Cargo where to
find it and what to link, and the executable finds it again at run time.

.. index:: Rust; linking Rust libraries

Using a Rust library from Rust
==============================

A Rust project may import another Rust library project. Each keeps its own
manifest and its own target directory, and GPR builds them in order.

The two crates are linked through the C ABI, not as Cargo dependencies. The
library exports C-callable symbols and the executable declares them as
external C functions. A crate that should be an ordinary Cargo dependency
belongs in ``Cargo.toml``, not in a separate GPR project.

A complete example, a Rust executable calling into a Rust static library:

.. code-block:: none

   rust_calls_rust/
   ├── main.gpr
   ├── rust_lib.gpr
   ├── rustlib/
   │   ├── Cargo.toml
   │   └── src/
   │       └── lib.rs
   └── rust/
       ├── Cargo.toml
       └── src/
           └── main.rs

``rust_lib.gpr``, the library project:

.. code-block:: gpr

   library project Rust_Lib is
      for Languages    use ("Rust");
      for Library_Name use "rust_lib";
      for Library_Dir  use "lib";
      for Source_Dirs  use ("rustlib/src");

      package Cargo is
         for Root use Project'Project_Dir & "rustlib";
      end Cargo;
   end Rust_Lib;

``rustlib/Cargo.toml``:

.. code-block:: toml

   [package]
   name = "rust_lib"
   version = "0.1.0"
   edition = "2024"

   [lib]
   crate-type = ["staticlib"]

``rustlib/src/lib.rs``, exporting a C-callable symbol:

.. code-block:: rust

   use std::os::raw::c_int;

   #[unsafe(no_mangle)]
   pub extern "C" fn rust_lib_double(x: c_int) -> c_int {
       x * 2
   }

``main.gpr``, which imports the library project:

.. code-block:: gpr

   with "rust_lib";

   project Main is
      for Languages   use ("Rust");
      for Source_Dirs use ("rust/src");

      package Cargo is
         for Root use Project'Project_Dir & "rust";
      end Cargo;
   end Main;

``rust/Cargo.toml``, which indicates nothing about the library:

.. code-block:: toml

   [package]
   name = "main_from_rust"
   version = "0.1.0"
   edition = "2024"

``rust/src/main.rs``, declaring the symbol as an external C function:

.. code-block:: rust

   use std::os::raw::c_int;

   unsafe extern "C" {
       fn rust_lib_double(x: c_int) -> c_int;
   }

   fn main() {
       let result = unsafe { rust_lib_double(21) };
       println!("Calling a Rust library from a Rust executable!");
       println!("21 * 2 = {}", result);
   }

Building ``main.gpr`` builds the library first, then the executable:

.. code-block:: shell

   $ gprbuild -p -P main.gpr
   $ ./rust/target/x86_64-unknown-linux-gnu/release/main_from_rust
   Calling a Rust library from a Rust executable!
   21 * 2 = 42

.. index:: cargo driver, Compiler.Driver

Selecting a custom cargo
========================

A GNAT installation provides a suitable ``cargo``, which GPR finds on its own.
A project normally has nothing to declare.

To use a different one, name it explicitly:

.. code-block:: gpr

   package Compiler is
      for Driver ("Rust") use "cargo";
   end Compiler;


.. index:: gprclean; Rust

Cleaning
========

``gprclean`` removes the artifacts Cargo built for the project, but leaves the
Cargo target directory itself in place. The ``--remove-cargo-build-dir``
switch removes that directory too. Use it with caution: other projects may
have built into the same directory, and they go with it.


.. index:: gprinstall; Rust

Installing
==========

.. warning::

   ``gprinstall`` has no support for Rust projects: the artifacts Cargo
   produces are not installed.
