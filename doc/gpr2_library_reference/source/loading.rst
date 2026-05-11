.. index:: project loading, GPR2.Project.Tree, Tree.Load

*******************
Loading a Project
*******************

Loading a project tree is the first step in using the GPR2 library. It parses
the root ``.gpr`` file, resolves all imports, extensions, and aggregations,
and optionally locates or generates the configuration project (``.cgpr``) that
describes the available compilers for each language.

The result is a ``GPR2.Project.Tree.Object`` that gives access to every project
view in the tree.


.. index:: GPR2.Options, GPR2.Reporter

Using GPR2.Options
==================

``GPR2.Options.Object`` is the standard way to configure a load. It mirrors
the switches accepted by all standard GPR tools (``-P``, ``-X``, ``--target``,
``--RTS``, ``--subdirs``, etc.) and passes them to ``Tree.Load`` as a single
object.

.. code-block:: ada

   with GPR2.Options;
   with GPR2.Project.Tree;

   Tree    : GPR2.Project.Tree.Object;
   Options : GPR2.Options.Object;

   Options.Add_Switch (GPR2.Options.P, "myproject.gpr");
   Options.Add_Switch (GPR2.Options.X, "BUILD=release");
   Options.Add_Switch (GPR2.Options.Target, "aarch64-elf");
   Options.Add_Switch (GPR2.Options.RTS, "ravenscar-sfp", Index => "Ada");

   if not Tree.Load (Options) then
      --  Errors have already been reported via the Reporter
      return;
   end if;

The ``Index`` parameter of ``Add_Switch`` holds the language qualifier for
switches such as ``--RTS:<lang>``.

``Tree.Load`` returns ``False`` if loading failed. All diagnostic messages are
forwarded to the reporter (a console reporter by default) as they are emitted,
so there is no need to iterate the log on failure.

Common switches:

``P``
  Project file path (``-P``)

``X``
  Scenario variable assignment (``-X name=value``)

``Target``
  Cross-compilation target (``--target=``)

``RTS``
  Runtime selection; use ``Index`` for the language (``--RTS[:lang]=``)

``Autoconf``
  Config file to generate if absent (``--autoconf=``)

``Config``
  Explicit config file to use (``--config=``)

``Subdirs``
  Suffix appended to obj/lib/exec directories (``--subdirs=``)

``Src_Subdirs``
  Extra source subdirectory prepended to each project (``--src-subdirs=``)

``Relocate_Build_Tree``
  Root directory for out-of-tree builds (``--relocate-build-tree``)

``Root_Dir``
  Base path used to compute relative relocations (``--root-dir=``)

``AP``
  Extra project search path directory (``-aP``)

``Implicit_With``
  Project added as an implicit dependency of all projects

``Db``
  Additional knowledge base directory (``--db``)

``Db_Minus``
  Skip the default knowledge base (``--db-``)


Integrating with GNATCOLL.Opt_Parse
====================================

For tools that use ``GNATCOLL.Opt_Parse`` for command-line parsing, the
``GPR2.Options.Opt_Parse`` package provides a generic package that registers
all standard GPR switches into an existing ``Argument_Parser`` and returns
a ready-to-use ``Options.Object``:

.. code-block:: ada

   with GNATCOLL.Opt_Parse;  use GNATCOLL.Opt_Parse;
   with GPR2.Options.Opt_Parse;

   Parser : Argument_Parser :=
     Create_Argument_Parser (Help => "My GPR tool");

   package GPR_Args is new GPR2.Options.Opt_Parse.Args (Parser => Parser);

   --  ... declare additional tool-specific arguments ...

   if not Parser.Parse then
      return;
   end if;

   declare
      Options : constant GPR2.Options.Object :=
                  GPR_Args.Parsed_GPR2_Options;
   begin
      if not Tree.Load (Options) then
         return;
      end if;
   end;


Load parameters
===============

``Tree.Load`` accepts several optional parameters beyond ``Options``:

``Reporter``
  Reporter used for all diagnostics during and after load. Defaults to a
  console reporter. Pass a custom reporter here or call
  ``Tree.Set_Reporter`` beforehand.

``With_Runtime``
  Whether runtime sources are included when populating source information.

``Artifacts_Info_Level``
  Source information to compute at load time. ``No_Source`` (default)
  skips source enumeration; ``Sources_Only`` resolves source lists;
  ``Sources_Units`` also parses unit information.

``Config``
  An explicit ``Configuration.Object``. When provided, ``--config`` and
  ``--autoconf`` options in ``Options`` are ignored.

``Environment``
  Environment variable set to use. Defaults to the process environment.

``Absent_Dir_Error``
  Whether a missing obj/lib/exec directory is a warning or an error.

``Create_Missing_Dirs``
  Whether to create missing obj/lib/exec directories automatically.

``Allow_Implicit_Project``
  When no project is specified and exactly one ``.gpr`` file exists in
  the current directory, load it implicitly.

``Check_Shared_Libs_Import``
  Report an error if a shared library project imports a static library.
