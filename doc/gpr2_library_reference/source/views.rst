.. index:: GPR2.Project.View, project view

*****
Views
*****

A **view** (``GPR2.Project.View.Object``) represents one project in the
loaded tree. It is the entry point for reading attributes, enumerating
sources, and navigating the project graph.


Project kinds
=============

Every view has a ``Kind`` (``GPR2.Project_Kind``):

``K_Standard``
  Regular project that owns source files and produces an executable
  or object files.

``K_Library``
  Produces a static or shared library.

``K_Abstract``
  No sources, no artifacts; used purely to share attribute definitions.

``K_Aggregate``
  Groups other projects for a single build invocation. Must be the
  root of the tree; cannot be used as an import inside another project.

``K_Aggregate_Library``
  A library built from the object files of its aggregated projects.
  Unlike ``K_Aggregate`` it can appear anywhere in the project graph.

``K_Configuration``
  Describes available compilers and tool settings. Loaded automatically
  by ``Tree.Load``; not written by application code.

The ``Qualifier`` accessor returns the kind as declared in the ``.gpr``
file; ``Kind`` resolves ``K_Standard`` vs ``K_Library`` when the qualifier
was omitted.


Accessing views
===============

After a successful ``Tree.Load``, views are accessed via the tree.

**By namespace root**

The most general entry point is ``Tree.Namespace_Root_Projects``, which
returns a ``View.Set.Object``:

* For a plain (non-aggregate) tree it contains the single root project.
* For a ``K_Aggregate`` tree it contains all directly aggregated
  sub-project roots.

.. code-block:: ada

   for Root of Tree.Namespace_Root_Projects loop
      --  process each namespace root
   end loop;

**Root project**

``Tree.Root_Project`` returns the top-level project that was passed to
``-P``. For a non-aggregate tree this is also the only namespace root. For
aggregate trees the root is the aggregate project itself; the aggregated
sub-projects are reached via ``Tree.Namespace_Root_Projects`` or
``Root.Aggregated``. Use ``Tree.Root_Project`` when you specifically need
the project that was explicitly loaded.


Iterating the tree
==================

``Tree.Iterate`` (or the Ada ``for ... of Tree`` loop via the
``Default_Iterator`` aspect) visits views in the tree:

.. code-block:: ada

   for View of Tree loop          -- uses Default_Iterator
      Ada.Text_IO.Put_Line (String (View.Name));
   end loop;

The iterator is controlled by three parameters:

``Kind : Iterator_Control``
   Which relationship edges to follow. The predefined constants are:

   * ``Default_Iterator`` - follows imported and extended projects,
     skips runtime and configuration projects.
   * ``Full_Iterator`` - follows all edges including runtime and
     configuration.

   The individual flags are ``I_Imported``, ``I_Extended``,
   ``I_Aggregated``, ``I_Recursive``, and ``I_Runtime``.

``Filter : Filter_Control``
   Which project kinds to yield. ``Default_Filter`` yields all kinds.
   ``Library_Filter`` yields only ``K_Library`` and
   ``K_Aggregate_Library`` projects.

``Status : Status_Control``
   Whether to include or exclude externally-built projects.
   ``Default_Status`` includes them.

Example - iterate only over library projects:

.. code-block:: ada

   for View of Tree.Iterate
     (Kind   => GPR2.Project.Default_Iterator,
      Filter => GPR2.Project.Library_Filter)
   loop
      Ada.Text_IO.Put_Line (String (View.Name));
   end loop;


Navigating from a view
======================

From any view you can reach related views:

``View.Imports``
  The set of projects directly imported by ``with`` clauses.

``View.Closure``
  The transitive closure of imported projects. Optional flags control
  whether extended and aggregated projects are included.

``View.Extended``
  The set of projects extended by this view (non-empty only when the
  view uses ``extends``).

``View.Extended_Root``
  The root project of the extension chain.

``View.Extending``
  The view that extends this one (if any).

``View.Aggregated``
  The set of projects aggregated by this view. Only valid when
  ``View.Kind in Aggregate_Kind``.


View properties
===============

``View.Name``
  Project name as declared in the ``.gpr`` file (``Name_Type``).

``View.Path_Name``
  Absolute path to the ``.gpr`` file (``GPR2.Path_Name.Object``).

``View.Dir_Name``
  Directory containing the ``.gpr`` file.

``View.Kind``
  Resolved project kind.

``View.Object_Directory``
  Resolved ``Object_Dir`` attribute path. Valid for standard, library,
  and aggregate-library projects.

``View.Source_Directories``
  Set of resolved source directory paths.

``View.Library_Directory``
  Resolved ``Library_Dir`` attribute. Valid for library projects.

``View.Executable_Directory``
  Resolved ``Exec_Dir`` attribute. Valid for standard and library
  projects.


Aggregate projects and namespace roots
======================================

For a non-aggregate tree there is exactly one namespace root, which is the
root project itself. For an aggregate tree every directly aggregated
sub-project is a namespace root.

Ada requires all compilation-unit names to be unique within a namespace.
GNAT derives source filenames from unit names by default, so this
uniqueness requirement translates to unique filenames within a namespace.
When iterating sources or resolving units, always start from a namespace
root rather than from an arbitrary view to work within one consistent
namespace.

``View.Is_Namespace_Root`` indicates whether a view is a namespace root.
Only namespace-root views may be passed to ``View.Units`` and
``View.Unit``.


.. index:: attribute access

Commonly used view API
======================

Attributes
----------

Attributes are the primary way to read project configuration. Each
attribute is identified by its name (a constant from
``GPR2.Project.Registry.Attribute``) and an optional index.

.. code-block:: ada

   --  Check whether an attribute is present before reading it
   if View.Has_Attribute (PRA.Compiler.Default_Switches) then
      Attr : constant GPR2.Project.Attribute.Object :=
               View.Attribute (PRA.Compiler.Default_Switches);
      Put_Line (Attr.Value.Text);
   end if;

Many attributes are indexed. Use ``View.Attribute`` with an
``Attribute_Index`` to look up a specific entry:

.. code-block:: ada

   --  Look up Compiler.Switches for a specific source file.
   --  The index is the concrete filename; the project may define it with a
   --  pattern such as Compiler.Switches ("autogen-*") - the library matches
   --  the concrete name against all defined patterns.
   Attr := View.Attribute
     (PRA.Compiler.Switches,
      GPR2.Project.Attribute_Index.Create ("autogen-blah.adb"));

   --  Or look up the switches for the Ada language:
   Attr := View.Attribute
     (PRA.Compiler.Switches,
      GPR2.Project.Attribute_Index.Create (Ada_Language));

To enumerate all entries of an indexed attribute:

.. code-block:: ada

   for Attr of View.Attributes (PRA.Compiler.Switches) loop
      Put_Line (Attr.Index.Text & " => " & Attr.Value.Text);
   end loop;

Sources
-------

``View.Sources`` returns all sources owned by the view (a
``Build.Source`` set). ``View.Visible_Sources`` resolves visibility at
the namespace level: when two views in the same namespace own a source
with the same filename, ``Visible_Sources`` returns only the one that
takes precedence. For namespace-root traversals prefer
``View.Visible_Sources``.

.. code-block:: ada

   for Src of View.Visible_Sources loop
      Put_Line (String (Src.Path_Name.Simple_Name));
   end loop;

Units
-----

``View.Units`` returns all Ada compilation units visible within the
namespace rooted at this view. It is only valid on a namespace root.
Each ``GPR2.Build.Compilation_Unit.Object`` groups the spec, body, and
any separates for one logical unit.

.. code-block:: ada

   --  View must satisfy View.Is_Namespace_Root
   for CU of View.Units loop
      Put_Line (String (CU.Name));
   end loop;
