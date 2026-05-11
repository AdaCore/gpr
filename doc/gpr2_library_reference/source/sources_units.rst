.. index:: source file, compilation unit, GPR2.Build

*****************
Sources and Units
*****************

Source and unit information is not populated during ``Tree.Load`` by default.
It must be explicitly requested, either at load time via the
``Artifacts_Info_Level`` parameter or afterwards by calling
``Tree.Update_Sources``.


Populating source information
==============================

The ``Source_Info_Option`` type controls how much work is done when
enumerating sources:

``Sources_Only``
  Source files are enumerated and unit names are inferred from
  filenames according to the naming scheme. Unit information may be
  inaccurate for Ada (ambiguity between body and separate, krunched
  filenames).

``Sources_Units``
  Unit information is resolved accurately by parsing ALI files (when
  available) or source content. This is the default for
  ``Update_Sources``.

``Sources_Units_Artifacts``
  As above, and also loads dependency data from ALI files.

**At load time** — pass ``Artifacts_Info_Level`` to ``Tree.Load``:

.. code-block:: ada

   Options.Add_Switch (GPR2.Options.P, "myproject.gpr");
   if not Tree.Load (Options,
                     Artifacts_Info_Level => GPR2.Sources_Units)
   then
      return;
   end if;

**After load** — call ``Tree.Update_Sources`` explicitly:

.. code-block:: ada

   if not Tree.Update_Sources (Option => GPR2.Sources_Units) then
      --  errors were reported to the reporter
      return;
   end if;

``Update_Sources`` can be called multiple times: to upgrade the level of
information (e.g. from ``Sources_Only`` to ``Sources_Units``), or to
accommodate filesystem changes — it performs a delta update of the source
base, adding newly appearing files and removing files that no longer exist,
without re-processing the entire tree.


The Source object
=================

``GPR2.Build.Source.Object`` (extending ``GPR2.Build.Source_Base.Object``)
represents one source file as seen by the build system.

Key accessors:

``Src.Path_Name``
  Absolute path to the source file (``GPR2.Path_Name.Object``).

``Src.Language``
  Language identifier (e.g. ``Ada_Language``).

``Src.Kind``
  ``S_Spec`` (spec or header file), ``S_Body`` (body or implementation
  source), or ``S_Separate`` (Ada separates only).

``Src.Has_Units``
  ``True`` for Ada sources. Ada is the only language for which GPR2
  tracks formal compilation units; for all other languages this is
  ``False``.

``Src.Owning_View``
  The view that owns this source.

``Src.Is_Inherited``
  ``True`` if the source is inherited from an extended project rather
  than owned directly.

``Src.Is_Visible``
  ``True`` if this source takes precedence within its namespace (i.e.
  it is the one returned by ``View.Visible_Sources``).

For Ada sources, a single file may contain more than one compilation unit
(``at`` clauses in the naming package). Use ``Src.Units`` to iterate all
unit entries, or ``Src.Unit`` to retrieve the one at a given index.


Iterating sources
=================

``View.Sources`` returns all sources owned by a view.
``View.Visible_Sources`` additionally resolves visibility: when two views
in the same namespace own a source with the same filename, only the one
that takes precedence is returned.

.. code-block:: ada

   --  All sources owned by a view
   for Src of View.Sources loop
      Put_Line (String (Src.Path_Name.Simple_Name)
                & "  [" & GPR2.Image (Src.Language) & "]");
   end loop;

   --  Visibility-resolved sources at a namespace root
   for Src of Root_View.Visible_Sources loop
      Put_Line (String (Src.Path_Name.Simple_Name));
   end loop;

Prefer ``Visible_Sources`` when working at the namespace level to avoid
processing shadowed sources.


Compilation units
=================

``GPR2.Build.Compilation_Unit.Object`` groups all parts of one Ada
compilation unit — spec, body, and separates — across the build tree.
Compilation units are Ada-specific; for other languages, source files are
individually enumerated via ``View.Sources``.

``CU.Name``
  Ada unit name (e.g. ``"My_Package.Child"``).

``CU.Has_Part (Kind)``
  Whether the unit has a spec (``S_Spec``), body (``S_Body``), or
  any separate.

``CU.Spec``
  ``Unit_Location`` for the spec (view + source path + index).

``CU.Main_Body``
  ``Unit_Location`` for the body.

``CU.Separates``
  Map of separate subunit names to their ``Unit_Location``.

``CU.Owning_View``
  The view that provides the main part (body if present, else spec).

``CU.Root_View``
  The namespace root this unit belongs to.

A ``Unit_Location`` record carries three fields: ``View``, ``Source``
(a ``Path_Name.Object``), and ``Index`` (the ``at`` position within the
file, or ``No_Index`` for single-unit files).


Iterating units
===============

``View.Units`` returns all compilation units whose main part is visible
within the namespace rooted at the given view. It requires
``Sources_Units`` or higher and the view must be a namespace root.

.. code-block:: ada

   --  View must satisfy View.Is_Namespace_Root
   for CU of Root_View.Units loop
      Put_Line (String (CU.Name));
      if CU.Has_Part (GPR2.S_Spec) then
         Put_Line ("  spec: "
                   & String (CU.Spec.Source.Simple_Name));
      end if;
      if CU.Has_Part (GPR2.S_Body) then
         Put_Line ("  body: "
                   & String (CU.Main_Body.Source.Simple_Name));
      end if;
   end loop;

To look up a specific unit by name:

.. code-block:: ada

   CU : constant GPR2.Build.Compilation_Unit.Object :=
          Root_View.Unit (+"My_Package.Child");

   if CU.Is_Defined then
      --  found
   end if;


Ada closure
===========

``Tree.For_Each_Ada_Closure`` visits every compilation unit in the Ada
transitive dependency closure of the tree's main programs (or library
interface units). It requires ``Sources_Units`` or higher.

.. code-block:: ada

   Tree.For_Each_Ada_Closure
     (Action => procedure (CU : GPR2.Build.Compilation_Unit.Object) is
      begin
         Put_Line (String (CU.Name));
      end);

Optional parameters:

``Mains``
  Restricts the closure entry points to a specific set of source
  filenames or unit names. Defaults to the ``Main`` attribute of the
  root project.

``All_Sources``
  When ``True``, visits all sources regardless of the closure.

``Root_Project_Only``
  When ``True``, only sources belonging to the root project are
  included.

``Externally_Built``
  When ``True``, also visits units from externally-built projects.
