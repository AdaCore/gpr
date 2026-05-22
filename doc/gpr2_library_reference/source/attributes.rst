.. index:: attribute, GPR2.Project.Attribute

**********
Attributes
**********

Attributes are the primary mechanism for storing project configuration in a
``.gpr`` file. Each attribute has a name, an optional index, and a value that
is either a single string or a list of strings.

The attribute API is spread across four packages:

* ``GPR2.Project.Registry.Pack`` - registers packages (built-in and custom).
* ``GPR2.Project.Registry.Attribute`` - registers attribute definitions and
  provides constants for every standard attribute.
* ``GPR2.Project.Attribute_Index`` - index values used to look up indexed
  attributes.
* ``GPR2.Project.Attribute`` - the attribute object returned by view queries.


Attribute registry
==================

Every attribute that GPR2 recognizes must be registered before the project
tree is loaded. Registration records the attribute's metadata: whether it
takes an index, what kind of value it holds, and which project kinds it is
allowed in.

GPR2 pre-registers all standard GPR attributes (``Compiler'Switches``,
``Object_Dir``, ``Main``, etc.) at library elaboration time. Tools that
define their own GPR packages must register those packages and their
attributes before calling ``Tree.Load``.

Registering a custom package
----------------------------

Use ``GPR2.Project.Registry.Pack.Add`` to introduce a new package:

.. code-block:: ada

   with GPR2.Project.Registry.Pack;

   --  Register a custom package allowed in all project kinds
   if not GPR2.Project.Registry.Pack.Exists (+"MyTool") then
      GPR2.Project.Registry.Pack.Add
        (Name     => +"MyTool",
         Projects => GPR2.Project.Registry.Pack.Everywhere);
   end if;

``Projects`` restricts which project kinds may contain the package.
``Everywhere`` allows it in all project kinds; ``No_Aggregates`` excludes
aggregate projects.

Once registered, packages with this name will be recognized in ``.gpr`` files:

.. code-block:: gpr

   package MyTool is
      for Switches use ("-O2");
   end MyTool;

Registering custom attributes
-----------------------------

Attributes are registered with ``GPR2.Project.Registry.Attribute.Add``.
The package the attribute belongs to must already be registered.

.. code-block:: ada

   with GPR2.Project.Registry.Attribute;
   with GPR2.Project.Registry.Pack;

   My_Package  : constant Package_Id     := +"MyTool";
   My_Switches : constant Q_Attribute_Id := (My_Package, +"Switches");

   if not GPR2.Project.Registry.Attribute.Exists (My_Switches) then
      GPR2.Project.Registry.Attribute.Add
        (Name                 => My_Switches,
         Index_Type           => GPR2.Project.Registry.Attribute.Language_Index,
         Value                => GPR2.Project.Registry.Attribute.List,
         Value_Case_Sensitive => True,
         Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);
   end if;

Key parameters of ``Add``:

``Index_Type``
  Kind of index the attribute accepts. ``No_Index`` for unindexed
  attributes; ``Language_Index`` for language names;
  ``FileGlob_Or_Language_Index`` for source globs or languages;
  ``File_Index`` / ``FileGlob_Index`` for source filenames or patterns.

``Value``
  ``Single`` for a scalar value, ``List`` for a list of strings.

``Value_Case_Sensitive``
  Whether the value strings are case-sensitive.

``Is_Allowed_In``
  Array of ``Project_Kind`` booleans; use ``Everywhere`` to allow
  the attribute in all project kinds.

``Index_Optional``
  If ``True``, the index may be omitted.

``Empty_Value``
  How empty values are treated: ``Allow``, ``Ignore`` (warning), or
  ``Error``.

``Inherit_From_Extended``
  Whether and how the attribute value is inherited from an extended
  project: ``Inherited``, ``Concatenated``, or ``Not_Inherited``.

``Config_Concatenable``
  If ``True``, the value is concatenated with any value found in the
  configuration project rather than overriding it.


Standard attribute constants
============================

``GPR2.Project.Registry.Attribute`` exposes a constant of type
``Q_Attribute_Id`` for every standard GPR attribute. Top-level attributes use
a simple name; package attributes are nested under a child package:

``GPR2.Project.Registry.Attribute.Object_Dir``
  ``Object_Dir``

``GPR2.Project.Registry.Attribute.Exec_Dir``
  ``Exec_Dir``

``GPR2.Project.Registry.Attribute.Source_Files``
  ``Source_Files``

``GPR2.Project.Registry.Attribute.Source_List_File``
  ``Source_List_File``

``GPR2.Project.Registry.Attribute.Main``
  ``Main``

``GPR2.Project.Registry.Attribute.Library_Name``
  ``Library_Name``

``GPR2.Project.Registry.Attribute.Compiler.Switches``
  ``Compiler'Switches``

``GPR2.Project.Registry.Attribute.Compiler.Default_Switches``
  ``Compiler'Default_Switches``

``GPR2.Project.Registry.Attribute.Compiler.Driver``
  ``Compiler'Driver``

``GPR2.Project.Registry.Attribute.Binder.Switches``
  ``Binder'Switches``

``GPR2.Project.Registry.Attribute.Linker.Switches``
  ``Linker'Switches``

``GPR2.Project.Registry.Attribute.Naming.Body_Suffix``
  ``Naming'Body_Suffix``

``GPR2.Project.Registry.Attribute.Builder.Default_Switches``
  ``Builder'Default_Switches``


Reading attributes from a view
==============================

Use ``View.Has_Attribute`` before accessing an attribute that may be absent:

.. code-block:: ada

   with GPR2.Project.Registry.Attribute;

   if View.Has_Attribute (GPR2.Project.Registry.Attribute.Object_Dir) then
      Attr : constant GPR2.Project.Attribute.Object :=
               View.Attribute (GPR2.Project.Registry.Attribute.Object_Dir);
      Put_Line (Attr.Value.Text);
   end if;

``View.Attribute`` returns ``Attribute.Undefined`` when the attribute is
absent and has no default, so the ``Has_Attribute`` guard is necessary for
optional attributes.


Single vs. list attributes
==========================

An attribute's ``Kind`` is either ``Single`` or ``List``:

.. code-block:: ada

   Attr : constant GPR2.Project.Attribute.Object :=
            View.Attribute (GPR2.Project.Registry.Attribute.Main);

   case Attr.Kind is
      when Single =>
         Put_Line (Attr.Value.Text);
      when List =>
         for V of Attr.Values loop
            Put_Line (V.Text);
         end loop;
   end case;

In practice the kind of each attribute is fixed by its registration, so you
rarely need to check ``Kind`` at runtime - just call ``Value`` for single
attributes and ``Values`` for list attributes.


Indexed attributes
==================

Many attributes are indexed (e.g. ``Compiler'Switches`` is indexed by
language or source filename). Pass an ``Attribute_Index`` to select one entry:

.. code-block:: ada

   with GPR2.Project.Attribute_Index;
   with GPR2.Project.Registry.Attribute;

   --  Look up Compiler'Switches for the Ada language
   Attr := View.Attribute
     (GPR2.Project.Registry.Attribute.Compiler.Switches,
      GPR2.Project.Attribute_Index.Create (Ada_Language));

   --  Look up Compiler'Switches for a specific source file.
   --  The index passed is a concrete filename. The project may define the
   --  attribute with a glob-pattern index such as Compiler'Switches ("autogen-*");
   --  the library resolves the lookup by pattern matching against defined
   --  indexes, or by falling back to the language of the source file.
   Attr := View.Attribute
     (GPR2.Project.Registry.Attribute.Compiler.Switches,
      GPR2.Project.Attribute_Index.Create_Source ("autogen-blah.adb"));

Creating an ``Attribute_Index``:

``Attribute_Index.Create (Value_Type)``
  String index (case-insensitive by default)

``Attribute_Index.Create_Source (Simple_Name)``
  Source filename (respects filesystem case sensitivity)

``Attribute_Index.Create (Language_Id)``
  Language identifier

``Attribute_Index.I_Others``
  The special ``others`` index

**Enumeration vs. lookup**

``View.Attributes`` returns all entries as they are written in the project
file - glob patterns appear as-is (e.g. the index value is ``"autogen-*"``
rather than any resolved filename). Use it to inspect or iterate raw
definitions.

``View.Attribute`` with a concrete index performs resolution: the library
tries to match the given value against each defined index pattern
(``FileGlob`` matching), and for source-file indexes also falls back to the
language of that source file. This is the right call when you want the
effective switches that apply to a particular source.

.. code-block:: ada

   --  Enumerate raw definitions (patterns as written in the project)
   for Attr of View.Attributes
     (GPR2.Project.Registry.Attribute.Compiler.Switches)
   loop
      Put_Line (Attr.Index.Value & " => " & Attr.Value.Text);
      --  Index.Value may be "autogen-*", "Ada", "others", etc.
   end loop;

   --  Resolve the effective switches for one specific source file
   Attr := View.Attribute
     (GPR2.Project.Registry.Attribute.Compiler.Switches,
      GPR2.Project.Attribute_Index.Create_Source ("autogen-blah.adb"));


Attribute metadata
==================

Beyond the value, an ``Attribute.Object`` carries useful metadata:

``Attr.Kind``
  ``Single`` or ``List``.

``Attr.Value``
  The single value (``Source_Reference.Value.Object``). Call ``.Text``
  to obtain the ``Value_Type`` string.

``Attr.Values``
  The list of values. Each element has a ``.Text`` accessor.

``Attr.Index``
  The ``Attribute_Index`` for this entry; call ``.Value`` or
  ``.Is_Others`` on the result.

``Attr.Has_Index``
  ``True`` if this attribute entry has an index.

``Attr.Is_Default``
  ``True`` if the value was synthesised from a GPR default rule rather
  than written explicitly in the project file.

``Attr.Is_From_Config``
  ``True`` if the value originates from the configuration project
  (``.cgpr``) rather than from the user project.
