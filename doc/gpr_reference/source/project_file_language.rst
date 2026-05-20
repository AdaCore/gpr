.. _Project_File_Language:

*********************
Project File Language
*********************

GPR project files use an Ada-like syntax. This chapter describes the
lexical rules, the structure of a project file, and the language constructs
available within it.


Overview
========

The fundamental purpose of a project file is to supply **attribute values**
to GPR tools. An attribute is a named configuration parameter - source
directories, compiler switches, library name, and so on - declared with a
``for`` clause (see :ref:`Attribute_Declarations`). Attributes may be
*indexed* by a key such as a language name or source file name, and are
grouped into **packages** that namespace them by tool
(``Compiler'Switches``, ``Linker'Switches``, ...).

**Typed variables** and ``case`` statements let attribute values vary by
configuration without duplicating the project file.


Lexical Elements
================

.. index:: identifier

Identifiers
-----------

Identifiers follow the same rules as Ada identifiers: they must start with a
letter and may be followed by letters, digits, or underscores. Two consecutive
underscores are not allowed. Identifiers are **case-insensitive**
(``"Name"`` and ``"name"`` are the same identifier).

::

    simple_name    ::= identifier
    name           ::= simple_name { . simple_name }
    attribute_name ::= identifier
    package_name   ::= identifier
    project_name   ::= name
    type_name      ::= identifier
    variable_name  ::= identifier

    project_ref   ::= project_name
    package_ref   ::= package_name
                   | project_name . package_name
    variable_ref  ::= variable_name
                   | package_name . variable_name
                   | project_name . variable_name
                   | project_name . package_name . variable_name
    attribute_ref ::= 'project' ''' attribute_name [ '(' string_expression ')' ]
                   | project_ref ''' attribute_name [ '(' string_expression ')' ]
                   | package_ref ''' attribute_name [ '(' string_expression ')' ]
    builtin_name  ::= 'external' | 'external_as_list' | 'file_as_list'
                   | 'lower' | 'upper' | 'split' | 'match' | 'filter_out'
                   | 'item_at' | 'remove_prefix' | 'remove_suffix'
                   | 'default' | 'alternative'


.. index:: string literal

Strings
-------

String literals are written between double quotes, as in Ada. They are in
general **case-sensitive**, except where noted (for example, file names are
case-insensitive on systems with case-insensitive file systems).


.. index:: reserved words

Reserved Words
--------------

The following Ada 95 reserved words are used in project files:

::

    abstract  all     at       case
    end       for     is       limited
    null      others  package  renames
    type      use     when     with

The following identifiers are additional reserved words specific to project
files:

::

    extends  external  external_as_list  file_as_list  project
    alternative  default  filter_out  item_at  lower  match
    remove_prefix  remove_suffix  split  upper

Note that ``aggregate`` and ``library`` are qualifiers that may precede the
keyword ``project`` but are not reserved words.


.. index:: comment

Comments
--------

Comments follow Ada syntax: two consecutive hyphens (``--``) start a comment
that extends to the end of the line.


.. index:: project file; structure

Project File Structure
======================

A project file consists of an optional context clause followed by a project
declaration.

::

    project        ::= context_clause project_declaration

    context_clause ::= { with_clause }
    with_clause    ::= [ 'limited' ] 'with' path_name { , path_name } ;
    path_name      ::= string_literal

    project_declaration ::= simple_project_declaration
                          | project_extension

    simple_project_declaration ::=
      [ qualifier ] 'project' project_name 'is'
        { declarative_item }
      'end' project_name ;

    project_extension ::=
      [ qualifier ] 'project' project_name 'extends' [ 'all' ] path_name 'is'
        { declarative_item }
      'end' project_name ;

    qualifier ::= 'abstract'
                | 'library'
                | 'aggregate'
                | 'aggregate' 'library'
                | 'configuration'
                | 'standard'

For the semantics of each qualifier see :ref:`Project_Kinds`. For the
semantics of ``extends`` and ``extends all`` see :ref:`RM_Project_Extension`.


.. index:: context clause, with clause

Context Clauses
---------------

::

    context_clause ::= { with_clause }
    with_clause    ::= [ 'limited' ] 'with' path_name { , path_name } ;
    path_name      ::= string_literal

A project may import other projects via ``with`` clauses. Imported projects
make their sources and attributes visible to the importing project.

A ``limited with`` allows two projects to have mutual visibility of each
other's compiled objects without creating a cycle in the attribute-dependency
graph. A project imported via ``limited with`` contributes its compiled
artifacts (object files, libraries) but its attributes cannot be referenced
by the importing project. This keeps attribute resolution strictly acyclic
while still permitting the mutual-view pattern needed by some system
architectures.

Path names in ``with`` clauses are strings containing a path to a ``.gpr``
file. The ``.gpr`` extension is optional and will be added automatically if
absent. Paths may be absolute or relative. Relative paths are resolved with
respect to the directory of the current project file, or against directories
listed in :envvar:`GPR_PROJECT_PATH` and :envvar:`ADA_PROJECT_PATH`. The
directory separator may always be ``/`` even on Windows.

A given project name may appear only once across all ``with`` clauses of the
same project. Cycles without ``limited with`` are forbidden.


.. index:: child project

Child projects
--------------

A project whose name is a dotted name (``Parent.Child``) is a *child project*
of ``Parent``. This is purely a naming convention expressing a close
relationship between the two; a child project does **not** implicitly import
its parent. An explicit ``with`` or ``extends`` clause is required.

The child project file name uses a dash in place of the dot:
``Math_Proj.Tests`` lives in ``math_proj-tests.gpr``.

.. code-block:: gpr

   -- math_proj-tests.gpr
   with "math_proj.gpr";
   project Math_Proj.Tests is
      --  Parent variables and attributes accessible via the parent name prefix:
      Obj_Dir := Math_Proj.Object_Dir;
      ...
   end Math_Proj.Tests;

Once the parent is imported or extended, its variables and attributes are
accessible using the parent project name as a prefix (e.g.
``Math_Proj.Object_Dir``).


.. index:: source ownership

Source ownership
----------------

Each project directly owns a set of **immediate sources**: files identified
through its source-related attributes (source directories, explicit file
lists, etc.). The full set of **sources** visible to a project extends this
with the immediate sources of every project it depends on, directly or
indirectly
(unless overridden by extension). For the rules governing basename uniqueness,
search order, and source shadowing in extensions, see
:ref:`Source_Resolution`.


.. index:: declaration

Declarations
============

Declarations introduce types, variables, attributes, and packages.
They are processed sequentially in the order they appear; a name becomes
visible immediately after its declaration.

::

    declarative_item ::= simple_declarative_item
                       | typed_string_declaration
                       | package_declaration

    simple_declarative_item ::= variable_declaration
                              | typed_variable_declaration
                              | attribute_declaration
                              | case_construction
                              | empty_declaration

    empty_declaration ::= 'null' ;

An empty declaration (``null;``) is valid anywhere a declaration is allowed
and has no effect.

**Scope rules:**

Declarations are scoped to either the project level or the enclosing package.
The following rules apply:

- **Typed string declarations** may only appear at the project level. Once
  declared, the type is visible throughout the entire project file, including
  inside packages, and may be referenced from other projects using a qualified
  name (``Project_Name.Type_Name``).

- **Package declarations** may only appear at the project level; packages
  cannot be nested.

- **Variable declarations** (typed or untyped) may appear at the project level
  or inside a package. A project-level variable is visible throughout the
  project file. A variable declared inside a package is local to that package
  and can be referenced from outside only via a qualified name
  (``Package_Name.Variable_Name`` or
  ``Project_Name.Package_Name.Variable_Name``).

- **Attribute declarations** may appear at the project level (setting
  project-level attributes) or inside a package (setting package attributes).
  Attributes from other projects or packages are accessible via qualified
  names.

- **Case constructions** may appear at both levels. The discriminant must be a
  typed variable already declared before the ``case`` construct. Inside a case
  arm, only attribute declarations, variable declarations (for variables
  already declared before the ``case``), nested case constructions, and
  ``null`` are allowed - type and package declarations are forbidden.


.. index:: string value, list value

Values
======

GPR projects manipulate two kinds of values:

**String**
  A single string, e.g. ``"debug"`` or ``"src/main.adb"``.

**List of strings**
  An ordered sequence of strings, e.g. ``("-O2", "-g")``. The empty list is
  written ``()``.

Attributes and variables each hold one of these two kinds. The kind of an
attribute is fixed by the language specification (see :ref:`Attributes`); the
kind of an untyped variable is inferred from its first declaration.

Values may incorporate previously declared variables and attributes via
references, call built-in functions, and may be combined using the ``&``
concatenation operator. Once any operand is a list the result is a list, and
the list must be the left operand. See `Built-in Functions`_ and
`Expressions`_ for the formal grammar.


.. index:: built-in function

Built-in Functions
==================

Built-in functions may be used inside expressions. Their names are not
reserved words and may be used as variable names elsewhere; a name is
interpreted as a built-in call only when immediately followed by ``(``.

::

    builtin_call ::= builtin_name '(' [ expression { , expression } ] ')'

A built-in call may return either a string or a list of strings depending on
the function; see `Built-in Functions`_ for the specific signatures of each.


.. index:: External (built-in), scenario variable

The ``External`` function
-------------------------

.. _External_Values:

.. _Scenario_Variable:

``External`` retrieves a string value from the build environment. The first
argument is the name of an external variable; the optional second argument is
the default value. Its value is resolved, in priority order, from:

1. The ``-X\ *name*\ =\ *value*`` command-line switch.
2. An environment variable of the same name.
3. The second argument, if supplied (the default value).

If none of these sources provides a value, an error is reported.

``External`` is typically used to initialize **typed variables** (see
`Typed String Declaration`_), which are then referenced in ``case``
constructions to vary attribute values across build scenarios. Such variables
are commonly called *scenario variables*.

.. code-block:: gpr

   type Build_Mode_Type is ("debug", "release");
   Build_Mode : Build_Mode_Type := external ("BUILD_MODE", "debug");


.. index:: External_As_List (built-in)

The ``External_As_List`` function
---------------------------------

The ``External_As_List`` function retrieves a list of strings from the
environment by splitting an external variable on a separator.

::

    external_as_list_value ::=
      'external_as_list' ( string_literal , string_literal )

The first argument is the external variable name; the second is the separator
string. The value is looked up on the command line first, then as an
environment variable. If undefined, an empty list is returned (no error).
Leading and trailing separators are discarded.

Key differences from ``External``:

- No default-value parameter; returns ``()`` when the variable is undefined.
- Returns a list, not a string.

Example:

.. code-block:: gpr

   --  If SWITCHES is "-O2,-g", External_As_List ("SWITCHES", ",")
   --  returns ("-O2", "-g").


.. index:: File_As_List (built-in)

The ``File_As_List`` function
-----------------------------

.. note::

   This function is not available in tools based on the legacy GPR1 framework.

``File_As_List`` reads a text file and returns its lines as a list of strings.

::

    list ::= 'file_as_list' ( string_literal )

The argument is the path to the file. Returns ``()`` if the file does not
exist or is empty.

.. code-block:: gpr

   Source_Files := file_as_list ("sources.txt");


.. index:: Split (built-in), Lower (built-in), Upper (built-in), Remove_Prefix (built-in), Remove_Suffix (built-in), Filter_Out (built-in), Match (built-in), Item_At (built-in), Default (built-in), Alternative (built-in)

String Manipulation Functions
-----------------------------

.. note::

   The functions in this section are not available in tools based on the
   legacy GPR1 framework. This includes all tools that have not yet migrated
   to GPR2.

``Split``
^^^^^^^^^

Splits a string on a separator and returns the parts as a list. Empty parts
are not included. Returns ``()`` if the string is empty or consists entirely
of separators.

::

    Split ( string_literal , string_literal )

.. code-block:: gpr

   --  Split ("-gnatf,-gnatv", ",")  =>  ("-gnatf", "-gnatv")

``Lower`` / ``Upper``
^^^^^^^^^^^^^^^^^^^^^

Return the argument with all characters converted to lower or upper case.
Accept a string or a list.

.. code-block:: gpr

   --  Lower ("FOO")          =>  "foo"
   --  Upper (("one","two"))  =>  ("ONE", "TWO")

``Remove_Prefix`` / ``Remove_Suffix``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Remove a fixed prefix or suffix from a string or from each element of a list,
if present.

.. code-block:: gpr

   --  Remove_Prefix (("libone", "two", "libthree"), "lib")
   --    =>  ("one", "two", "three")

   --  Remove_Suffix ("libZ.so", ".so")  =>  "libZ"

``Filter_Out``
^^^^^^^^^^^^^^

Removes from a list all elements matching a regular-expression pattern.

.. code-block:: gpr

   --  Filter_Out (("value1", "or", "another", "one"), ".*o.*")
   --    =>  ("value1")

``Match``
^^^^^^^^^

Returns elements of a string or list that match a regular-expression pattern.
An optional third argument provides a replacement pattern applied to each
match.

.. code-block:: gpr

   --  Match ("x86_64-linux-gnu", "linux")  =>  "linux"

   --  Match (("value1","or","another","one"), "(.*r)", "r:\1")
   --    =>  ("r:or", "r:another")

``Item_At``
^^^^^^^^^^^

Returns one element from a list by index. Negative indices count from the
end (``"-1"`` is the last element).

.. code-block:: gpr

   --  Item_At (("one","two","three","last"), "2")   =>  "two"
   --  Item_At (("one","two","three","last"), "-1")  =>  "last"

``Default`` / ``Alternative``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``Default`` returns its second argument when the first is the empty string;
otherwise it returns the first argument.

``Alternative`` returns its second argument when the first is *not* the empty
string; otherwise it returns the first argument.

.. code-block:: gpr

   --  Default     ("",  "fallback")  =>  "fallback"
   --  Default     ("x", "fallback")  =>  "x"
   --  Alternative ("",  "fallback")  =>  ""
   --  Alternative ("x", "fallback")  =>  "fallback"


.. index:: expression, concatenation

Expressions
===========

An **expression** builds a value from literals, variable references, attribute
references, built-in function calls, and concatenation:

::

    string_literal ::= "{string_element}"    --  Same as Ada

    string_expression ::= string_literal
        | variable_ref
        | attribute_ref
        | builtin_call
        | string_expression & string_expression

    string_list ::= ( string_expression { , string_expression } )
        | variable_ref
        | attribute_ref
        | builtin_call

    term       ::= string_expression | string_list
    expression ::= term { & term }

Concatenation rules follow Ada conventions. Once any operand is a list, the
result is a list:

.. code-block:: ada

   function "&" (X : String;      Y : String)      return String;
   function "&" (X : String_List; Y : String)      return String_List;
   function "&" (X : String_List; Y : String_List) return String_List;

Example:

.. code-block:: gpr

   List  := () & File_Name;                  --  one element
   List2 := List & (File_Name & ".orig");    --  two elements
   Big   := List & List2;                    --  three elements
   --  Illegal := "gnat.adc" & List2;        --  error: list must be left operand


.. index:: typed string declaration

Typed String Declaration
========================

.. _Typed_String_Declaration:

A **type declaration** introduces a finite set of string literals. Variables
declared with this type are restricted to the listed values. Type declarations
may only appear at the project level, not inside a package.

::

    typed_string_declaration ::=
      'type' type_name 'is'
        ( string_literal { , string_literal } ) ;

String literals in the list are case-sensitive and must be distinct. Example:

.. code-block:: gpr

   type OS is ("GNU/Linux", "Unix", "Windows", "VMS");

Variables of a string type are called **typed variables**; all others are
**untyped variables**. A type declared in another project may be referenced
using a qualified name (``Project_Name.Type_Name``).


.. index:: variable

Variables
=========

.. _Variables:

**Variables** store a string or list-of-strings value and may appear in
expressions. A variable declaration creates the variable and assigns it a
value. Before its first declaration, a variable defaults to ``""`` (empty
string).


.. index:: typed variable

Typed Variables
---------------

A typed variable must be declared exactly once. Its type restricts the
values it may hold, and because it can only be set once, all ``case``
constructions in the file see a consistent value - making it behave
effectively as a constant.

::

    typed_variable_declaration ::=
      variable_name : type_name := string_expression ;

.. code-block:: gpr

   type OS_Type is ("GNU/Linux", "Unix", "Windows");
   OS : OS_Type := external ("OS", "GNU/Linux");


.. index:: untyped variable

Untyped Variables
-----------------

An untyped variable may be declared and reassigned any number of times. Its
kind (string or list) is fixed by the first declaration; subsequent
declarations must match.

::

    variable_declaration ::= variable_name := expression ;

.. code-block:: gpr

   Name      := "readme.txt";
   Save_Name := Name & ".saved";
   Flags     := ("-O2", "-g");


.. index:: variable reference

Variable References
-------------------

A variable may be referenced by its simple name or a qualified name
(``variable_ref`` as defined in `Identifiers`_):

.. code-block:: gpr

   Mode                      --  variable in current scope
   Compiler.Opt_Level        --  variable in a package
   Common.Build_Mode         --  variable in an imported project
   Common.Compiler.Opt_Level --  variable in a package of an imported project

A simple name resolves to the current package (if any) or the current project.
Qualified names may refer to a package in the current project, an imported
project, a base project (direct or indirect), or a package within any of
those.


.. _Attribute_Declarations:

.. index:: attribute declaration, indexed attribute

Attribute Declarations
======================

Attributes are the primary mechanism for communicating information to build
tools. An attribute declaration uses the ``for ... use`` syntax:

::

    attribute_declaration ::=
        'for' attribute_name 'use' expression ;
      | 'for' attribute_name '(' string_expression ')' 'use' expression ;

The optional parenthesised string expression is the **index** of the
attribute. Indexed attributes associate different values with different
keys - for example, per-language compiler switches:

.. code-block:: gpr

   package Compiler is
      for Switches ("Ada") use ("-O2", "-gnat2022");
      for Switches ("C")   use ("-O2", "-Wall");
   end Compiler;

An attribute may also be declared without an index, in which case it has a
single value for the whole project or package:

.. code-block:: gpr

   for Library_Name use "mylib";

Different attributes accept different kinds of indexes:

**Language** (case-insensitive)
  A language identifier such as ``"Ada"`` or ``"C"``. Used by most
  ``Compiler``, ``Binder``, ``Linker``, and ``Naming`` attributes.

  ``for Compiler'Switches ("Ada") use ...;``

**File / Glob** (case-sensitivity host-dependent)
  A source file simple name or a glob pattern (``*``, ``?``, ``[]``).
  When a glob pattern is used, the attribute applies to every source file
  whose name matches that pattern. Some attributes also accept ``others``
  as an index, which matches any source file not matched by a more
  specific index in the same project. Whether ``others`` is accepted is
  specified per attribute in :ref:`Attributes`.

  ``for Compiler'Switches ("main.adb") use ...;``

  ``for Compiler'Switches ("*.c") use ...;``

**File / Glob / Language** (case-sensitivity host-dependent for files, case-insensitive for languages)
  Accepts a source file simple name, a glob pattern, or a language
  identifier. Strings containing dots or glob characters (``*``, ``?``,
  ``[``, ``]``) are treated as file names or glob patterns; all other
  strings are treated as language identifiers. Some attributes also
  accept ``others`` as a catch-all index. Whether ``others`` is accepted
  is specified per attribute in :ref:`Attributes`.

  When multiple declarations for the same attribute exist in a project
  with different indexes, the value that applies to a given source file
  is resolved in the following priority order:

  1. An index that is an exact match on the file name.
  2. An index that is a glob pattern matching the file name.
  3. An index matching the file's language.
  4. The ``others`` index, if present.

  Used by ``Compiler'Switches`` and ``Roots``.

**Unit** (case-insensitive)
  An Ada unit name. Used by ``Naming'Spec`` and ``Naming'Body``.

  ``for Naming'Spec ("My.Package") use "my-package.ads";``

**String**
  An arbitrary string key. Used for instance by the aggregate-project
  attribute ``External``, where the index is the name of an external
  variable.

  ``for External ("BUILD_MODE") use "release";``

Attribute references (``attribute_ref`` as defined in `Identifiers`_) allow
reading values from other projects or packages:

.. code-block:: gpr

   for Switches ("Ada") use Common.Compiler'Switches ("Ada") & ("-g");

For the full list of attributes and their semantics, see :ref:`Attributes`.
Individual tools may define additional attributes in their own packages; refer
to each tool's documentation.


.. index:: package declaration

Packages
========

A project file may contain **packages**, which group related attributes -
typically all attributes used by one tool. A given package name may appear at
most once per project file.

::

    package_declaration ::= package_spec | package_renaming | package_extension

    package_spec ::=
      'package' package_name 'is'
         { simple_declarative_item }
      'end' package_name ;

The standard packages recognized in all project files are:

``Binder``
  Options for the binder (``gnatbind`` / GPRbuild).

``Builder``
  Global build options (executable names, global compilation switches).

``Clean``
  Options for ``gprclean``.

``Compiler``
  Compilation options per language.

``Install``
  Options for ``gprinstall``.

``Linker``
  Link options.

``Naming``
  Source-file naming conventions.

Other tool-specific packages may be defined by individual tools; refer to each
tool's documentation.

.. note::

   Each tool only reads the packages it recognizes; unknown package
   *declarations* are silently ignored. However, referencing an attribute or
   variable from an unknown package in an expression - for example, reading
   ``Clean'Switches`` inside a project loaded by GPRbuild - will cause a
   parsing error and the project will be rejected. Avoid cross-package
   attribute references unless you can guarantee that every tool loading the
   project knows the referenced package.

A minimal (empty) package:

.. code-block:: gpr

   project Simple is
      package Builder is
      end Builder;
   end Simple;

A package may contain attribute declarations, variable declarations, and case
constructions.

.. note::

   When a name could refer to either a project or a package, it always
   designates the project. Avoid naming projects after standard package names
   or names starting with ``gnat``.


.. index:: package renaming

Package Renaming
----------------

A package may be defined by a **renaming declaration**, which gives the new
package the same attributes as a package declared in another project. The
renamed project must appear in the current project's context clause (or be its
base project). No attributes may be added or overridden in a renaming; use a
package extension for that.

::

    package_renaming ::=
      'package' package_name 'renames'
        project_name . package_name ;

Package renaming is a common way to share settings: define the authoritative
package in one project file and rename it in all projects that need the same
settings.


.. index:: package extension

Package Extension
-----------------

A **package extension** works like a renaming but also allows adding or
overriding attributes. It is particularly useful in project extension: a
package inherited from the base project can be explicitly extended to add or
override specific attributes without replacing it entirely.

::

    package_extension ::=
      'package' package_name 'extends'
        project_name . package_name 'is'
         { simple_declarative_item }
      'end' package_name ;


.. index:: case construction

Case Constructions
==================

.. _Case_Constructions:

A ``case`` construction selects attribute and variable declarations based on
the value of a typed variable, enabling conditional project configuration.

::

    case_construction ::=
      'case' variable_ref 'is' { case_item } 'end' 'case' ;

    case_item ::=
      'when' discrete_choice_list '=>'
        { case_construction
        | attribute_declaration
        | variable_declaration
        | empty_declaration }

    discrete_choice_list ::= string_literal { '|' string_literal } | 'others'

Rules:

- All choices must be distinct.
- All values of the type must be covered, either explicitly or via ``others``.
- ``others`` must be the last alternative.
- The case expression must be a variable (typed or untyped) whose value is
  known at load time.
- Inside a ``case``, only case constructions, attribute declarations,
  variable declarations (for already-declared variables), and ``null``
  declarations are allowed. Type and package declarations are not.

Example:

.. code-block:: gpr

   project MyProj is
      type OS_Type is ("GNU/Linux", "Unix", "Windows", "VMS");
      OS : OS_Type := external ("OS", "GNU/Linux");

      package Compiler is
         case OS is
            when "GNU/Linux" | "Unix" =>
               for Switches ("Ada") use ("-gnath");
            when "Windows" =>
               for Switches ("Ada") use ("-gnatP");
            when others =>
               null;
         end case;
      end Compiler;
   end MyProj;
