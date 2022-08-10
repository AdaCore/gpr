.. |with| replace:: :samp:`with`
.. |withs| replace:: :samp:`with`\ s
.. |withed| replace:: :samp:`with`\ ed
.. |withing| replace:: :samp:`with`\ ing
.. |limited_with| replace:: :samp:`limited with`

.. -- Example: A |withing| unit has a |with| clause, it |withs| a |withed| unit


.. _Project_Declaration:

Project Declaration
-------------------

Project files have an Ada-like syntax. The minimal project file is:

  .. code-block:: gpr

       project Empty is
       end Empty;

The identifier ``Empty`` is the name of the project.
This project name must be present after the reserved
word ``end`` at the end of the project file, followed by a semicolon.

**Identifiers** (i.e., the user-defined names such as project or variable names)
have the same syntax as Ada identifiers: they must start with a letter,
and be followed by zero or more letters, digits or underscore characters;
it is also illegal to have two underscores next to each other. Identifiers
are always case-insensitive (``"Name"`` is the same as ``"name"``).

::

    simple_name ::= identifier
    name        ::= simple_name { . simple_name }

**Strings** are used for values of attributes or as indexes for these
attributes. They are in general case sensitive, except when noted
otherwise (in particular, strings representing file names will be case
insensitive on some systems, so that ``"file.adb"`` and ``"File.adb"`` both
represent the same file).

.. index:: Reserved words (in project files)

**Reserved words** are the standard Ada 95 reserved words, plus several
others listed below, and cannot be used for identifiers.
In particular, the following Ada 95 reserved words are currently
used in project files:

  ::

      abstract  all     at       case
      end       for     is       limited
      null      others  package  renames
      type      use     when     with

The additional project file reserved words are:

  ::

     extends external external_as_list project

Note that ``aggregate`` and ``library`` are qualifiers that may appear before
the keyword ``project``, but they are not themselves keywords.

To avoid possible compatibility issues in the future, we recommend that
the reserved words introduced by Ada 2005 and Ada 2012 not be used as
identifiers in project files. Note also that new reserved words
may be added to the project file syntax in a later release.


**Comments** in project files have the same syntax as in Ada, two consecutive
hyphens through the end of the line.

.. index:: Independent project

.. _Independent_Project:

A project may be an **independent project**, entirely defined by a single
project file. Any source file in an independent project depends only
on the predefined library and other source files in the same project.
Alternatively, a project may depend on other projects in various ways:

*  by **importing** them through context clauses (|with| clauses), or
*  by **extending** at most one other project (its base project).

A given project may exhibit either or both of these dependencies; for example:

  .. code-block:: gpr

       with "imported_proj.gpr";
       project My_Project extends "base_proj.gpr" is
       end My_Project;

The import dependencies form a **directed graph**, potentially cyclic when using
**limited with**. The subgraph reflecting the **extends** relationship is a tree
(hierarchy).

A path name denotes a project file. It can be absolute or relative.
An absolute path name includes a sequence of directories, in the syntax of
the host operating system, that uniquely identifies the project file in the
file system. A relative path name identifies the project file, relative
to the directory that contains the current project, or relative to a
directory listed in the environment variables :envvar:`ADA_PROJECT_PATH` and
:envvar:`GPR_PROJECT_PATH`. Path names are case sensitive if file names in the host
operating system are case sensitive. As a special case, the directory
separator can always be ``'/'`` even on Windows systems, so that project files
can be made portable across architectures.
The syntax of the environment variables :envvar:`ADA_PROJECT_PATH` and
:envvar:`GPR_PROJECT_PATH` is a list of directory names separated by colons on Unix and
semicolons on Windows.

A given project name can appear only once in a context clause, and may not
appear in different context clauses for the same project.

It is illegal for a project imported by a context clause to refer, directly
or indirectly, to the project in which this context clause appears (the
dependency graph cannot contain cycles), except when one of the |with| clauses
in the cycle is a |limited_with|.

.. index:: Immediate sources of a project
.. index:: Sources of a project

A project's **immediate sources** are the source files directly defined by
that project, either implicitly by residing in the project source directories,
or explicitly through any of the source-related attributes.
More generally, a project's **sources** are the immediate sources of the
project together with the immediate sources (unless overridden) of any project
on which it depends directly or indirectly.

::

      project        ::= context_clause project_declaration

      context_clause ::= {with_clause}
      with_clause    ::= [ 'limited' ] 'with' path_name { , path_name } ;
      path_name      ::= string_literal

      project_declaration ::= simple_project_declaration | project_extension

      simple_project_declaration ::=
        [ qualifier ] 'project' <project_>name 'is'
          {declarative_item}
        'end' <project_>name ;

      project_extension ::=
       [ qualifier ] 'project' <project_>name 'extends' [ 'all' ] <base_project_>name 'is'
         {declarative_item}
       'end' <project_>name ;

     qualifier ::=
       'abstract' | identifier [ identifier ]


.. _Qualified_Projects:

Qualified Projects
------------------

Immediately preceding the reserved  ``project``, a **qualifier** may be
specified which identifies the nature of the project. The following
qualifiers are allowed:

.. index:: Standard project

.. _Standard_Project:

**standard**:
  A standard project is a non-library project with source files.
  This is the default (implicit) qualifier.

.. index:: Abstract project

**abstract**:
  A project with no source files.
  Such a project must either have no declaration for attributes ``Source_Dirs``,
  ``Source_Files``, ``Languages`` or ``Source_List_File``, or one of
  ``Source_Dirs``, ``Source_Files``, or ``Languages`` must be declared
  as empty. If it extends another project, the base project must also be
  an abstract project.

.. index:: Aggregate project

**aggregate**:
  A project whose sources are aggregated from other project files.

**aggregate library**:
  A library whose sources are aggregated from other project
  or library project files.

.. index:: Library project

**library**:
  A library project must define both of the attributes
  `Library_Name` and `Library_Dir`.

.. index:: Configuration project

.. _Configuration_Project:

**configuration**:
  A configuration project cannot be in a project tree.
  It describes compilers and other tools to *gprbuild*.


.. index:: Declarations in project files

.. _Declarations:

Declarations
------------

Declarations introduce new entities that denote types, variables, attributes,
and packages. Some declarations can only appear immediately within a project
declaration. Others can appear within a project or within a package.

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

An empty declaration is allowed anywhere a declaration is allowed. It has
no effect.


.. index:: Packages in project files

.. _Packages:

Packages
--------

A project file may contain **packages**, which group attributes (typically
all the attributes that are used by one of the GNAT tools).

A package with a given name may only appear once in a project file.
The following packages are currently supported in project files
(See :ref:`Attributes` for the list of attributes that each can contain).

.. index:: Binder package

*Binder*
  This package specifies characteristics useful when invoking the binder either
  directly via the *gnat* driver or when using *GPRbuild*.
  See :ref:`Main_Subprograms`.

.. index:: Builder package

*Builder*
  This package specifies the compilation options used when building an
  executable or a library for a project. Most of the options should be
  set in one of ``Compiler``, ``Binder`` or ``Linker`` packages,
  but there are some general options that should be defined in this
  package. See :ref:`Main_Subprograms`, and :ref:`Executable_File_Names` in
  particular.

.. index:: Check package
.. index:: gnatcheck tool

*Check*
  This package specifies the options used when calling the coding standard
  verification tool *gnatcheck*. Its attributes
  ``Default_Switches`` and ``Switches`` have the same semantics as for the package
  ``Builder``. The first string should always be :option:`-rules` to specify
  that all the other options belong to the ``-rules`` section of the
  parameters to *gnatcheck*.

.. index:: Clean package
.. index:: gprclean tool

*Clean*
  This package specifies the options used when cleaning a project or a project
  tree using the tools *gnatclean* or *gprclean*.

.. index:: Compiler package

*Compiler*
  This package specifies the compilation options used by the compiler for
  each language. See :ref:`Tools_Options_in_Project_Files`.

.. index:: Cross_Reference package
.. index:: gnatxref tool

*Cross_Reference*
  This package specifies the options used when calling the library tool
  *gnatxref* via the *gnat* driver. Its attributes
  ``Default_Switches`` and ``Switches`` have the same semantics as for the
  package ``Builder``.

.. index:: Documentation package
.. index:: gnatdoc tool

*Documentation*
  This package specifies the options used when calling the tool
  *gnatdoc*.

.. index:: Eliminate package
.. index:: gnatelim tool

*Eliminate*
  This package specifies the options used when calling the tool
  *gnatelim*. Its attributes
  ``Default_Switches`` and ``Switches`` have the same semantics as for the
  package ``Builder``.

.. index:: Finder package
.. index:: gnatfind tool

*Finder*
  This package specifies the options used when calling the search tool
  *gnatfind* via the *gnat* driver. Its attributes
  ``Default_Switches`` and ``Switches`` have the same semantics as for the
  package ``Builder``.

.. index:: Gnatls package
.. index:: gnatls tool

*Gnatls*
  This package specifies the options to use when invoking *gnatls*
  via the *gnat* driver.

.. index:: Gnatstub package
.. index:: gnatstub tool

*Gnatstub*
  This package specifies the options used when calling the tool
  *gnatstub*. Its attributes
  ``Default_Switches`` and ``Switches`` have the same semantics as for the
  package ``Builder``.

.. index:: IDE package

*IDE*
  This package specifies the options used when starting an integrated
  development environment, for instance *GPS* or *GNATbench*.

.. index:: Install package
.. index:: gprinstall tool

*Install*
  This package specifies the options used when installing a project
  with *gprinstall*. See :ref:`Package_Install_Attributes`.

.. index:: Linker package

*Linker*
  This package specifies the options used by the linker.
  See :ref:`Main_Subprograms`.

.. index:: Metrics package
.. index:: gnatmetric tool

*Metrics*
  This package specifies the options used when calling the tool
  *gnatmetric*. Its attributes
  ``Default_Switches`` and ``Switches`` have the same semantics as for the
  package ``Builder``.

.. index:: Naming package

*Naming*
  This package specifies the naming conventions that apply
  to the source files in a project. In particular, these conventions are
  used to automatically find all source files in the source directories,
  or given a file name to find out its language for proper processing.
  See :ref:`Naming_Schemes`.

.. index:: Pretty_Printer package
.. index:: gnatpp tool

*Pretty_Printer*
  This package specifies the options used when calling the formatting tool
  *gnatpp*. Its attributes
  ``Default_Switches`` and ``Switches`` have the same semantics as for the
  package ``Builder``.

.. index:: Remote package
.. index:: Distributed compilation

*Remote*
  This package is used by *GPRbuild* to describe how distributed
  compilation should be done.

.. index:: Stack package
.. index:: gnatstack tool

*Stack*
  This package specifies the options used when calling the tool
  *gnatstack*. Its attributes
  **Default_Switches** and **Switches** have the same semantics as for the
  package `Builder`.

.. index:: Synchronize package
.. index:: gnatsync tool

*Synchronize*
  This package specifies the options used when calling the tool
  *gnatsync* via the *gnat* driver.

In its simplest form, a package may be empty:

  .. code-block:: gpr

       project Simple is
         package Builder is
         end Builder;
       end Simple;

A package may contain **attribute declarations**,
**variable declarations** and **case constructions**, as will be
described below.

When there is ambiguity between a project name and a package name,
the name always designates the project. To avoid possible confusion, it is
always a good idea to avoid naming a project with one of the
names allowed for packages or any name that starts with `gnat`.

.. index:: Renaming declaration
.. index:: Package renaming

.. rubric:: Package renaming

A package may be defined by a **renaming declaration**. The new package
renames a package declared in a different project file, and has the same
attributes as the package it renames. The name of the renamed package
must be the same as the name of the renaming package. The project must
contain a package declaration with this name, and the project
must appear in the context clause of the current project, or be its
base or
parent project. It is not possible to add or override attributes to the renaming
project. If you need to do so, you should use an **extending declaration**
(see below).

Packages that are renamed in other project files often come from project files
that have no sources: they are just used as templates. Any modification in the
template will be reflected automatically in all the project files that rename
a package from the template. This is a very common way to share settings
between projects.

.. index:: Package extension
.. index:: Extending declaration

.. _Package_Extension:

.. rubric:: Package extension

A package can also be defined by an **extending declaration**. This is
similar to a **renaming declaration**, except that it is possible to add or
override attributes.

::

      package_declaration ::= package_spec | package_renaming | package_extension

      package_spec ::=
        'package' <package_>simple_name 'is'
           { simple_declarative_item }
        'end' package_identifier ;

      package_renaming ::=
        'package' <package_>simple_name 'renames'
              <project_>simple_name.package_identifier ;

      package_extension ::=
        'package' <package_>simple_name 'extends'
              <project_>simple_name.package_identifier 'is'
           { simple_declarative_item }
        'end' package_identifier ;


.. index:: Expressions in project files

.. _Expressions:

Expressions
-----------

An expression is any value that can be assigned to an attribute or a
variable. It is either a literal value, or a construct requiring run-time
computation by the Project Manager. In a project file, the computed value of
an expression is either a string or a list of strings.

A string value is one of:

* A literal string, for instance ``"comm/my_proj.gpr"``
* The name of a variable that evaluates to a string (see :ref:`Variables`)
* The name of an attribute that evaluates to a string (see :ref:`Attributes`)
* An external reference (see :ref:`External_Values`)
* A concatenation of the above, as in ``"prefix_" & Var``.

A list of strings is one of the following:

* A parenthesized comma-separated list of zero or more string expressions, for
  instance ``(File_Name, "gnat.adc", File_Name & ".orig")`` or ``()``.
* The name of a variable that evaluates to a list of strings
* The name of an attribute that evaluates to a list of strings
* A concatenation of a list of strings and a string (as defined above), for
  instance ``("A", "B") & "C"``
* A concatenation of two lists of strings

The following is the grammar for expressions

::

      string_literal ::= "{string_element}"  --  Same as Ada

      string_expression ::= string_literal
          | <variable_>name
          | external_value
          | attribute_reference
          | ( string_expression { & string_expression } )

      string_list  ::= ( string_expression { , string_expression } )
         | <string_variable>_name
         | <string_>attribute_reference

      term ::= string_expression | string_list

      expression ::= term { & term }     --  Concatenation

Concatenation involves strings and list of strings. As soon as a list of
strings is involved, the result of the concatenation is a list of strings. The
following Ada declarations show the existing operators:

  .. code-block:: ada

       function "&" (X : String;      Y : String)      return String;
       function "&" (X : String_List; Y : String)      return String_List;
       function "&" (X : String_List; Y : String_List) return String_List;


Here are some specific examples:

  .. code-block:: ada

       List := () & File_Name; --  One string in this list
       List2 := List & (File_Name & ".orig"); -- Two strings
       Big_List := List & Lists2;  --  Three strings
       Illegal := "gnat.adc" & List2;  --  Illegal, must start with list


.. index:: Built-in function

.. _Builtin_Functions:

Built-in Functions
------------------

Built-in functions may be used in expressions. The names of built-in functions
are not reserved words and may also be used as variable names.
In an expression, a built-in function is recognized if its name is immediately
followed by an open parenthesis ('(').


.. index:: Built-in function; Alternative

.. _Alternative:

The function ``Alternative``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. include:: ../share/legacy.rst

The function Alternative takes two arguments. It returns the second
argument if the first one is not the empty string.

  ::

      Alternative ("", "this is the default value")

      => ""

      Alternative ("x86_64-linux-gnu", "linux")

      => "linux"


.. index:: Built-in function; Default

.. _Default:

The function ``Default``
^^^^^^^^^^^^^^^^^^^^^^^^

.. include:: ../share/legacy.rst

The function Default takes two arguments. It returns the second
argument if the first one is the empty string.

  ::

      Default ("", "this is the default value")

      => "this is the default value"

      Default ("One", "this is the default value")

      => "One"


.. index:: Built-in function; External

.. _External_Values:

The function ``External``
^^^^^^^^^^^^^^^^^^^^^^^^^

An external value is an expression whose value is obtained from the command
that invoked the processing of the current project file (typically a
*gprbuild* command).

The syntax of a single string external value is::

    external_value ::= 'external' ( string_literal [, string_literal] )

The first string_literal is the name of the external variable, whose
value (a string) may be specified by an environment variable with this name,
or on the command line via the :samp:`-X{name}={value}` option.
The command line takes precedence if the name is defined in both contexts,
thus allowing the user to locally override an environment variable.
The second string_literal,
if present, is the default to use if there is no specification for this
external value either on the command line or in the environment.
If the value of the external variable is not obtained from an
environment variable or the command line, and the invocation of
the ``external`` function does not supply a second parameter, then
an error is reported.

An external reference may be part of a string expression or of a string
list expression, and can therefore appear in a variable declaration or
an attribute declaration.

.. _Scenario_Variable:

This construct is typically used to initialize *typed variables*, which
are then used in *case* constructions to control the value assigned to
attributes in various scenarios. Thus such variables are often called
*scenario variables*.


.. index:: Built-in function; External_As_List

The function ``External_As_List``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An external value is an expression whose value is obtained from the command
that invoked the processing of the current project file (typically a
*gprbuild* command).

The syntax for a string list external value is::

    external_value ::= 'external_as_list' ( string_literal , string_literal )

The first string_literal is the name of the external variable, with
the same interpretation as for the ``external`` function; it is
looked up first on the command line (as the name in a :samp:`-X{name}={value}`
option) and, if not so specified, then as an environment variable.
If it is not defined by either of these, then the function returns
an empty list.
The second string_literal is the separator between each component of the
string list.
An empty list is returned if the separator
is an empty string or if the external value is only one separator.

Any separator at the beginning or at the end of the external value is
discarded. Then, if there is no separator in the external value, the result is
a string list with only one string. Otherwise, any string between the beginning
and the first separator, between two consecutive separators and between the
last separator and the end are components of the string list.

Note the following differences between ``External`` and ``External_As_List``:

* The ``External_As_List`` function has no default value for the external
  variable

* The ``External_As_List`` function returns an empty list, and does not
  report an error, when the value of the external variable is undefined.

These differences reflect the different use cases for the two functions.
External variables evaluated by the ``External`` function are
often used for configuration control, and misspellings should be
detected as errors rather than silently returning the empty string. If the
user intended an empty string as the result when the external variable
was undefined, then this could easily be obtained:

  ::

     External ("SOME_VAR", "")

In contrast, the ``External_As_List`` function more typically is used
for external variables that may or may not have definitions (for example,
lists of options or paths) and then the desired result in the undefined
case is an empty list, not a reported error.

Here is an example of the ``External_As_List`` function:

  ::

      External_As_List ("SWITCHES", ",")

If the external value of ``SWITCHES`` is ``"-O2,-g"``,
the result is ``("-O2", "-g")``.

If the external value is ``",-O2,-g,"``,
the result is also ``("-O2", "-g")``.

if the external value is ``"-gnatv"``,
the result is ``("-gnatv")``.

If the external value is ``",,"``, the result is (``""``).

If the external value is ``","``, the result is ``()``, the empty string list.


.. index:: Built-in function; Filter_Out

.. _Filter_Out:

The function ``Filter_Out``
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. include:: ../share/legacy.rst

The function Filter_Out takes two arguments. The first argument must
be a list and the second one a simple string. The second argument is a
pattern (regular expression). Elements in the list matching the
pattern will be removed from the list.

  ::
      List := ("value1", "or", "another", "one");

Example removing all values containing the letter 'o':

  ::
      Filter_Out (List, ".*o.*")

      => ("value1")

Example removing all values:

  ::
      Filter_Out (List, ".*")

      => ()


.. index:: Built-in function; Item_At

.. _Item_At:

The function ``Item_At``
^^^^^^^^^^^^^^^^^^^^^^^^

.. include:: ../share/legacy.rst

The function Item_At takes two arguments. The first argument must
be a list and the second argument an integer image (simple
string). The number represents the index of the item to return from
the list. If the number is negative it is an index starting from the
end of the list. That is, "-1" is the last list item.

  ::
      List := ("one", "two", "three", "last");

      Item_At (List, "2")

      => "two"

      Item_At (List, "-1")

      => "last"


.. index:: Built-in function; Lower

.. _Lower:

The function ``Lower``
^^^^^^^^^^^^^^^^^^^^^^

.. include:: ../share/legacy.rst

Function Lower takes a single argument which can be a simple string or
a list. It returns the argument with all strings in lower case.

Example:

  ::

      Lower ("The Lower Built-In")

      => "the lower built-in"

Example with a list:

  ::
      List := ("One", "Two");

      Lower (List)

      => ("one", "two")


.. index:: Built-in function; Match

.. _Match:

The function ``Match``
^^^^^^^^^^^^^^^^^^^^^^

.. include:: ../share/legacy.rst

Function Match takes two mandatory arguments. The first argument is a
simple string or a list. The second argument is the pattern (regular
expression) to match. An optional third argument can be given which is
the replacement pattern for the matching strings.

Example:

  ::

      Match ("x86_64-linux-gnu", "linux")

      => "linux"

Example with a list and a replacement pattern:

  ::
      List := ("value1", "or", "another", "one");

      Match (List, "(.*r)", "r:\1")

      => ("r:or", "r:another")

In the above example we match all strings in List containing the
letter 'r' and the result is formed with 'r:' as prefix concatenated
with the matching string.


.. index:: Built-in function; Remove_Prefix

.. _Remove_Prefix:

The function ``Remove_Prefix``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. include:: ../share/legacy.rst

The function Remove_Prefix takes two arguments. The first argument can
be a simple string or a list. The second argument is a simple string
representing the prefix to remove if present. The prefix is removed
from the simple string or from each element of the list.

  ::

      List := ("libone", "two", "libthree")

      Remove_Prefix (List, "lib")

      => ("one", "two", "three")

      Remove_Prefix ("libZ.so", "lib")

      => "Z.so"


.. index:: Built-in function; Remove_Suffix

.. _Remove_Suffix:

The function ``Remove_Suffix``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. include:: ../share/legacy.rst

The function Remove_Suffix takes two arguments. The first argument can
be a simple string or a list. The second argument is a simple string
representing the suffix to remove if present. The suffix is removed
from the simple string or from each element of the list.

  ::

      List := ("libone", "two", "libthree")

      Remove_Suffix (List, "one")

      => ("lib", "two", "libthree")

      Remove_Suffix ("libZ.so", ".so")

      => "libZ"


.. index:: Built-in function; Split

.. _Split:

The function ``Split``
^^^^^^^^^^^^^^^^^^^^^^

Function Split takes two single string parameters and return a string list.

Example:

  ::

      Split ("-gnatf,-gnatv", ",")

      => ("-gnatf", "gnatv")

The first string argument is the string to be split. The second argument is
the separator. Each occurrence of the separator in the first argument is a place
where it is split. If the first argument is an empty string or contains only
occurrences of the separator, then the result is an empty string list.
If the argument does not contains any occurrence of the separator, then the
result is a list with only one string: the first argument. Empty strings are
not included in the result.

  ::

      Split ("-gnatf   -gnatv", " ")

      => ("-gnatf", "gnatv")


.. index:: Built-in function; Upper

.. _Upper:

The function ``Upper``
^^^^^^^^^^^^^^^^^^^^^^

.. include:: ../share/legacy.rst

Function Upper takes a single argument which can be a simple string or
a list. It returns the argument with all strings in upper case.

Example:

  ::

      Upper ("The Upper Built-In")

      => "THE UPPER BUILT-IN"

Example with a list:

  ::
      List := ("One", "Two");

      Upper (List)

      => ("ONE", "TWO")


.. index:: Type declaration

.. _Typed_String_Declaration:

Typed String Declaration
------------------------

A **type declaration** introduces a discrete set of string literals.
If a string variable is declared to have this type, its value
is restricted to the given set of literals. These are the only named
types in project files. A type declaration may only appear at the project
level, not inside a package.

::

     typed_string_declaration ::=
       'type' <typed_string_>simple_name 'is' ( string_literal {, string_literal} );

The string literals in the list are case sensitive and must all be different.
They may include any graphic characters allowed in Ada, including spaces.
Here is an example of a string type declaration:

  .. code-block:: ada

       type OS is ("GNU/Linux", "Unix", "Windows", "VMS");

Variables of a string type are called **typed variables**; all other
variables are called **untyped variables**. Typed variables are
particularly useful in `case` constructions, to support conditional
attribute declarations. (See :ref:`Case_Constructions`).

A string type may be referenced by its name if it has been declared in the same
project file, or by an expanded name whose prefix is the name of the project
in which it is declared.

.. index:: Variables in project files

.. _Variables:

Variables
---------

**Variables** store values (strings or list of strings) and can appear
as part of an expression. The declaration of a variable creates the
variable and assigns the value of the expression to it. The name of the
variable is available immediately after the assignment symbol, if you
need to reuse its old value to compute the new value. Before the completion
of its first declaration, the value of a variable defaults to the empty
string (``""``).

A **typed** variable can be used as part of a **case** expression to
compute the value, but it can only be declared once in the project file,
so that all case constructions see the same value for the variable. This
provides more consistency and makes the project easier to understand.
The syntax for its declaration is identical to the Ada syntax for an
object declaration. In effect, a typed variable acts as a constant.

An **untyped** variable can be declared and overridden multiple times
within the same project. It is declared implicitly through an Ada
assignment. The first declaration establishes the kind of the variable
(string or list of strings) and successive declarations must respect
the initial kind. Assignments are executed in the order in which they
appear, so the new value replaces the old one and any subsequent reference
to the variable uses the new value.

A variable may be declared at the project file level, or within a package.

::

     typed_variable_declaration ::=
       <typed_variable_>simple_name : <typed_string_>name := string_expression;

     variable_declaration ::= <variable_>simple_name := expression;

Here are some examples of variable declarations:

  .. code-block:: gpr

       This_OS : OS := external ("OS"); --  a typed variable declaration
       That_OS := "GNU/Linux";          --  an untyped variable declaration

       Name      := "readme.txt";
       Save_Name := Name & ".saved";

       Empty_List := ();
       List_With_One_Element := ("-gnaty");
       List_With_Two_Elements := List_With_One_Element & "-gnatg";
       Long_List := ("main.ada", "pack1_.ada", "pack1.ada", "pack2_.ada");

A **variable reference** may take several forms:

* The simple variable name, for a variable in the current package (if any)
  or in the current project
* An expanded name, whose prefix is a context name.

A **context** may be one of the following:

* The name of an existing package in the current project
* The name of an imported project of the current project
* The name of a direct or indirect base project (i.e., a project extended by the current
  project, either directly or indirectly)
* An expanded name whose prefix is an imported/parent project name, and
  whose selector is a package name in that project.

.. index:: Case construction

.. _Case_Constructions:

Case Constructions
------------------

A **case** construction is used in a project file to effect conditional
behavior. Through this construction, you can set the value of attributes
and variables depending on the value previously assigned to a typed
variable.

All choices in a choice list must be distinct. Unlike Ada, the choice
lists of all alternatives do not need to include all values of the type.
An `others` choice must appear last in the list of alternatives.

The syntax of a ``case`` construction is based on the Ada case construction
(although the ``null`` declaration for empty alternatives is optional).

The case expression must be a string variable, either typed or not, whose value
is often given by an external reference (see :ref:`External_Values`).

Each alternative starts with the reserved word ``when``, either a list of
literal strings separated by the ``"|"`` character or the reserved word
``others``, and the ``"=>"`` token.
When the case expression is a typed string variable, each literal string must
belong to the string type that is the type of the case variable.
After each ``=>``, there are zero or more declarations.  The only
declarations allowed in a case construction are other case constructions,
attribute declarations, and variable declarations. String type declarations and
package declarations are not allowed. Variable declarations are restricted to
variables that have already been declared before the case construction.

::

     case_construction ::=
       'case' <variable_>name 'is' {case_item} 'end' 'case' ;

     case_item ::=
       'when' discrete_choice_list =>
         {case_declaration
           | attribute_declaration
           | variable_declaration
           | empty_declaration}

     discrete_choice_list ::= string_literal {| string_literal} | 'others'

Here is a typical example, with a typed string variable:

  .. code-block:: gpr

       project MyProj is
          type OS_Type is ("GNU/Linux", "Unix", "Windows", "VMS");
          OS : OS_Type := external ("OS", "GNU/Linux");

          package Compiler is
            case OS is
              when "GNU/Linux" | "Unix" =>
                for Switches ("Ada")
                    use ("-gnath");
              when "Windows" =>
                for Switches ("Ada")
                    use ("-gnatP");
              when others =>
                null;
            end case;
          end Compiler;
       end MyProj;
