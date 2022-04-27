A project (and its packages) may have **attributes** that define
the project's properties.  Some attributes have values that are strings;
others have values that are string lists.

::

     attribute_declaration ::=
        simple_attribute_declaration | indexed_attribute_declaration

     simple_attribute_declaration ::= 'for' attribute_designator 'use' expression ;

     indexed_attribute_declaration ::=
       'for' *<indexed_attribute_>*simple_name ( string_literal) 'use' expression ;

     attribute_designator ::=
       <simple_attribute_>simple_name
       | <indexed_attribute_>simple_name ( string_literal )

There are two categories of attributes: **simple attributes**
and **indexed attributes**.
Each simple attribute has a default value: the empty string (for string
attributes) and the empty list (for string list attributes).
An attribute declaration defines a new value for an attribute, and overrides
the previous value. The syntax of a simple attribute declaration is similar to
that of an attribute definition clause in Ada.

Some attributes are indexed. These attributes are mappings whose
domain is a set of strings. They are declared one association
at a time, by specifying a point in the domain and the corresponding image
of the attribute.
Like untyped variables and simple attributes, indexed attributes
may be declared several times. Each declaration supplies a new value for the
attribute, and replaces the previous setting.

Here are some examples of attribute declarations:

  .. code-block:: gpr

       --  simple attributes
       for Object_Dir use "objects";
       for Source_Dirs use ("units", "test/drivers");

       --  indexed attributes
       for Body ("main") use "Main.ada";
       for Switches ("main.ada")
           use ("-v", "-gnatv");
       for Switches ("main.ada") use Builder'Switches ("main.ada") & "-g";

       --  indexed attributes copy (from package Builder in project Default)
       --  The package name must always be specified, even if it is the current
       --  package.
       for Default_Switches use Default.Builder'Default_Switches;

When an attribute is defined in the configuration project but not in the user
project, it is inherited in the user project.

When a single string attribute is defined in both the configuration project
and the user project, its value in the user project is as declared; the value
in the configuration project does not matter.

For string list attributes, there are two cases. Some of these attributes are
**configuration concatenable**. For these attributes, when they are declared
in both the configuration project and the user project, the final value is
the concatenation of the value in the configuration project with the value
in the user project. The configuration concatenable attributes are indicated
in the list below.

Attributes references may appear anywhere in expressions, and are used
to retrieve the value previously assigned to the attribute. If an attribute
has not been set in a given package or project, its value defaults to the
empty string or the empty list, with some exceptions.

::

    attribute_reference ::=
      attribute_prefix ' <simple_attribute>_simple_name [ (string_literal) ]

    attribute_prefix ::= 'project'
      | <project_>simple_name
      | package_identifier
      | <project_>simple_name . package_identifier

Here are some examples:

  ::

      project'Object_Dir
      Naming'Dot_Replacement
      Imported_Project'Source_Dirs
      Imported_Project.Naming'Casing
      Builder'Default_Switches ("Ada")

The exceptions to the empty defaults are:

* ``Object_Dir``: default is ``"."``
* ``Exec_Dir``: default is ``'Object_Dir``, that is, the value of attribute
  ``Object_Dir`` in the same project, declared or defaulted
* ``Source_Dirs``: default is ``(".")``

The prefix of an attribute may be:

* ``project`` for an attribute of the current project
* The name of an existing package of the current project
* The name of an imported project
* The name of a parent project that is extended by the current project
* An expanded name whose prefix is imported/base/parent project name,
  and whose selector is a package name

In the following sections, all predefined attributes are succinctly described,
first the project level attributes (that is, those attributes that are not in a
package), then the attributes in the different packages.

It is possible for different tools to dynamically create new packages with
attributes, or new attributes in predefined packages. These attributes are
not documented here.

The attributes under Configuration headings are usually found only in
configuration project files.

The characteristics of each attribute are indicated as follows:

* **Type of value**

  The value of an attribute may be a single string, indicated by the word
  "single", or a string list, indicated by the word "list".

* **Read-only**

  When the attribute is read-only -- that is when a declaration for
  the attribute is forbidden -- this is indicated by the "read-only".

* **Optional index**

  If an optional index is allowed in the value of the attribute (both single
  and list), this is indicated by the words "optional index".

* **Indexed attribute**

  An indexed attribute is indicated by the word "indexed".

* **Case-sensitivity of the index**

  For an indexed attribute, if the index is case-insensitive, this is indicated
  by the words "case-insensitive index".

* **File name index**

  For an indexed attribute, when the index is a file name, this is indicated by
  the words "file name index". The index may or may not be case-sensitive,
  depending on the platform.

* **others allowed in index**

  For an indexed attribute, if it is allowed to use **others** as the index,
  this is indicated by the words "others allowed".

  When **others** is used as the index of an indexed attribute, the value of
  the attribute indexed by **others** is used when no other index would apply.

* **configuration concatenable**

  For a string list attribute, the final value if the attribute is declared
  in both the configuration project and the user project is the concatenation
  of the two value, configuration then user.
