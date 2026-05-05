Attributes communicate build properties to GPR tools. They are declared with
the ``for … use`` syntax described in :ref:`Project_File_Language`; this
chapter lists all predefined attributes and their semantics.

**Default values**

Every attribute has a default value that applies when no declaration is
present. The default is indicated in each attribute's description below.

**Interaction with the configuration project**

When an attribute is declared in the configuration project but not in the
user project, the user project inherits the configuration value.

When a single-value attribute is declared in both, the user project's
declaration takes precedence.

For list attributes marked **configuration concatenable**, the final value
is the concatenation of the configuration project's value followed by the
user project's value.

**Reading attribute values**

Attribute values may be referenced in expressions anywhere in a project file.
If an attribute has not been set, its value defaults as described in each
attribute's entry. For the reference syntax see :ref:`Project_File_Language`.

**How to read the attribute descriptions**

Each attribute entry below indicates:

* **Type** — ``single`` (string) or ``list`` (string list).
* **Read-only** — the attribute is set by the build system; user declarations
  are forbidden.
* **Indexed by** — the kind of index accepted; see :ref:`Project_File_Language`
  for the full description of each index kind. Possible values:

  * *language* — case-insensitive language identifier
  * *file name* — simple file name without directory components
  * *source glob* — simple file name or glob pattern; case sensitivity is
    host-dependent
  * *source glob or language* — either a source glob or a language identifier;
    resolution priority: exact file name, glob, language, ``others``
  * *unit* — Ada unit name, case-insensitive
  * *string* — arbitrary string key
  * *external reference* — name of an external variable

* **Others index allowed** — the ``others`` index is accepted as a catch-all
  for this attribute.
* **Configuration concatenable** — for list attributes, the final value is the
  configuration value concatenated with the user value.
* **Inheritance** — by default, attributes are inherited from extended projects.
  Deviations are noted as *not inherited from extended* or *concatenated from
  extended*.

In addition to the attributes listed here, individual tools may define their
own attributes in standard or tool-specific packages; refer to each tool's
documentation.
