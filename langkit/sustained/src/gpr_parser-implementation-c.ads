
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--










with Ada.Exceptions;                  use Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Conversion;

with System;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Gpr_Parser_Support.Slocs; use Gpr_Parser_Support.Slocs;
with Gpr_Parser_Support.Text;  use Gpr_Parser_Support.Text;

with Gpr_Parser.Common;   use Gpr_Parser.Common;




--  Internal package: defines data types and subprograms to provide the
--  implementation of the exported C API (see the corresponding C header file).

private package Gpr_Parser.Implementation.C is

   subtype gpr_analysis_context is Internal_Context;
   --  This type represents a context for all source analysis. This is the
   --  first type you need to create to use Gpr_Parser. It will contain the
   --  results of all analysis, and is the main holder for all the data.
   --
   --  You can create several analysis contexts if you need to, which enables
   --  you, for example to:
   --
   --  * analyze several different projects at the same time;
   --
   --  * analyze different parts of the same projects in parallel.
   --
   --  In the current design, contexts always keep all of their analysis units
   --  allocated. If you need to get this memory released, the only option at
   --  your disposal is to destroy your analysis context instance.
   --
   --  This structure is partially opaque: some fields are exposed to allow
   --  direct access, for performance concerns.

   subtype gpr_analysis_unit is Internal_Unit;
   --  This type represents the analysis of a single file.
   --
   --  This type has strong-reference semantics and is ref-counted.
   --  Furthermore, a reference to a unit contains an implicit reference to the
   --  context that owns it. This means that keeping a reference to a unit will
   --  keep the context and all the unit it contains allocated.
   --
   --  This structure is partially opaque: some fields are exposed to allow
   --  direct access, for performance concerns.

   type gpr_base_node is new System.Address;
   --  Data type for all nodes. Nodes are assembled to make up a tree.  See the
   --  node primitives below to inspect such trees.
   --
   --  Unlike for contexts and units, this type has weak-reference semantics:
   --  keeping a reference to a node has no effect on the decision to keep the
   --  unit that it owns allocated. This means that once all references to the
   --  context and units related to a node are dropped, the context and its
   --  units are deallocated and the node becomes a stale reference: most
   --  operations on it will raise a ``Stale_Reference_Error``.
   --
   --  Note that since reparsing an analysis unit deallocates all the nodes it
   --  contains, this operation makes all reference to these nodes stale as
   --  well.

   type gpr_node_kind_enum is new int;
   --  Kind of AST nodes in parse trees.

   



subtype gpr_base_entity is Internal_Entity;
type gpr_base_entity_Ptr is access Internal_Entity;




   type gpr_symbol_type is record
      Data, Bounds : System.Address;
   end record
      with Convention => C;
   --  Reference to a symbol. Symbols are owned by analysis contexts, so they
   --  must not outlive them. This type exists only in the C API, and roughly
   --  wraps the corresponding Ada type (an array fat pointer).

   subtype gpr_string_type is String_Type;

   --  Helper data structures for source location handling

   type gpr_source_location is record
      Line   : Unsigned_32;
      Column : Unsigned_16;
   end record
     with Convention => C;

   type gpr_source_location_range is record
      Start_S, End_S : gpr_source_location;
   end record
     with Convention => C;

   type gpr_text is record
      Chars  : System.Address;
      --  Address for the content of the string.

      Length : size_t;
      --  Size of the string (in characters).

      Is_Allocated : int;
   end record
     with Convention => C;
   --  String encoded in UTF-32 (native endianness).

   type gpr_big_integer is new System.Address;
   --  Arbitrarily large integer.

   type gpr_token is record
      Context                   : gpr_analysis_context;
      Token_Data                : Token_Data_Handler_Access;
      Token_Index, Trivia_Index : int;

      Kind       : int;
      Text       : gpr_text;
      Sloc_Range : gpr_source_location_range;
   end record
     with Convention => C;
   --  Reference to a token in an analysis unit.

   type gpr_diagnostic is record
      Sloc_Range : gpr_source_location_range;
      Message    : gpr_text;
      --  When the API returns a diagnostic, it is up to the caller to free the
      --  message string.
   end record
     with Convention => C;
   --  Diagnostic for an analysis unit: cannot open the source file, parsing
   --  error, ...

   type gpr_exception_kind is (
      Exception_File_Read_Error, Exception_Bad_Type_Error, Exception_Out_Of_Bounds_Error, Exception_Invalid_Input, Exception_Invalid_Symbol_Error, Exception_Invalid_Unit_Name_Error, Exception_Native_Exception, Exception_Precondition_Failure, Exception_Property_Error, Exception_Template_Args_Error, Exception_Template_Format_Error, Exception_Template_Instantiation_Error, Exception_Stale_Reference_Error, Exception_Syntax_Error, Exception_Unknown_Charset, Exception_Malformed_Tree_Error
   ) with Convention => C;
   --  Enumerated type describing all possible exceptions that need to be
   --  handled in the C bindings.

   type gpr_exception is record
      Kind : gpr_exception_kind;
      --  The kind of this exception.

      Information : chars_ptr;
      --  Message and context information associated with this exception.
   end record;
   --  Holder for native exceptions-related information.  Memory management for
   --  this and all the fields is handled by the library: one just has to make
   --  sure not to keep references to it.
   --
   --  .. TODO: For the moment, this structure contains already formatted
   --     information, but depending on possible future Ada runtime
   --     improvements, this might change.

   type gpr_exception_Ptr is access gpr_exception;

   type gpr_bool is new Unsigned_8;
   subtype uint32_t is Unsigned_32;

      subtype gpr_analysis_unit_kind is Analysis_Unit_Kind;
      subtype gpr_lookup_kind is Lookup_Kind;
      subtype gpr_designated_env_kind is Designated_Env_Kind;
      subtype gpr_grammar_rule is Grammar_Rule;

   procedure Free (Address : System.Address)
     with Export        => True,
          Convention    => C,
          External_Name => "gpr_free";
   --  Free dynamically allocated memory.
   --
   --  This is a helper to free objects from dynamic languages.
   --  Helper to free objects in dynamic languages

   procedure gpr_destroy_text (T : access gpr_text)
     with Export        => True,
          Convention    => C,
          External_Name => "gpr_destroy_text";
   --  If this text object owns the buffer it references, free this buffer.
   --
   --  Note that even though this accepts a pointer to a text object, it does
   --  not deallocates the text object itself but rather the buffer it
   --  references.

   procedure gpr_symbol_text
     (Symbol : access gpr_symbol_type; Text : access gpr_text)
      with Export, Convention => C,
           External_Name => "gpr_symbol_text";
   --  Return the text associated to this symbol.

   function gpr_create_big_integer
     (Text : access gpr_text) return gpr_big_integer
      with Export, Convention => C,
           External_Name => "gpr_create_big_integer";
   --  Create a big integer from its string representation (in base 10).

   procedure gpr_big_integer_text
     (Bigint : gpr_big_integer; Text : access gpr_text)
      with Export, Convention => C,
           External_Name => "gpr_big_integer_text";
   --  Return the string representation (in base 10) of this big integer.

   procedure gpr_big_integer_decref
     (Bigint : gpr_big_integer)
      with Export, Convention => C,
           External_Name => "gpr_big_integer_decref";
   --  Decrease the reference count for this big integer.

   procedure gpr_get_versions
     (Version, Build_Date : access chars_ptr)
      with Export, Convention => C,
           External_Name => "gpr_get_versions";
   --  Allocate strings to represent the library version number and build date
   --  and put them in Version/Build_Date. Callers are expected to call free()
   --  on the returned string once done.

   function gpr_create_string
     (Content : System.Address; Length : int) return gpr_string_type
      with Export, Convention => C,
           External_Name => "gpr_create_string";
   --  Create a string value from its content (UTF32 with native endianity).
   --
   --  Note that the CONTENT buffer argument is copied: the returned value does
   --  not contain a reference to it.

   procedure gpr_string_dec_ref (Self : gpr_string_type)
      with Export, Convention => C,
           External_Name => "gpr_string_dec_ref";
   --  Decrease the reference count for this string.

   ------------------
   -- File readers --
   ------------------

   type gpr_file_reader is new System.Address;
   --  Interface to override how source files are fetched and decoded.

   type gpr_file_reader_destroy_callback is access procedure
     (Data : System.Address)
      with Convention => C;
   --  Callback type for functions that are called when destroying a file
   --  reader.

   type gpr_file_reader_read_callback is access procedure
     (Data       : System.Address;
      Filename   : chars_ptr;
      Charset    : chars_ptr;
      Read_BOM   : int;
      Buffer     : access gpr_text;
      Diagnostic : access gpr_diagnostic)
      with Convention => C;
   --  Callback type for functions that are called to fetch the decoded source
   --  buffer for a requested filename.

   --------------------
   -- Event handlers --
   --------------------

   type gpr_event_handler is new System.Address;
   --  Interface to handle events sent by the analysis context.

   type gpr_event_handler_unit_requested_callback is access procedure
     (Data               : System.Address;
      Context            : gpr_analysis_context;
      Name               : access constant gpr_text;
      From               : gpr_analysis_unit;
      Found              : gpr_bool;
      Is_Not_Found_Error : gpr_bool)
      with Convention => C;
   --  Callback that will be called when a unit is requested from the context
   --  ``Context``.
   --
   --  ``Name`` is the name of the requested unit.
   --
   --  ``From`` is the unit from which the unit was requested.
   --
   --  ``Found`` indicates whether the requested unit was found or not.
   --
   --  ``Is_Not_Found_Error`` indicates whether the fact that the unit was not
   --  found is an error or not.
   --
   --  .. warning:: The interface of this callback is probably subject to
   --     change, so should be treated as experimental.

   type gpr_event_handler_unit_parsed_callback is access procedure
     (Data     : System.Address;
      Context  : gpr_analysis_context;
      Unit     : gpr_analysis_unit;
      Reparsed : gpr_bool)
      with Convention => C;
   --  Callback that will be called when any unit is parsed from the context
   --  ``Context``.
   --
   --  ``Unit`` is the resulting unit.
   --
   --  ``Reparsed`` indicates whether the unit was reparsed, or whether it was
   --  the first parse.

   type gpr_event_handler_destroy_callback is access procedure
     (Data : System.Address)
      with Convention => C;
   --  Callback type for functions that are called when destroying an event
   --  handler.

   --------------------
   -- Unit providers --
   --------------------

   type gpr_unit_provider is new System.Address;
   --  Interface to fetch analysis units from a name and a unit kind.
   --
   --  The unit provider mechanism provides an abstraction which assumes that
   --  to any couple (unit name, unit kind) we can associate at most one source
   --  file. This means that several couples can be associated to the same
   --  source file, but on the other hand, only one one source file can be
   --  associated to a couple.
   --
   --  This is used to make the semantic analysis able to switch from one
   --  analysis units to another.
   --
   --  See the documentation of each unit provider for the exact semantics of
   --  the unit name/kind information.

   -------------------------
   -- Analysis primitives --
   -------------------------

   function gpr_allocate_analysis_context
     return gpr_analysis_context
     with Export,
          Convention    => C,
          External_name => "gpr_allocate_analysis_context";
   --  Allocate a new analysis context.

   procedure gpr_initialize_analysis_context
     (Context       : gpr_analysis_context;
      Charset       : chars_ptr;
      File_Reader   : gpr_file_reader;
      Unit_Provider : gpr_unit_provider;
      Event_Handler : gpr_event_handler;
      With_Trivia   : int;
      Tab_Stop      : int)
      with Export,
           Convention    => C,
           External_name => "gpr_initialize_analysis_context";
   --  Initialize an analysis context. Must be called right after
   --  ``Allocate_Context`` on its result.
   --
   --  Having separate primitives for allocation/initialization allows library
   --  bindings to have a context wrapper (created between the two calls) ready
   --  when callbacks that happen during context initialization (for instance
   --  "unit parsed" events).

   function gpr_context_incref
     (Context : gpr_analysis_context)
      return gpr_analysis_context
      with Export        => True,
           Convention    => C,
           External_name => "gpr_context_incref";
   --  Increase the reference count to an analysis context. Return the
   --  reference for convenience.

   procedure gpr_context_decref
     (Context : gpr_analysis_context)
      with Export        => True,
           Convention    => C,
           External_name => "gpr_context_decref";
   --  Decrease the reference count to an analysis context. Destruction happens
   --  when the ref-count reaches 0.

   function gpr_context_symbol
     (Context : gpr_analysis_context;
      Text    : access gpr_text;
      Symbol  : access gpr_symbol_type) return int
      with Export, Convention => C,
           External_name => "gpr_context_symbol";
   --  If the given string is a valid symbol, yield it as a symbol and return
   --  true. Otherwise, return false.

   procedure gpr_context_discard_errors_in_populate_lexical_env
     (Context : gpr_analysis_context;
      Discard : int)
      with Export        => True,
           Convention    => C,
           External_name => "gpr_context_discard_errors_in_populate_lexical_env";
   --  Debug helper. Set whether ``Property_Error`` exceptions raised in
   --  ``Populate_Lexical_Env`` should be discarded. They are by default.

   function gpr_get_analysis_unit_from_file
     (Context           : gpr_analysis_context;
      Filename, Charset : chars_ptr;
      Reparse           : int;
      Rule              : gpr_grammar_rule)
      return gpr_analysis_unit
      with Export        => True,
           Convention    => C,
           External_name =>
              "gpr_get_analysis_unit_from_file";
   --  Create a new analysis unit for ``Filename`` or return the existing one
   --  if any. If ``Reparse`` is true and the analysis unit already exists,
   --  reparse it from ``Filename``.
   --
   --  ``Rule`` controls which grammar rule is used to parse the unit.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as file opening, decoding, lexing or parsing
   --  failure, return an analysis unit anyway: errors are described as
   --  diagnostics of the returned analysis unit.

   function gpr_get_analysis_unit_from_buffer
     (Context           : gpr_analysis_context;
      Filename, Charset : chars_ptr;
      Buffer            : chars_ptr;
      Buffer_Size       : size_t;
      Rule              : gpr_grammar_rule)
      return gpr_analysis_unit
      with Export        => True,
           Convention    => C,
           External_name =>
              "gpr_get_analysis_unit_from_buffer";
   --  Create a new analysis unit for ``Filename`` or return the existing one
   --  if any. Whether the analysis unit already exists or not, (re)parse it
   --  from the source code in ``Buffer``.
   --
   --  ``Rule`` controls which grammar rule is used to parse the unit.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as file opening, decoding, lexing or parsing
   --  failure, return an analysis unit anyway: errors are described as
   --  diagnostics of the returned analysis unit.


   procedure gpr_unit_root
     (Unit     : gpr_analysis_unit;
      Result_P : gpr_base_entity_Ptr)
      with Export        => True,
           Convention    => C,
           External_name => "gpr_unit_root";
   --  Return the root node for this unit, or ``NULL`` if there is none.

   procedure gpr_unit_first_token
     (Unit  : gpr_analysis_unit;
      Token : access gpr_token)
      with Export        => True,
           Convention    => C,
           External_name => "gpr_unit_first_token";
   --  Return a reference to the first token scanned in this unit.

   procedure gpr_unit_last_token
     (Unit  : gpr_analysis_unit;
      Token : access gpr_token)
      with Export        => True,
           Convention    => C,
           External_name => "gpr_unit_last_token";
   --  Return a reference to the last token scanned in this unit.

   function gpr_unit_token_count
     (Unit : gpr_analysis_unit) return int
      with Export        => True,
           Convention    => C,
           External_Name => "gpr_unit_token_count";
   --  Return the number of tokens in this unit.

   function gpr_unit_trivia_count
     (Unit : gpr_analysis_unit) return int
      with Export        => True,
           Convention    => C,
           External_Name => "gpr_unit_trivia_count";
   --  Return the number of trivias in this unit. This is 0 for units that were
   --  parsed with trivia analysis disabled.

   procedure gpr_unit_lookup_token
     (Unit   : gpr_analysis_unit;
      Sloc   : access gpr_source_location;
      Result : access gpr_token)
      with Export        => True,
           Convention    => C,
           External_Name => "gpr_unit_lookup_token";
   --  Look for a token in this unit that contains the given source location.
   --  If this falls before the first token, return the first token. If this
   --  falls between two tokens, return the token that appears before. If this
   --  falls after the last token, return the last token. If there is no token
   --  in this unit, return no token.

   procedure gpr_unit_dump_lexical_env
     (Unit : gpr_analysis_unit)
      with Export        => True,
           Convention    => C,
           External_Name => "gpr_unit_dump_lexical_env";

   function gpr_unit_filename
     (Unit : gpr_analysis_unit)
      return chars_ptr
      with Export        => True,
           Convention    => C,
           External_name => "gpr_unit_filename";
   --  Return the filename this unit is associated to.
   --
   --  The returned string is dynamically allocated and the caller must free it
   --  when done with it.

   function gpr_unit_diagnostic_count
     (Unit : gpr_analysis_unit) return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "gpr_unit_diagnostic_count";
   --  Return the number of diagnostics associated to this unit.

   function gpr_unit_diagnostic
     (Unit         : gpr_analysis_unit;
      N            : unsigned;
      Diagnostic_P : access gpr_diagnostic) return int
      with Export        => True,
           Convention    => C,
           External_name => "gpr_unit_diagnostic";
   --  Get the Nth diagnostic in this unit and store it into ``*diagnostic_p``.
   --  Return zero on failure (when N is too big).

   function gpr_unit_context
     (Unit : gpr_analysis_unit)
      return gpr_analysis_context
      with Export        => True,
           Convention    => C,
           External_name => "gpr_unit_context";
   --  Return the context that owns this unit.

   procedure gpr_unit_reparse_from_file
     (Unit : gpr_analysis_unit; Charset : chars_ptr)
      with Export        => True,
           Convention    => C,
           External_name => "gpr_unit_reparse_from_file";
   --  Reparse an analysis unit from the associated file.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   procedure gpr_unit_reparse_from_buffer
     (Unit        : gpr_analysis_unit;
      Charset     : chars_ptr;
      Buffer      : chars_ptr;
      Buffer_Size : size_t)
      with Export        => True,
           Convention    => C,
           External_name => "gpr_unit_reparse_from_buffer";
   --  Reparse an analysis unit from a buffer.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   function gpr_unit_populate_lexical_env
     (Unit : gpr_analysis_unit
   ) return int
      with Export        => True,
           Convention    => C,
           External_name => "gpr_unit_populate_lexical_env";
   --  Create lexical environments for this analysis unit, according to the
   --  specifications given in the language spec.
   --
   --  If not done before, it will be automatically called during semantic
   --  analysis. Calling it before enables one to control where the latency
   --  occurs.
   --
   --  Depending on whether errors are discarded (see
   --  ``Discard_Errors_In_Populate_Lexical_Env``), return ``0`` on failure and
   --  ``1`` on success.

   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

   function gpr_is_equivalent
     (L, R : gpr_base_entity_Ptr) return gpr_bool
      with Export        => True,
           Convention    => C,
           External_name => "gpr_node_is_equivalent";
   --  Return whether the two nodes are equivalent.

   function gpr_hash
     (Node : gpr_base_entity_Ptr) return uint32_t
      with Export        => True,
           Convention    => C,
           External_name => "gpr_node_hash";
   --  Return a hash for the given node.

   function gpr_node_kind
     (Node : gpr_base_entity_Ptr) return gpr_node_kind_enum
      with Export        => True,
           Convention    => C,
           External_name => "gpr_node_kind";
   --  Return the kind of this node.

   procedure gpr_kind_name
     (Kind : gpr_node_kind_enum; Result : access gpr_text)
      with Export        => True,
           Convention    => C,
           External_name => "gpr_kind_name";
   --  Helper for textual dump: return the kind name for this node. The
   --  returned string is a copy and thus must be free'd by the caller.

   function gpr_node_unit
     (Node : gpr_base_entity_Ptr) return gpr_analysis_unit
      with Export => True,
           Convention => C,
           External_Name => "gpr_node_unit";
   --  Return the analysis unit that owns this node.

   function gpr_is_token_node
     (Node : gpr_base_entity_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "gpr_node_is_token_node";
   --  Return whether this node is a node that contains only a single token.

   function gpr_is_synthetic
     (Node : gpr_base_entity_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "gpr_node_is_synthetic";
   --  Return whether this node is synthetic.

   procedure gpr_node_image
     (Node : gpr_base_entity_Ptr; Result : access gpr_text)
      with Export        => True,
           Convention    => C,
           External_name => "gpr_node_image";
   --  Return a representation of this node as a string.

   procedure gpr_node_text
     (Node : gpr_base_entity_Ptr;
      Text : access gpr_text)
      with Export, Convention => C,
           External_Name      => "gpr_node_text";
   --  Return the source buffer slice corresponding to the text that spans
   --  between the first and the last tokens of this node.
   --
   --  Note that this returns the empty string for synthetic nodes.

   procedure gpr_node_sloc_range
     (Node         : gpr_base_entity_Ptr;
      Sloc_Range_P : access gpr_source_location_range)
      with Export        => True,
           Convention    => C,
           External_name => "gpr_node_sloc_range";
   --  Return the spanning source location range for this node.
   --
   --  Note that this returns the sloc of the parent for synthetic nodes.

   procedure gpr_lookup_in_node
     (Node   : gpr_base_entity_Ptr;
      Sloc   : gpr_source_location;
      Result : gpr_base_entity_Ptr)
      with Export        => True,
           Convention    => C,
           External_name => "gpr_lookup_in_node";
   --  Return the bottom-most node from in ``Node`` and its children which
   --  contains ``Sloc``, or ``NULL`` if there is none.

   function gpr_node_children_count
     (Node : gpr_base_entity_Ptr) return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "gpr_node_children_count";
   --  Return the number of children in this node.

   function gpr_node_child
     (Node    : gpr_base_entity_Ptr;
      N       : unsigned;
      Child_P : gpr_base_entity_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "gpr_node_child";
   --  Return the Nth child for in this node's fields and store it into
   --  ``*child_p``.  Return zero on failure (when ``N`` is too big).

   function gpr_text_to_locale_string
     (Text : gpr_text) return System.Address
      with Export        => True,
           Convention    => C,
           External_name => "gpr_text_to_locale_string";
   --  Encode some text using the current locale. The result is dynamically
   --  allocated: it is up to the caller to free it when done with it.
   --
   --  This is a development helper to make it quick and easy to print token
   --  and diagnostic text: it ignores errors (when the locale does not support
   --  some characters). Production code should use real conversion routines
   --  such as libiconv's in order to deal with UTF-32 texts.

   ------------------
   -- File readers --
   ------------------

   function gpr_create_file_reader
     (Data         : System.Address;
      Destroy_Func : gpr_file_reader_destroy_callback;
      Read_Func    : gpr_file_reader_read_callback) return gpr_file_reader
      with Export        => True,
           Convention    => C,
           External_name => "gpr_create_file_reader";
   --  Create a file reader. When done with it, the result must be passed to
   --  ``gpr_dec_ref_file_reader``.
   --
   --  Pass as ``data`` a pointer to hold your private data: it will be passed
   --  to all callbacks below.
   --
   --  ``destroy`` is a callback that is called by ``gpr_dec_ref_file_reader``
   --  to leave a chance to free resources that ``data`` may hold.
   --
   --  ``read`` is a callback. For a given filename/charset and whether to read
   --  the BOM (Byte Order Mark), it tries to fetch the contents of the source
   --  file, returned in ``Contents``. If there is an error, it must return it
   --  in ``Diagnostic`` instead.

   procedure gpr_dec_ref_file_reader
     (File_Reader : gpr_file_reader)
      with Export        => True,
           Convention    => C,
           External_name =>
              "gpr_dec_ref_file_reader";
   --  Release an ownership share for this file reader. This destroys the file
   --  reader if there are no shares left.

   


   --------------------
   -- Event handlers --
   --------------------

   function gpr_create_event_handler
     (Data                : System.Address;
      Destroy_Func        : gpr_event_handler_destroy_callback;
      Unit_Requested_Func : gpr_event_handler_unit_requested_callback;
      Unit_Parsed_Func    : gpr_event_handler_unit_parsed_callback)
      return gpr_event_handler
      with Export        => True,
           Convention    => C,
           External_name => "gpr_create_event_handler";
   --  Create an event handler. When done with it, the result must be passed to
   --  ``gpr_dec_ref_event_handler``.
   --
   --  Pass as ``data`` a pointer to hold your private data: it will be passed
   --  to all callbacks below.
   --
   --  ``destroy`` is a callback that is called by
   --  ``gpr_dec_ref_event_handler`` to leave a chance to free resources that
   --  ``data`` may hold.
   --
   --  ``unit_requested`` is a callback that will be called when a unit is
   --  requested.
   --
   --  .. warning:: Please note that the unit requested callback can be called
   --     *many* times for the same unit, so in all likeliness, those events
   --     should be filtered if they're used to forward diagnostics to the
   --     user.
   --
   --  ``unit_parsed`` is a callback that will be called when a unit is parsed.

   procedure gpr_dec_ref_event_handler
     (Handler : gpr_event_handler)
      with Export        => True,
           Convention    => C,
           External_name =>
              "gpr_dec_ref_event_handler";
   --  Release an ownership share for this event handler. This destroys the
   --  event handler if there are no shares left.

   


   --------------------
   -- Unit providers --
   --------------------

   procedure gpr_dec_ref_unit_provider
     (Provider : gpr_unit_provider)
      with Export        => True,
           Convention    => C,
           External_name =>
              "gpr_dec_ref_unit_provider";
   --  Release an ownership share for this unit provider. This destroys the
   --  unit provider if there are no shares left.

   


   ------------------
   -- Struct types --
   ------------------


   -----------------
   -- Array types --
   -----------------

         



subtype gpr_gpr_node_array is Internal_Entity_Array_Access;
type gpr_gpr_node_array_Ptr is access Internal_Entity_Array_Access;

function gpr_gpr_node_array_create (Length : int) return Internal_Entity_Array_Access
   with Export        => True,
        Convention    => C,
        External_name => "gpr_gpr_node_array_create";

procedure gpr_gpr_node_array_inc_ref (A : Internal_Entity_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "gpr_gpr_node_array_inc_ref";

procedure gpr_gpr_node_array_dec_ref (A : Internal_Entity_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "gpr_gpr_node_array_dec_ref";



   --------------------
   -- Iterator types --
   --------------------


   ----------
   -- Misc --
   ----------

   function gpr_get_last_exception return gpr_exception_Ptr
     with Export        => True,
          Convention    => C,
          External_Name => "gpr_get_last_exception";
   --  Return exception information for the last error that happened in the
   --  current thread. Will be automatically allocated on error and free'd on
   --  the next error.

   function gpr_exception_name
     (Kind : gpr_exception_kind) return chars_ptr
      with Export, Convention => C;
   --  Return the name of the given exception kind. Callers are responsible for
   --  free'ing the result.

   procedure Clear_Last_Exception;
   --  Free the information contained in Last_Exception

   procedure Set_Last_Exception (Exc : Exception_Occurrence);
   --  Free the information contained in Last_Exception and replace it with
   --  newly allocated information from Exc.

   procedure Set_Last_Exception (Id : Exception_Id; Message : String);
   --  Likewise, but put destructured exception information. This is useful to
   --  pass messages that are longer than what the Ada runtime accepts (i.e.
   --  allows to avoid truncated error messages).

   function gpr_token_kind_name (Kind : int) return chars_ptr
      with Export => True,
           Convention => C,
           External_Name => "gpr_token_kind_name";
   --  Return a human-readable name for a token kind.
   --
   --  The returned string is dynamically allocated and the caller must free it
   --  when done with it.
   --
   --  If the given kind is invalid, return ``NULL`` and set the last exception
   --  accordingly.

   procedure gpr_token_next
     (Token      : gpr_token;
      Next_Token : access gpr_token)
      with Export        => True,
           Convention    => C,
           External_name => "gpr_token_next";
   --  Return a reference to the next token in the corresponding analysis unit.

   procedure gpr_token_previous
     (Token          : gpr_token;
      Previous_Token : access gpr_token)
      with Export        => True,
           Convention    => C,
           External_name => "gpr_token_previous";
   --  Return a reference to the previous token in the corresponding analysis
   --  unit.

   function gpr_token_range_text
     (First, Last : gpr_token;
      Text        : access gpr_text) return int
      with Export => True,
           Convention => C,
           External_Name => "gpr_token_range_text";
   --  Compute the source buffer slice corresponding to the text that spans
   --  between the ``First`` and ``Last`` tokens (both included). This yields
   --  an empty slice if ``Last`` actually appears before ``First``. Put the
   --  result in ``RESULT``.
   --
   --  This returns ``0`` if ``First`` and ``Last`` don't belong to the same
   --  analysis unit. Return ``1`` if successful.

   function gpr_token_is_equivalent
     (Left  : gpr_token;
      Right : gpr_token) return gpr_bool
      with Export        => True,
           Convention    => C,
           External_name => "gpr_token_is_equivalent";
   --  Return whether ``L`` and ``R`` are structurally equivalent tokens. This
   --  means that their position in the stream won't be taken into account,
   --  only the kind and text of the token.

   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

   --  All these primitives return their result through an OUT parameter. They
   --  return a boolean telling whether the operation was successful (it can
   --  fail if the node does not have the proper type, for instance). When an
   --  AST node is returned, its ref-count is left as-is.

           
   

   
   

   function gpr_gpr_node_parent
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_gpr_node_parent";
   --  Return the syntactic parent for this node. Return null for the root
   --  node.

           
   

   
   

   function gpr_gpr_node_parents
     (Node : gpr_base_entity_Ptr;

         With_Self :
            
            gpr_bool;

      Value_P : access gpr_gpr_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_gpr_node_parents";
   --  Return an array that contains the lexical parents, this node included
   --  iff ``with_self`` is True. Nearer parents are first in the list.

           
   

   
   

   function gpr_gpr_node_children
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_gpr_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_gpr_node_children";
   --  Return an array that contains the direct lexical children.
   --
   --  .. warning:: This constructs a whole array every-time you call it, and
   --     as such is less efficient than calling the ``Child`` built-in.

           
   

   
   

   function gpr_gpr_node_token_start
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_token) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_gpr_node_token_start";
   --  Return the first token used to parse this node.

           
   

   
   

   function gpr_gpr_node_token_end
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_token) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_gpr_node_token_end";
   --  Return the last token used to parse this node.

           
   

   
   

   function gpr_gpr_node_child_index
     (Node : gpr_base_entity_Ptr;


      Value_P : access int) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_gpr_node_child_index";
   --  Return the 0-based index for Node in its parent's children.

           
   

   
   

   function gpr_gpr_node_previous_sibling
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_gpr_node_previous_sibling";
   --  Return the node's previous sibling, or null if there is no such sibling.

           
   

   
   

   function gpr_gpr_node_next_sibling
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_gpr_node_next_sibling";
   --  Return the node's next sibling, or null if there is no such sibling.

           
   

   
   

   function gpr_gpr_node_unit
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_analysis_unit) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_gpr_node_unit";
   --  Return the analysis unit owning this node.

           
   

   
   

   function gpr_gpr_node_is_ghost
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_gpr_node_is_ghost";
   --  Return whether the node is a ghost.
   --
   --  Unlike regular nodes, ghost nodes cover no token in the input source:
   --  they are logically located instead between two tokens. Both the
   --  ``token_start`` and the ``token_end`` of all ghost nodes is the token
   --  right after this logical position.

           
   

   
   

   function gpr_gpr_node_full_sloc_image
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_gpr_node_full_sloc_image";
   --  Return a string containing the filename + the sloc in GNU conformant
   --  format. Useful to create diagnostics from a node.

           
   

   
   

   function gpr_ada_access_subp_f_subp_kind
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_access_subp_f_subp_kind";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Ada_Entity_Kind_Function`,
   --  :ada:ref:`Ada_Entity_Kind_Procedure`

           
   

   
   

   function gpr_ada_access_subp_f_skips
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_access_subp_f_skips";
   

           
   

   
   

   function gpr_ada_pragma_f_skips
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_pragma_f_skips";
   

           
   

   
   

   function gpr_ada_use_f_skips
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_use_f_skips";
   

           
   

   
   

   function gpr_ada_with_f_has_limited
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_with_f_has_limited";
   

           
   

   
   

   function gpr_ada_with_f_has_private
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_with_f_has_private";
   

           
   

   
   

   function gpr_ada_with_f_packages
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_with_f_packages";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Identifier`, :ada:ref:`Prefix`

           
   

   
   

   function gpr_ada_generic_f_skips
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_generic_f_skips";
   

           
   

   
   

   function gpr_ada_library_item_f_generic_stub
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_library_item_f_generic_stub";
   

           
   

   
   

   function gpr_ada_library_item_f_separate
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_library_item_f_separate";
   

           
   

   
   

   function gpr_ada_library_item_f_main
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_library_item_f_main";
   

           
   

   
   

   function gpr_ada_main_f_name
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_main_f_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Identifier`, :ada:ref:`Prefix`

           
   

   
   

   function gpr_ada_pkg_f_has_private
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_pkg_f_has_private";
   

           
   

   
   

   function gpr_ada_subp_f_subp_kind
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_subp_f_subp_kind";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Ada_Entity_Kind_Function`,
   --  :ada:ref:`Ada_Entity_Kind_Procedure`

           
   

   
   

   function gpr_ada_prelude_f_context_clauses
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_prelude_f_context_clauses";
   

           
   

   
   

   function gpr_ada_prelude_f_library_item
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_prelude_f_library_item";
   

           
   

   
   

   function gpr_ada_separate_f_parent_name
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_separate_f_parent_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Identifier`, :ada:ref:`Prefix`

           
   

   
   

   function gpr_ada_with_formal_f_kind
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_with_formal_f_kind";
   

           
   

   
   

   function gpr_ada_with_formal_f_skips
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_ada_with_formal_f_skips";
   

           
   

   
   

   function gpr_all_qualifier_p_as_bool
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_all_qualifier_p_as_bool";
   --  Return whether this is an instance of AllQualifierPresent

           
   

   
   

   function gpr_attribute_decl_f_attr_name
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_attribute_decl_f_attr_name";
   

           
   

   
   

   function gpr_attribute_decl_f_attr_index
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_attribute_decl_f_attr_index";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Others_Designator`, :ada:ref:`String_Literal_At`

           
   

   
   

   function gpr_attribute_decl_f_expr
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_attribute_decl_f_expr";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Builtin_Function_Call`, :ada:ref:`String_Literal_At`,
   --  :ada:ref:`Terms`, :ada:ref:`Variable_Reference`

           
   

   
   

   function gpr_attribute_reference_f_attribute_name
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_attribute_reference_f_attribute_name";
   

           
   

   
   

   function gpr_attribute_reference_f_attribute_index
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_attribute_reference_f_attribute_index";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Others_Designator`, :ada:ref:`String_Literal`

           
   

   
   

   function gpr_builtin_function_call_f_function_name
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_builtin_function_call_f_function_name";
   

           
   

   
   

   function gpr_builtin_function_call_f_parameters
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_builtin_function_call_f_parameters";
   

           
   

   
   

   function gpr_case_construction_f_var_ref
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_case_construction_f_var_ref";
   

           
   

   
   

   function gpr_case_construction_f_items
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_case_construction_f_items";
   

           
   

   
   

   function gpr_case_item_f_choice
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_case_item_f_choice";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Others_Designator`, :ada:ref:`String_Literal`

           
   

   
   

   function gpr_case_item_f_decls
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_case_item_f_decls";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Attribute_Decl`, :ada:ref:`Case_Construction`,
   --  :ada:ref:`Empty_Decl`, :ada:ref:`Variable_Decl`

           
   

   
   

   function gpr_compilation_unit_f_project
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_compilation_unit_f_project";
   

           
   

   
   

   function gpr_prefix_f_prefix
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_prefix_f_prefix";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Identifier`, :ada:ref:`Prefix`

           
   

   
   

   function gpr_prefix_f_suffix
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_prefix_f_suffix";
   

           
   

   
   

   function gpr_limited_node_p_as_bool
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_limited_node_p_as_bool";
   --  Return whether this is an instance of LimitedPresent

           
   

   
   

   function gpr_package_decl_f_pkg_name
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_package_decl_f_pkg_name";
   

           
   

   
   

   function gpr_package_decl_f_pkg_spec
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_package_decl_f_pkg_spec";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Package_Renaming`, :ada:ref:`Package_Spec`

           
   

   
   

   function gpr_package_extension_f_extended_name
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_package_extension_f_extended_name";
   

           
   

   
   

   function gpr_package_renaming_f_renamed_name
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_package_renaming_f_renamed_name";
   

           
   

   
   

   function gpr_package_spec_f_extension
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_package_spec_f_extension";
   

           
   

   
   

   function gpr_package_spec_f_decls
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_package_spec_f_decls";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Attribute_Decl`, :ada:ref:`Case_Construction`,
   --  :ada:ref:`Empty_Decl`, :ada:ref:`Variable_Decl`

           
   

   
   

   function gpr_package_spec_f_end_name
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_package_spec_f_end_name";
   

           
   

   
   

   function gpr_private_node_p_as_bool
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_private_node_p_as_bool";
   --  Return whether this is an instance of PrivatePresent

           
   

   
   

   function gpr_project_f_context_clauses
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_project_f_context_clauses";
   

           
   

   
   

   function gpr_project_f_project_decl
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_project_f_project_decl";
   

           
   

   
   

   function gpr_project_declaration_f_qualifier
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_project_declaration_f_qualifier";
   

           
   

   
   

   function gpr_project_declaration_f_project_name
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_project_declaration_f_project_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Identifier`, :ada:ref:`Prefix`

           
   

   
   

   function gpr_project_declaration_f_extension
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_project_declaration_f_extension";
   

           
   

   
   

   function gpr_project_declaration_f_decls
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_project_declaration_f_decls";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Attribute_Decl`, :ada:ref:`Case_Construction`,
   --  :ada:ref:`Empty_Decl`, :ada:ref:`Package_Decl`,
   --  :ada:ref:`Typed_String_Decl`, :ada:ref:`Variable_Decl`

           
   

   
   

   function gpr_project_declaration_f_end_name
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_project_declaration_f_end_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Identifier`, :ada:ref:`Prefix`

           
   

   
   

   function gpr_project_extension_f_is_all
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_project_extension_f_is_all";
   

           
   

   
   

   function gpr_project_extension_f_path_name
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_project_extension_f_path_name";
   

           
   

   
   

   function gpr_string_literal_at_f_str_lit
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_string_literal_at_f_str_lit";
   

           
   

   
   

   function gpr_string_literal_at_f_at_lit
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_string_literal_at_f_at_lit";
   

           
   

   
   

   function gpr_terms_f_terms
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_terms_f_terms";
   

           
   

   
   

   function gpr_type_reference_f_var_type_name
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_type_reference_f_var_type_name";
   

           
   

   
   

   function gpr_typed_string_decl_f_type_id
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_typed_string_decl_f_type_id";
   

           
   

   
   

   function gpr_typed_string_decl_f_string_literals
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_typed_string_decl_f_string_literals";
   

           
   

   
   

   function gpr_variable_decl_f_var_name
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_variable_decl_f_var_name";
   

           
   

   
   

   function gpr_variable_decl_f_var_type
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_variable_decl_f_var_type";
   

           
   

   
   

   function gpr_variable_decl_f_expr
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_variable_decl_f_expr";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Builtin_Function_Call`, :ada:ref:`String_Literal_At`,
   --  :ada:ref:`Terms`, :ada:ref:`Variable_Reference`

           
   

   
   

   function gpr_variable_reference_f_variable_name
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_variable_reference_f_variable_name";
   

           
   

   
   

   function gpr_variable_reference_f_attribute_ref
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_variable_reference_f_attribute_ref";
   

           
   

   
   

   function gpr_with_decl_f_is_limited
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_with_decl_f_is_limited";
   

           
   

   
   

   function gpr_with_decl_f_path_names
     (Node : gpr_base_entity_Ptr;


      Value_P : access gpr_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "gpr_with_decl_f_path_names";
   


   ------------------------
   -- Conversion helpers --
   ------------------------

   --  The following conversion helpers are use by the various C bindings

   function Wrap (S : Source_Location) return gpr_source_location is
     ((Unsigned_32 (S.Line), Unsigned_16 (S.Column)));
   function Unwrap (S : gpr_source_location) return Source_Location is
     ((Line_Number (S.Line), Column_Number (S.Column)));

   function Wrap (S : Source_Location_Range) return gpr_source_location_range is
     ((Start_S => (Unsigned_32 (S.Start_Line), Unsigned_16 (S.Start_Column)),
       End_S   => (Unsigned_32 (S.End_Line),   Unsigned_16 (S.End_Column))));
   function Unwrap (S : gpr_source_location_range) return Source_Location_Range is
     ((Line_Number (S.Start_S.Line),
       Line_Number (S.End_S.Line),
       Column_Number (S.Start_S.Column),
       Column_Number (S.End_S.Column)));

   function Wrap (S : Unbounded_Wide_Wide_String) return gpr_text;

   function Wrap_Alloc (S : Text_Type) return gpr_text;
   function Wrap
     (S     : Text_Cst_Access;
      First : Positive;
      Last  : Natural) return gpr_text;

   function Wrap (T : Text_Cst_Access) return gpr_text is
     (if T = null
      then (Chars => System.Null_Address, Length => 0, Is_Allocated => 0)
      else (Chars => T.all'Address, Length => T.all'Length, Is_Allocated => 0));
   function Wrap (T : Text_Access) return gpr_text is
     (Wrap (Text_Cst_Access (T)));

   function Wrap_Big_Integer is new Ada.Unchecked_Conversion
     (Big_Integer_Type, gpr_big_integer);
   function Unwrap_Big_Integer is new Ada.Unchecked_Conversion
     (gpr_big_integer, Big_Integer_Type);

   --  Probably because the following conversions involve fat pointers, using
   --  the No_Strict_Aliasing pragma here has no effect. Silence the warning,
   --  since all read/writes for the pointed values are made through the "real"
   --  fat pointer (Symbol_Type) and not the fake one (gpr_symbol_type): strict
   --  aliasing issues should not happen.

   pragma Warnings (Off, "possible aliasing problem for type");
   function Wrap_Symbol is new Ada.Unchecked_Conversion
     (Symbol_Type, gpr_symbol_type);
   function Unwrap_Symbol is new Ada.Unchecked_Conversion
     (gpr_symbol_type, Symbol_Type);
   pragma Warnings (On, "possible aliasing problem for type");

   function Wrap is new Ada.Unchecked_Conversion
     (Bare_Gpr_Node, gpr_base_node);
   function Unwrap is new Ada.Unchecked_Conversion
     (gpr_base_node, Bare_Gpr_Node);

   function Wrap (Token : Token_Reference) return gpr_token;
   function Unwrap (Token : gpr_token) return Token_Reference;

   function Wrap_Private_File_Reader is new Ada.Unchecked_Conversion
     (Internal_File_Reader_Access, gpr_file_reader);
   function Unwrap_Private_File_Reader is new Ada.Unchecked_Conversion
     (gpr_file_reader, Internal_File_Reader_Access);

   function Wrap_Private_Event_Handler is new Ada.Unchecked_Conversion
     (Internal_Event_Handler_Access, gpr_event_handler);
   function Unwrap_Private_Event_Handler is new Ada.Unchecked_Conversion
     (gpr_event_handler, Internal_Event_Handler_Access);

   function Wrap_Private_Provider is new Ada.Unchecked_Conversion
     (Internal_Unit_Provider_Access, gpr_unit_provider);
   function Unwrap_Private_Provider is new Ada.Unchecked_Conversion
     (gpr_unit_provider, Internal_Unit_Provider_Access);

   function Convert is new Ada.Unchecked_Conversion
     (chars_ptr, System.Address);



end Gpr_Parser.Implementation.C;
