
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--









with Ada.Containers;
private with Ada.Finalization;
with Ada.Strings.Unbounded;

with GNATCOLL.Refcount;


with Gpr_Parser_Support.File_Readers; use Gpr_Parser_Support.File_Readers;
with Gpr_Parser_Support.Lexical_Envs; use Gpr_Parser_Support.Lexical_Envs;
with Gpr_Parser_Support.Symbols;      use Gpr_Parser_Support.Symbols;

with Gpr_Parser_Support.Token_Data_Handlers;
use Gpr_Parser_Support.Token_Data_Handlers;

with Gpr_Parser.Common; use Gpr_Parser.Common;
private with Gpr_Parser.Implementation;
private with Gpr_Parser.Debug;




--  This package provides types and primitives to analyze source files as
--  analysis units.
--
--  This is the entry point to parse and process a unit:
--
--  * First create an analysis context with
--    :ada:ref:`Gpr_Parser.Analysis.Create_Context`.
--
--  * Then get analysis units out of it using the ``Get_From_*`` functions. The
--    most used of them is :ada:ref:`Gpr_Parser.Analysis.Get_From_File`,
--    which allows you to get an analysis unit out of a file path.
--
--  .. code-block:: ada
--
--      with Libadalang.Analysis;
--
--      procedure Main is
--         package Lib renames Gpr_Parser.Analysis;
--
--         Context : constant Lib.Analysis_Context := Lib.Create_Context;
--         Unit    : constant Lib.Analysis_Unit :=
--           Context.Get_From_File ("/path/to/source/file");
--      begin
--         Unit.Print;
--      end Main;


package Gpr_Parser.Analysis is

   use Support.Diagnostics, Support.Slocs, Support.Text;

   type Analysis_Context is tagged private;
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

   type Analysis_Unit is new Gpr_Parser_Support.Text.Text_Buffer_Ifc with private;
   --  This type represents the analysis of a single file.
   --
   --  This type has strong-reference semantics and is ref-counted.
   --  Furthermore, a reference to a unit contains an implicit reference to the
   --  context that owns it. This means that keeping a reference to a unit will
   --  keep the context and all the unit it contains allocated.

   No_Analysis_Context : constant Analysis_Context;
   --  Special value to mean the absence of analysis context

   No_Analysis_Unit : constant Analysis_Unit;
   --  Special value to mean the absence of analysis unit. No analysis units
   --  can be passed this value.

   ---------------
   -- AST nodes --
   ---------------

      type Gpr_Node is tagged private;
      --  Data type for all nodes. Nodes are assembled to make up a tree.  See
      --  the node primitives below to inspect such trees.
      --
      --  Unlike for contexts and units, this type has weak-reference
      --  semantics: keeping a reference to a node has no effect on the
      --  decision to keep the unit that it owns allocated. This means that
      --  once all references to the context and units related to a node are
      --  dropped, the context and its units are deallocated and the node
      --  becomes a stale reference: most operations on it will raise a
      --  ``Stale_Reference_Error``.
      --
      --  Note that since reparsing an analysis unit deallocates all the nodes
      --  it contains, this operation makes all reference to these nodes stale
      --  as well.
      --
      

      function Equals (L, R : Gpr_Node) return Boolean;
      --  Comparison function, meant to compare two nodes.
      --
      --  .. note: For complex reasons, we cannot expose this function as the
      --     ``"="`` operator. This is the function you need to use as the
      --     equality function for containers instantiations.
      type Ada_Prelude_Node is new Gpr_Node with private
      ;
      

      type Ada_Access_Subp is new Ada_Prelude_Node with private
      ;
      

      type Ada_Context_Clause is new Ada_Prelude_Node with private
      ;
      

      type Base_List is new Gpr_Node with private
      ;
      

      type Ada_Context_Clause_List is new Base_List with private
         with Iterable => (First       => Ada_Context_Clause_List_First,
                           Next        => Ada_Context_Clause_List_Next,
                           Has_Element => Ada_Context_Clause_List_Has_Element,
                           Element     => Ada_Context_Clause_List_Element)
      ;
      --  List of AdaContextClause.

      type Ada_Entity_Kind is new Ada_Prelude_Node with private
      ;
      

      type Ada_Entity_Kind_Function is new Ada_Entity_Kind with private
      ;
      

      type Ada_Entity_Kind_Package is new Ada_Entity_Kind with private
      ;
      

      type Ada_Entity_Kind_Procedure is new Ada_Entity_Kind with private
      ;
      

      type Ada_Generic is new Ada_Prelude_Node with private
      ;
      

      type Ada_Library_Item is new Ada_Prelude_Node with private
      ;
      

      type Ada_Main is new Ada_Prelude_Node with private
      ;
      

      type Ada_Pkg is new Ada_Main with private
      ;
      

      type Ada_Pkg_Body is new Ada_Main with private
      ;
      

      type Ada_Pragma is new Ada_Context_Clause with private
      ;
      

      type Ada_Prelude is new Ada_Prelude_Node with private
      ;
      

      type Ada_Prelude_Node_List is new Base_List with private
         with Iterable => (First       => Ada_Prelude_Node_List_First,
                           Next        => Ada_Prelude_Node_List_Next,
                           Has_Element => Ada_Prelude_Node_List_Has_Element,
                           Element     => Ada_Prelude_Node_List_Element)
      ;
      --  List of AdaPreludeNode.
      --
      --  This list node can contain one of the following nodes:
      --  :ada:ref:`Ada_Access_Subp`, :ada:ref:`Ada_Skip`,
      --  :ada:ref:`Ada_With_Formal`

      type Ada_Separate is new Ada_Prelude_Node with private
      ;
      

      type Ada_Skip is new Ada_Prelude_Node with private
      ;
      

      type Ada_Skip_List is new Base_List with private
         with Iterable => (First       => Ada_Skip_List_First,
                           Next        => Ada_Skip_List_Next,
                           Has_Element => Ada_Skip_List_Has_Element,
                           Element     => Ada_Skip_List_Element)
      ;
      --  List of AdaSkip.

      type Ada_Subp is new Ada_Main with private
      ;
      

      type Ada_Use is new Ada_Context_Clause with private
      ;
      

      type Ada_With is new Ada_Context_Clause with private
      ;
      

      type Ada_With_Formal is new Ada_Prelude_Node with private
      ;
      

      type All_Qualifier is new Gpr_Node with private
      ;
      

      type All_Qualifier_Absent is new All_Qualifier with private
      ;
      

      type All_Qualifier_Present is new All_Qualifier with private
      ;
      

      type Attribute_Decl is new Gpr_Node with private
      ;
      

      type Attribute_Reference is new Gpr_Node with private
      ;
      

      type Builtin_Function_Call is new Gpr_Node with private
      ;
      

      type Case_Construction is new Gpr_Node with private
      ;
      

      type Case_Item is new Gpr_Node with private
      ;
      

      type Case_Item_List is new Base_List with private
         with Iterable => (First       => Case_Item_List_First,
                           Next        => Case_Item_List_Next,
                           Has_Element => Case_Item_List_Has_Element,
                           Element     => Case_Item_List_Element)
      ;
      --  List of CaseItem.

      type Gpr_Node_List is new Base_List with private
         with Iterable => (First       => Gpr_Node_List_First,
                           Next        => Gpr_Node_List_Next,
                           Has_Element => Gpr_Node_List_Has_Element,
                           Element     => Gpr_Node_List_Element)
      ;
      --  List of GprNode.
      --
      --  This list node can contain one of the following nodes:
      --  :ada:ref:`Attribute_Decl`, :ada:ref:`Builtin_Function_Call`,
      --  :ada:ref:`Case_Construction`, :ada:ref:`Empty_Decl`,
      --  :ada:ref:`Others_Designator`, :ada:ref:`Package_Decl`,
      --  :ada:ref:`Project_Reference`, :ada:ref:`String_Literal_At`,
      --  :ada:ref:`String_Literal`, :ada:ref:`Terms`,
      --  :ada:ref:`Typed_String_Decl`, :ada:ref:`Variable_Decl`,
      --  :ada:ref:`Variable_Reference`

      type Choices is new Gpr_Node_List with private
      ;
      --  This list node can contain one of the following nodes:
      --  :ada:ref:`Others_Designator`, :ada:ref:`String_Literal`

      type Compilation_Unit is new Gpr_Node with private
      ;
      

      type Empty_Decl is new Gpr_Node with private
      ;
      

      type Expr is new Gpr_Node with private
      ;
      

      type Expr_List is new Base_List with private
         with Iterable => (First       => Expr_List_First,
                           Next        => Expr_List_Next,
                           Has_Element => Expr_List_Has_Element,
                           Element     => Expr_List_Element)
      ;
      --  List of Expr.
      --
      --  This list node can contain one of the following nodes:
      --  :ada:ref:`Identifier`, :ada:ref:`Prefix`

      type Single_Tok_Node is new Expr with private
      ;
      

      type Identifier is new Single_Tok_Node with private
      ;
      

      type Identifier_List is new Base_List with private
         with Iterable => (First       => Identifier_List_First,
                           Next        => Identifier_List_Next,
                           Has_Element => Identifier_List_Has_Element,
                           Element     => Identifier_List_Element)
      ;
      --  List of Identifier.

      type Limited_Node is new Gpr_Node with private
      ;
      

      type Limited_Absent is new Limited_Node with private
      ;
      

      type Limited_Present is new Limited_Node with private
      ;
      

      type Num_Literal is new Single_Tok_Node with private
      ;
      

      type Others_Designator is new Gpr_Node with private
      ;
      

      type Package_Decl is new Gpr_Node with private
      ;
      

      type Package_Extension is new Gpr_Node with private
      ;
      

      type Package_Renaming is new Gpr_Node with private
      ;
      

      type Package_Spec is new Gpr_Node with private
      ;
      

      type Prefix is new Expr with private
      ;
      

      type Private_Node is new Gpr_Node with private
      ;
      

      type Private_Absent is new Private_Node with private
      ;
      

      type Private_Present is new Private_Node with private
      ;
      

      type Project is new Gpr_Node with private
      ;
      

      type Project_Declaration is new Gpr_Node with private
      ;
      

      type Project_Extension is new Gpr_Node with private
      ;
      

      type Project_Qualifier is new Gpr_Node with private
      ;
      

      type Project_Qualifier_Abstract is new Project_Qualifier with private
      ;
      

      type Project_Qualifier_Aggregate is new Project_Qualifier with private
      ;
      

      type Project_Qualifier_Aggregate_Library is new Project_Qualifier with private
      ;
      

      type Project_Qualifier_Configuration is new Project_Qualifier with private
      ;
      

      type Project_Qualifier_Library is new Project_Qualifier with private
      ;
      

      type Project_Qualifier_Standard is new Project_Qualifier with private
      ;
      

      type Project_Reference is new Gpr_Node with private
      ;
      

      type String_Literal is new Single_Tok_Node with private
      ;
      

      type String_Literal_At is new Gpr_Node with private
      ;
      

      type String_Literal_List is new Base_List with private
         with Iterable => (First       => String_Literal_List_First,
                           Next        => String_Literal_List_Next,
                           Has_Element => String_Literal_List_Has_Element,
                           Element     => String_Literal_List_Element)
      ;
      --  List of StringLiteral.

      type Term_List is new Gpr_Node_List with private
      ;
      --  This list node can contain one of the following nodes:
      --  :ada:ref:`Builtin_Function_Call`, :ada:ref:`Project_Reference`,
      --  :ada:ref:`String_Literal_At`, :ada:ref:`Terms`,
      --  :ada:ref:`Variable_Reference`

      type Term_List_List is new Base_List with private
         with Iterable => (First       => Term_List_List_First,
                           Next        => Term_List_List_Next,
                           Has_Element => Term_List_List_Has_Element,
                           Element     => Term_List_List_Element)
      ;
      --  List of TermList.

      type Terms is new Gpr_Node with private
      ;
      

      type Type_Reference is new Gpr_Node with private
      ;
      

      type Typed_String_Decl is new Gpr_Node with private
      ;
      

      type Variable_Decl is new Gpr_Node with private
      ;
      

      type Variable_Reference is new Gpr_Node with private
      ;
      

      type With_Decl is new Gpr_Node with private
      ;
      

      type With_Decl_List is new Base_List with private
         with Iterable => (First       => With_Decl_List_First,
                           Next        => With_Decl_List_Next,
                           Has_Element => With_Decl_List_Has_Element,
                           Element     => With_Decl_List_Element)
      ;
      --  List of WithDecl.


      No_Gpr_Node : constant Gpr_Node;
      --  Special value to represent the absence of a node. Note that every
      --  node type derived from the root type has a similar ``No_Node``
      --  constant.
      No_Ada_Prelude_Node : constant Ada_Prelude_Node;
      --% no-document: True
      No_Ada_Access_Subp : constant Ada_Access_Subp;
      --% no-document: True
      No_Ada_Context_Clause : constant Ada_Context_Clause;
      --% no-document: True
      No_Base_List : constant Base_List;
      --% no-document: True
      No_Ada_Context_Clause_List : constant Ada_Context_Clause_List;
      --% no-document: True
      No_Ada_Entity_Kind : constant Ada_Entity_Kind;
      --% no-document: True
      No_Ada_Entity_Kind_Function : constant Ada_Entity_Kind_Function;
      --% no-document: True
      No_Ada_Entity_Kind_Package : constant Ada_Entity_Kind_Package;
      --% no-document: True
      No_Ada_Entity_Kind_Procedure : constant Ada_Entity_Kind_Procedure;
      --% no-document: True
      No_Ada_Generic : constant Ada_Generic;
      --% no-document: True
      No_Ada_Library_Item : constant Ada_Library_Item;
      --% no-document: True
      No_Ada_Main : constant Ada_Main;
      --% no-document: True
      No_Ada_Pkg : constant Ada_Pkg;
      --% no-document: True
      No_Ada_Pkg_Body : constant Ada_Pkg_Body;
      --% no-document: True
      No_Ada_Pragma : constant Ada_Pragma;
      --% no-document: True
      No_Ada_Prelude : constant Ada_Prelude;
      --% no-document: True
      No_Ada_Prelude_Node_List : constant Ada_Prelude_Node_List;
      --% no-document: True
      No_Ada_Separate : constant Ada_Separate;
      --% no-document: True
      No_Ada_Skip : constant Ada_Skip;
      --% no-document: True
      No_Ada_Skip_List : constant Ada_Skip_List;
      --% no-document: True
      No_Ada_Subp : constant Ada_Subp;
      --% no-document: True
      No_Ada_Use : constant Ada_Use;
      --% no-document: True
      No_Ada_With : constant Ada_With;
      --% no-document: True
      No_Ada_With_Formal : constant Ada_With_Formal;
      --% no-document: True
      No_All_Qualifier : constant All_Qualifier;
      --% no-document: True
      No_All_Qualifier_Absent : constant All_Qualifier_Absent;
      --% no-document: True
      No_All_Qualifier_Present : constant All_Qualifier_Present;
      --% no-document: True
      No_Attribute_Decl : constant Attribute_Decl;
      --% no-document: True
      No_Attribute_Reference : constant Attribute_Reference;
      --% no-document: True
      No_Builtin_Function_Call : constant Builtin_Function_Call;
      --% no-document: True
      No_Case_Construction : constant Case_Construction;
      --% no-document: True
      No_Case_Item : constant Case_Item;
      --% no-document: True
      No_Case_Item_List : constant Case_Item_List;
      --% no-document: True
      No_Gpr_Node_List : constant Gpr_Node_List;
      --% no-document: True
      No_Choices : constant Choices;
      --% no-document: True
      No_Compilation_Unit : constant Compilation_Unit;
      --% no-document: True
      No_Empty_Decl : constant Empty_Decl;
      --% no-document: True
      No_Expr : constant Expr;
      --% no-document: True
      No_Expr_List : constant Expr_List;
      --% no-document: True
      No_Single_Tok_Node : constant Single_Tok_Node;
      --% no-document: True
      No_Identifier : constant Identifier;
      --% no-document: True
      No_Identifier_List : constant Identifier_List;
      --% no-document: True
      No_Limited_Node : constant Limited_Node;
      --% no-document: True
      No_Limited_Absent : constant Limited_Absent;
      --% no-document: True
      No_Limited_Present : constant Limited_Present;
      --% no-document: True
      No_Num_Literal : constant Num_Literal;
      --% no-document: True
      No_Others_Designator : constant Others_Designator;
      --% no-document: True
      No_Package_Decl : constant Package_Decl;
      --% no-document: True
      No_Package_Extension : constant Package_Extension;
      --% no-document: True
      No_Package_Renaming : constant Package_Renaming;
      --% no-document: True
      No_Package_Spec : constant Package_Spec;
      --% no-document: True
      No_Prefix : constant Prefix;
      --% no-document: True
      No_Private_Node : constant Private_Node;
      --% no-document: True
      No_Private_Absent : constant Private_Absent;
      --% no-document: True
      No_Private_Present : constant Private_Present;
      --% no-document: True
      No_Project : constant Project;
      --% no-document: True
      No_Project_Declaration : constant Project_Declaration;
      --% no-document: True
      No_Project_Extension : constant Project_Extension;
      --% no-document: True
      No_Project_Qualifier : constant Project_Qualifier;
      --% no-document: True
      No_Project_Qualifier_Abstract : constant Project_Qualifier_Abstract;
      --% no-document: True
      No_Project_Qualifier_Aggregate : constant Project_Qualifier_Aggregate;
      --% no-document: True
      No_Project_Qualifier_Aggregate_Library : constant Project_Qualifier_Aggregate_Library;
      --% no-document: True
      No_Project_Qualifier_Configuration : constant Project_Qualifier_Configuration;
      --% no-document: True
      No_Project_Qualifier_Library : constant Project_Qualifier_Library;
      --% no-document: True
      No_Project_Qualifier_Standard : constant Project_Qualifier_Standard;
      --% no-document: True
      No_Project_Reference : constant Project_Reference;
      --% no-document: True
      No_String_Literal : constant String_Literal;
      --% no-document: True
      No_String_Literal_At : constant String_Literal_At;
      --% no-document: True
      No_String_Literal_List : constant String_Literal_List;
      --% no-document: True
      No_Term_List : constant Term_List;
      --% no-document: True
      No_Term_List_List : constant Term_List_List;
      --% no-document: True
      No_Terms : constant Terms;
      --% no-document: True
      No_Type_Reference : constant Type_Reference;
      --% no-document: True
      No_Typed_String_Decl : constant Typed_String_Decl;
      --% no-document: True
      No_Variable_Decl : constant Variable_Decl;
      --% no-document: True
      No_Variable_Reference : constant Variable_Reference;
      --% no-document: True
      No_With_Decl : constant With_Decl;
      --% no-document: True
      No_With_Decl_List : constant With_Decl_List;
      --% no-document: True

   function Is_Null (Node : Gpr_Node'Class) return Boolean;
   --  Return whether this node is a null node reference.

   function Is_Token_Node
     (Node : Gpr_Node'Class) return Boolean;
   --  Return whether this node is a node that contains only a single token.

   function Is_Synthetic
     (Node : Gpr_Node'Class) return Boolean;
   --  Return whether this node is synthetic.

   function "=" (L, R : Gpr_Node'Class) return Boolean;
   --  Return whether ``L`` and ``R`` designate the same node

   function Image (Node : Gpr_Node'Class) return String;
   --  Return a short string describing ``Node``, or None" if ``Node.Is_Null``
   --  is true.

   -------------------
   -- Event handler --
   -------------------

   type Event_Handler_Interface is interface;
   --  Interface to handle events sent by the analysis context.

   procedure Unit_Requested_Callback
     (Self               : in out Event_Handler_Interface;
      Context            : Analysis_Context'Class;
      Name               : Text_Type;
      From               : Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean) is null;
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

   procedure Unit_Parsed_Callback
     (Self     : in out Event_Handler_Interface;
      Context  : Analysis_Context'Class;
      Unit     : Analysis_Unit'Class;
      Reparsed : Boolean) is null;
   --  Callback that will be called when any unit is parsed from the context
   --  ``Context``.
   --
   --  ``Unit`` is the resulting unit.
   --
   --  ``Reparsed`` indicates whether the unit was reparsed, or whether it was
   --  the first parse.

   procedure Release (Self : in out Event_Handler_Interface) is abstract;
   --  Actions to perform when releasing resources associated to Self

   procedure Do_Release (Self : in out Event_Handler_Interface'Class);
   --  Helper for the instantiation below

   package Event_Handler_References is new GNATCOLL.Refcount.Shared_Pointers
     (Event_Handler_Interface'Class, Do_Release);

   subtype Event_Handler_Reference is Event_Handler_References.Ref;
   No_Event_Handler_Ref : Event_Handler_Reference renames
      Event_Handler_References.Null_Ref;

   function Create_Event_Handler_Reference
     (Handler : Event_Handler_Interface'Class) return Event_Handler_Reference;
   --  Simple wrapper around the GNATCOLL.Refcount API to create event handler
   --  references.

   --------------------
   -- Unit providers --
   --------------------

   type Unit_Provider_Interface is interface;
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

   function Get_Unit_Filename
     (Provider : Unit_Provider_Interface;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String is abstract;
   --  Return the filename corresponding to the given unit name/unit kind.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   function Get_Unit
     (Provider    : Unit_Provider_Interface;
      Context     : Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Analysis_Unit'Class is abstract;
   --  Fetch and return the analysis unit referenced by the given unit name.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   procedure Release (Provider : in out Unit_Provider_Interface) is abstract;
   --  Actions to perform when releasing resources associated to Provider

   procedure Do_Release (Provider : in out Unit_Provider_Interface'Class);
   --  Helper for the instantiation below

   package Unit_Provider_References is new GNATCOLL.Refcount.Shared_Pointers
     (Unit_Provider_Interface'Class, Do_Release);

   subtype Unit_Provider_Reference is Unit_Provider_References.Ref;
   No_Unit_Provider_Reference : Unit_Provider_Reference renames
      Unit_Provider_References.Null_Ref;

   function Create_Unit_Provider_Reference
     (Provider : Unit_Provider_Interface'Class) return Unit_Provider_Reference;
   --  Simple wrapper around the GNATCOLL.Refcount API to create unit provider
   --  references.

   ---------------------------------
   -- Analysis context primitives --
   ---------------------------------

   function Create_Context
     (Charset       : String := Default_Charset;
      File_Reader   : File_Reader_Reference := No_File_Reader_Reference;
      Unit_Provider : Unit_Provider_Reference := No_Unit_Provider_Reference;
      Event_Handler : Event_Handler_Reference := No_Event_Handler_Ref;
      With_Trivia   : Boolean := True;
      Tab_Stop      : Positive := 8)
      return Analysis_Context;
   --  Create a new analysis context.
   --
   --  ``Charset`` will be used as a default charset to decode input sources in
   --  analysis units. Please see ``GNATCOLL.Iconv`` for several supported
   --  charsets. Be careful: passing an unsupported charset is not guaranteed
   --  to raise an error here. If no charset is provided, ``"iso-8859-1"`` is
   --  the default.
   --
   --  .. TODO: Passing an unsupported charset here is not guaranteed to raise
   --     an error right here, but this would be really helpful for users.
   --
   --  When ``With_Trivia`` is true, the parsed analysis units will contain
   --  trivias.
   --
   --  If provided, ``File_Reader`` will be used to fetch the contents of
   --  source files instead of the default, which is to just read it from the
   --  filesystem and decode it using the regular charset rules. Note that if
   --  provided, all parsing APIs that provide a buffer are forbidden, and any
   --  use of the rewriting API with the returned context is rejected.
   --
   --  If provided, ``Unit_Provider`` will be used to query the file name that
   --  corresponds to a unit reference during semantic analysis. If it is
   --  ``null``, the default one is used instead.
   --
   --  If provided, ``Event_Handler`` will be notified when various events
   --  happen.
   --
   --  ``Tab_Stop`` is a positive number to describe the effect of tabulation
   --  characters on the column number in source files.
   --% belongs-to: Analysis_Context

   function Has_Unit
     (Context       : Analysis_Context'Class;
      Unit_Filename : String) return Boolean;
   --  Return whether ``Context`` contains a unit correponding to
   --  ``Unit_Filename``.

   function Get_From_File
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
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
   --
   --  It is invalid to pass ``True`` to ``Reparse`` if a rewriting context is
   --  active.

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
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
   --
   --  Calling this is invalid if a rewriting context is active.

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : Ada.Strings.Unbounded.Unbounded_String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
   --  Likewise, but working on an unbounded string

   function Get_With_Error
     (Context  : Analysis_Context'Class;
      Filename : String;
      Error    : Text_Type;
      Charset  : String := "";
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
   --  If a Unit for ``Filename`` already exists, return it unchanged.
   --  Otherwise, create an empty analysis unit for ``Filename`` with a
   --  diagnostic that contains the ``Error`` message.


   function Unit_Provider
     (Context : Analysis_Context'Class) return Unit_Provider_Reference;
   --  Return the unit provider for ``Context``
   --
   --% belongs-to: Analysis_Context

   function Hash (Context : Analysis_Context) return Ada.Containers.Hash_Type;
   --  Return a hash for this context, to be used in hash tables.

   function Has_With_Trivia (Context : Analysis_Context'Class) return Boolean;
   --  Return whether ``Context`` keeps trivia when parsing units

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Analysis_Context'Class; Discard : Boolean);
   --  Debug helper. Set whether ``Property_Error`` exceptions raised in
   --  ``Populate_Lexical_Env`` should be discarded. They are by default.

   procedure Set_Logic_Resolution_Timeout
     (Context : Analysis_Context'Class; Timeout : Natural);
   --  If ``Timeout`` is greater than zero, set a timeout for the resolution of
   --  logic equations. The unit is the number of steps in ANY/ALL relations.
   --  If ``Timeout`` is zero, disable the timeout. By default, the timeout is
   --  ``100 000`` steps.

   procedure Set_Lookup_Cache_Mode (Mode : Lookup_Cache_Kind);
   --  Set the lexical environments lookup cache mode according to ``Mode``.
   --  Note: Mainly meant for debugging the default mode.

   function Has_Rewriting_Handle
     (Context : Analysis_Context'Class) return Boolean;
   --  Return whether ``Context`` has a rewriting handler (see
   --  ``Gpr_Parser.Rewriting``), i.e. whether it is in the process of
   --  rewriting. If true, this means that the set of currently loaded analysis
   --  units is frozen until the rewriting process is done.

   function Get_Symbol_Table
     (Context : Analysis_Context'Class) return Symbol_Table;
   --  Return the symbol table attached to this context. Useful for users
   --  needing their own symbolization and wanting to share it with their
   --  language frontend.
   --
   --  WARNING: EXPERIMENTAL & UNSAFE - The Symbol_Table exposes an unsafe API,
   --  that might be subject to some changes, use with caution.

   ------------------------------
   -- Analysis unit primitives --
   ------------------------------

   function Context (Unit : Analysis_Unit'Class) return Analysis_Context;
   --  Return the context that owns this unit.

   function Hash (Unit : Analysis_Unit) return Ada.Containers.Hash_Type;
   --  Return a hash for this unit, to be used in hash tables.

   procedure Reparse (Unit : Analysis_Unit'Class; Charset : String := "");
   --  Reparse an analysis unit from the associated file.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   procedure Reparse
     (Unit    : Analysis_Unit'Class;
      Charset : String := "";
      Buffer  : String);
   --  Reparse an analysis unit from a buffer.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   procedure Populate_Lexical_Env (Unit : Analysis_Unit'Class);
   --  Create lexical environments for this analysis unit, according to the
   --  specifications given in the language spec.
   --
   --  If not done before, it will be automatically called during semantic
   --  analysis. Calling it before enables one to control where the latency
   --  occurs.
   --
   --  Depending on whether errors are discarded (see
   --  ``Discard_Errors_In_Populate_Lexical_Env``), raise a ``Property_Error``
   --  on failure.

   function Get_Filename (Unit : Analysis_Unit'Class) return String;
   --  Return the filename this unit is associated to.

   function Get_Charset (Unit : Analysis_Unit'Class) return String;
   --  Return the charset that was used to parse Unit

   function Has_Diagnostics (Unit : Analysis_Unit'Class) return Boolean;
   --  Return whether this unit has associated diagnostics.

   function Diagnostics (Unit : Analysis_Unit'Class) return Diagnostics_Array;
   --  Return an array that contains the diagnostics associated to this unit.

   function Format_GNU_Diagnostic
     (Unit : Analysis_Unit'Class; D : Diagnostic) return String;
   --  Format a diagnostic in a GNU fashion. See
   --  <https://www.gnu.org/prep/standards/html_node/Errors.html>.

   pragma Warnings (Off, "defined after private extension");
   function Root (Unit : Analysis_Unit'Class) return Gpr_Node;
   --  Return the root node for this unit, or ``null`` if there is none.
   pragma Warnings (On, "defined after private extension");

   function First_Token (Unit : Analysis_Unit'Class) return Token_Reference;
   --  Return a reference to the first token scanned in this unit.

   function Last_Token (Unit : Analysis_Unit'Class) return Token_Reference;
   --  Return a reference to the last token scanned in this unit.

   function Token_Count (Unit : Analysis_Unit'Class) return Natural;
   --  Return the number of tokens in this unit.

   function Trivia_Count (Unit : Analysis_Unit'Class) return Natural;
   --  Return the number of trivias in this unit. This is 0 for units that were
   --  parsed with trivia analysis disabled.

   function Unit (Token : Token_Reference) return Analysis_Unit;
   --  Return the analysis unit that owns ``Token``

   function Text (Unit : Analysis_Unit'Class) return Text_Type;
   --  Return the source buffer associated to this unit.

   function Lookup_Token
     (Unit : Analysis_Unit'Class; Sloc : Source_Location)
      return Token_Reference;
   --  Look for a token in this unit that contains the given source location.
   --  If this falls before the first token, return the first token. If this
   --  falls between two tokens, return the token that appears before. If this
   --  falls after the last token, return the last token. If there is no token
   --  in this unit, return no token.

   procedure Dump_Lexical_Env (Unit : Analysis_Unit'Class);
   --  Debug helper: output the lexical envs for the given analysis unit.

   procedure Trigger_Envs_Debug (Is_Active : Boolean);
   --  Debug helper: activate debug traces for lexical envs lookups

   procedure Print (Unit : Analysis_Unit'Class; Show_Slocs : Boolean := True);
   --  Debug helper: output the AST and eventual diagnostic for this unit on
   --  standard output.
   --
   --  If Show_Slocs, include AST nodes' source locations in the output.

   procedure PP_Trivia (Unit : Analysis_Unit'Class);
   --  Debug helper: output a minimal AST with mixed trivias

   overriding function Get_Line
     (Unit : Analysis_Unit; Line_Number : Positive) return Text_Type;
   --  Return the line of text at line number ``Line_Number``

   type Child_Record (Kind : Child_Or_Trivia := Child) is record
      case Kind is
         when Child =>
            Node : Gpr_Node;
         when Trivia =>
            Trivia : Token_Reference;
      end case;
   end record;
   --  Variant that holds either an AST node or a token

   type Children_Array is array (Positive range <>) of Child_Record;

   function Children_And_Trivia
     (Node : Gpr_Node'Class) return Children_Array;
   --  Return the children of this node interleaved with Trivia token nodes, so
   --  that:
   --
   --  - Every trivia contained between ``Node.Start_Token`` and
   --    ``Node.End_Token - 1`` will be part of the returned array.
   --
   --  - Nodes and trivias will be lexically ordered.

   ---------------------
   -- Composite types --
   ---------------------

            
   type Gpr_Node_Array is
      array (Positive range <>) of Gpr_Node;



   --------------------
   -- Token Iterator --
   --------------------

   type Token_Iterator is private
      with Iterable => (First       => First_Token,
                        Next        => Next_Token,
                        Has_Element => Has_Element,
                        Element     => Element);
   --  Allow iteration on a range of tokens corresponding to a node

   function First_Token (Self : Token_Iterator) return Token_Reference;
   --  Return the first token corresponding to the node

   function Next_Token
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference;
   --  Return the token that follows Tok in the token stream

   function Has_Element
     (Self : Token_Iterator; Tok : Token_Reference) return Boolean;
   --  Return if Tok is in Self's iteration range

   function Element
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference;
   --  Identity function: helper for the Iterable aspect

   -------------------------
   -- AST Node primitives --
   -------------------------

   function Kind
     (Node : Gpr_Node'Class) return Gpr_Node_Kind_Type;
   function Kind_Name (Node : Gpr_Node'Class) return String;
   --  Return the concrete kind for Node

   pragma Warnings (Off, "defined after private extension");




         
   function Parent
     (Node : Gpr_Node'Class) return Gpr_Node;
   --  Return the syntactic parent for this node. Return null for the root
   --  node.
   --% belongs-to: Gpr_Node

         
   function Parents
     (Node : Gpr_Node'Class;
      With_Self : Boolean := True) return Gpr_Node_Array;
   --  Return an array that contains the lexical parents, this node included
   --  iff ``with_self`` is True. Nearer parents are first in the list.
   --% belongs-to: Gpr_Node

         
   function Children
     (Node : Gpr_Node'Class) return Gpr_Node_Array;
   --  Return an array that contains the direct lexical children.
   --
   --  .. warning:: This constructs a whole array every-time you call it, and
   --     as such is less efficient than calling the ``Child`` built-in.
   --% belongs-to: Gpr_Node

         
   function Token_Start
     (Node : Gpr_Node'Class) return Token_Reference;
   --  Return the first token used to parse this node.
   --% belongs-to: Gpr_Node

         
   function Token_End
     (Node : Gpr_Node'Class) return Token_Reference;
   --  Return the last token used to parse this node.
   --% belongs-to: Gpr_Node

         
   function Child_Index
     (Node : Gpr_Node'Class) return Integer;
   --  Return the 0-based index for Node in its parent's children.
   --% belongs-to: Gpr_Node

         
   function Previous_Sibling
     (Node : Gpr_Node'Class) return Gpr_Node;
   --  Return the node's previous sibling, or null if there is no such sibling.
   --% belongs-to: Gpr_Node

         
   function Next_Sibling
     (Node : Gpr_Node'Class) return Gpr_Node;
   --  Return the node's next sibling, or null if there is no such sibling.
   --% belongs-to: Gpr_Node

         
   function Unit
     (Node : Gpr_Node'Class) return Analysis_Unit;
   --  Return the analysis unit owning this node.
   --% belongs-to: Gpr_Node

         
   function Is_Ghost
     (Node : Gpr_Node'Class) return Boolean;
   --  Return whether the node is a ghost.
   --
   --  Unlike regular nodes, ghost nodes cover no token in the input source:
   --  they are logically located instead between two tokens. Both the
   --  ``token_start`` and the ``token_end`` of all ghost nodes is the token
   --  right after this logical position.
   --% belongs-to: Gpr_Node

         
   function Full_Sloc_Image
     (Node : Gpr_Node'Class) return Text_Type;
   --  Return a string containing the filename + the sloc in GNU conformant
   --  format. Useful to create diagnostics from a node.
   --% belongs-to: Gpr_Node










         
   

   function F_Subp_Kind
     (Node : Ada_Access_Subp'Class) return Ada_Entity_Kind;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Ada_Entity_Kind_Function`,
   --  :ada:ref:`Ada_Entity_Kind_Procedure`
   --% belongs-to: Ada_Access_Subp

      function F_Subp_Kind
        (Node : Ada_Access_Subp'Class) return Gpr_Ada_Entity_Kind;
      --% belongs-to: Ada_Access_Subp

         
   

   function F_Skips
     (Node : Ada_Access_Subp'Class) return Ada_Skip_List;
   --% belongs-to: Ada_Access_Subp















         function List_Child
           (Node : Ada_Context_Clause_List'Class; Index : Positive)
            return Ada_Context_Clause;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Ada_Context_Clause_List_First (Node : Ada_Context_Clause_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Ada_Context_Clause_List_Next
           (Node : Ada_Context_Clause_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Ada_Context_Clause_List_Has_Element
           (Node : Ada_Context_Clause_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Ada_Context_Clause_List_Element
           (Node : Ada_Context_Clause_List; Cursor : Positive)
            return Ada_Context_Clause'Class;
         --  Implementation detail for the Iterable aspect


























         
   

   function F_Skips
     (Node : Ada_Generic'Class) return Gpr_Node;
   --% belongs-to: Ada_Generic







         
   

   function F_Generic_Stub
     (Node : Ada_Library_Item'Class) return Ada_Generic;
   --% belongs-to: Ada_Library_Item


         
   

   function F_Separate
     (Node : Ada_Library_Item'Class) return Ada_Separate;
   --% belongs-to: Ada_Library_Item


         
   

   function F_Main
     (Node : Ada_Library_Item'Class) return Ada_Main;
   --% belongs-to: Ada_Library_Item







         
   

   function F_Name
     (Node : Ada_Main'Class) return Expr;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Identifier`, :ada:ref:`Prefix`
   --% belongs-to: Ada_Main







         
   

   function F_Has_Private
     (Node : Ada_Pkg'Class) return Private_Node;
   --% belongs-to: Ada_Pkg

      function F_Has_Private (Node : Ada_Pkg'Class) return Boolean;
      --% belongs-to: Ada_Pkg












         
   

   function F_Skips
     (Node : Ada_Pragma'Class) return Ada_Skip_List;
   --% belongs-to: Ada_Pragma







         
   

   function F_Context_Clauses
     (Node : Ada_Prelude'Class) return Ada_Context_Clause_List;
   --% belongs-to: Ada_Prelude


         
   

   function F_Library_Item
     (Node : Ada_Prelude'Class) return Ada_Library_Item;
   --% belongs-to: Ada_Prelude





         function List_Child
           (Node : Ada_Prelude_Node_List'Class; Index : Positive)
            return Ada_Prelude_Node;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Ada_Prelude_Node_List_First (Node : Ada_Prelude_Node_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Ada_Prelude_Node_List_Next
           (Node : Ada_Prelude_Node_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Ada_Prelude_Node_List_Has_Element
           (Node : Ada_Prelude_Node_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Ada_Prelude_Node_List_Element
           (Node : Ada_Prelude_Node_List; Cursor : Positive)
            return Ada_Prelude_Node'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Parent_Name
     (Node : Ada_Separate'Class) return Expr;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Identifier`, :ada:ref:`Prefix`
   --% belongs-to: Ada_Separate










         function List_Child
           (Node : Ada_Skip_List'Class; Index : Positive)
            return Ada_Skip;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Ada_Skip_List_First (Node : Ada_Skip_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Ada_Skip_List_Next
           (Node : Ada_Skip_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Ada_Skip_List_Has_Element
           (Node : Ada_Skip_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Ada_Skip_List_Element
           (Node : Ada_Skip_List; Cursor : Positive)
            return Ada_Skip'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Subp_Kind
     (Node : Ada_Subp'Class) return Ada_Entity_Kind;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Ada_Entity_Kind_Function`,
   --  :ada:ref:`Ada_Entity_Kind_Procedure`
   --% belongs-to: Ada_Subp

      function F_Subp_Kind
        (Node : Ada_Subp'Class) return Gpr_Ada_Entity_Kind;
      --% belongs-to: Ada_Subp






         
   

   function F_Skips
     (Node : Ada_Use'Class) return Ada_Skip_List;
   --% belongs-to: Ada_Use







         
   

   function F_Has_Limited
     (Node : Ada_With'Class) return Limited_Node;
   --% belongs-to: Ada_With

      function F_Has_Limited (Node : Ada_With'Class) return Boolean;
      --% belongs-to: Ada_With


         
   

   function F_Has_Private
     (Node : Ada_With'Class) return Private_Node;
   --% belongs-to: Ada_With

      function F_Has_Private (Node : Ada_With'Class) return Boolean;
      --% belongs-to: Ada_With


         
   

   function F_Packages
     (Node : Ada_With'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Identifier`, :ada:ref:`Prefix`
   --% belongs-to: Ada_With







         
   

   function F_Kind
     (Node : Ada_With_Formal'Class) return Ada_Entity_Kind;
   --% belongs-to: Ada_With_Formal

      function F_Kind
        (Node : Ada_With_Formal'Class) return Gpr_Ada_Entity_Kind;
      --% belongs-to: Ada_With_Formal

         
   

   function F_Skips
     (Node : Ada_With_Formal'Class) return Ada_Skip_List;
   --% belongs-to: Ada_With_Formal








         
   function P_As_Bool
     (Node : All_Qualifier'Class) return Boolean;
   --  Return whether this is an instance of AllQualifierPresent
   --% belongs-to: All_Qualifier















         
   

   function F_Attr_Name
     (Node : Attribute_Decl'Class) return Identifier;
   --% belongs-to: Attribute_Decl


         
   

   function F_Attr_Index
     (Node : Attribute_Decl'Class) return Gpr_Node;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Others_Designator`, :ada:ref:`String_Literal_At`
   --% belongs-to: Attribute_Decl


         
   

   function F_Expr
     (Node : Attribute_Decl'Class) return Term_List;
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Builtin_Function_Call`, :ada:ref:`Project_Reference`,
   --  :ada:ref:`String_Literal_At`, :ada:ref:`Terms`,
   --  :ada:ref:`Variable_Reference`
   --% belongs-to: Attribute_Decl







         
   

   function F_Attribute_Name
     (Node : Attribute_Reference'Class) return Identifier;
   --% belongs-to: Attribute_Reference


         
   

   function F_Attribute_Index
     (Node : Attribute_Reference'Class) return Gpr_Node;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Others_Designator`, :ada:ref:`String_Literal`
   --% belongs-to: Attribute_Reference







         
   

   function F_Function_Name
     (Node : Builtin_Function_Call'Class) return Identifier;
   --% belongs-to: Builtin_Function_Call


         
   

   function F_Parameters
     (Node : Builtin_Function_Call'Class) return Terms;
   --% belongs-to: Builtin_Function_Call







         
   

   function F_Var_Ref
     (Node : Case_Construction'Class) return Variable_Reference;
   --% belongs-to: Case_Construction


         
   

   function F_Items
     (Node : Case_Construction'Class) return Case_Item_List;
   --% belongs-to: Case_Construction







         
   

   function F_Choice
     (Node : Case_Item'Class) return Choices;
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Others_Designator`, :ada:ref:`String_Literal`
   --% belongs-to: Case_Item


         
   

   function F_Decls
     (Node : Case_Item'Class) return Gpr_Node_List;
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Attribute_Decl`, :ada:ref:`Case_Construction`,
   --  :ada:ref:`Empty_Decl`, :ada:ref:`Variable_Decl`
   --% belongs-to: Case_Item





         function List_Child
           (Node : Case_Item_List'Class; Index : Positive)
            return Case_Item;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Case_Item_List_First (Node : Case_Item_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Case_Item_List_Next
           (Node : Case_Item_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Case_Item_List_Has_Element
           (Node : Case_Item_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Case_Item_List_Element
           (Node : Case_Item_List; Cursor : Positive)
            return Case_Item'Class;
         --  Implementation detail for the Iterable aspect





         function Gpr_Node_List_First (Node : Gpr_Node_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Gpr_Node_List_Next
           (Node : Gpr_Node_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Gpr_Node_List_Has_Element
           (Node : Gpr_Node_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Gpr_Node_List_Element
           (Node : Gpr_Node_List; Cursor : Positive)
            return Gpr_Node'Class;
         --  Implementation detail for the Iterable aspect











         
   

   function F_Project
     (Node : Compilation_Unit'Class) return Project;
   --% belongs-to: Compilation_Unit















         function List_Child
           (Node : Expr_List'Class; Index : Positive)
            return Expr;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Expr_List_First (Node : Expr_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Expr_List_Next
           (Node : Expr_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Expr_List_Has_Element
           (Node : Expr_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Expr_List_Element
           (Node : Expr_List; Cursor : Positive)
            return Expr'Class;
         --  Implementation detail for the Iterable aspect














         function List_Child
           (Node : Identifier_List'Class; Index : Positive)
            return Identifier;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Identifier_List_First (Node : Identifier_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Identifier_List_Next
           (Node : Identifier_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Identifier_List_Has_Element
           (Node : Identifier_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Identifier_List_Element
           (Node : Identifier_List; Cursor : Positive)
            return Identifier'Class;
         --  Implementation detail for the Iterable aspect







         
   function P_As_Bool
     (Node : Limited_Node'Class) return Boolean;
   --  Return whether this is an instance of LimitedPresent
   --% belongs-to: Limited_Node

























         
   

   function F_Pkg_Name
     (Node : Package_Decl'Class) return Identifier;
   --% belongs-to: Package_Decl


         
   

   function F_Pkg_Spec
     (Node : Package_Decl'Class) return Gpr_Node;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Package_Renaming`, :ada:ref:`Package_Spec`
   --% belongs-to: Package_Decl







         
   

   function F_Extended_Name
     (Node : Package_Extension'Class) return Identifier_List;
   --% belongs-to: Package_Extension







         
   

   function F_Renamed_Name
     (Node : Package_Renaming'Class) return Identifier_List;
   --% belongs-to: Package_Renaming







         
   

   function F_Extension
     (Node : Package_Spec'Class) return Package_Extension;
   --% belongs-to: Package_Spec


         
   

   function F_Decls
     (Node : Package_Spec'Class) return Gpr_Node_List;
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Attribute_Decl`, :ada:ref:`Case_Construction`,
   --  :ada:ref:`Empty_Decl`, :ada:ref:`Variable_Decl`
   --% belongs-to: Package_Spec


         
   

   function F_End_Name
     (Node : Package_Spec'Class) return Identifier;
   --% belongs-to: Package_Spec







         
   

   function F_Prefix
     (Node : Prefix'Class) return Expr;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Identifier`, :ada:ref:`Prefix`
   --% belongs-to: Prefix


         
   

   function F_Suffix
     (Node : Prefix'Class) return Identifier;
   --% belongs-to: Prefix








         
   function P_As_Bool
     (Node : Private_Node'Class) return Boolean;
   --  Return whether this is an instance of PrivatePresent
   --% belongs-to: Private_Node















         
   

   function F_Context_Clauses
     (Node : Project'Class) return With_Decl_List;
   --% belongs-to: Project


         
   

   function F_Project_Decl
     (Node : Project'Class) return Project_Declaration;
   --% belongs-to: Project







         
   

   function F_Qualifier
     (Node : Project_Declaration'Class) return Project_Qualifier;
   --% belongs-to: Project_Declaration

      function F_Qualifier
        (Node : Project_Declaration'Class) return Gpr_Project_Qualifier;
      --% belongs-to: Project_Declaration

         
   

   function F_Project_Name
     (Node : Project_Declaration'Class) return Expr;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Identifier`, :ada:ref:`Prefix`
   --% belongs-to: Project_Declaration


         
   

   function F_Extension
     (Node : Project_Declaration'Class) return Project_Extension;
   --% belongs-to: Project_Declaration


         
   

   function F_Decls
     (Node : Project_Declaration'Class) return Gpr_Node_List;
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Attribute_Decl`, :ada:ref:`Case_Construction`,
   --  :ada:ref:`Empty_Decl`, :ada:ref:`Package_Decl`,
   --  :ada:ref:`Typed_String_Decl`, :ada:ref:`Variable_Decl`
   --% belongs-to: Project_Declaration


         
   

   function F_End_Name
     (Node : Project_Declaration'Class) return Expr;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Identifier`, :ada:ref:`Prefix`
   --% belongs-to: Project_Declaration







         
   

   function F_Is_All
     (Node : Project_Extension'Class) return All_Qualifier;
   --% belongs-to: Project_Extension

      function F_Is_All (Node : Project_Extension'Class) return Boolean;
      --% belongs-to: Project_Extension


         
   

   function F_Path_Name
     (Node : Project_Extension'Class) return String_Literal;
   --% belongs-to: Project_Extension










































         
   

   function F_Attr_Ref
     (Node : Project_Reference'Class) return Attribute_Reference;
   --% belongs-to: Project_Reference












         
   

   function F_Str_Lit
     (Node : String_Literal_At'Class) return String_Literal;
   --% belongs-to: String_Literal_At


         
   

   function F_At_Lit
     (Node : String_Literal_At'Class) return Num_Literal;
   --% belongs-to: String_Literal_At





         function List_Child
           (Node : String_Literal_List'Class; Index : Positive)
            return String_Literal;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function String_Literal_List_First (Node : String_Literal_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function String_Literal_List_Next
           (Node : String_Literal_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function String_Literal_List_Has_Element
           (Node : String_Literal_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function String_Literal_List_Element
           (Node : String_Literal_List; Cursor : Positive)
            return String_Literal'Class;
         --  Implementation detail for the Iterable aspect









         function List_Child
           (Node : Term_List_List'Class; Index : Positive)
            return Term_List;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Term_List_List_First (Node : Term_List_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Term_List_List_Next
           (Node : Term_List_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Term_List_List_Has_Element
           (Node : Term_List_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Term_List_List_Element
           (Node : Term_List_List; Cursor : Positive)
            return Term_List'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Terms
     (Node : Terms'Class) return Term_List_List;
   --% belongs-to: Terms







         
   

   function F_Var_Type_Name
     (Node : Type_Reference'Class) return Identifier_List;
   --% belongs-to: Type_Reference







         
   

   function F_Type_Id
     (Node : Typed_String_Decl'Class) return Identifier;
   --% belongs-to: Typed_String_Decl


         
   

   function F_String_Literals
     (Node : Typed_String_Decl'Class) return String_Literal_List;
   --% belongs-to: Typed_String_Decl







         
   

   function F_Var_Name
     (Node : Variable_Decl'Class) return Identifier;
   --% belongs-to: Variable_Decl


         
   

   function F_Var_Type
     (Node : Variable_Decl'Class) return Type_Reference;
   --% belongs-to: Variable_Decl


         
   

   function F_Expr
     (Node : Variable_Decl'Class) return Term_List;
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Builtin_Function_Call`, :ada:ref:`Project_Reference`,
   --  :ada:ref:`String_Literal_At`, :ada:ref:`Terms`,
   --  :ada:ref:`Variable_Reference`
   --% belongs-to: Variable_Decl







         
   

   function F_Variable_Name
     (Node : Variable_Reference'Class) return Identifier_List;
   --% belongs-to: Variable_Reference


         
   

   function F_Attribute_Ref
     (Node : Variable_Reference'Class) return Attribute_Reference;
   --% belongs-to: Variable_Reference







         
   

   function F_Is_Limited
     (Node : With_Decl'Class) return Limited_Node;
   --% belongs-to: With_Decl

      function F_Is_Limited (Node : With_Decl'Class) return Boolean;
      --% belongs-to: With_Decl


         
   

   function F_Path_Names
     (Node : With_Decl'Class) return String_Literal_List;
   --% belongs-to: With_Decl





         function List_Child
           (Node : With_Decl_List'Class; Index : Positive)
            return With_Decl;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function With_Decl_List_First (Node : With_Decl_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function With_Decl_List_Next
           (Node : With_Decl_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function With_Decl_List_Has_Element
           (Node : With_Decl_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function With_Decl_List_Element
           (Node : With_Decl_List; Cursor : Positive)
            return With_Decl'Class;
         --  Implementation detail for the Iterable aspect



   pragma Warnings (On, "defined after private extension");

   -------------------------------
   -- Tree traversal operations --
   -------------------------------

   function Children_Count
     (Node : Gpr_Node'Class) return Natural;
   --  Return the number of children ``Node`` has

   function First_Child_Index
     (Node : Gpr_Node'Class) return Natural;
   --  Return the index of the first child ``Node`` has

   function Last_Child_Index
     (Node : Gpr_Node'Class) return Natural;
   --  Return the index of the last child ``Node`` has, or 0 if there is no
   --  child.

   pragma Warnings (Off, "defined after private extension");
   procedure Get_Child
     (Node            : Gpr_Node'Class;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Gpr_Node);
   --  Return the ``Index``'th child of node, storing it into ``Result``.
   --
   --  Child indexing is 1-based. Store in ``Index_In_Bounds`` whether ``Node``
   --  had such a child: if not (i.e. ``Index`` is out-of-bounds), set
   --  ``Result`` to a null node.

   function Child
     (Node  : Gpr_Node'Class;
      Index : Positive)
      return Gpr_Node;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function First_Child
     (Node : Gpr_Node'Class) return Gpr_Node;
   --  Return the first child ``Node`` has, or ``No_Gpr_Node``
   --  if there is none.

   function Last_Child
     (Node : Gpr_Node'Class) return Gpr_Node;
   --  Return the last child ``Node`` has, or ``No_Gpr_Node`` if
   --  there is none.
   pragma Warnings (On, "defined after private extension");

   function Traverse
     (Node  : Gpr_Node'Class;
      Visit : access function (Node : Gpr_Node'Class)
                               return Visit_Status)
     return Visit_Status;
   --  Call ``Visit`` on ``Node`` and all its children, transitively. Calls
   --  happen in prefix order (i.e. top-down and left first). The traversal is
   --  controlled as follows by the result returned by Visit:
   --
   --  ``Into``
   --     The traversal continues normally with the syntactic children of the
   --     node just processed.
   --
   --  ``Over``
   --     The children of the node just processed are skipped and excluded from
   --     the traversal, but otherwise processing continues elsewhere in the
   --     tree.
   --
   --  ``Stop``
   --     The entire traversal is immediately abandoned, and the original call
   --     to ``Traverse`` returns ``Stop``.

   procedure Traverse
     (Node  : Gpr_Node'Class;
      Visit : access function (Node : Gpr_Node'Class)
                               return Visit_Status);
   --  This is the same as ``Traverse`` function except that no result is
   --  returned i.e. the ``Traverse`` function is called and the result is
   --  simply discarded.

   ----------------------------------------
   -- Source location-related operations --
   ----------------------------------------

   function Sloc_Range
     (Node : Gpr_Node'Class) return Source_Location_Range;
   --  Return the source location range corresponding to the set of tokens from
   --  which Node was parsed.

   function Compare
     (Node : Gpr_Node'Class;
      Sloc : Source_Location) return Relative_Position;
   --  Compare Sloc to the sloc range of Node

   pragma Warnings (Off, "defined after private extension");
   function Lookup
     (Node : Gpr_Node'Class;
      Sloc : Source_Location) return Gpr_Node;
   --  Look for the bottom-most AST node whose sloc range contains Sloc. Return
   --  it, or null if no such node was found.
   pragma Warnings (On, "defined after private extension");

   -----------------------
   -- Lexical utilities --
   -----------------------

   function Text (Node : Gpr_Node'Class) return Text_Type;
   --  Return the source buffer slice corresponding to the text that spans
   --  between the first and the last tokens of this node.
   --
   --  Note that this returns the empty string for synthetic nodes.

   function Token_Range
     (Node : Gpr_Node'Class) return Token_Iterator;
   --  Return an iterator on the range of tokens encompassed by Node

   


   -------------------
   -- Debug helpers --
   -------------------

   procedure Print
     (Node        : Gpr_Node'Class;
      Show_Slocs  : Boolean := True;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children.
   --
   --  If Show_Slocs, include AST nodes' source locations in the output.
   --
   --  Line_Prefix is prepended to each output line.

   procedure PP_Trivia
     (Node        : Gpr_Node'Class;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children along
   --  with the trivia associated to them. Line_Prefix is prepended to each
   --  output line.

   procedure Assign_Names_To_Logic_Vars (Node : Gpr_Node'Class);
   --  Debug helper: Assign names to every logical variable in the root node,
   --  so that we can trace logical variables.

   --  The following As_* functions convert references to nodes from one type
   --  to another (Gpr_Node can refer to any node type). They
   --  raise a Constraint_Error if the conversion is invalid.

   pragma Warnings (Off, "defined after private extension");
      function As_Gpr_Node
        (Node : Gpr_Node'Class) return Gpr_Node;
      --% no-document: True
      function As_Ada_Prelude_Node
        (Node : Gpr_Node'Class) return Ada_Prelude_Node;
      --% no-document: True
      function As_Ada_Access_Subp
        (Node : Gpr_Node'Class) return Ada_Access_Subp;
      --% no-document: True
      function As_Ada_Context_Clause
        (Node : Gpr_Node'Class) return Ada_Context_Clause;
      --% no-document: True
      function As_Base_List
        (Node : Gpr_Node'Class) return Base_List;
      --% no-document: True
      function As_Ada_Context_Clause_List
        (Node : Gpr_Node'Class) return Ada_Context_Clause_List;
      --% no-document: True
      function As_Ada_Entity_Kind
        (Node : Gpr_Node'Class) return Ada_Entity_Kind;
      --% no-document: True
      function As_Ada_Entity_Kind_Function
        (Node : Gpr_Node'Class) return Ada_Entity_Kind_Function;
      --% no-document: True
      function As_Ada_Entity_Kind_Package
        (Node : Gpr_Node'Class) return Ada_Entity_Kind_Package;
      --% no-document: True
      function As_Ada_Entity_Kind_Procedure
        (Node : Gpr_Node'Class) return Ada_Entity_Kind_Procedure;
      --% no-document: True
      function As_Ada_Generic
        (Node : Gpr_Node'Class) return Ada_Generic;
      --% no-document: True
      function As_Ada_Library_Item
        (Node : Gpr_Node'Class) return Ada_Library_Item;
      --% no-document: True
      function As_Ada_Main
        (Node : Gpr_Node'Class) return Ada_Main;
      --% no-document: True
      function As_Ada_Pkg
        (Node : Gpr_Node'Class) return Ada_Pkg;
      --% no-document: True
      function As_Ada_Pkg_Body
        (Node : Gpr_Node'Class) return Ada_Pkg_Body;
      --% no-document: True
      function As_Ada_Pragma
        (Node : Gpr_Node'Class) return Ada_Pragma;
      --% no-document: True
      function As_Ada_Prelude
        (Node : Gpr_Node'Class) return Ada_Prelude;
      --% no-document: True
      function As_Ada_Prelude_Node_List
        (Node : Gpr_Node'Class) return Ada_Prelude_Node_List;
      --% no-document: True
      function As_Ada_Separate
        (Node : Gpr_Node'Class) return Ada_Separate;
      --% no-document: True
      function As_Ada_Skip
        (Node : Gpr_Node'Class) return Ada_Skip;
      --% no-document: True
      function As_Ada_Skip_List
        (Node : Gpr_Node'Class) return Ada_Skip_List;
      --% no-document: True
      function As_Ada_Subp
        (Node : Gpr_Node'Class) return Ada_Subp;
      --% no-document: True
      function As_Ada_Use
        (Node : Gpr_Node'Class) return Ada_Use;
      --% no-document: True
      function As_Ada_With
        (Node : Gpr_Node'Class) return Ada_With;
      --% no-document: True
      function As_Ada_With_Formal
        (Node : Gpr_Node'Class) return Ada_With_Formal;
      --% no-document: True
      function As_All_Qualifier
        (Node : Gpr_Node'Class) return All_Qualifier;
      --% no-document: True
      function As_All_Qualifier_Absent
        (Node : Gpr_Node'Class) return All_Qualifier_Absent;
      --% no-document: True
      function As_All_Qualifier_Present
        (Node : Gpr_Node'Class) return All_Qualifier_Present;
      --% no-document: True
      function As_Attribute_Decl
        (Node : Gpr_Node'Class) return Attribute_Decl;
      --% no-document: True
      function As_Attribute_Reference
        (Node : Gpr_Node'Class) return Attribute_Reference;
      --% no-document: True
      function As_Builtin_Function_Call
        (Node : Gpr_Node'Class) return Builtin_Function_Call;
      --% no-document: True
      function As_Case_Construction
        (Node : Gpr_Node'Class) return Case_Construction;
      --% no-document: True
      function As_Case_Item
        (Node : Gpr_Node'Class) return Case_Item;
      --% no-document: True
      function As_Case_Item_List
        (Node : Gpr_Node'Class) return Case_Item_List;
      --% no-document: True
      function As_Gpr_Node_List
        (Node : Gpr_Node'Class) return Gpr_Node_List;
      --% no-document: True
      function As_Choices
        (Node : Gpr_Node'Class) return Choices;
      --% no-document: True
      function As_Compilation_Unit
        (Node : Gpr_Node'Class) return Compilation_Unit;
      --% no-document: True
      function As_Empty_Decl
        (Node : Gpr_Node'Class) return Empty_Decl;
      --% no-document: True
      function As_Expr
        (Node : Gpr_Node'Class) return Expr;
      --% no-document: True
      function As_Expr_List
        (Node : Gpr_Node'Class) return Expr_List;
      --% no-document: True
      function As_Single_Tok_Node
        (Node : Gpr_Node'Class) return Single_Tok_Node;
      --% no-document: True
      function As_Identifier
        (Node : Gpr_Node'Class) return Identifier;
      --% no-document: True
      function As_Identifier_List
        (Node : Gpr_Node'Class) return Identifier_List;
      --% no-document: True
      function As_Limited_Node
        (Node : Gpr_Node'Class) return Limited_Node;
      --% no-document: True
      function As_Limited_Absent
        (Node : Gpr_Node'Class) return Limited_Absent;
      --% no-document: True
      function As_Limited_Present
        (Node : Gpr_Node'Class) return Limited_Present;
      --% no-document: True
      function As_Num_Literal
        (Node : Gpr_Node'Class) return Num_Literal;
      --% no-document: True
      function As_Others_Designator
        (Node : Gpr_Node'Class) return Others_Designator;
      --% no-document: True
      function As_Package_Decl
        (Node : Gpr_Node'Class) return Package_Decl;
      --% no-document: True
      function As_Package_Extension
        (Node : Gpr_Node'Class) return Package_Extension;
      --% no-document: True
      function As_Package_Renaming
        (Node : Gpr_Node'Class) return Package_Renaming;
      --% no-document: True
      function As_Package_Spec
        (Node : Gpr_Node'Class) return Package_Spec;
      --% no-document: True
      function As_Prefix
        (Node : Gpr_Node'Class) return Prefix;
      --% no-document: True
      function As_Private_Node
        (Node : Gpr_Node'Class) return Private_Node;
      --% no-document: True
      function As_Private_Absent
        (Node : Gpr_Node'Class) return Private_Absent;
      --% no-document: True
      function As_Private_Present
        (Node : Gpr_Node'Class) return Private_Present;
      --% no-document: True
      function As_Project
        (Node : Gpr_Node'Class) return Project;
      --% no-document: True
      function As_Project_Declaration
        (Node : Gpr_Node'Class) return Project_Declaration;
      --% no-document: True
      function As_Project_Extension
        (Node : Gpr_Node'Class) return Project_Extension;
      --% no-document: True
      function As_Project_Qualifier
        (Node : Gpr_Node'Class) return Project_Qualifier;
      --% no-document: True
      function As_Project_Qualifier_Abstract
        (Node : Gpr_Node'Class) return Project_Qualifier_Abstract;
      --% no-document: True
      function As_Project_Qualifier_Aggregate
        (Node : Gpr_Node'Class) return Project_Qualifier_Aggregate;
      --% no-document: True
      function As_Project_Qualifier_Aggregate_Library
        (Node : Gpr_Node'Class) return Project_Qualifier_Aggregate_Library;
      --% no-document: True
      function As_Project_Qualifier_Configuration
        (Node : Gpr_Node'Class) return Project_Qualifier_Configuration;
      --% no-document: True
      function As_Project_Qualifier_Library
        (Node : Gpr_Node'Class) return Project_Qualifier_Library;
      --% no-document: True
      function As_Project_Qualifier_Standard
        (Node : Gpr_Node'Class) return Project_Qualifier_Standard;
      --% no-document: True
      function As_Project_Reference
        (Node : Gpr_Node'Class) return Project_Reference;
      --% no-document: True
      function As_String_Literal
        (Node : Gpr_Node'Class) return String_Literal;
      --% no-document: True
      function As_String_Literal_At
        (Node : Gpr_Node'Class) return String_Literal_At;
      --% no-document: True
      function As_String_Literal_List
        (Node : Gpr_Node'Class) return String_Literal_List;
      --% no-document: True
      function As_Term_List
        (Node : Gpr_Node'Class) return Term_List;
      --% no-document: True
      function As_Term_List_List
        (Node : Gpr_Node'Class) return Term_List_List;
      --% no-document: True
      function As_Terms
        (Node : Gpr_Node'Class) return Terms;
      --% no-document: True
      function As_Type_Reference
        (Node : Gpr_Node'Class) return Type_Reference;
      --% no-document: True
      function As_Typed_String_Decl
        (Node : Gpr_Node'Class) return Typed_String_Decl;
      --% no-document: True
      function As_Variable_Decl
        (Node : Gpr_Node'Class) return Variable_Decl;
      --% no-document: True
      function As_Variable_Reference
        (Node : Gpr_Node'Class) return Variable_Reference;
      --% no-document: True
      function As_With_Decl
        (Node : Gpr_Node'Class) return With_Decl;
      --% no-document: True
      function As_With_Decl_List
        (Node : Gpr_Node'Class) return With_Decl_List;
      --% no-document: True

   function Hash
     (Node : Gpr_Node) return Ada.Containers.Hash_Type;
   --  Generic hash function, to be used for nodes as keys in hash tables
   pragma Warnings (On, "defined after private extension");

private

   type Internal_Context_Access is
      access all Implementation.Analysis_Context_Type;
   type Internal_Unit_Access is
      access all Implementation.Analysis_Unit_Type;

   type Analysis_Context is new Ada.Finalization.Controlled with record
      Internal : Internal_Context_Access;
   end record;

   overriding procedure Initialize (Context : in out Analysis_Context);
   overriding procedure Adjust (Context : in out Analysis_Context);
   overriding procedure Finalize (Context : in out Analysis_Context);

   type Analysis_Unit is new Gpr_Parser_Support.Text.Text_Buffer_Ifc with record
      Internal : Internal_Unit_Access;

      Context : Analysis_Context;
      --  Keep a reference to the owning context so that the context lives as
      --  long as there is at least one reference to one of its units.
   end record;

   No_Analysis_Context : constant Analysis_Context :=
     (Ada.Finalization.Controlled with Internal => null);
   No_Analysis_Unit    : constant Analysis_Unit :=
     (Internal => null,
      Context  => (Ada.Finalization.Controlled with Internal => null));

   --------------------------
   -- AST nodes (internal) --
   --------------------------

         type Gpr_Node is tagged record
            Internal   : Implementation.AST_Envs.Entity;
            Safety_Net : Implementation.Node_Safety_Net;
         end record;
      No_Gpr_Node : constant Gpr_Node :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Prelude_Node is new Gpr_Node with null record;
      No_Ada_Prelude_Node : constant Ada_Prelude_Node :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Access_Subp is new Ada_Prelude_Node with null record;
      No_Ada_Access_Subp : constant Ada_Access_Subp :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Context_Clause is new Ada_Prelude_Node with null record;
      No_Ada_Context_Clause : constant Ada_Context_Clause :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Base_List is new Gpr_Node with null record;
      No_Base_List : constant Base_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Context_Clause_List is new Base_List with null record;
      No_Ada_Context_Clause_List : constant Ada_Context_Clause_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Entity_Kind is new Ada_Prelude_Node with null record;
      No_Ada_Entity_Kind : constant Ada_Entity_Kind :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Entity_Kind_Function is new Ada_Entity_Kind with null record;
      No_Ada_Entity_Kind_Function : constant Ada_Entity_Kind_Function :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Entity_Kind_Package is new Ada_Entity_Kind with null record;
      No_Ada_Entity_Kind_Package : constant Ada_Entity_Kind_Package :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Entity_Kind_Procedure is new Ada_Entity_Kind with null record;
      No_Ada_Entity_Kind_Procedure : constant Ada_Entity_Kind_Procedure :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Generic is new Ada_Prelude_Node with null record;
      No_Ada_Generic : constant Ada_Generic :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Library_Item is new Ada_Prelude_Node with null record;
      No_Ada_Library_Item : constant Ada_Library_Item :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Main is new Ada_Prelude_Node with null record;
      No_Ada_Main : constant Ada_Main :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Pkg is new Ada_Main with null record;
      No_Ada_Pkg : constant Ada_Pkg :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Pkg_Body is new Ada_Main with null record;
      No_Ada_Pkg_Body : constant Ada_Pkg_Body :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Pragma is new Ada_Context_Clause with null record;
      No_Ada_Pragma : constant Ada_Pragma :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Prelude is new Ada_Prelude_Node with null record;
      No_Ada_Prelude : constant Ada_Prelude :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Prelude_Node_List is new Base_List with null record;
      No_Ada_Prelude_Node_List : constant Ada_Prelude_Node_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Separate is new Ada_Prelude_Node with null record;
      No_Ada_Separate : constant Ada_Separate :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Skip is new Ada_Prelude_Node with null record;
      No_Ada_Skip : constant Ada_Skip :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Skip_List is new Base_List with null record;
      No_Ada_Skip_List : constant Ada_Skip_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Subp is new Ada_Main with null record;
      No_Ada_Subp : constant Ada_Subp :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_Use is new Ada_Context_Clause with null record;
      No_Ada_Use : constant Ada_Use :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_With is new Ada_Context_Clause with null record;
      No_Ada_With : constant Ada_With :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ada_With_Formal is new Ada_Prelude_Node with null record;
      No_Ada_With_Formal : constant Ada_With_Formal :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type All_Qualifier is new Gpr_Node with null record;
      No_All_Qualifier : constant All_Qualifier :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type All_Qualifier_Absent is new All_Qualifier with null record;
      No_All_Qualifier_Absent : constant All_Qualifier_Absent :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type All_Qualifier_Present is new All_Qualifier with null record;
      No_All_Qualifier_Present : constant All_Qualifier_Present :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Attribute_Decl is new Gpr_Node with null record;
      No_Attribute_Decl : constant Attribute_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Attribute_Reference is new Gpr_Node with null record;
      No_Attribute_Reference : constant Attribute_Reference :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Builtin_Function_Call is new Gpr_Node with null record;
      No_Builtin_Function_Call : constant Builtin_Function_Call :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Case_Construction is new Gpr_Node with null record;
      No_Case_Construction : constant Case_Construction :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Case_Item is new Gpr_Node with null record;
      No_Case_Item : constant Case_Item :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Case_Item_List is new Base_List with null record;
      No_Case_Item_List : constant Case_Item_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Gpr_Node_List is new Base_List with null record;
      No_Gpr_Node_List : constant Gpr_Node_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Choices is new Gpr_Node_List with null record;
      No_Choices : constant Choices :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Compilation_Unit is new Gpr_Node with null record;
      No_Compilation_Unit : constant Compilation_Unit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Empty_Decl is new Gpr_Node with null record;
      No_Empty_Decl : constant Empty_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Expr is new Gpr_Node with null record;
      No_Expr : constant Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Expr_List is new Base_List with null record;
      No_Expr_List : constant Expr_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Single_Tok_Node is new Expr with null record;
      No_Single_Tok_Node : constant Single_Tok_Node :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Identifier is new Single_Tok_Node with null record;
      No_Identifier : constant Identifier :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Identifier_List is new Base_List with null record;
      No_Identifier_List : constant Identifier_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Limited_Node is new Gpr_Node with null record;
      No_Limited_Node : constant Limited_Node :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Limited_Absent is new Limited_Node with null record;
      No_Limited_Absent : constant Limited_Absent :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Limited_Present is new Limited_Node with null record;
      No_Limited_Present : constant Limited_Present :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Num_Literal is new Single_Tok_Node with null record;
      No_Num_Literal : constant Num_Literal :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Others_Designator is new Gpr_Node with null record;
      No_Others_Designator : constant Others_Designator :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Package_Decl is new Gpr_Node with null record;
      No_Package_Decl : constant Package_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Package_Extension is new Gpr_Node with null record;
      No_Package_Extension : constant Package_Extension :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Package_Renaming is new Gpr_Node with null record;
      No_Package_Renaming : constant Package_Renaming :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Package_Spec is new Gpr_Node with null record;
      No_Package_Spec : constant Package_Spec :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Prefix is new Expr with null record;
      No_Prefix : constant Prefix :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Private_Node is new Gpr_Node with null record;
      No_Private_Node : constant Private_Node :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Private_Absent is new Private_Node with null record;
      No_Private_Absent : constant Private_Absent :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Private_Present is new Private_Node with null record;
      No_Private_Present : constant Private_Present :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Project is new Gpr_Node with null record;
      No_Project : constant Project :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Project_Declaration is new Gpr_Node with null record;
      No_Project_Declaration : constant Project_Declaration :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Project_Extension is new Gpr_Node with null record;
      No_Project_Extension : constant Project_Extension :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Project_Qualifier is new Gpr_Node with null record;
      No_Project_Qualifier : constant Project_Qualifier :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Project_Qualifier_Abstract is new Project_Qualifier with null record;
      No_Project_Qualifier_Abstract : constant Project_Qualifier_Abstract :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Project_Qualifier_Aggregate is new Project_Qualifier with null record;
      No_Project_Qualifier_Aggregate : constant Project_Qualifier_Aggregate :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Project_Qualifier_Aggregate_Library is new Project_Qualifier with null record;
      No_Project_Qualifier_Aggregate_Library : constant Project_Qualifier_Aggregate_Library :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Project_Qualifier_Configuration is new Project_Qualifier with null record;
      No_Project_Qualifier_Configuration : constant Project_Qualifier_Configuration :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Project_Qualifier_Library is new Project_Qualifier with null record;
      No_Project_Qualifier_Library : constant Project_Qualifier_Library :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Project_Qualifier_Standard is new Project_Qualifier with null record;
      No_Project_Qualifier_Standard : constant Project_Qualifier_Standard :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Project_Reference is new Gpr_Node with null record;
      No_Project_Reference : constant Project_Reference :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type String_Literal is new Single_Tok_Node with null record;
      No_String_Literal : constant String_Literal :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type String_Literal_At is new Gpr_Node with null record;
      No_String_Literal_At : constant String_Literal_At :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type String_Literal_List is new Base_List with null record;
      No_String_Literal_List : constant String_Literal_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Term_List is new Gpr_Node_List with null record;
      No_Term_List : constant Term_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Term_List_List is new Base_List with null record;
      No_Term_List_List : constant Term_List_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Terms is new Gpr_Node with null record;
      No_Terms : constant Terms :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Type_Reference is new Gpr_Node with null record;
      No_Type_Reference : constant Type_Reference :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Typed_String_Decl is new Gpr_Node with null record;
      No_Typed_String_Decl : constant Typed_String_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Variable_Decl is new Gpr_Node with null record;
      No_Variable_Decl : constant Variable_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Variable_Reference is new Gpr_Node with null record;
      No_Variable_Reference : constant Variable_Reference :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type With_Decl is new Gpr_Node with null record;
      No_With_Decl : constant With_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type With_Decl_List is new Base_List with null record;
      No_With_Decl_List : constant With_Decl_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);

   procedure Check_Safety_Net (Self : Gpr_Node'Class);
   --  Check that Self's node and rebindings are still valid, raising a
   --  Stale_Reference_Error if one is not.

   --------------------------------
   -- Token Iterator (internals) --
   --------------------------------

   type Token_Iterator is record
      Node : Gpr_Node;
      Last : Token_Index;
   end record;

   ---------------------------------
   -- Composite types (internals) --
   ---------------------------------

            


   --  The dummy references to these packages forces them to be included in
   --  statically linked builds (thanks to the binder). This benefits the GDB
   --  helpers at no cost.

   Version : String renames Gpr_Parser.Version;
   procedure RN (Node : Gpr_Parser.Implementation.Bare_Gpr_Node)
      renames Gpr_Parser.Debug.PN;

end Gpr_Parser.Analysis;
