
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--




with GNATCOLL.GMP.Integers;

with Gpr_Parser_Support.Errors;
private with Gpr_Parser_Support.Internal.Analysis;
with Gpr_Parser_Support.Symbols; use Gpr_Parser_Support.Symbols;
with Gpr_Parser_Support.Token_Data_Handlers;
use Gpr_Parser_Support.Token_Data_Handlers;
with Gpr_Parser_Support.Types;   use Gpr_Parser_Support.Types;


--  This package provides types and functions used in the whole Gpr_Parser
--  package tree.

package Gpr_Parser.Common is

   use Support.Slocs, Support.Text;

   subtype Big_Integer is GNATCOLL.GMP.Integers.Big_Integer;
   --  Shortcut for ``GNATCOLL.GMP.Integers.Big_Integer``

   

   Default_Charset : constant String := "iso-8859-1";
   --  Default charset to use when creating analysis contexts

   ----------------
   -- Exceptions --
   ----------------

   File_Read_Error : exception renames Gpr_Parser_Support.Errors.File_Read_Error;
   --  Subprograms may raise this when they cannot open a source file. Note
   --  that this does *not* concern analysis unit getters, which create
   --  diagnostic vectors for such errors.

   Invalid_Input : exception renames Gpr_Parser_Support.Errors.Invalid_Input;
   --  Raised by lexing functions (``Gpr_Parser.Lexer``) when the input
   --  contains an invalid byte sequence.

   Invalid_Symbol_Error : exception renames Gpr_Parser_Support.Errors.Invalid_Symbol_Error;
   --  Exception raise when an invalid symbol is passed to a subprogram.

   Invalid_Unit_Name_Error : exception renames Gpr_Parser_Support.Errors.Invalid_Unit_Name_Error;
   --  Raised when an invalid unit name is provided.

   Native_Exception : exception renames Gpr_Parser_Support.Errors.Native_Exception;
   --  Exception raised in language bindings when the underlying C API reports
   --  an unexpected error that occurred in the library.
   --
   --  This kind of exception is raised for internal errors: they should never
   --  happen in normal situations and if they are raised at some point, it
   --  means the library state is potentially corrupted.
   --
   --  Nevertheless, the library does its best not to crash the program,
   --  materializing internal errors using this kind of exception.

   Precondition_Failure : exception renames Gpr_Parser_Support.Errors.Precondition_Failure;
   --  Exception raised when an API is called while its preconditions are not
   --  satisfied.

   Property_Error : exception renames Gpr_Parser_Support.Errors.Property_Error;
   --  Exception that is raised when an error occurs while evaluating any
   --  function whose name starts with ``P_``. This is the only exceptions that
   --  such functions can raise.

   Stale_Reference_Error : exception renames Gpr_Parser_Support.Errors.Stale_Reference_Error;
   --  Exception raised while trying to access data that was deallocated. This
   --  happens when one tries to use a node whose unit has been reparsed, for
   --  instance.

   Syntax_Error : exception renames Gpr_Parser_Support.Errors.Syntax_Error;
   --  Subprograms may raise this when they try to parse invalid syntax. Note
   --  that this does *not* concern analysis unit getters, which create
   --  diagnostic vectors for such errors.

   Unknown_Charset : exception renames Gpr_Parser_Support.Errors.Unknown_Charset;
   --  Raised by lexing functions (``Gpr_Parser.Lexer``) when the input charset
   --  is not supported.

   -------------------
   -- Introspection --
   -------------------

   Bad_Type_Error : exception renames Gpr_Parser_Support.Errors.Introspection.Bad_Type_Error;
   --  Raised when introspection functions (``Gpr_Parser.Introspection``) are
   --  provided mismatching types/values.

   Out_Of_Bounds_Error : exception renames Gpr_Parser_Support.Errors.Introspection.Out_Of_Bounds_Error;
   --  Raised when introspection functions (``Gpr_Parser.Introspection``) are
   --  passed an out of bounds index.

   ---------------
   -- Rewriting --
   ---------------

   Template_Args_Error : exception renames Gpr_Parser_Support.Errors.Rewriting.Template_Args_Error;
   --  Exception raised when the provided arguments for a template don't match
   --  what the template expects.

   Template_Format_Error : exception renames Gpr_Parser_Support.Errors.Rewriting.Template_Format_Error;
   --  Exception raised when a template has an invalid syntax, such as badly
   --  formatted placeholders.

   Template_Instantiation_Error : exception renames Gpr_Parser_Support.Errors.Rewriting.Template_Instantiation_Error;
   --  Exception raised when the instantiation of a template cannot be parsed.

   ---------------
   -- Unparsing --
   ---------------

   Malformed_Tree_Error : exception renames Gpr_Parser_Support.Errors.Unparsing.Malformed_Tree_Error;
   --  Raised when unparsing functions working on rewritten trees
   --  (``Gpr_Parser.Rewriting``) are called on malformed trees.


   ----------------------------
   -- Misc enumeration types --
   ----------------------------

      type Analysis_Unit_Kind is
        (Unit_Specification, Unit_Body)
         with Convention => C;
      --  Specify a kind of analysis unit. Specification units provide an
      --  interface to the outer world while body units provide an
      --  implementation for the corresponding interface.


      function Trace_Image (Self : Analysis_Unit_Kind) return String
      is (Self'Image);

      type Lookup_Kind is
        (Recursive, Flat, Minimal)
         with Convention => C;
      


      function Trace_Image (Self : Lookup_Kind) return String
      is (Self'Image);

      type Designated_Env_Kind is
        (None, Current_Env, Named_Env, Direct_Env)
         with Convention => C;
      --  Discriminant for DesignatedEnv structures.


      function Trace_Image (Self : Designated_Env_Kind) return String
      is (Self'Image);

      type Grammar_Rule is
        (Project_Qualifier_Rule, Project_Extension_Rule, Project_Declaration_Rule, Project_Rule, Declarative_Items_Rule, Declarative_Item_Rule, Simple_Declarative_Items_Rule, Simple_Declarative_Item_Rule, Variable_Decl_Rule, Attribute_Decl_Rule, Associative_Array_Index_Rule, Package_Decl_Rule, Package_Renaming_Rule, Package_Extension_Rule, Package_Spec_Rule, Empty_Declaration_Rule, Case_Construction_Rule, Case_Item_Rule, Others_Designator_Rule, Choice_Rule, Discrete_Choice_List_Rule, With_Decl_Rule, Context_Clauses_Rule, Typed_String_Decl_Rule, Identifier_Rule, String_Literal_Rule, Num_Literal_Rule, Static_Name_Rule, Attribute_Reference_Rule, Variable_Reference_Rule, Type_Reference_Rule, Builtin_Function_Call_Rule, Expression_Rule, Expression_List_Rule, String_Literal_At_Rule, Term_Rule, Compilation_Unit_Rule)
         with Convention => C;
      --  Gramar rule to use for parsing.


      function Trace_Image (Self : Grammar_Rule) return String
      is (Self'Image);


   -----------
   -- Nodes --
   -----------

   type Gpr_Node_Kind_Type is
     (Gpr_All_Qualifier_Absent, Gpr_All_Qualifier_Present, Gpr_Attribute_Decl, Gpr_Attribute_Reference, Gpr_Case_Item_List, Gpr_Gpr_Node_List, Gpr_Choices, Gpr_Term_List, Gpr_Identifier_List, Gpr_String_Literal_List, Gpr_Term_List_List, Gpr_With_Decl_List, Gpr_Builtin_Function_Call, Gpr_Case_Construction, Gpr_Case_Item, Gpr_Compilation_Unit, Gpr_Empty_Decl, Gpr_Prefix, Gpr_Identifier, Gpr_Num_Literal, Gpr_String_Literal, Gpr_Limited_Absent, Gpr_Limited_Present, Gpr_Others_Designator, Gpr_Package_Decl, Gpr_Package_Extension, Gpr_Package_Renaming, Gpr_Package_Spec, Gpr_Project, Gpr_Project_Declaration, Gpr_Project_Extension, Gpr_Project_Qualifier_Abstract, Gpr_Project_Qualifier_Aggregate, Gpr_Project_Qualifier_Aggregate_Library, Gpr_Project_Qualifier_Configuration, Gpr_Project_Qualifier_Library, Gpr_Project_Qualifier_Standard, Gpr_String_Literal_At, Gpr_Terms, Gpr_Type_Reference, Gpr_Typed_String_Decl, Gpr_Variable_Decl, Gpr_Variable_Reference, Gpr_With_Decl);
   --  Type for concrete nodes

   for Gpr_Node_Kind_Type use
     (Gpr_All_Qualifier_Absent => 1, Gpr_All_Qualifier_Present => 2, Gpr_Attribute_Decl => 3, Gpr_Attribute_Reference => 4, Gpr_Case_Item_List => 5, Gpr_Gpr_Node_List => 6, Gpr_Choices => 7, Gpr_Term_List => 8, Gpr_Identifier_List => 9, Gpr_String_Literal_List => 10, Gpr_Term_List_List => 11, Gpr_With_Decl_List => 12, Gpr_Builtin_Function_Call => 13, Gpr_Case_Construction => 14, Gpr_Case_Item => 15, Gpr_Compilation_Unit => 16, Gpr_Empty_Decl => 17, Gpr_Prefix => 18, Gpr_Identifier => 19, Gpr_Num_Literal => 20, Gpr_String_Literal => 21, Gpr_Limited_Absent => 22, Gpr_Limited_Present => 23, Gpr_Others_Designator => 24, Gpr_Package_Decl => 25, Gpr_Package_Extension => 26, Gpr_Package_Renaming => 27, Gpr_Package_Spec => 28, Gpr_Project => 29, Gpr_Project_Declaration => 30, Gpr_Project_Extension => 31, Gpr_Project_Qualifier_Abstract => 32, Gpr_Project_Qualifier_Aggregate => 33, Gpr_Project_Qualifier_Aggregate_Library => 34, Gpr_Project_Qualifier_Configuration => 35, Gpr_Project_Qualifier_Library => 36, Gpr_Project_Qualifier_Standard => 37, Gpr_String_Literal_At => 38, Gpr_Terms => 39, Gpr_Type_Reference => 40, Gpr_Typed_String_Decl => 41, Gpr_Variable_Decl => 42, Gpr_Variable_Reference => 43, Gpr_With_Decl => 44);

      subtype Gpr_Gpr_Node is Gpr_Node_Kind_Type
            range Gpr_All_Qualifier_Absent .. Gpr_With_Decl;
      --% no-document: True
      subtype Gpr_All_Qualifier is Gpr_Node_Kind_Type
            range Gpr_All_Qualifier_Absent .. Gpr_All_Qualifier_Present;
      --% no-document: True
      subtype Gpr_All_Qualifier_Absent_Range is Gpr_Node_Kind_Type
            range Gpr_All_Qualifier_Absent .. Gpr_All_Qualifier_Absent;
      --% no-document: True
      subtype Gpr_All_Qualifier_Present_Range is Gpr_Node_Kind_Type
            range Gpr_All_Qualifier_Present .. Gpr_All_Qualifier_Present;
      --% no-document: True
      subtype Gpr_Attribute_Decl_Range is Gpr_Node_Kind_Type
            range Gpr_Attribute_Decl .. Gpr_Attribute_Decl;
      --% no-document: True
      subtype Gpr_Attribute_Reference_Range is Gpr_Node_Kind_Type
            range Gpr_Attribute_Reference .. Gpr_Attribute_Reference;
      --% no-document: True
      subtype Gpr_Base_List is Gpr_Node_Kind_Type
            range Gpr_Case_Item_List .. Gpr_With_Decl_List;
      --% no-document: True
      subtype Gpr_Case_Item_List_Range is Gpr_Node_Kind_Type
            range Gpr_Case_Item_List .. Gpr_Case_Item_List;
      --% no-document: True
      subtype Gpr_Gpr_Node_List_Range is Gpr_Node_Kind_Type
            range Gpr_Gpr_Node_List .. Gpr_Term_List;
      --% no-document: True
      subtype Gpr_Choices_Range is Gpr_Node_Kind_Type
            range Gpr_Choices .. Gpr_Choices;
      --% no-document: True
      subtype Gpr_Term_List_Range is Gpr_Node_Kind_Type
            range Gpr_Term_List .. Gpr_Term_List;
      --% no-document: True
      subtype Gpr_Identifier_List_Range is Gpr_Node_Kind_Type
            range Gpr_Identifier_List .. Gpr_Identifier_List;
      --% no-document: True
      subtype Gpr_String_Literal_List_Range is Gpr_Node_Kind_Type
            range Gpr_String_Literal_List .. Gpr_String_Literal_List;
      --% no-document: True
      subtype Gpr_Term_List_List_Range is Gpr_Node_Kind_Type
            range Gpr_Term_List_List .. Gpr_Term_List_List;
      --% no-document: True
      subtype Gpr_With_Decl_List_Range is Gpr_Node_Kind_Type
            range Gpr_With_Decl_List .. Gpr_With_Decl_List;
      --% no-document: True
      subtype Gpr_Builtin_Function_Call_Range is Gpr_Node_Kind_Type
            range Gpr_Builtin_Function_Call .. Gpr_Builtin_Function_Call;
      --% no-document: True
      subtype Gpr_Case_Construction_Range is Gpr_Node_Kind_Type
            range Gpr_Case_Construction .. Gpr_Case_Construction;
      --% no-document: True
      subtype Gpr_Case_Item_Range is Gpr_Node_Kind_Type
            range Gpr_Case_Item .. Gpr_Case_Item;
      --% no-document: True
      subtype Gpr_Compilation_Unit_Range is Gpr_Node_Kind_Type
            range Gpr_Compilation_Unit .. Gpr_Compilation_Unit;
      --% no-document: True
      subtype Gpr_Empty_Decl_Range is Gpr_Node_Kind_Type
            range Gpr_Empty_Decl .. Gpr_Empty_Decl;
      --% no-document: True
      subtype Gpr_Expr is Gpr_Node_Kind_Type
            range Gpr_Prefix .. Gpr_String_Literal;
      --% no-document: True
      subtype Gpr_Prefix_Range is Gpr_Node_Kind_Type
            range Gpr_Prefix .. Gpr_Prefix;
      --% no-document: True
      subtype Gpr_Single_Tok_Node is Gpr_Node_Kind_Type
            range Gpr_Identifier .. Gpr_String_Literal;
      --% no-document: True
      subtype Gpr_Identifier_Range is Gpr_Node_Kind_Type
            range Gpr_Identifier .. Gpr_Identifier;
      --% no-document: True
      subtype Gpr_Num_Literal_Range is Gpr_Node_Kind_Type
            range Gpr_Num_Literal .. Gpr_Num_Literal;
      --% no-document: True
      subtype Gpr_String_Literal_Range is Gpr_Node_Kind_Type
            range Gpr_String_Literal .. Gpr_String_Literal;
      --% no-document: True
      subtype Gpr_Limited_Node is Gpr_Node_Kind_Type
            range Gpr_Limited_Absent .. Gpr_Limited_Present;
      --% no-document: True
      subtype Gpr_Limited_Absent_Range is Gpr_Node_Kind_Type
            range Gpr_Limited_Absent .. Gpr_Limited_Absent;
      --% no-document: True
      subtype Gpr_Limited_Present_Range is Gpr_Node_Kind_Type
            range Gpr_Limited_Present .. Gpr_Limited_Present;
      --% no-document: True
      subtype Gpr_Others_Designator_Range is Gpr_Node_Kind_Type
            range Gpr_Others_Designator .. Gpr_Others_Designator;
      --% no-document: True
      subtype Gpr_Package_Decl_Range is Gpr_Node_Kind_Type
            range Gpr_Package_Decl .. Gpr_Package_Decl;
      --% no-document: True
      subtype Gpr_Package_Extension_Range is Gpr_Node_Kind_Type
            range Gpr_Package_Extension .. Gpr_Package_Extension;
      --% no-document: True
      subtype Gpr_Package_Renaming_Range is Gpr_Node_Kind_Type
            range Gpr_Package_Renaming .. Gpr_Package_Renaming;
      --% no-document: True
      subtype Gpr_Package_Spec_Range is Gpr_Node_Kind_Type
            range Gpr_Package_Spec .. Gpr_Package_Spec;
      --% no-document: True
      subtype Gpr_Project_Range is Gpr_Node_Kind_Type
            range Gpr_Project .. Gpr_Project;
      --% no-document: True
      subtype Gpr_Project_Declaration_Range is Gpr_Node_Kind_Type
            range Gpr_Project_Declaration .. Gpr_Project_Declaration;
      --% no-document: True
      subtype Gpr_Project_Extension_Range is Gpr_Node_Kind_Type
            range Gpr_Project_Extension .. Gpr_Project_Extension;
      --% no-document: True
      subtype Gpr_Project_Qualifier is Gpr_Node_Kind_Type
            range Gpr_Project_Qualifier_Abstract .. Gpr_Project_Qualifier_Standard;
      --% no-document: True
      subtype Gpr_Project_Qualifier_Abstract_Range is Gpr_Node_Kind_Type
            range Gpr_Project_Qualifier_Abstract .. Gpr_Project_Qualifier_Abstract;
      --% no-document: True
      subtype Gpr_Project_Qualifier_Aggregate_Range is Gpr_Node_Kind_Type
            range Gpr_Project_Qualifier_Aggregate .. Gpr_Project_Qualifier_Aggregate;
      --% no-document: True
      subtype Gpr_Project_Qualifier_Aggregate_Library_Range is Gpr_Node_Kind_Type
            range Gpr_Project_Qualifier_Aggregate_Library .. Gpr_Project_Qualifier_Aggregate_Library;
      --% no-document: True
      subtype Gpr_Project_Qualifier_Configuration_Range is Gpr_Node_Kind_Type
            range Gpr_Project_Qualifier_Configuration .. Gpr_Project_Qualifier_Configuration;
      --% no-document: True
      subtype Gpr_Project_Qualifier_Library_Range is Gpr_Node_Kind_Type
            range Gpr_Project_Qualifier_Library .. Gpr_Project_Qualifier_Library;
      --% no-document: True
      subtype Gpr_Project_Qualifier_Standard_Range is Gpr_Node_Kind_Type
            range Gpr_Project_Qualifier_Standard .. Gpr_Project_Qualifier_Standard;
      --% no-document: True
      subtype Gpr_String_Literal_At_Range is Gpr_Node_Kind_Type
            range Gpr_String_Literal_At .. Gpr_String_Literal_At;
      --% no-document: True
      subtype Gpr_Terms_Range is Gpr_Node_Kind_Type
            range Gpr_Terms .. Gpr_Terms;
      --% no-document: True
      subtype Gpr_Type_Reference_Range is Gpr_Node_Kind_Type
            range Gpr_Type_Reference .. Gpr_Type_Reference;
      --% no-document: True
      subtype Gpr_Typed_String_Decl_Range is Gpr_Node_Kind_Type
            range Gpr_Typed_String_Decl .. Gpr_Typed_String_Decl;
      --% no-document: True
      subtype Gpr_Variable_Decl_Range is Gpr_Node_Kind_Type
            range Gpr_Variable_Decl .. Gpr_Variable_Decl;
      --% no-document: True
      subtype Gpr_Variable_Reference_Range is Gpr_Node_Kind_Type
            range Gpr_Variable_Reference .. Gpr_Variable_Reference;
      --% no-document: True
      subtype Gpr_With_Decl_Range is Gpr_Node_Kind_Type
            range Gpr_With_Decl .. Gpr_With_Decl;
      --% no-document: True

   subtype Synthetic_Nodes is Gpr_Node_Kind_Type
      with Static_Predicate =>
         False
   ;
   --  Set of nodes that are synthetic.
      --
      --  Parsers cannot create synthetic nodes, so these correspond to no
      --  source text. These nodes are created dynamically for convenience
      --  during semantic analysis.

   Default_Grammar_Rule : constant Grammar_Rule := Compilation_Unit_Rule;
   --  Default grammar rule to use when parsing analysis units

   ------------------
   -- Lexer inputs --
   ------------------

   type Lexer_Input_Kind is
     (File,
      --  Readable source file

      Bytes_Buffer,
      --  Buffer of undecoded bytes

      Text_Buffer
      --  Buffer of decoded bytes
   );
   --  Kind of lexer input

   subtype Undecoded_Lexer_Input is
      Lexer_Input_Kind range File ..  Bytes_Buffer;

   ------------
   -- Tokens --
   ------------

   type Token_Kind is (
      Gpr_Termination,
Gpr_Lexing_Failure,
Gpr_Identifier,
Gpr_All,
Gpr_Abstract,
Gpr_At,
Gpr_Case,
Gpr_End,
Gpr_For,
Gpr_Is,
Gpr_Limited,
Gpr_Private,
Gpr_Null,
Gpr_Others,
Gpr_Package,
Gpr_Renames,
Gpr_Type,
Gpr_Use,
Gpr_Pragma,
Gpr_When,
Gpr_With,
Gpr_Extends,
Gpr_Par_Open,
Gpr_Par_Close,
Gpr_Semicolon,
Gpr_Colon,
Gpr_Comma,
Gpr_Dot,
Gpr_Amp,
Gpr_Tick,
Gpr_Pipe,
Gpr_Assign,
Gpr_Arrow,
Gpr_String,
Gpr_Number,
Gpr_Label,
Gpr_Char,
Gpr_Comment,
Gpr_Whitespace
   );
   --  Kind of token: indentifier, string literal, ...

   type Token_Family is
     (Alphanumericals, Default_Family);
   --  Groups of token kinds, to make the processing of some groups of token
   --  uniform.


   Token_Kind_To_Family : array (Token_Kind) of Token_Family :=
     (Gpr_Termination => Default_Family, Gpr_Lexing_Failure => Default_Family, Gpr_Identifier => Alphanumericals, Gpr_All => Alphanumericals, Gpr_Abstract => Alphanumericals, Gpr_At => Alphanumericals, Gpr_Case => Alphanumericals, Gpr_End => Alphanumericals, Gpr_For => Alphanumericals, Gpr_Is => Alphanumericals, Gpr_Limited => Alphanumericals, Gpr_Private => Default_Family, Gpr_Null => Alphanumericals, Gpr_Others => Alphanumericals, Gpr_Package => Alphanumericals, Gpr_Renames => Alphanumericals, Gpr_Type => Alphanumericals, Gpr_Use => Alphanumericals, Gpr_Pragma => Alphanumericals, Gpr_When => Alphanumericals, Gpr_With => Alphanumericals, Gpr_Extends => Alphanumericals, Gpr_Par_Open => Default_Family, Gpr_Par_Close => Default_Family, Gpr_Semicolon => Default_Family, Gpr_Colon => Default_Family, Gpr_Comma => Default_Family, Gpr_Dot => Default_Family, Gpr_Amp => Default_Family, Gpr_Tick => Default_Family, Gpr_Pipe => Default_Family, Gpr_Assign => Default_Family, Gpr_Arrow => Default_Family, Gpr_String => Alphanumericals, Gpr_Number => Alphanumericals, Gpr_Label => Alphanumericals, Gpr_Char => Alphanumericals, Gpr_Comment => Default_Family, Gpr_Whitespace => Default_Family);
   --  Associate a token family to all token kinds
   --
   --% document-value: False

   function Token_Kind_Name (Token_Id : Token_Kind) return String;
   --  Return a human-readable name for a token kind.

   function Token_Kind_Literal (Token_Id : Token_Kind) return Text_Type;
   --  Return the canonical literal corresponding to this token kind, or an
   --  empty string if this token has no literal.

   function Token_Error_Image (Token_Id : Token_Kind) return String;
   --  Return a string representation of ``Token_Id`` that is suitable in error
   --  messages.

   function To_Token_Kind (Raw : Raw_Token_Kind) return Token_Kind
      with Inline;
   function From_Token_Kind (Kind : Token_Kind) return Raw_Token_Kind
      with Inline;

   function Is_Token_Node (Kind : Gpr_Node_Kind_Type) return Boolean;
   --  Return whether Kind corresponds to a token node

   function Is_List_Node (Kind : Gpr_Node_Kind_Type) return Boolean;
   --  Return whether Kind corresponds to a list node

   function Is_Error_Node (Kind : Gpr_Node_Kind_Type) return Boolean;
   --  Return whether Kind corresponds to an error node

   type Visit_Status is (Into, Over, Stop);
   --  Helper type to control the node traversal process. See the
   --  ``Gpr_Parser.Analysis.Traverse`` function.

   -----------------------
   -- Lexical utilities --
   -----------------------

   type Token_Reference is private;
   --  Reference to a token in an analysis unit.

   No_Token : constant Token_Reference;

   type Token_Data_Type is private;

   function "<" (Left, Right : Token_Reference) return Boolean;
   --  Assuming ``Left`` and ``Right`` belong to the same analysis unit, return
   --  whether ``Left`` came before ``Right`` in the source file.

   function Next
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference;
   --  Return a reference to the next token in the corresponding analysis unit.

   function Previous
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference;
   --  Return a reference to the previous token in the corresponding analysis
   --  unit.

   function Data (Token : Token_Reference) return Token_Data_Type;
   --  Return the data associated to ``Token``

   function Is_Equivalent (L, R : Token_Reference) return Boolean;
   --  Return whether ``L`` and ``R`` are structurally equivalent tokens. This
   --  means that their position in the stream won't be taken into account,
   --  only the kind and text of the token.

   function Image (Token : Token_Reference) return String;
   --  Debug helper: return a human-readable text to represent a token

   function Text (Token : Token_Reference) return Text_Type;
   --  Return the text of the token as ``Text_Type``

   function Text (First, Last : Token_Reference) return Text_Type;
   --  Compute the source buffer slice corresponding to the text that spans
   --  between the ``First`` and ``Last`` tokens (both included). This yields
   --  an empty slice if ``Last`` actually appears before ``First``.
   --
   --  This raises a ``Constraint_Error`` if ``First`` and ``Last`` don't
   --  belong to the same analysis unit.

   function Get_Symbol (Token : Token_Reference) return Symbol_Type;
   --  Assuming that ``Token`` refers to a token that contains a symbol, return
   --  the corresponding symbol.

   function Kind (Token_Data : Token_Data_Type) return Token_Kind;
   --  Kind for this token.

   function Is_Trivia (Token : Token_Reference) return Boolean;
   --  Return whether this token is a trivia. If it's not, it's a regular
   --  token.

   function Is_Trivia (Token_Data : Token_Data_Type) return Boolean;
   --  Return whether this token is a trivia. If it's not, it's a regular
   --  token.

   function Index (Token : Token_Reference) return Token_Index;
   --  One-based index for this token/trivia. Tokens and trivias get their own
   --  index space.

   function Index (Token_Data : Token_Data_Type) return Token_Index;
   --  One-based index for this token/trivia. Tokens and trivias get their own
   --  index space.

   function Sloc_Range
     (Token_Data : Token_Data_Type) return Source_Location_Range;
   --  Source location range for this token. Note that the end bound is
   --  exclusive.

   function Origin_Filename (Token : Token_Reference) return String;
   --  Return the name of the file whose content was scanned to create Token.
   --  Return an empty string if the source comes from a memory buffer instead
   --  of a file.

   function Origin_Charset (Token : Token_Reference) return String;
   --  Return the charset used to decode the source that was scanned to create
   --  Token. Return an empty string if the source was already decoded during
   --  the scan.

   function Convert
     (TDH      : Token_Data_Handler;
      Token    : Token_Reference;
      Raw_Data : Stored_Token_Data) return Token_Data_Type;
   --  Turn data from ``TDH`` and ``Raw_Data`` into a user-ready token data
   --  record.

   type Child_Or_Trivia is (Child, Trivia);
   --  Discriminator for the ``Child_Record`` type

   function Raw_Data (T : Token_Reference) return Stored_Token_Data;
   --  Return the raw token data for ``T``

   function Token_Node_Kind (Kind : Gpr_Node_Kind_Type) return Token_Kind
      with Pre => Is_Token_Node (Kind);
   --  Return the token kind corresponding to the given token node kind

   


private

   type Token_Safety_Net is record
      Context         : Gpr_Parser_Support.Internal.Analysis.Internal_Context;
      Context_Version : Version_Number;
      --  Analysis context and version number at the time this safety net was
      --  produced.
      --
      --  TODO: it is not possible to refer to
      --  $.Implementation.Internal_Context from this spec (otherwise we get a
      --  circular dependency). For now, use the generic pointer from
      --  Gpr_Parser_Support (hack), but in the future the Token_Reference type
      --  (and this this safety net type) will go to the generic API, so we
      --  will get rid of this hack.

      TDH_Version : Version_Number;
      --  Version of the token data handler at the time this safety net was
      --  produced.
   end record;
   --  Information to embed in public APIs with token references, used to check
   --  before using the references that they are not stale.

   No_Token_Safety_Net : constant Token_Safety_Net :=
     (Gpr_Parser_Support.Internal.Analysis.No_Internal_Context, 0, 0);

   type Token_Reference is record
      TDH : Token_Data_Handler_Access;
      --  Token data handler that owns this token

      Index : Token_Or_Trivia_Index;
      --  Identifier for the trivia or the token this refers to

      Safety_Net : Token_Safety_Net;
   end record;

   procedure Check_Safety_Net (Self : Token_Reference);
   --  If ``Self`` is a stale token reference, raise a
   --  ``Stale_Reference_Error`` error.

   No_Token : constant Token_Reference :=
     (null, No_Token_Or_Trivia_Index, No_Token_Safety_Net);

   type Token_Data_Type is record
      Kind : Token_Kind;
      --  See documentation for the Kind accessor

      Is_Trivia : Boolean;
      --  See documentation for the Is_Trivia accessor

      Index : Token_Index;
      --  See documentation for the Index accessor

      Source_Buffer : Text_Cst_Access;
      --  Text for the original source file

      Source_First : Positive;
      Source_Last  : Natural;
      --  Bounds in Source_Buffer for the text corresponding to this token

      Sloc_Range : Source_Location_Range;
      --  See documenation for the Sloc_Range accessor
   end record;

end Gpr_Parser.Common;
