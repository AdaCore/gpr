
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
        (Project_Qualifier_Rule, Project_Extension_Rule, Project_Declaration_Rule, Project_Rule, Declarative_Items_Rule, Declarative_Item_Rule, Simple_Declarative_Items_Rule, Simple_Declarative_Item_Rule, Variable_Decl_Rule, Attribute_Decl_Rule, Associative_Array_Index_Rule, Package_Decl_Rule, Package_Renaming_Rule, Package_Extension_Rule, Package_Spec_Rule, Empty_Declaration_Rule, Case_Construction_Rule, Case_Item_Rule, Others_Designator_Rule, Choice_Rule, Discrete_Choice_List_Rule, With_Decl_Rule, Context_Clauses_Rule, Ada_With_Clause_Rule, Ada_Context_Rule, Ada_Context_Item_Rule, Ada_Context_Skip_Rule, Ada_Use_Clause_Rule, Ada_Pragma_Rule, Ada_Subp_Kind_Rule, Ada_Pkg_Kind_Rule, Ada_Library_Item_Rule, Ada_Prelude_Rule, Typed_String_Decl_Rule, Identifier_Rule, String_Literal_Rule, Num_Literal_Rule, Static_Name_Rule, Attribute_Reference_Rule, Variable_Reference_Rule, Type_Reference_Rule, Builtin_Function_Call_Rule, Expression_Rule, Expression_List_Rule, String_Literal_At_Rule, Project_Reference_Rule, Term_Rule, Compilation_Unit_Rule)
         with Convention => C;
      --  Gramar rule to use for parsing.


      function Trace_Image (Self : Grammar_Rule) return String
      is (Self'Image);


   -----------
   -- Nodes --
   -----------

   type Gpr_Node_Kind_Type is
     (Gpr_Ada_Access_Subp, Gpr_Ada_Pragma, Gpr_Ada_Use, Gpr_Ada_With, Gpr_Ada_Entity_Kind_Function, Gpr_Ada_Entity_Kind_Package, Gpr_Ada_Entity_Kind_Procedure, Gpr_Ada_Generic, Gpr_Ada_Library_Item, Gpr_Ada_Pkg, Gpr_Ada_Pkg_Body, Gpr_Ada_Subp, Gpr_Ada_Prelude, Gpr_Ada_Separate, Gpr_Ada_Skip, Gpr_Ada_With_Formal, Gpr_All_Qualifier_Absent, Gpr_All_Qualifier_Present, Gpr_Attribute_Decl, Gpr_Attribute_Reference, Gpr_Ada_Context_Clause_List, Gpr_Ada_Prelude_Node_List, Gpr_Ada_Skip_List, Gpr_Case_Item_List, Gpr_Expr_List, Gpr_Gpr_Node_List, Gpr_Choices, Gpr_Term_List, Gpr_Identifier_List, Gpr_String_Literal_List, Gpr_Term_List_List, Gpr_With_Decl_List, Gpr_Builtin_Function_Call, Gpr_Case_Construction, Gpr_Case_Item, Gpr_Compilation_Unit, Gpr_Empty_Decl, Gpr_Prefix, Gpr_Identifier, Gpr_Num_Literal, Gpr_String_Literal, Gpr_Limited_Absent, Gpr_Limited_Present, Gpr_Others_Designator, Gpr_Package_Decl, Gpr_Package_Extension, Gpr_Package_Renaming, Gpr_Package_Spec, Gpr_Private_Absent, Gpr_Private_Present, Gpr_Project, Gpr_Project_Declaration, Gpr_Project_Extension, Gpr_Project_Qualifier_Abstract, Gpr_Project_Qualifier_Aggregate, Gpr_Project_Qualifier_Aggregate_Library, Gpr_Project_Qualifier_Configuration, Gpr_Project_Qualifier_Library, Gpr_Project_Qualifier_Standard, Gpr_Project_Reference, Gpr_String_Literal_At, Gpr_Terms, Gpr_Type_Reference, Gpr_Typed_String_Decl, Gpr_Variable_Decl, Gpr_Variable_Reference, Gpr_With_Decl);
   --  Type for concrete nodes

   for Gpr_Node_Kind_Type use
     (Gpr_Ada_Access_Subp => 1, Gpr_Ada_Pragma => 2, Gpr_Ada_Use => 3, Gpr_Ada_With => 4, Gpr_Ada_Entity_Kind_Function => 5, Gpr_Ada_Entity_Kind_Package => 6, Gpr_Ada_Entity_Kind_Procedure => 7, Gpr_Ada_Generic => 8, Gpr_Ada_Library_Item => 9, Gpr_Ada_Pkg => 10, Gpr_Ada_Pkg_Body => 11, Gpr_Ada_Subp => 12, Gpr_Ada_Prelude => 13, Gpr_Ada_Separate => 14, Gpr_Ada_Skip => 15, Gpr_Ada_With_Formal => 16, Gpr_All_Qualifier_Absent => 17, Gpr_All_Qualifier_Present => 18, Gpr_Attribute_Decl => 19, Gpr_Attribute_Reference => 20, Gpr_Ada_Context_Clause_List => 21, Gpr_Ada_Prelude_Node_List => 22, Gpr_Ada_Skip_List => 23, Gpr_Case_Item_List => 24, Gpr_Expr_List => 25, Gpr_Gpr_Node_List => 26, Gpr_Choices => 27, Gpr_Term_List => 28, Gpr_Identifier_List => 29, Gpr_String_Literal_List => 30, Gpr_Term_List_List => 31, Gpr_With_Decl_List => 32, Gpr_Builtin_Function_Call => 33, Gpr_Case_Construction => 34, Gpr_Case_Item => 35, Gpr_Compilation_Unit => 36, Gpr_Empty_Decl => 37, Gpr_Prefix => 38, Gpr_Identifier => 39, Gpr_Num_Literal => 40, Gpr_String_Literal => 41, Gpr_Limited_Absent => 42, Gpr_Limited_Present => 43, Gpr_Others_Designator => 44, Gpr_Package_Decl => 45, Gpr_Package_Extension => 46, Gpr_Package_Renaming => 47, Gpr_Package_Spec => 48, Gpr_Private_Absent => 49, Gpr_Private_Present => 50, Gpr_Project => 51, Gpr_Project_Declaration => 52, Gpr_Project_Extension => 53, Gpr_Project_Qualifier_Abstract => 54, Gpr_Project_Qualifier_Aggregate => 55, Gpr_Project_Qualifier_Aggregate_Library => 56, Gpr_Project_Qualifier_Configuration => 57, Gpr_Project_Qualifier_Library => 58, Gpr_Project_Qualifier_Standard => 59, Gpr_Project_Reference => 60, Gpr_String_Literal_At => 61, Gpr_Terms => 62, Gpr_Type_Reference => 63, Gpr_Typed_String_Decl => 64, Gpr_Variable_Decl => 65, Gpr_Variable_Reference => 66, Gpr_With_Decl => 67);

      subtype Gpr_Gpr_Node is Gpr_Node_Kind_Type
            range Gpr_Ada_Access_Subp .. Gpr_With_Decl;
      --% no-document: True
      subtype Gpr_Ada_Prelude_Node is Gpr_Node_Kind_Type
            range Gpr_Ada_Access_Subp .. Gpr_Ada_With_Formal;
      --% no-document: True
      subtype Gpr_Ada_Access_Subp_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Access_Subp .. Gpr_Ada_Access_Subp;
      --% no-document: True
      subtype Gpr_Ada_Context_Clause is Gpr_Node_Kind_Type
            range Gpr_Ada_Pragma .. Gpr_Ada_With;
      --% no-document: True
      subtype Gpr_Ada_Pragma_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Pragma .. Gpr_Ada_Pragma;
      --% no-document: True
      subtype Gpr_Ada_Use_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Use .. Gpr_Ada_Use;
      --% no-document: True
      subtype Gpr_Ada_With_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_With .. Gpr_Ada_With;
      --% no-document: True
      subtype Gpr_Ada_Entity_Kind is Gpr_Node_Kind_Type
            range Gpr_Ada_Entity_Kind_Function .. Gpr_Ada_Entity_Kind_Procedure;
      --% no-document: True
      subtype Gpr_Ada_Entity_Kind_Function_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Entity_Kind_Function .. Gpr_Ada_Entity_Kind_Function;
      --% no-document: True
      subtype Gpr_Ada_Entity_Kind_Package_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Entity_Kind_Package .. Gpr_Ada_Entity_Kind_Package;
      --% no-document: True
      subtype Gpr_Ada_Entity_Kind_Procedure_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Entity_Kind_Procedure .. Gpr_Ada_Entity_Kind_Procedure;
      --% no-document: True
      subtype Gpr_Ada_Generic_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Generic .. Gpr_Ada_Generic;
      --% no-document: True
      subtype Gpr_Ada_Library_Item_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Library_Item .. Gpr_Ada_Library_Item;
      --% no-document: True
      subtype Gpr_Ada_Main is Gpr_Node_Kind_Type
            range Gpr_Ada_Pkg .. Gpr_Ada_Subp;
      --% no-document: True
      subtype Gpr_Ada_Pkg_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Pkg .. Gpr_Ada_Pkg;
      --% no-document: True
      subtype Gpr_Ada_Pkg_Body_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Pkg_Body .. Gpr_Ada_Pkg_Body;
      --% no-document: True
      subtype Gpr_Ada_Subp_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Subp .. Gpr_Ada_Subp;
      --% no-document: True
      subtype Gpr_Ada_Prelude_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Prelude .. Gpr_Ada_Prelude;
      --% no-document: True
      subtype Gpr_Ada_Separate_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Separate .. Gpr_Ada_Separate;
      --% no-document: True
      subtype Gpr_Ada_Skip_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Skip .. Gpr_Ada_Skip;
      --% no-document: True
      subtype Gpr_Ada_With_Formal_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_With_Formal .. Gpr_Ada_With_Formal;
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
            range Gpr_Ada_Context_Clause_List .. Gpr_With_Decl_List;
      --% no-document: True
      subtype Gpr_Ada_Context_Clause_List_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Context_Clause_List .. Gpr_Ada_Context_Clause_List;
      --% no-document: True
      subtype Gpr_Ada_Prelude_Node_List_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Prelude_Node_List .. Gpr_Ada_Prelude_Node_List;
      --% no-document: True
      subtype Gpr_Ada_Skip_List_Range is Gpr_Node_Kind_Type
            range Gpr_Ada_Skip_List .. Gpr_Ada_Skip_List;
      --% no-document: True
      subtype Gpr_Case_Item_List_Range is Gpr_Node_Kind_Type
            range Gpr_Case_Item_List .. Gpr_Case_Item_List;
      --% no-document: True
      subtype Gpr_Expr_List_Range is Gpr_Node_Kind_Type
            range Gpr_Expr_List .. Gpr_Expr_List;
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
      subtype Gpr_Private_Node is Gpr_Node_Kind_Type
            range Gpr_Private_Absent .. Gpr_Private_Present;
      --% no-document: True
      subtype Gpr_Private_Absent_Range is Gpr_Node_Kind_Type
            range Gpr_Private_Absent .. Gpr_Private_Absent;
      --% no-document: True
      subtype Gpr_Private_Present_Range is Gpr_Node_Kind_Type
            range Gpr_Private_Present .. Gpr_Private_Present;
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
      subtype Gpr_Project_Reference_Range is Gpr_Node_Kind_Type
            range Gpr_Project_Reference .. Gpr_Project_Reference;
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

   -------------------
   -- Introspection --
   -------------------

   --  Unlike ``Gpr_Node_Kind_Type``, the following enumeration contains entries
   --  for abstract nodes.

   type Any_Node_Type_Id is (
      None, Gpr_Node_Type_Id, Ada_Prelude_Node_Type_Id, Ada_Access_Subp_Type_Id, Ada_Context_Clause_Type_Id, Ada_Pragma_Type_Id, Ada_Use_Type_Id, Ada_With_Type_Id, Ada_Entity_Kind_Type_Id, Ada_Entity_Kind_Function_Type_Id, Ada_Entity_Kind_Package_Type_Id, Ada_Entity_Kind_Procedure_Type_Id, Ada_Generic_Type_Id, Ada_Library_Item_Type_Id, Ada_Main_Type_Id, Ada_Pkg_Type_Id, Ada_Pkg_Body_Type_Id, Ada_Subp_Type_Id, Ada_Prelude_Type_Id, Ada_Separate_Type_Id, Ada_Skip_Type_Id, Ada_With_Formal_Type_Id, All_Qualifier_Type_Id, All_Qualifier_Absent_Type_Id, All_Qualifier_Present_Type_Id, Attribute_Decl_Type_Id, Attribute_Reference_Type_Id, Base_List_Type_Id, Ada_Context_Clause_List_Type_Id, Ada_Prelude_Node_List_Type_Id, Ada_Skip_List_Type_Id, Case_Item_List_Type_Id, Expr_List_Type_Id, Gpr_Node_List_Type_Id, Choices_Type_Id, Term_List_Type_Id, Identifier_List_Type_Id, String_Literal_List_Type_Id, Term_List_List_Type_Id, With_Decl_List_Type_Id, Builtin_Function_Call_Type_Id, Case_Construction_Type_Id, Case_Item_Type_Id, Compilation_Unit_Type_Id, Empty_Decl_Type_Id, Expr_Type_Id, Prefix_Type_Id, Single_Tok_Node_Type_Id, Identifier_Type_Id, Num_Literal_Type_Id, String_Literal_Type_Id, Limited_Node_Type_Id, Limited_Absent_Type_Id, Limited_Present_Type_Id, Others_Designator_Type_Id, Package_Decl_Type_Id, Package_Extension_Type_Id, Package_Renaming_Type_Id, Package_Spec_Type_Id, Private_Node_Type_Id, Private_Absent_Type_Id, Private_Present_Type_Id, Project_Type_Id, Project_Declaration_Type_Id, Project_Extension_Type_Id, Project_Qualifier_Type_Id, Project_Qualifier_Abstract_Type_Id, Project_Qualifier_Aggregate_Type_Id, Project_Qualifier_Aggregate_Library_Type_Id, Project_Qualifier_Configuration_Type_Id, Project_Qualifier_Library_Type_Id, Project_Qualifier_Standard_Type_Id, Project_Reference_Type_Id, String_Literal_At_Type_Id, Terms_Type_Id, Type_Reference_Type_Id, Typed_String_Decl_Type_Id, Variable_Decl_Type_Id, Variable_Reference_Type_Id, With_Decl_Type_Id
   );

   subtype Node_Type_Id is Any_Node_Type_Id
      range Gpr_Node_Type_Id
            .. With_Decl_Type_Id;

   type Node_Type_Id_Array is array (Positive range <>) of Node_Type_Id;

   type Any_Value_Kind is (
      None,
      Boolean_Value,
      Integer_Value,
      Big_Integer_Value,
      Character_Value,
      String_Value,
      Token_Value,
      Unbounded_Text_Value,
      Analysis_Unit_Value,
      Node_Value

      , Analysis_Unit_Kind_Value
      , Lookup_Kind_Value
      , Designated_Env_Kind_Value
      , Grammar_Rule_Value

      , Gpr_Node_Array_Value
   );
   subtype Value_Kind is
      Any_Value_Kind range Boolean_Value ..  Any_Value_Kind'Last;
   --  Enumeration for all types used to interact with properties

   
   subtype Enum_Value_Kind is Value_Kind with Static_Predicate =>
      Enum_Value_Kind in Analysis_Unit_Kind_Value | Lookup_Kind_Value | Designated_Env_Kind_Value | Grammar_Rule_Value;
   --  Subrange for all enum types

   
   subtype Array_Value_Kind is Value_Kind with Static_Predicate =>
      Array_Value_Kind in Gpr_Node_Array_Value;
   --  Subrange for all array types

      pragma Warnings (Off, "null range");
   subtype Struct_Value_Kind is Value_Kind
         range Any_Value_Kind'Last .. Any_Value_Kind'First
   ;
      pragma Warnings (On, "null range");
   --  Subrange for all struct types

   type Type_Constraint (Kind : Value_Kind := Value_Kind'First) is record
      case Kind is
         when Node_Value =>
            Node_Type : Node_Type_Id;
            --  Base type for nodes that satisfy this constraint

         when others =>
            null;
      end case;
   end record;
   --  Type constraint for a polymorphic value

   type Type_Constraint_Array is array (Positive range <>) of Type_Constraint;

   

   type Any_Member_Reference is
      (None, Ada_Access_Subp_F_Subp_Kind, Ada_Access_Subp_F_Skips, Ada_Pragma_F_Skips, Ada_Use_F_Skips, Ada_With_F_Has_Limited, Ada_With_F_Has_Private, Ada_With_F_Packages, Ada_Generic_F_Skips, Ada_Library_Item_F_Generic_Stub, Ada_Library_Item_F_Separate, Ada_Library_Item_F_Main, Ada_Main_F_Name, Ada_Pkg_F_Has_Private, Ada_Subp_F_Subp_Kind, Ada_Prelude_F_Context_Clauses, Ada_Prelude_F_Library_Item, Ada_Separate_F_Parent_Name, Ada_With_Formal_F_Kind, Ada_With_Formal_F_Skips, Attribute_Decl_F_Attr_Name, Attribute_Decl_F_Attr_Index, Attribute_Decl_F_Expr, Attribute_Reference_F_Attribute_Name, Attribute_Reference_F_Attribute_Index, Builtin_Function_Call_F_Function_Name, Builtin_Function_Call_F_Parameters, Case_Construction_F_Var_Ref, Case_Construction_F_Items, Case_Item_F_Choice, Case_Item_F_Decls, Compilation_Unit_F_Project, Prefix_F_Prefix, Prefix_F_Suffix, Package_Decl_F_Pkg_Name, Package_Decl_F_Pkg_Spec, Package_Extension_F_Extended_Name, Package_Renaming_F_Renamed_Name, Package_Spec_F_Extension, Package_Spec_F_Decls, Package_Spec_F_End_Name, Project_F_Context_Clauses, Project_F_Project_Decl, Project_Declaration_F_Qualifier, Project_Declaration_F_Project_Name, Project_Declaration_F_Extension, Project_Declaration_F_Decls, Project_Declaration_F_End_Name, Project_Extension_F_Is_All, Project_Extension_F_Path_Name, Project_Reference_F_Attr_Ref, String_Literal_At_F_Str_Lit, String_Literal_At_F_At_Lit, Terms_F_Terms, Type_Reference_F_Var_Type_Name, Typed_String_Decl_F_Type_Id, Typed_String_Decl_F_String_Literals, Variable_Decl_F_Var_Name, Variable_Decl_F_Var_Type, Variable_Decl_F_Expr, Variable_Reference_F_Variable_Name, Variable_Reference_F_Attribute_Ref, With_Decl_F_Is_Limited, With_Decl_F_Path_Names, Gpr_Node_Parent, Gpr_Node_Parents, Gpr_Node_Children, Gpr_Node_Token_Start, Gpr_Node_Token_End, Gpr_Node_Child_Index, Gpr_Node_Previous_Sibling, Gpr_Node_Next_Sibling, Gpr_Node_Unit, Gpr_Node_Is_Ghost, Gpr_Node_Full_Sloc_Image, All_Qualifier_P_As_Bool, Limited_Node_P_As_Bool, Private_Node_P_As_Bool);
   subtype Member_Reference is Any_Member_Reference range
      Ada_Access_Subp_F_Subp_Kind
      ..  Private_Node_P_As_Bool;
   --  Enumeration of all data attached to structs/nodes (fields and
   --  properties).

   subtype Node_Member_Reference is Member_Reference range
      Ada_Access_Subp_F_Subp_Kind
      ..  Private_Node_P_As_Bool;
   --  Subrange for members of nodes only

   type Member_Reference_Array is
      array (Positive range <>) of Member_Reference;

      pragma Warnings (Off, "null range");
   subtype Struct_Field_Reference is Member_Reference range
         
      Private_Node_P_As_Bool
      .. Ada_Access_Subp_F_Subp_Kind
   ;
      pragma Warnings (On, "null range");

   type Struct_Field_Reference_Array is
      array (Positive range <>) of Struct_Field_Reference;

   subtype Syntax_Field_Reference is Member_Reference range
         
      Ada_Access_Subp_F_Subp_Kind
      .. With_Decl_F_Path_Names
   ;
   --  Enumeration of all syntax fields for regular nodes

   type Syntax_Field_Reference_Array is
      array (Positive range <>) of Syntax_Field_Reference;

   subtype Property_Reference is Member_Reference
      range Gpr_Node_Parent
         .. Private_Node_P_As_Bool;
   --  Enumeration of all available node properties

   type Property_Reference_Array is
      array (Positive range <>) of Property_Reference;

   


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
