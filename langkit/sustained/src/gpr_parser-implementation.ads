
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--











with Ada.Containers;              use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Exceptions;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System; use System;

with GNATCOLL.GMP.Integers;
with GNATCOLL.Traces;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Gpr_Parser_Support.Adalog.Logic_Var;
with Gpr_Parser_Support.Adalog.Solver;
with Gpr_Parser_Support.Adalog.Solver_Interface;

with Gpr_Parser_Support.Bump_Ptr;    use Gpr_Parser_Support.Bump_Ptr;
with Gpr_Parser_Support.Cheap_Sets;
with Gpr_Parser_Support.File_Readers; use Gpr_Parser_Support.File_Readers;
with Gpr_Parser_Support.Lexical_Envs; use Gpr_Parser_Support.Lexical_Envs;
with Gpr_Parser_Support.Lexical_Envs_Impl;
with Gpr_Parser_Support.Symbols;      use Gpr_Parser_Support.Symbols;
with Gpr_Parser_Support.Symbols.Precomputed;
with Gpr_Parser_Support.Token_Data_Handlers;
use Gpr_Parser_Support.Token_Data_Handlers;
with Gpr_Parser_Support.Types;        use Gpr_Parser_Support.Types;
with Gpr_Parser_Support.Vectors;

with Gpr_Parser.Parsers; use Gpr_Parser.Parsers;
with Gpr_Parser.Common;  use Gpr_Parser.Common;
with Gpr_Parser.Lexer_Implementation;
use Gpr_Parser.Lexer_Implementation;




--  Internal package: low-level primitives to implement public types and
--  operations in Gpr_Parser.Analysis.

private package Gpr_Parser.Implementation is

   use Support.Diagnostics, Support.Slocs, Support.Text;

   ------------
   -- Traces --
   ------------

   Main_Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPR_PARSER.MAIN_TRACE", GNATCOLL.Traces.From_Config);

   PLE_Errors_Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPR_PARSER.PLE_ERRORS", GNATCOLL.Traces.From_Config);

   -------------------------------------
   -- Symbols and token data handlers --
   -------------------------------------

   type Precomputed_Symbol_Index is
         (
            
               Precomputed_Sym_Access, --  access
               Precomputed_Sym_Aggregate, --  aggregate
               Precomputed_Sym_Body, --  body
               Precomputed_Sym_Configuration, --  configuration
               Precomputed_Sym_Function, --  function
               Precomputed_Sym_Generic, --  generic
               Precomputed_Sym_Library, --  library
               Precomputed_Sym_Procedure, --  procedure
               Precomputed_Sym_Project, --  project
               Precomputed_Sym_Separate, --  separate
               Precomputed_Sym_Standard --  standard
         )
   ;

   function Precomputed_Symbol
     (Index : Precomputed_Symbol_Index) return Text_Type;

   --  GNAT emits an incorrect value not in range in instantiation warning...
   --  So deactivate them at the instantiation point.
   pragma Warnings (Off, "value not in range");
   package Precomputed_Symbols is new Gpr_Parser_Support.Symbols.Precomputed
     (Precomputed_Symbol_Index, Precomputed_Symbol);
   pragma Warnings (On, "value not in range");

   --------------------
   -- Analysis types --
   --------------------

   type Analysis_Context_Type;
   type Internal_Context is access all Analysis_Context_Type;

   type Analysis_Unit_Type;
   type Internal_Unit is access all Analysis_Unit_Type;

   type Root_Node_Record;
   type Bare_Gpr_Node is access all Root_Node_Record;
   No_Bare_Gpr_Node : constant Bare_Gpr_Node := null;
   --  Most generic AST node type

   pragma No_Strict_Aliasing (Internal_Context);
   pragma No_Strict_Aliasing (Internal_Unit);
   pragma No_Strict_Aliasing (Bare_Gpr_Node);

   function "<" (Left, Right : Bare_Gpr_Node) return Boolean;
   --  Abritrary but deterministic ordering criteria for parsing nodes. This
   --  handles null nodes as well. Raise a Property_Error for synthetic nodes.

   function Is_Null (Node : Bare_Gpr_Node) return Boolean;
   function Kind (Node : Bare_Gpr_Node) return Gpr_Node_Kind_Type;

         subtype Bare_Ada_Prelude_Node is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Prelude_Node)
               or else Kind (Bare_Ada_Prelude_Node) in Gpr_Ada_Prelude_Node;
         subtype Bare_Ada_Access_Subp is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Access_Subp)
               or else Kind (Bare_Ada_Access_Subp) in Gpr_Ada_Access_Subp_Range;
         subtype Bare_Ada_Context_Clause is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Context_Clause)
               or else Kind (Bare_Ada_Context_Clause) in Gpr_Ada_Context_Clause;
         subtype Bare_Ada_Pragma is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Pragma)
               or else Kind (Bare_Ada_Pragma) in Gpr_Ada_Pragma_Range;
         subtype Bare_Ada_Use is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Use)
               or else Kind (Bare_Ada_Use) in Gpr_Ada_Use_Range;
         subtype Bare_Ada_With is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_With)
               or else Kind (Bare_Ada_With) in Gpr_Ada_With_Range;
         subtype Bare_Ada_Entity_Kind is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Entity_Kind)
               or else Kind (Bare_Ada_Entity_Kind) in Gpr_Ada_Entity_Kind;
         subtype Bare_Ada_Entity_Kind_Function is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Entity_Kind_Function)
               or else Kind (Bare_Ada_Entity_Kind_Function) in Gpr_Ada_Entity_Kind_Function_Range;
         subtype Bare_Ada_Entity_Kind_Package is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Entity_Kind_Package)
               or else Kind (Bare_Ada_Entity_Kind_Package) in Gpr_Ada_Entity_Kind_Package_Range;
         subtype Bare_Ada_Entity_Kind_Procedure is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Entity_Kind_Procedure)
               or else Kind (Bare_Ada_Entity_Kind_Procedure) in Gpr_Ada_Entity_Kind_Procedure_Range;
         subtype Bare_Ada_Generic is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Generic)
               or else Kind (Bare_Ada_Generic) in Gpr_Ada_Generic_Range;
         subtype Bare_Ada_Library_Item is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Library_Item)
               or else Kind (Bare_Ada_Library_Item) in Gpr_Ada_Library_Item_Range;
         subtype Bare_Ada_Main is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Main)
               or else Kind (Bare_Ada_Main) in Gpr_Ada_Main;
         subtype Bare_Ada_Pkg is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Pkg)
               or else Kind (Bare_Ada_Pkg) in Gpr_Ada_Pkg_Range;
         subtype Bare_Ada_Pkg_Body is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Pkg_Body)
               or else Kind (Bare_Ada_Pkg_Body) in Gpr_Ada_Pkg_Body_Range;
         subtype Bare_Ada_Subp is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Subp)
               or else Kind (Bare_Ada_Subp) in Gpr_Ada_Subp_Range;
         subtype Bare_Ada_Prelude is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Prelude)
               or else Kind (Bare_Ada_Prelude) in Gpr_Ada_Prelude_Range;
         subtype Bare_Ada_Separate is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Separate)
               or else Kind (Bare_Ada_Separate) in Gpr_Ada_Separate_Range;
         subtype Bare_Ada_Skip is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Skip)
               or else Kind (Bare_Ada_Skip) in Gpr_Ada_Skip_Range;
         subtype Bare_Ada_With_Formal is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_With_Formal)
               or else Kind (Bare_Ada_With_Formal) in Gpr_Ada_With_Formal_Range;
         subtype Bare_All_Qualifier is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_All_Qualifier)
               or else Kind (Bare_All_Qualifier) in Gpr_All_Qualifier;
         subtype Bare_All_Qualifier_Absent is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_All_Qualifier_Absent)
               or else Kind (Bare_All_Qualifier_Absent) in Gpr_All_Qualifier_Absent_Range;
         subtype Bare_All_Qualifier_Present is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_All_Qualifier_Present)
               or else Kind (Bare_All_Qualifier_Present) in Gpr_All_Qualifier_Present_Range;
         subtype Bare_Attribute_Decl is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Attribute_Decl)
               or else Kind (Bare_Attribute_Decl) in Gpr_Attribute_Decl_Range;
         subtype Bare_Attribute_Reference is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Attribute_Reference)
               or else Kind (Bare_Attribute_Reference) in Gpr_Attribute_Reference_Range;
         subtype Bare_Base_List is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Base_List)
               or else Kind (Bare_Base_List) in Gpr_Base_List;
         subtype Bare_Ada_Context_Clause_List is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Context_Clause_List)
               or else Kind (Bare_Ada_Context_Clause_List) in Gpr_Ada_Context_Clause_List_Range;
         subtype Bare_Ada_Prelude_Node_List is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Prelude_Node_List)
               or else Kind (Bare_Ada_Prelude_Node_List) in Gpr_Ada_Prelude_Node_List_Range;
         subtype Bare_Ada_Skip_List is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ada_Skip_List)
               or else Kind (Bare_Ada_Skip_List) in Gpr_Ada_Skip_List_Range;
         subtype Bare_Case_Item_List is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Case_Item_List)
               or else Kind (Bare_Case_Item_List) in Gpr_Case_Item_List_Range;
         subtype Bare_Expr_List is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Expr_List)
               or else Kind (Bare_Expr_List) in Gpr_Expr_List_Range;
         subtype Bare_Gpr_Node_List is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Gpr_Node_List)
               or else Kind (Bare_Gpr_Node_List) in Gpr_Gpr_Node_List_Range;
         subtype Bare_Choices is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Choices)
               or else Kind (Bare_Choices) in Gpr_Choices_Range;
         subtype Bare_Term_List is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Term_List)
               or else Kind (Bare_Term_List) in Gpr_Term_List_Range;
         subtype Bare_Identifier_List is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Identifier_List)
               or else Kind (Bare_Identifier_List) in Gpr_Identifier_List_Range;
         subtype Bare_String_Literal_List is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_String_Literal_List)
               or else Kind (Bare_String_Literal_List) in Gpr_String_Literal_List_Range;
         subtype Bare_Term_List_List is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Term_List_List)
               or else Kind (Bare_Term_List_List) in Gpr_Term_List_List_Range;
         subtype Bare_With_Decl_List is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_With_Decl_List)
               or else Kind (Bare_With_Decl_List) in Gpr_With_Decl_List_Range;
         subtype Bare_Builtin_Function_Call is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Builtin_Function_Call)
               or else Kind (Bare_Builtin_Function_Call) in Gpr_Builtin_Function_Call_Range;
         subtype Bare_Case_Construction is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Case_Construction)
               or else Kind (Bare_Case_Construction) in Gpr_Case_Construction_Range;
         subtype Bare_Case_Item is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Case_Item)
               or else Kind (Bare_Case_Item) in Gpr_Case_Item_Range;
         subtype Bare_Compilation_Unit is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Compilation_Unit)
               or else Kind (Bare_Compilation_Unit) in Gpr_Compilation_Unit_Range;
         subtype Bare_Empty_Decl is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Empty_Decl)
               or else Kind (Bare_Empty_Decl) in Gpr_Empty_Decl_Range;
         subtype Bare_Expr is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Expr)
               or else Kind (Bare_Expr) in Gpr_Expr;
         subtype Bare_Prefix is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Prefix)
               or else Kind (Bare_Prefix) in Gpr_Prefix_Range;
         subtype Bare_Single_Tok_Node is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Single_Tok_Node)
               or else Kind (Bare_Single_Tok_Node) in Gpr_Single_Tok_Node;
         subtype Bare_Identifier is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Identifier)
               or else Kind (Bare_Identifier) in Gpr_Identifier_Range;
         subtype Bare_Num_Literal is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Num_Literal)
               or else Kind (Bare_Num_Literal) in Gpr_Num_Literal_Range;
         subtype Bare_String_Literal is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_String_Literal)
               or else Kind (Bare_String_Literal) in Gpr_String_Literal_Range;
         subtype Bare_Limited_Node is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Limited_Node)
               or else Kind (Bare_Limited_Node) in Gpr_Limited_Node;
         subtype Bare_Limited_Absent is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Limited_Absent)
               or else Kind (Bare_Limited_Absent) in Gpr_Limited_Absent_Range;
         subtype Bare_Limited_Present is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Limited_Present)
               or else Kind (Bare_Limited_Present) in Gpr_Limited_Present_Range;
         subtype Bare_Others_Designator is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Others_Designator)
               or else Kind (Bare_Others_Designator) in Gpr_Others_Designator_Range;
         subtype Bare_Package_Decl is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Package_Decl)
               or else Kind (Bare_Package_Decl) in Gpr_Package_Decl_Range;
         subtype Bare_Package_Extension is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Package_Extension)
               or else Kind (Bare_Package_Extension) in Gpr_Package_Extension_Range;
         subtype Bare_Package_Renaming is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Package_Renaming)
               or else Kind (Bare_Package_Renaming) in Gpr_Package_Renaming_Range;
         subtype Bare_Package_Spec is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Package_Spec)
               or else Kind (Bare_Package_Spec) in Gpr_Package_Spec_Range;
         subtype Bare_Private_Node is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Private_Node)
               or else Kind (Bare_Private_Node) in Gpr_Private_Node;
         subtype Bare_Private_Absent is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Private_Absent)
               or else Kind (Bare_Private_Absent) in Gpr_Private_Absent_Range;
         subtype Bare_Private_Present is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Private_Present)
               or else Kind (Bare_Private_Present) in Gpr_Private_Present_Range;
         subtype Bare_Project is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Project)
               or else Kind (Bare_Project) in Gpr_Project_Range;
         subtype Bare_Project_Declaration is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Project_Declaration)
               or else Kind (Bare_Project_Declaration) in Gpr_Project_Declaration_Range;
         subtype Bare_Project_Extension is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Project_Extension)
               or else Kind (Bare_Project_Extension) in Gpr_Project_Extension_Range;
         subtype Bare_Project_Qualifier is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Project_Qualifier)
               or else Kind (Bare_Project_Qualifier) in Gpr_Project_Qualifier;
         subtype Bare_Project_Qualifier_Abstract is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Project_Qualifier_Abstract)
               or else Kind (Bare_Project_Qualifier_Abstract) in Gpr_Project_Qualifier_Abstract_Range;
         subtype Bare_Project_Qualifier_Aggregate is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Project_Qualifier_Aggregate)
               or else Kind (Bare_Project_Qualifier_Aggregate) in Gpr_Project_Qualifier_Aggregate_Range;
         subtype Bare_Project_Qualifier_Aggregate_Library is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Project_Qualifier_Aggregate_Library)
               or else Kind (Bare_Project_Qualifier_Aggregate_Library) in Gpr_Project_Qualifier_Aggregate_Library_Range;
         subtype Bare_Project_Qualifier_Configuration is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Project_Qualifier_Configuration)
               or else Kind (Bare_Project_Qualifier_Configuration) in Gpr_Project_Qualifier_Configuration_Range;
         subtype Bare_Project_Qualifier_Library is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Project_Qualifier_Library)
               or else Kind (Bare_Project_Qualifier_Library) in Gpr_Project_Qualifier_Library_Range;
         subtype Bare_Project_Qualifier_Standard is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Project_Qualifier_Standard)
               or else Kind (Bare_Project_Qualifier_Standard) in Gpr_Project_Qualifier_Standard_Range;
         subtype Bare_Project_Reference is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Project_Reference)
               or else Kind (Bare_Project_Reference) in Gpr_Project_Reference_Range;
         subtype Bare_String_Literal_At is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_String_Literal_At)
               or else Kind (Bare_String_Literal_At) in Gpr_String_Literal_At_Range;
         subtype Bare_Terms is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Terms)
               or else Kind (Bare_Terms) in Gpr_Terms_Range;
         subtype Bare_Type_Reference is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Type_Reference)
               or else Kind (Bare_Type_Reference) in Gpr_Type_Reference_Range;
         subtype Bare_Typed_String_Decl is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Typed_String_Decl)
               or else Kind (Bare_Typed_String_Decl) in Gpr_Typed_String_Decl_Range;
         subtype Bare_Variable_Decl is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Variable_Decl)
               or else Kind (Bare_Variable_Decl) in Gpr_Variable_Decl_Range;
         subtype Bare_Variable_Reference is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Variable_Reference)
               or else Kind (Bare_Variable_Reference) in Gpr_Variable_Reference_Range;
         subtype Bare_With_Decl is Bare_Gpr_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_With_Decl)
               or else Kind (Bare_With_Decl) in Gpr_With_Decl_Range;

   package Alloc_AST_List_Array is new Gpr_Parser_Support.Bump_Ptr.Array_Alloc
     (Element_T  => Bare_Gpr_Node,
      Index_Type => Positive);
   --  Allocator for array of nodes, used in list nodes

   type Rewriting_Handle_Pointer is new System.Address;
   No_Rewriting_Handle_Pointer : constant Rewriting_Handle_Pointer :=
      Rewriting_Handle_Pointer (System.Null_Address);

      Properties_Traces : constant GNATCOLL.Traces.Trace_Handle :=
         GNATCOLL.Traces.Create
           ("LANGKIT.PROPERTIES", GNATCOLL.Traces.On
           );

   function Short_Text_Image (Self : Bare_Gpr_Node) return Text_Type;
   --  Return a short representation of the node, containing just the kind
   --  name and the sloc, or "None" if Self is null.

   function Is_Token_Node (Node : Bare_Gpr_Node) return Boolean;
   --  Return whether this node is a node that contains only a single token.

   function Is_Synthetic (Node : Bare_Gpr_Node) return Boolean;
   --  Return whether this node is synthetic.

   procedure Raise_Property_Exception
     (Node    : Bare_Gpr_Node;
      Exc     : Ada.Exceptions.Exception_Id;
      Message : String)
     with No_Return;
   --  Raise an exception of the given type and with the given message. Prepend
   --  the sloc of the given node to the exception message.

   ---------------------------
   -- Iterators safety nets --
   ---------------------------

   type Iterator_Safety_Net is record
      Context         : Internal_Context;
      Context_Serial  : Version_Number;
      Context_Version : Version_Number;
      --  Analysis context, its serial number and version number at the time
      --  this safety net was produced.
   end record;

   No_Iterator_Safety_Net : constant Iterator_Safety_Net := (null, 0, 0);

   function Create_Safety_Net
     (Context : Internal_Context) return Iterator_Safety_Net;
   --  Create an iterator safety net from the given Context

   procedure Check_Safety_Net (Self : Iterator_Safety_Net);
   --  Check that the given iterator safety net is still valid, raising a
   --  Stale_Reference_Error if it is not.

   -----------------
   -- String type --
   -----------------

   type String_Record (Length : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Content : Text_Type (1 .. Length);
   end record;

   type String_Type is access all String_Record;

   Empty_String_Record : aliased String_Record :=
     (Length => 0, Ref_Count => -1, Content => (others => <>));
   Empty_String        : constant String_Type := Empty_String_Record'Access;

   procedure Inc_Ref (Self : String_Type);
   procedure Dec_Ref (Self : in out String_Type);
   procedure Free is new Ada.Unchecked_Deallocation
     (String_Record, String_Type);

   function Create_String (Content : Text_Type) return String_Type;
   function Create_String (Content : Unbounded_Text_Type) return String_Type;
   --  Create string values from their content. The overload for unbounded
   --  strings makes it easier for callers to avoid using the secondary stack,
   --  which can be a problem for big strings.

   function Concat_String (Left, Right : String_Type) return String_Type;
   --  Return a new string that is the concatenation of ``Left`` and ``Right``

   function Equivalent (Left, Right : String_Type) return Boolean;
   --  Return whether ``Left`` and ``Right`` contain equal strings

   ---------------------------
   -- Environments handling --
   ---------------------------

   
      type Internal_Metadata;
      

   

      

      type Internal_Metadata is record

            null;
      end record
        with Convention => C;




   


      function Trace_Image (R : Internal_Metadata) return String;


   
      


      No_Metadata : constant Internal_Metadata :=
      (null record);


   function Hash (Self : Internal_Metadata) return Hash_Type;

   
      type Internal_Inner_Env_Assoc;
      

   

      

      type Internal_Inner_Env_Assoc is record

               Key : aliased Symbol_Type;
               
               
               Value : aliased Bare_Gpr_Node;
               
               
               Metadata : aliased Internal_Metadata;
               
               
      end record
        with Convention => C;




   


      function Trace_Image (R : Internal_Inner_Env_Assoc) return String;


   
      


      No_Inner_Env_Assoc : constant Internal_Inner_Env_Assoc :=
      (
               Key => null, 
               Value => No_Bare_Gpr_Node, 
               Metadata => No_Metadata
      );

   function Get_Key (Self : Internal_Inner_Env_Assoc) return Symbol_Type
   is (Self.Key);
   function Get_Node
     (Self : Internal_Inner_Env_Assoc) return Bare_Gpr_Node
   is (Self.Value);
   function Get_Metadata
     (Self : Internal_Inner_Env_Assoc) return Internal_Metadata
   is (Self.Metadata);

   
   type Internal_Inner_Env_Assoc_Array_Record;
   type Internal_Inner_Env_Assoc_Array_Access is access all Internal_Inner_Env_Assoc_Array_Record;

      
   type Internal_Internal_Inner_Env_Assoc_Iterator;
   type Internal_Inner_Env_Assoc_Iterator_Access is access all Internal_Internal_Inner_Env_Assoc_Iterator;


   

   

   type Internal_Internal_Inner_Env_Assoc_Array is
      array (Positive range <>) of Internal_Inner_Env_Assoc;

   type Internal_Inner_Env_Assoc_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Inner_Env_Assoc_Array (1 .. N);
   end record;

   Empty_Internal_Inner_Env_Assoc_Array_Record : aliased Internal_Inner_Env_Assoc_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Inner_Env_Assoc_Array_Type : constant Internal_Inner_Env_Assoc_Array_Access :=
      Empty_Internal_Inner_Env_Assoc_Array_Record'Access;


   function Create_Internal_Inner_Env_Assoc_Array (Items_Count : Natural) return Internal_Inner_Env_Assoc_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Inner_Env_Assoc_Array
     (Items : Internal_Internal_Inner_Env_Assoc_Array) return Internal_Inner_Env_Assoc_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Gpr_Node;
      T       : Internal_Inner_Env_Assoc_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Inner_Env_Assoc;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Inner_Env_Assoc_Array_Access) return Internal_Inner_Env_Assoc_Array_Access;


   function Length (T : Internal_Inner_Env_Assoc_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Inner_Env_Assoc_Array_Access);
   procedure Dec_Ref (T : in out Internal_Inner_Env_Assoc_Array_Access);

   function Equivalent (L, R : Internal_Inner_Env_Assoc_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Inner_Env_Assoc_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Inner_Env_Assoc_Array_Record, Internal_Inner_Env_Assoc_Array_Access);

      

   

   type Internal_Internal_Inner_Env_Assoc_Iterator is record
      Ref_Count : Integer;
      --  Reference count. The iterator is freed when this drops to zero.
      --  Negative values are interpreted as "always living singleton".

      Safety_Net : Iterator_Safety_Net;
      --  Safety net for the iterator. Used to check that values produced by
      --  the iterator are still valid. Unlike for other types, we put the
      --  safety net in the internal type so that it can be used in all other
      --  APIs (Python, ...).
      --
      --  While other types (except nodes) are "deeply" converted to native
      --  APIs (for instance: internal arrays are turned into native Python
      --  lists, likewise for array items, etc.), iterators are lazy, so the
      --  deep conversion is not possible.

      Elements : Internal_Inner_Env_Assoc_Array_Access;
      Index    : Positive;
   end record;

   Empty_Internal_Internal_Inner_Env_Assoc_Iterator : aliased Internal_Internal_Inner_Env_Assoc_Iterator :=
     (Ref_Count  => -1,
      Safety_Net => No_Iterator_Safety_Net,
      Elements   => No_Internal_Inner_Env_Assoc_Array_Type,
      Index      => 1);
   No_Internal_Inner_Env_Assoc_Iterator_Type : constant Internal_Inner_Env_Assoc_Iterator_Access :=
      Empty_Internal_Internal_Inner_Env_Assoc_Iterator'Access;

   function Next
     (T       : Internal_Inner_Env_Assoc_Iterator_Access;
      Element : out Internal_Inner_Env_Assoc) return Boolean;

   procedure Inc_Ref (T : Internal_Inner_Env_Assoc_Iterator_Access);
   procedure Dec_Ref (T : in out Internal_Inner_Env_Assoc_Iterator_Access);

      function Trace_Image (A : Internal_Inner_Env_Assoc_Iterator_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Internal_Inner_Env_Assoc_Iterator, Internal_Inner_Env_Assoc_Iterator_Access);


   function Inner_Env_Assoc_Get
     (Self  : Internal_Inner_Env_Assoc_Array_Access;
      Index : Positive) return Internal_Inner_Env_Assoc
   is (Self.Items (Index));

   function Combine
     (L, R : Internal_Metadata) return Internal_Metadata;
   --  The combine function on environments metadata does a boolean Or on every
   --  boolean component of the env metadata.

   function Can_Reach (El, From : Bare_Gpr_Node) return Boolean;
   --  Return whether El can reach From, from a sequential viewpoint. If
   --  elements are declared in different units, it will always return True,
   --  eg this does not handle general visibility issues, just sequentiality of
   --  declarations.

   function AST_Envs_Node_Text_Image
     (Node  : Bare_Gpr_Node;
      Short : Boolean := True) return Text_Type;
   --  Return a "sourcefile:lineno:columnno" corresponding to the starting sloc
   --  of Node. Used to create a human-readable representation for env.
   --  rebindings.

   function Is_Rebindable (Node : Bare_Gpr_Node) return Boolean;

   function Acquire_Rebinding
     (Node             : Bare_Gpr_Node;
      Parent           : Env_Rebindings;
      Old_Env, New_Env : Lexical_Env) return Env_Rebindings;
   --  Initialize and return a fresh rebinding

   procedure Release_Rebinding (Self : in out Env_Rebindings);
   --  Mark the rebinding as unused, so that a future call to Acquire_Rebinding
   --  can return it.

   procedure Register_Rebinding
     (Node : Bare_Gpr_Node; Rebinding : Env_Rebindings);
   --  Register a rebinding to be destroyed when Node's analysis unit is
   --  destroyed or reparsed.

   function Element_Parent
     (Node : Bare_Gpr_Node) return Bare_Gpr_Node;

   function Hash (Node : Bare_Gpr_Node) return Hash_Type;
   function Node_Unit (Node : Bare_Gpr_Node) return Generic_Unit_Ptr;
   function Named_Hash (Node : Bare_Gpr_Node) return Hash_Type is
     (Hash (Node));

   No_Analysis_Unit : constant Internal_Unit := null;

   function Convert_Unit is new Ada.Unchecked_Conversion
     (Generic_Unit_Ptr, Internal_Unit);
   function Convert_Unit is new Ada.Unchecked_Conversion
     (Internal_Unit, Generic_Unit_Ptr);

   function Unit_Version (Unit : Generic_Unit_Ptr) return Version_Number;
   --  Return the version for Unit. Version is a number that is incremented
   --  every time Unit changes.

   function Get_Context_Version
     (Node : Bare_Gpr_Node) return Version_Number;
   --  Assuming that Node is not null, return the version number for Node's
   --  context, which is incremented every time a unit in this context is
   --  parsed.

   type Ref_Category is
     (Nocat);
   type Ref_Categories is array (Ref_Category) of Boolean;
   pragma Pack (Ref_Categories);

   function Properties_May_Raise
     (Exc : Ada.Exceptions.Exception_Occurrence) return Boolean;
   --  Return if ``Exc`` is one of the exceptions that properties are allowed
   --  to raise.

   package AST_Envs is new Gpr_Parser_Support.Lexical_Envs_Impl
     (Get_Unit_Version         => Unit_Version,
      Node_Type                => Bare_Gpr_Node,
      Node_Metadata            => Internal_Metadata,
      No_Node                  => null,
      Empty_Metadata           => No_Metadata,
      Node_Unit                => Node_Unit,
      Node_Hash                => Named_Hash,
      Metadata_Hash            => Hash,
      Combine                  => Combine,
      Node_Text_Image          => AST_Envs_Node_Text_Image,
      Acquire_Rebinding        => Acquire_Rebinding,
      Register_Rebinding       => Register_Rebinding,
      Ref_Category             => Ref_Category,
      Ref_Categories           => Ref_Categories,
      Inner_Env_Assoc          => Internal_Inner_Env_Assoc,
      Inner_Env_Assoc_Array    => Internal_Inner_Env_Assoc_Array_Access,
      Get                      => Inner_Env_Assoc_Get);

   use AST_Envs;
   subtype Internal_Entity is AST_Envs.Entity;
   subtype Internal_Entity_Info is AST_Envs.Entity_Info;

   No_Entity_Info : constant Internal_Entity_Info :=
     (No_Metadata, null, False);
   No_Entity : constant Internal_Entity :=
     (null, No_Entity_Info);

   function Hash_Entity (Self : Internal_Entity) return Hash_Type;
   --  Hash function to use in the public API. It is like the regular one, but
   --  compares metadata according to the user specification in the DSL.

   function Compare_Entity (Left, Right : Internal_Entity) return Boolean;
   --  Equality function to use in the public API. It is like the regular one,
   --  but compares metadata according to the user specification in the DSL.

   function Compare_Metadata (L, R : Internal_Metadata) return Boolean;
   --  Compare metadata ``L`` and ``R`` for public entity comparison

   function Create_Dynamic_Lexical_Env
     (Self              : Bare_Gpr_Node;
      Assocs_Getter     : Inner_Env_Assocs_Resolver;
      Assoc_Resolver    : Entity_Resolver;
      Transitive_Parent : Boolean) return Lexical_Env;
   --  Helper for properties code generation: wrapper around
   --  AST_Envs.Create_Dynamic_Lexical_Env.

      function Hash (B : Boolean) return Hash_Type;





   --------------------------
   -- Big integers wrapper --
   --------------------------

   type Big_Integer_Record is limited record
      Value     : GNATCOLL.GMP.Integers.Big_Integer;
      Ref_Count : Integer;
      --  Number of owners. When it drops to 0, this record can be destroyed.
      --  If -1, this is a static big integer: Inc_Ref and Dec_Ref are no-ops.
   end record;

   type Big_Integer_Type is access all Big_Integer_Record;
   pragma No_Strict_Aliasing (Big_Integer_Type);

   function Create_Big_Integer
     (Image : String; Base : Integer := 10) return Big_Integer_Type;
   function Create_Big_Integer
     (Big_Int : GNATCOLL.GMP.Integers.Big_Integer) return Big_Integer_Type;
   function Create_Big_Integer (Int : Integer) return Big_Integer_Type;
   function Create_Public_Big_Integer
     (Big_Int : Big_Integer_Type) return GNATCOLL.GMP.Integers.Big_Integer;

   No_Big_Integer_Record : aliased Big_Integer_Record :=
     (Value => <>, Ref_Count => -1);
   No_Big_Integer : constant Big_Integer_Type := No_Big_Integer_Record'Access;

   function To_Integer
     (Self    : Bare_Gpr_Node;
      Big_Int : Big_Integer_Type) return Integer;
   --  Convert ``Big_Int`` into a regular integer, raising a ``Property_Error``
   --  if it is out of range (using ``Self`` to provide context for this
   --  error).

   procedure Inc_Ref (Big_Int : Big_Integer_Type);
   procedure Dec_Ref (Big_Int : in out Big_Integer_Type);

   function Equivalent (Left, Right : Big_Integer_Type) return Boolean;
   function "<" (Left, Right : Big_Integer_Type) return Boolean;
   function "<=" (Left, Right : Big_Integer_Type) return Boolean;
   function ">" (Left, Right : Big_Integer_Type) return Boolean;
   function ">=" (Left, Right : Big_Integer_Type) return Boolean;

   function "+" (Left, Right : Big_Integer_Type) return Big_Integer_Type;
   function "-" (Left, Right : Big_Integer_Type) return Big_Integer_Type;

   function Trace_Image (I : Big_Integer_Type) return String;

      function Trace_Image
        (Node       : Bare_Gpr_Node;
         Decoration : Boolean := True) return String;

   function Is_Incomplete (Node : Bare_Gpr_Node) return Boolean;
   --  Return whether this node is incomplete or not.  Incomplete nodes are a
   --  result of the parsing of a node failing as a result of a Cut parser
   --  annotation.

   function Kind_Name (Node : Bare_Gpr_Node) return String;
   --  Return the concrete kind for Node

   ---------------------------
   -- Adalog instantiations --
   ---------------------------

   function Text_Image (Ent : Internal_Entity) return Text_Type;
   function Image (Ent : Internal_Entity) return String;
   --  Return a representation of this node as a string.

   package Entity_Vars is new Gpr_Parser_Support.Adalog.Logic_Var
     (Value_Type => Internal_Entity, Value_Image => Image);
   package Solver_Ifc is new Gpr_Parser_Support.Adalog.Solver_Interface
     (Entity_Vars);
   package Solver is new Gpr_Parser_Support.Adalog.Solver (Solver_Ifc);

   subtype Logic_Var is Entity_Vars.Logic_Var;
   subtype Logic_Var_Record is Entity_Vars.Logic_Var_Record;
   Null_Var : constant Logic_Var := null;
   Null_Var_Record : constant Logic_Var_Record := (Reset => True, others => <>);

   subtype Logic_Equation is Solver.Relation;
   Null_Logic_Equation : constant Logic_Equation := Solver.No_Relation;

      function Trace_Image (K : Analysis_Unit_Kind) return String;
      function Trace_Image (B : Boolean) return String;
      function Trace_Image (I : Integer) return String;
      function Trace_Image (S : Symbol_Type) return String;
      function Trace_Image (C : Character_Type) return String;
      function Trace_Image (S : String_Type) return String;
      function Trace_Image (Env : Lexical_Env) return String;
      function Trace_Image (R : Env_Rebindings) return String;
      function Trace_Image (Unit : Internal_Unit) return String;
      function Trace_Image (Eq : Logic_Equation) return String;
      function Trace_Image (Var : Logic_Var) return String;
      function Trace_Image (T : Token_Reference) return String renames Image;
      function Trace_Image (Self : Ref_Categories) return String;

   


   -----------------------------------------------
   -- Structure types (incomplete declarations) --
   -----------------------------------------------

         
      type Internal_Designated_Env;
      --  Designate an environment for an env spec action.
   --
   --  The designated environment can be either, depending on the ``Kind``
   --  field:
   --
   --  * If ``Kind`` is ``None``, no environment is designated.
   --
   --  * If ``Kind`` is ``Current_Env``, designate the current environment at
   --    this point during PLE.
   --
   --  * If ``Kind`` is ``Named_Env``, designate the environment which has
   --    precedence for the ``Env_Name`` environment name. If ``Env_Name`` is
   --    null, this designates to environment.
   --
   --  * If ``Kind`` is ``Direct_Env``, the direct value for the designated
   --    environment. That environment must be a primary one and cannot be
   --    foreign to the node currently processed by PLE. If it is the empty
   --    environment, do nothing.

         

         

         
      type Internal_Entity_Ada_Prelude_Node;
      

         
      type Internal_Entity_Ada_Access_Subp;
      

         
      type Internal_Entity_Ada_Context_Clause;
      

         
      type Internal_Entity_Base_List;
      

         
      type Internal_Entity_Ada_Context_Clause_List;
      

         
      type Internal_Entity_Ada_Entity_Kind;
      

         
      type Internal_Entity_Ada_Entity_Kind_Function;
      

         
      type Internal_Entity_Ada_Entity_Kind_Package;
      

         
      type Internal_Entity_Ada_Entity_Kind_Procedure;
      

         
      type Internal_Entity_Ada_Generic;
      

         
      type Internal_Entity_Ada_Library_Item;
      

         
      type Internal_Entity_Ada_Main;
      

         
      type Internal_Entity_Ada_Pkg;
      

         
      type Internal_Entity_Ada_Pkg_Body;
      

         
      type Internal_Entity_Ada_Pragma;
      

         
      type Internal_Entity_Ada_Prelude;
      

         
      type Internal_Entity_Ada_Prelude_Node_List;
      

         
      type Internal_Entity_Ada_Separate;
      

         
      type Internal_Entity_Ada_Skip;
      

         
      type Internal_Entity_Ada_Skip_List;
      

         
      type Internal_Entity_Ada_Subp;
      

         
      type Internal_Entity_Ada_Use;
      

         
      type Internal_Entity_Ada_With;
      

         
      type Internal_Entity_Ada_With_Formal;
      

         
      type Internal_Entity_All_Qualifier;
      

         
      type Internal_Entity_All_Qualifier_Absent;
      

         
      type Internal_Entity_All_Qualifier_Present;
      

         
      type Internal_Entity_Attribute_Decl;
      

         
      type Internal_Entity_Attribute_Reference;
      

         
      type Internal_Entity_Builtin_Function_Call;
      

         
      type Internal_Entity_Case_Construction;
      

         
      type Internal_Entity_Case_Item;
      

         
      type Internal_Entity_Case_Item_List;
      

         
      type Internal_Entity_Gpr_Node_List;
      

         
      type Internal_Entity_Choices;
      

         
      type Internal_Entity_Compilation_Unit;
      

         
      type Internal_Entity_Empty_Decl;
      

         
      type Internal_Entity_Expr;
      

         
      type Internal_Entity_Expr_List;
      

         
      type Internal_Entity_Single_Tok_Node;
      

         
      type Internal_Entity_Identifier;
      

         
      type Internal_Entity_Identifier_List;
      

         
      type Internal_Entity_Limited_Node;
      

         
      type Internal_Entity_Limited_Absent;
      

         
      type Internal_Entity_Limited_Present;
      

         
      type Internal_Entity_Num_Literal;
      

         
      type Internal_Entity_Others_Designator;
      

         
      type Internal_Entity_Package_Decl;
      

         
      type Internal_Entity_Package_Extension;
      

         
      type Internal_Entity_Package_Renaming;
      

         
      type Internal_Entity_Package_Spec;
      

         
      type Internal_Entity_Prefix;
      

         
      type Internal_Entity_Private_Node;
      

         
      type Internal_Entity_Private_Absent;
      

         
      type Internal_Entity_Private_Present;
      

         
      type Internal_Entity_Project;
      

         
      type Internal_Entity_Project_Declaration;
      

         
      type Internal_Entity_Project_Extension;
      

         
      type Internal_Entity_Project_Qualifier;
      

         
      type Internal_Entity_Project_Qualifier_Abstract;
      

         
      type Internal_Entity_Project_Qualifier_Aggregate;
      

         
      type Internal_Entity_Project_Qualifier_Aggregate_Library;
      

         
      type Internal_Entity_Project_Qualifier_Configuration;
      

         
      type Internal_Entity_Project_Qualifier_Library;
      

         
      type Internal_Entity_Project_Qualifier_Standard;
      

         
      type Internal_Entity_Project_Reference;
      

         
      type Internal_Entity_String_Literal;
      

         
      type Internal_Entity_String_Literal_At;
      

         
      type Internal_Entity_String_Literal_List;
      

         
      type Internal_Entity_Term_List;
      

         
      type Internal_Entity_Term_List_List;
      

         
      type Internal_Entity_Terms;
      

         
      type Internal_Entity_Type_Reference;
      

         
      type Internal_Entity_Typed_String_Decl;
      

         
      type Internal_Entity_Variable_Decl;
      

         
      type Internal_Entity_Variable_Reference;
      

         
      type Internal_Entity_With_Decl;
      

         
      type Internal_Entity_With_Decl_List;
      

         
      type Internal_Env_Assoc;
      


   -------------------------------------------
   -- Array types (incomplete declarations) --
   -------------------------------------------

         
   type Bare_Gpr_Node_Array_Record;
   type Bare_Gpr_Node_Array_Access is access all Bare_Gpr_Node_Array_Record;

         
   type Internal_Entity_Array_Record;
   type Internal_Entity_Array_Access is access all Internal_Entity_Array_Record;

         
   type Lexical_Env_Array_Record;
   type Lexical_Env_Array_Access is access all Lexical_Env_Array_Record;

         
   type Symbol_Type_Array_Record;
   type Symbol_Type_Array_Access is access all Symbol_Type_Array_Record;


   ----------------------------------------------
   -- Iterator types (incomplete declarations) --
   ----------------------------------------------

         
   type Internal_Bare_Gpr_Node_Iterator;
   type Bare_Gpr_Node_Iterator_Access is access all Internal_Bare_Gpr_Node_Iterator;

         
   type Internal_Internal_Entity_Iterator;
   type Internal_Entity_Iterator_Access is access all Internal_Internal_Entity_Iterator;


   -----------------------------------------
   -- Structure types (full declarations) --
   -----------------------------------------

         

      

      type Internal_Designated_Env is record

               Kind : aliased Designated_Env_Kind;
               
               
               Env_Name : aliased Symbol_Type;
               
               
               Direct_Env : aliased Lexical_Env;
               
               
      end record
        with Convention => C;
      No_Designated_Env : constant Internal_Designated_Env;

      procedure Inc_Ref (R : Internal_Designated_Env);
      procedure Dec_Ref (R : in out Internal_Designated_Env);


      function Equivalent (L, R : Internal_Designated_Env) return Boolean;

   


      function Trace_Image (R : Internal_Designated_Env) return String;


         





   
      function Hash (R : Internal_Entity_Info) return Hash_Type;


      function Trace_Image (R : Internal_Entity_Info) return String;


         



      function Create_Internal_Entity
        (Node : Bare_Gpr_Node; Info : Internal_Entity_Info)
         return Internal_Entity;


   
      function Hash (R : Internal_Entity) return Hash_Type;


      function Trace_Image (R : Internal_Entity) return String;


         

      

      type Internal_Entity_Ada_Prelude_Node is record

               Node : aliased Bare_Ada_Prelude_Node;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Prelude_Node : constant Internal_Entity_Ada_Prelude_Node;


      function Create_Internal_Entity_Ada_Prelude_Node
        (Node : Bare_Ada_Prelude_Node; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Prelude_Node;


   


      function Trace_Image (R : Internal_Entity_Ada_Prelude_Node) return String;


         

      

      type Internal_Entity_Ada_Access_Subp is record

               Node : aliased Bare_Ada_Access_Subp;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Access_Subp : constant Internal_Entity_Ada_Access_Subp;


      function Create_Internal_Entity_Ada_Access_Subp
        (Node : Bare_Ada_Access_Subp; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Access_Subp;


   


      function Trace_Image (R : Internal_Entity_Ada_Access_Subp) return String;


         

      

      type Internal_Entity_Ada_Context_Clause is record

               Node : aliased Bare_Ada_Context_Clause;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Context_Clause : constant Internal_Entity_Ada_Context_Clause;


      function Create_Internal_Entity_Ada_Context_Clause
        (Node : Bare_Ada_Context_Clause; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Context_Clause;


   


      function Trace_Image (R : Internal_Entity_Ada_Context_Clause) return String;


         

      

      type Internal_Entity_Base_List is record

               Node : aliased Bare_Base_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Base_List : constant Internal_Entity_Base_List;


      function Create_Internal_Entity_Base_List
        (Node : Bare_Base_List; Info : Internal_Entity_Info)
         return Internal_Entity_Base_List;


   


      function Trace_Image (R : Internal_Entity_Base_List) return String;


         

      

      type Internal_Entity_Ada_Context_Clause_List is record

               Node : aliased Bare_Ada_Context_Clause_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Context_Clause_List : constant Internal_Entity_Ada_Context_Clause_List;


      function Create_Internal_Entity_Ada_Context_Clause_List
        (Node : Bare_Ada_Context_Clause_List; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Context_Clause_List;


   


      function Trace_Image (R : Internal_Entity_Ada_Context_Clause_List) return String;


         

      

      type Internal_Entity_Ada_Entity_Kind is record

               Node : aliased Bare_Ada_Entity_Kind;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Entity_Kind : constant Internal_Entity_Ada_Entity_Kind;


      function Create_Internal_Entity_Ada_Entity_Kind
        (Node : Bare_Ada_Entity_Kind; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Entity_Kind;


   


      function Trace_Image (R : Internal_Entity_Ada_Entity_Kind) return String;


         

      

      type Internal_Entity_Ada_Entity_Kind_Function is record

               Node : aliased Bare_Ada_Entity_Kind_Function;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Entity_Kind_Function : constant Internal_Entity_Ada_Entity_Kind_Function;


      function Create_Internal_Entity_Ada_Entity_Kind_Function
        (Node : Bare_Ada_Entity_Kind_Function; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Entity_Kind_Function;


   


      function Trace_Image (R : Internal_Entity_Ada_Entity_Kind_Function) return String;


         

      

      type Internal_Entity_Ada_Entity_Kind_Package is record

               Node : aliased Bare_Ada_Entity_Kind_Package;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Entity_Kind_Package : constant Internal_Entity_Ada_Entity_Kind_Package;


      function Create_Internal_Entity_Ada_Entity_Kind_Package
        (Node : Bare_Ada_Entity_Kind_Package; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Entity_Kind_Package;


   


      function Trace_Image (R : Internal_Entity_Ada_Entity_Kind_Package) return String;


         

      

      type Internal_Entity_Ada_Entity_Kind_Procedure is record

               Node : aliased Bare_Ada_Entity_Kind_Procedure;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Entity_Kind_Procedure : constant Internal_Entity_Ada_Entity_Kind_Procedure;


      function Create_Internal_Entity_Ada_Entity_Kind_Procedure
        (Node : Bare_Ada_Entity_Kind_Procedure; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Entity_Kind_Procedure;


   


      function Trace_Image (R : Internal_Entity_Ada_Entity_Kind_Procedure) return String;


         

      

      type Internal_Entity_Ada_Generic is record

               Node : aliased Bare_Ada_Generic;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Generic : constant Internal_Entity_Ada_Generic;


      function Create_Internal_Entity_Ada_Generic
        (Node : Bare_Ada_Generic; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Generic;


   


      function Trace_Image (R : Internal_Entity_Ada_Generic) return String;


         

      

      type Internal_Entity_Ada_Library_Item is record

               Node : aliased Bare_Ada_Library_Item;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Library_Item : constant Internal_Entity_Ada_Library_Item;


      function Create_Internal_Entity_Ada_Library_Item
        (Node : Bare_Ada_Library_Item; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Library_Item;


   


      function Trace_Image (R : Internal_Entity_Ada_Library_Item) return String;


         

      

      type Internal_Entity_Ada_Main is record

               Node : aliased Bare_Ada_Main;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Main : constant Internal_Entity_Ada_Main;


      function Create_Internal_Entity_Ada_Main
        (Node : Bare_Ada_Main; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Main;


   


      function Trace_Image (R : Internal_Entity_Ada_Main) return String;


         

      

      type Internal_Entity_Ada_Pkg is record

               Node : aliased Bare_Ada_Pkg;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Pkg : constant Internal_Entity_Ada_Pkg;


      function Create_Internal_Entity_Ada_Pkg
        (Node : Bare_Ada_Pkg; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Pkg;


   


      function Trace_Image (R : Internal_Entity_Ada_Pkg) return String;


         

      

      type Internal_Entity_Ada_Pkg_Body is record

               Node : aliased Bare_Ada_Pkg_Body;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Pkg_Body : constant Internal_Entity_Ada_Pkg_Body;


      function Create_Internal_Entity_Ada_Pkg_Body
        (Node : Bare_Ada_Pkg_Body; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Pkg_Body;


   


      function Trace_Image (R : Internal_Entity_Ada_Pkg_Body) return String;


         

      

      type Internal_Entity_Ada_Pragma is record

               Node : aliased Bare_Ada_Pragma;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Pragma : constant Internal_Entity_Ada_Pragma;


      function Create_Internal_Entity_Ada_Pragma
        (Node : Bare_Ada_Pragma; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Pragma;


   


      function Trace_Image (R : Internal_Entity_Ada_Pragma) return String;


         

      

      type Internal_Entity_Ada_Prelude is record

               Node : aliased Bare_Ada_Prelude;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Prelude : constant Internal_Entity_Ada_Prelude;


      function Create_Internal_Entity_Ada_Prelude
        (Node : Bare_Ada_Prelude; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Prelude;


   


      function Trace_Image (R : Internal_Entity_Ada_Prelude) return String;


         

      

      type Internal_Entity_Ada_Prelude_Node_List is record

               Node : aliased Bare_Ada_Prelude_Node_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Prelude_Node_List : constant Internal_Entity_Ada_Prelude_Node_List;


      function Create_Internal_Entity_Ada_Prelude_Node_List
        (Node : Bare_Ada_Prelude_Node_List; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Prelude_Node_List;


   


      function Trace_Image (R : Internal_Entity_Ada_Prelude_Node_List) return String;


         

      

      type Internal_Entity_Ada_Separate is record

               Node : aliased Bare_Ada_Separate;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Separate : constant Internal_Entity_Ada_Separate;


      function Create_Internal_Entity_Ada_Separate
        (Node : Bare_Ada_Separate; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Separate;


   


      function Trace_Image (R : Internal_Entity_Ada_Separate) return String;


         

      

      type Internal_Entity_Ada_Skip is record

               Node : aliased Bare_Ada_Skip;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Skip : constant Internal_Entity_Ada_Skip;


      function Create_Internal_Entity_Ada_Skip
        (Node : Bare_Ada_Skip; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Skip;


   


      function Trace_Image (R : Internal_Entity_Ada_Skip) return String;


         

      

      type Internal_Entity_Ada_Skip_List is record

               Node : aliased Bare_Ada_Skip_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Skip_List : constant Internal_Entity_Ada_Skip_List;


      function Create_Internal_Entity_Ada_Skip_List
        (Node : Bare_Ada_Skip_List; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Skip_List;


   


      function Trace_Image (R : Internal_Entity_Ada_Skip_List) return String;


         

      

      type Internal_Entity_Ada_Subp is record

               Node : aliased Bare_Ada_Subp;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Subp : constant Internal_Entity_Ada_Subp;


      function Create_Internal_Entity_Ada_Subp
        (Node : Bare_Ada_Subp; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Subp;


   


      function Trace_Image (R : Internal_Entity_Ada_Subp) return String;


         

      

      type Internal_Entity_Ada_Use is record

               Node : aliased Bare_Ada_Use;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_Use : constant Internal_Entity_Ada_Use;


      function Create_Internal_Entity_Ada_Use
        (Node : Bare_Ada_Use; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Use;


   


      function Trace_Image (R : Internal_Entity_Ada_Use) return String;


         

      

      type Internal_Entity_Ada_With is record

               Node : aliased Bare_Ada_With;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_With : constant Internal_Entity_Ada_With;


      function Create_Internal_Entity_Ada_With
        (Node : Bare_Ada_With; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_With;


   


      function Trace_Image (R : Internal_Entity_Ada_With) return String;


         

      

      type Internal_Entity_Ada_With_Formal is record

               Node : aliased Bare_Ada_With_Formal;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ada_With_Formal : constant Internal_Entity_Ada_With_Formal;


      function Create_Internal_Entity_Ada_With_Formal
        (Node : Bare_Ada_With_Formal; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_With_Formal;


   


      function Trace_Image (R : Internal_Entity_Ada_With_Formal) return String;


         

      

      type Internal_Entity_All_Qualifier is record

               Node : aliased Bare_All_Qualifier;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_All_Qualifier : constant Internal_Entity_All_Qualifier;


      function Create_Internal_Entity_All_Qualifier
        (Node : Bare_All_Qualifier; Info : Internal_Entity_Info)
         return Internal_Entity_All_Qualifier;


   


      function Trace_Image (R : Internal_Entity_All_Qualifier) return String;


         

      

      type Internal_Entity_All_Qualifier_Absent is record

               Node : aliased Bare_All_Qualifier_Absent;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_All_Qualifier_Absent : constant Internal_Entity_All_Qualifier_Absent;


      function Create_Internal_Entity_All_Qualifier_Absent
        (Node : Bare_All_Qualifier_Absent; Info : Internal_Entity_Info)
         return Internal_Entity_All_Qualifier_Absent;


   


      function Trace_Image (R : Internal_Entity_All_Qualifier_Absent) return String;


         

      

      type Internal_Entity_All_Qualifier_Present is record

               Node : aliased Bare_All_Qualifier_Present;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_All_Qualifier_Present : constant Internal_Entity_All_Qualifier_Present;


      function Create_Internal_Entity_All_Qualifier_Present
        (Node : Bare_All_Qualifier_Present; Info : Internal_Entity_Info)
         return Internal_Entity_All_Qualifier_Present;


   


      function Trace_Image (R : Internal_Entity_All_Qualifier_Present) return String;


         

      

      type Internal_Entity_Attribute_Decl is record

               Node : aliased Bare_Attribute_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Attribute_Decl : constant Internal_Entity_Attribute_Decl;


      function Create_Internal_Entity_Attribute_Decl
        (Node : Bare_Attribute_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Attribute_Decl;


   


      function Trace_Image (R : Internal_Entity_Attribute_Decl) return String;


         

      

      type Internal_Entity_Attribute_Reference is record

               Node : aliased Bare_Attribute_Reference;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Attribute_Reference : constant Internal_Entity_Attribute_Reference;


      function Create_Internal_Entity_Attribute_Reference
        (Node : Bare_Attribute_Reference; Info : Internal_Entity_Info)
         return Internal_Entity_Attribute_Reference;


   


      function Trace_Image (R : Internal_Entity_Attribute_Reference) return String;


         

      

      type Internal_Entity_Builtin_Function_Call is record

               Node : aliased Bare_Builtin_Function_Call;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Builtin_Function_Call : constant Internal_Entity_Builtin_Function_Call;


      function Create_Internal_Entity_Builtin_Function_Call
        (Node : Bare_Builtin_Function_Call; Info : Internal_Entity_Info)
         return Internal_Entity_Builtin_Function_Call;


   


      function Trace_Image (R : Internal_Entity_Builtin_Function_Call) return String;


         

      

      type Internal_Entity_Case_Construction is record

               Node : aliased Bare_Case_Construction;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Case_Construction : constant Internal_Entity_Case_Construction;


      function Create_Internal_Entity_Case_Construction
        (Node : Bare_Case_Construction; Info : Internal_Entity_Info)
         return Internal_Entity_Case_Construction;


   


      function Trace_Image (R : Internal_Entity_Case_Construction) return String;


         

      

      type Internal_Entity_Case_Item is record

               Node : aliased Bare_Case_Item;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Case_Item : constant Internal_Entity_Case_Item;


      function Create_Internal_Entity_Case_Item
        (Node : Bare_Case_Item; Info : Internal_Entity_Info)
         return Internal_Entity_Case_Item;


   


      function Trace_Image (R : Internal_Entity_Case_Item) return String;


         

      

      type Internal_Entity_Case_Item_List is record

               Node : aliased Bare_Case_Item_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Case_Item_List : constant Internal_Entity_Case_Item_List;


      function Create_Internal_Entity_Case_Item_List
        (Node : Bare_Case_Item_List; Info : Internal_Entity_Info)
         return Internal_Entity_Case_Item_List;


   


      function Trace_Image (R : Internal_Entity_Case_Item_List) return String;


         

      

      type Internal_Entity_Gpr_Node_List is record

               Node : aliased Bare_Gpr_Node_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Gpr_Node_List : constant Internal_Entity_Gpr_Node_List;


      function Create_Internal_Entity_Gpr_Node_List
        (Node : Bare_Gpr_Node_List; Info : Internal_Entity_Info)
         return Internal_Entity_Gpr_Node_List;


   


      function Trace_Image (R : Internal_Entity_Gpr_Node_List) return String;


         

      

      type Internal_Entity_Choices is record

               Node : aliased Bare_Choices;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Choices : constant Internal_Entity_Choices;


      function Create_Internal_Entity_Choices
        (Node : Bare_Choices; Info : Internal_Entity_Info)
         return Internal_Entity_Choices;


   


      function Trace_Image (R : Internal_Entity_Choices) return String;


         

      

      type Internal_Entity_Compilation_Unit is record

               Node : aliased Bare_Compilation_Unit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Compilation_Unit : constant Internal_Entity_Compilation_Unit;


      function Create_Internal_Entity_Compilation_Unit
        (Node : Bare_Compilation_Unit; Info : Internal_Entity_Info)
         return Internal_Entity_Compilation_Unit;


   


      function Trace_Image (R : Internal_Entity_Compilation_Unit) return String;


         

      

      type Internal_Entity_Empty_Decl is record

               Node : aliased Bare_Empty_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Empty_Decl : constant Internal_Entity_Empty_Decl;


      function Create_Internal_Entity_Empty_Decl
        (Node : Bare_Empty_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Empty_Decl;


   


      function Trace_Image (R : Internal_Entity_Empty_Decl) return String;


         

      

      type Internal_Entity_Expr is record

               Node : aliased Bare_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Expr : constant Internal_Entity_Expr;


      function Create_Internal_Entity_Expr
        (Node : Bare_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Expr;


   


      function Trace_Image (R : Internal_Entity_Expr) return String;


         

      

      type Internal_Entity_Expr_List is record

               Node : aliased Bare_Expr_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Expr_List : constant Internal_Entity_Expr_List;


      function Create_Internal_Entity_Expr_List
        (Node : Bare_Expr_List; Info : Internal_Entity_Info)
         return Internal_Entity_Expr_List;


   


      function Trace_Image (R : Internal_Entity_Expr_List) return String;


         

      

      type Internal_Entity_Single_Tok_Node is record

               Node : aliased Bare_Single_Tok_Node;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Single_Tok_Node : constant Internal_Entity_Single_Tok_Node;


      function Create_Internal_Entity_Single_Tok_Node
        (Node : Bare_Single_Tok_Node; Info : Internal_Entity_Info)
         return Internal_Entity_Single_Tok_Node;


   


      function Trace_Image (R : Internal_Entity_Single_Tok_Node) return String;


         

      

      type Internal_Entity_Identifier is record

               Node : aliased Bare_Identifier;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Identifier : constant Internal_Entity_Identifier;


      function Create_Internal_Entity_Identifier
        (Node : Bare_Identifier; Info : Internal_Entity_Info)
         return Internal_Entity_Identifier;


   


      function Trace_Image (R : Internal_Entity_Identifier) return String;


         

      

      type Internal_Entity_Identifier_List is record

               Node : aliased Bare_Identifier_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Identifier_List : constant Internal_Entity_Identifier_List;


      function Create_Internal_Entity_Identifier_List
        (Node : Bare_Identifier_List; Info : Internal_Entity_Info)
         return Internal_Entity_Identifier_List;


   


      function Trace_Image (R : Internal_Entity_Identifier_List) return String;


         

      

      type Internal_Entity_Limited_Node is record

               Node : aliased Bare_Limited_Node;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Limited_Node : constant Internal_Entity_Limited_Node;


      function Create_Internal_Entity_Limited_Node
        (Node : Bare_Limited_Node; Info : Internal_Entity_Info)
         return Internal_Entity_Limited_Node;


   


      function Trace_Image (R : Internal_Entity_Limited_Node) return String;


         

      

      type Internal_Entity_Limited_Absent is record

               Node : aliased Bare_Limited_Absent;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Limited_Absent : constant Internal_Entity_Limited_Absent;


      function Create_Internal_Entity_Limited_Absent
        (Node : Bare_Limited_Absent; Info : Internal_Entity_Info)
         return Internal_Entity_Limited_Absent;


   


      function Trace_Image (R : Internal_Entity_Limited_Absent) return String;


         

      

      type Internal_Entity_Limited_Present is record

               Node : aliased Bare_Limited_Present;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Limited_Present : constant Internal_Entity_Limited_Present;


      function Create_Internal_Entity_Limited_Present
        (Node : Bare_Limited_Present; Info : Internal_Entity_Info)
         return Internal_Entity_Limited_Present;


   


      function Trace_Image (R : Internal_Entity_Limited_Present) return String;


         

      

      type Internal_Entity_Num_Literal is record

               Node : aliased Bare_Num_Literal;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Num_Literal : constant Internal_Entity_Num_Literal;


      function Create_Internal_Entity_Num_Literal
        (Node : Bare_Num_Literal; Info : Internal_Entity_Info)
         return Internal_Entity_Num_Literal;


   


      function Trace_Image (R : Internal_Entity_Num_Literal) return String;


         

      

      type Internal_Entity_Others_Designator is record

               Node : aliased Bare_Others_Designator;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Others_Designator : constant Internal_Entity_Others_Designator;


      function Create_Internal_Entity_Others_Designator
        (Node : Bare_Others_Designator; Info : Internal_Entity_Info)
         return Internal_Entity_Others_Designator;


   


      function Trace_Image (R : Internal_Entity_Others_Designator) return String;


         

      

      type Internal_Entity_Package_Decl is record

               Node : aliased Bare_Package_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Package_Decl : constant Internal_Entity_Package_Decl;


      function Create_Internal_Entity_Package_Decl
        (Node : Bare_Package_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Package_Decl;


   


      function Trace_Image (R : Internal_Entity_Package_Decl) return String;


         

      

      type Internal_Entity_Package_Extension is record

               Node : aliased Bare_Package_Extension;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Package_Extension : constant Internal_Entity_Package_Extension;


      function Create_Internal_Entity_Package_Extension
        (Node : Bare_Package_Extension; Info : Internal_Entity_Info)
         return Internal_Entity_Package_Extension;


   


      function Trace_Image (R : Internal_Entity_Package_Extension) return String;


         

      

      type Internal_Entity_Package_Renaming is record

               Node : aliased Bare_Package_Renaming;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Package_Renaming : constant Internal_Entity_Package_Renaming;


      function Create_Internal_Entity_Package_Renaming
        (Node : Bare_Package_Renaming; Info : Internal_Entity_Info)
         return Internal_Entity_Package_Renaming;


   


      function Trace_Image (R : Internal_Entity_Package_Renaming) return String;


         

      

      type Internal_Entity_Package_Spec is record

               Node : aliased Bare_Package_Spec;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Package_Spec : constant Internal_Entity_Package_Spec;


      function Create_Internal_Entity_Package_Spec
        (Node : Bare_Package_Spec; Info : Internal_Entity_Info)
         return Internal_Entity_Package_Spec;


   


      function Trace_Image (R : Internal_Entity_Package_Spec) return String;


         

      

      type Internal_Entity_Prefix is record

               Node : aliased Bare_Prefix;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Prefix : constant Internal_Entity_Prefix;


      function Create_Internal_Entity_Prefix
        (Node : Bare_Prefix; Info : Internal_Entity_Info)
         return Internal_Entity_Prefix;


   


      function Trace_Image (R : Internal_Entity_Prefix) return String;


         

      

      type Internal_Entity_Private_Node is record

               Node : aliased Bare_Private_Node;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Private_Node : constant Internal_Entity_Private_Node;


      function Create_Internal_Entity_Private_Node
        (Node : Bare_Private_Node; Info : Internal_Entity_Info)
         return Internal_Entity_Private_Node;


   


      function Trace_Image (R : Internal_Entity_Private_Node) return String;


         

      

      type Internal_Entity_Private_Absent is record

               Node : aliased Bare_Private_Absent;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Private_Absent : constant Internal_Entity_Private_Absent;


      function Create_Internal_Entity_Private_Absent
        (Node : Bare_Private_Absent; Info : Internal_Entity_Info)
         return Internal_Entity_Private_Absent;


   


      function Trace_Image (R : Internal_Entity_Private_Absent) return String;


         

      

      type Internal_Entity_Private_Present is record

               Node : aliased Bare_Private_Present;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Private_Present : constant Internal_Entity_Private_Present;


      function Create_Internal_Entity_Private_Present
        (Node : Bare_Private_Present; Info : Internal_Entity_Info)
         return Internal_Entity_Private_Present;


   


      function Trace_Image (R : Internal_Entity_Private_Present) return String;


         

      

      type Internal_Entity_Project is record

               Node : aliased Bare_Project;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Project : constant Internal_Entity_Project;


      function Create_Internal_Entity_Project
        (Node : Bare_Project; Info : Internal_Entity_Info)
         return Internal_Entity_Project;


   


      function Trace_Image (R : Internal_Entity_Project) return String;


         

      

      type Internal_Entity_Project_Declaration is record

               Node : aliased Bare_Project_Declaration;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Project_Declaration : constant Internal_Entity_Project_Declaration;


      function Create_Internal_Entity_Project_Declaration
        (Node : Bare_Project_Declaration; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Declaration;


   


      function Trace_Image (R : Internal_Entity_Project_Declaration) return String;


         

      

      type Internal_Entity_Project_Extension is record

               Node : aliased Bare_Project_Extension;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Project_Extension : constant Internal_Entity_Project_Extension;


      function Create_Internal_Entity_Project_Extension
        (Node : Bare_Project_Extension; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Extension;


   


      function Trace_Image (R : Internal_Entity_Project_Extension) return String;


         

      

      type Internal_Entity_Project_Qualifier is record

               Node : aliased Bare_Project_Qualifier;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Project_Qualifier : constant Internal_Entity_Project_Qualifier;


      function Create_Internal_Entity_Project_Qualifier
        (Node : Bare_Project_Qualifier; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Qualifier;


   


      function Trace_Image (R : Internal_Entity_Project_Qualifier) return String;


         

      

      type Internal_Entity_Project_Qualifier_Abstract is record

               Node : aliased Bare_Project_Qualifier_Abstract;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Project_Qualifier_Abstract : constant Internal_Entity_Project_Qualifier_Abstract;


      function Create_Internal_Entity_Project_Qualifier_Abstract
        (Node : Bare_Project_Qualifier_Abstract; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Qualifier_Abstract;


   


      function Trace_Image (R : Internal_Entity_Project_Qualifier_Abstract) return String;


         

      

      type Internal_Entity_Project_Qualifier_Aggregate is record

               Node : aliased Bare_Project_Qualifier_Aggregate;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Project_Qualifier_Aggregate : constant Internal_Entity_Project_Qualifier_Aggregate;


      function Create_Internal_Entity_Project_Qualifier_Aggregate
        (Node : Bare_Project_Qualifier_Aggregate; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Qualifier_Aggregate;


   


      function Trace_Image (R : Internal_Entity_Project_Qualifier_Aggregate) return String;


         

      

      type Internal_Entity_Project_Qualifier_Aggregate_Library is record

               Node : aliased Bare_Project_Qualifier_Aggregate_Library;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Project_Qualifier_Aggregate_Library : constant Internal_Entity_Project_Qualifier_Aggregate_Library;


      function Create_Internal_Entity_Project_Qualifier_Aggregate_Library
        (Node : Bare_Project_Qualifier_Aggregate_Library; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Qualifier_Aggregate_Library;


   


      function Trace_Image (R : Internal_Entity_Project_Qualifier_Aggregate_Library) return String;


         

      

      type Internal_Entity_Project_Qualifier_Configuration is record

               Node : aliased Bare_Project_Qualifier_Configuration;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Project_Qualifier_Configuration : constant Internal_Entity_Project_Qualifier_Configuration;


      function Create_Internal_Entity_Project_Qualifier_Configuration
        (Node : Bare_Project_Qualifier_Configuration; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Qualifier_Configuration;


   


      function Trace_Image (R : Internal_Entity_Project_Qualifier_Configuration) return String;


         

      

      type Internal_Entity_Project_Qualifier_Library is record

               Node : aliased Bare_Project_Qualifier_Library;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Project_Qualifier_Library : constant Internal_Entity_Project_Qualifier_Library;


      function Create_Internal_Entity_Project_Qualifier_Library
        (Node : Bare_Project_Qualifier_Library; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Qualifier_Library;


   


      function Trace_Image (R : Internal_Entity_Project_Qualifier_Library) return String;


         

      

      type Internal_Entity_Project_Qualifier_Standard is record

               Node : aliased Bare_Project_Qualifier_Standard;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Project_Qualifier_Standard : constant Internal_Entity_Project_Qualifier_Standard;


      function Create_Internal_Entity_Project_Qualifier_Standard
        (Node : Bare_Project_Qualifier_Standard; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Qualifier_Standard;


   


      function Trace_Image (R : Internal_Entity_Project_Qualifier_Standard) return String;


         

      

      type Internal_Entity_Project_Reference is record

               Node : aliased Bare_Project_Reference;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Project_Reference : constant Internal_Entity_Project_Reference;


      function Create_Internal_Entity_Project_Reference
        (Node : Bare_Project_Reference; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Reference;


   


      function Trace_Image (R : Internal_Entity_Project_Reference) return String;


         

      

      type Internal_Entity_String_Literal is record

               Node : aliased Bare_String_Literal;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_String_Literal : constant Internal_Entity_String_Literal;


      function Create_Internal_Entity_String_Literal
        (Node : Bare_String_Literal; Info : Internal_Entity_Info)
         return Internal_Entity_String_Literal;


   


      function Trace_Image (R : Internal_Entity_String_Literal) return String;


         

      

      type Internal_Entity_String_Literal_At is record

               Node : aliased Bare_String_Literal_At;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_String_Literal_At : constant Internal_Entity_String_Literal_At;


      function Create_Internal_Entity_String_Literal_At
        (Node : Bare_String_Literal_At; Info : Internal_Entity_Info)
         return Internal_Entity_String_Literal_At;


   


      function Trace_Image (R : Internal_Entity_String_Literal_At) return String;


         

      

      type Internal_Entity_String_Literal_List is record

               Node : aliased Bare_String_Literal_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_String_Literal_List : constant Internal_Entity_String_Literal_List;


      function Create_Internal_Entity_String_Literal_List
        (Node : Bare_String_Literal_List; Info : Internal_Entity_Info)
         return Internal_Entity_String_Literal_List;


   


      function Trace_Image (R : Internal_Entity_String_Literal_List) return String;


         

      

      type Internal_Entity_Term_List is record

               Node : aliased Bare_Term_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Term_List : constant Internal_Entity_Term_List;


      function Create_Internal_Entity_Term_List
        (Node : Bare_Term_List; Info : Internal_Entity_Info)
         return Internal_Entity_Term_List;


   


      function Trace_Image (R : Internal_Entity_Term_List) return String;


         

      

      type Internal_Entity_Term_List_List is record

               Node : aliased Bare_Term_List_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Term_List_List : constant Internal_Entity_Term_List_List;


      function Create_Internal_Entity_Term_List_List
        (Node : Bare_Term_List_List; Info : Internal_Entity_Info)
         return Internal_Entity_Term_List_List;


   


      function Trace_Image (R : Internal_Entity_Term_List_List) return String;


         

      

      type Internal_Entity_Terms is record

               Node : aliased Bare_Terms;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Terms : constant Internal_Entity_Terms;


      function Create_Internal_Entity_Terms
        (Node : Bare_Terms; Info : Internal_Entity_Info)
         return Internal_Entity_Terms;


   


      function Trace_Image (R : Internal_Entity_Terms) return String;


         

      

      type Internal_Entity_Type_Reference is record

               Node : aliased Bare_Type_Reference;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Type_Reference : constant Internal_Entity_Type_Reference;


      function Create_Internal_Entity_Type_Reference
        (Node : Bare_Type_Reference; Info : Internal_Entity_Info)
         return Internal_Entity_Type_Reference;


   


      function Trace_Image (R : Internal_Entity_Type_Reference) return String;


         

      

      type Internal_Entity_Typed_String_Decl is record

               Node : aliased Bare_Typed_String_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Typed_String_Decl : constant Internal_Entity_Typed_String_Decl;


      function Create_Internal_Entity_Typed_String_Decl
        (Node : Bare_Typed_String_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Typed_String_Decl;


   


      function Trace_Image (R : Internal_Entity_Typed_String_Decl) return String;


         

      

      type Internal_Entity_Variable_Decl is record

               Node : aliased Bare_Variable_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Variable_Decl : constant Internal_Entity_Variable_Decl;


      function Create_Internal_Entity_Variable_Decl
        (Node : Bare_Variable_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Variable_Decl;


   


      function Trace_Image (R : Internal_Entity_Variable_Decl) return String;


         

      

      type Internal_Entity_Variable_Reference is record

               Node : aliased Bare_Variable_Reference;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Variable_Reference : constant Internal_Entity_Variable_Reference;


      function Create_Internal_Entity_Variable_Reference
        (Node : Bare_Variable_Reference; Info : Internal_Entity_Info)
         return Internal_Entity_Variable_Reference;


   


      function Trace_Image (R : Internal_Entity_Variable_Reference) return String;


         

      

      type Internal_Entity_With_Decl is record

               Node : aliased Bare_With_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_With_Decl : constant Internal_Entity_With_Decl;


      function Create_Internal_Entity_With_Decl
        (Node : Bare_With_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_With_Decl;


   


      function Trace_Image (R : Internal_Entity_With_Decl) return String;


         

      

      type Internal_Entity_With_Decl_List is record

               Node : aliased Bare_With_Decl_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_With_Decl_List : constant Internal_Entity_With_Decl_List;


      function Create_Internal_Entity_With_Decl_List
        (Node : Bare_With_Decl_List; Info : Internal_Entity_Info)
         return Internal_Entity_With_Decl_List;


   


      function Trace_Image (R : Internal_Entity_With_Decl_List) return String;


         

      

      type Internal_Env_Assoc is record

               Key : aliased Symbol_Type;
               
               
               Value : aliased Bare_Gpr_Node;
               
               
               Dest_Env : aliased Internal_Designated_Env;
               
               
               Metadata : aliased Internal_Metadata;
               
               
      end record
        with Convention => C;
      No_Env_Assoc : constant Internal_Env_Assoc;

      procedure Inc_Ref (R : Internal_Env_Assoc);
      procedure Dec_Ref (R : in out Internal_Env_Assoc);


      function Equivalent (L, R : Internal_Env_Assoc) return Boolean;

   


      function Trace_Image (R : Internal_Env_Assoc) return String;



   -----------------
   -- Array types --
   -----------------

   --  We implement array types as discriminated records so that binding to C
   --  can be done without copy.

         

   

   type Internal_Bare_Gpr_Node_Array is
      array (Positive range <>) of Bare_Gpr_Node;

   type Bare_Gpr_Node_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Bare_Gpr_Node_Array (1 .. N);
   end record;

   Empty_Bare_Gpr_Node_Array_Record : aliased Bare_Gpr_Node_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Bare_Gpr_Node_Array_Type : constant Bare_Gpr_Node_Array_Access :=
      Empty_Bare_Gpr_Node_Array_Record'Access;


   function Create_Bare_Gpr_Node_Array (Items_Count : Natural) return Bare_Gpr_Node_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Bare_Gpr_Node_Array
     (Items : Internal_Bare_Gpr_Node_Array) return Bare_Gpr_Node_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Gpr_Node;
      T       : Bare_Gpr_Node_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Bare_Gpr_Node;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Bare_Gpr_Node_Array_Access) return Bare_Gpr_Node_Array_Access;


   function Length (T : Bare_Gpr_Node_Array_Access) return Natural;

   procedure Inc_Ref (T : Bare_Gpr_Node_Array_Access);
   procedure Dec_Ref (T : in out Bare_Gpr_Node_Array_Access);

   function Equivalent (L, R : Bare_Gpr_Node_Array_Access) return Boolean;


      function Trace_Image (A : Bare_Gpr_Node_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Bare_Gpr_Node_Array_Record, Bare_Gpr_Node_Array_Access);

         

   

   type Internal_Internal_Entity_Array is
      array (Positive range <>) of Internal_Entity;

   type Internal_Entity_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Entity_Array (1 .. N);
   end record;

   Empty_Internal_Entity_Array_Record : aliased Internal_Entity_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Array_Type : constant Internal_Entity_Array_Access :=
      Empty_Internal_Entity_Array_Record'Access;

   function Create_Internal_Entity_Array
     (Items : AST_Envs.Entity_Array) return Internal_Entity_Array_Access;

   function Create_Internal_Entity_Array (Items_Count : Natural) return Internal_Entity_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Array
     (Items : Internal_Internal_Entity_Array) return Internal_Entity_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Gpr_Node;
      T       : Internal_Entity_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Entity_Array_Access) return Internal_Entity_Array_Access;


   function Length (T : Internal_Entity_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Array_Access);

   function Equivalent (L, R : Internal_Entity_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Entity_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Entity_Array_Record, Internal_Entity_Array_Access);

         

   

   type Internal_Lexical_Env_Array is
      array (Positive range <>) of Lexical_Env;

   type Lexical_Env_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Lexical_Env_Array (1 .. N);
   end record;

   Empty_Lexical_Env_Array_Record : aliased Lexical_Env_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Lexical_Env_Array_Type : constant Lexical_Env_Array_Access :=
      Empty_Lexical_Env_Array_Record'Access;


   function Create_Lexical_Env_Array (Items_Count : Natural) return Lexical_Env_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Lexical_Env_Array
     (Items : Internal_Lexical_Env_Array) return Lexical_Env_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Gpr_Node;
      T       : Lexical_Env_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Lexical_Env;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Lexical_Env_Array_Access) return Lexical_Env_Array_Access;


   function Length (T : Lexical_Env_Array_Access) return Natural;

   procedure Inc_Ref (T : Lexical_Env_Array_Access);
   procedure Dec_Ref (T : in out Lexical_Env_Array_Access);

   function Equivalent (L, R : Lexical_Env_Array_Access) return Boolean;


      function Trace_Image (A : Lexical_Env_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Lexical_Env_Array_Record, Lexical_Env_Array_Access);

         

   

   type Internal_Symbol_Type_Array is
      array (Positive range <>) of Symbol_Type;

   type Symbol_Type_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Symbol_Type_Array (1 .. N);
   end record;

   Empty_Symbol_Type_Array_Record : aliased Symbol_Type_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Symbol_Type_Array_Type : constant Symbol_Type_Array_Access :=
      Empty_Symbol_Type_Array_Record'Access;


   function Create_Symbol_Type_Array (Items_Count : Natural) return Symbol_Type_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Symbol_Type_Array
     (Items : Internal_Symbol_Type_Array) return Symbol_Type_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Gpr_Node;
      T       : Symbol_Type_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Symbol_Type;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Symbol_Type_Array_Access) return Symbol_Type_Array_Access;


   function Length (T : Symbol_Type_Array_Access) return Natural;

   procedure Inc_Ref (T : Symbol_Type_Array_Access);
   procedure Dec_Ref (T : in out Symbol_Type_Array_Access);

   function Equivalent (L, R : Symbol_Type_Array_Access) return Boolean;


      function Trace_Image (A : Symbol_Type_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Symbol_Type_Array_Record, Symbol_Type_Array_Access);


   --------------------
   -- Iterator types --
   --------------------

         

   

   type Internal_Bare_Gpr_Node_Iterator is record
      Ref_Count : Integer;
      --  Reference count. The iterator is freed when this drops to zero.
      --  Negative values are interpreted as "always living singleton".

      Safety_Net : Iterator_Safety_Net;
      --  Safety net for the iterator. Used to check that values produced by
      --  the iterator are still valid. Unlike for other types, we put the
      --  safety net in the internal type so that it can be used in all other
      --  APIs (Python, ...).
      --
      --  While other types (except nodes) are "deeply" converted to native
      --  APIs (for instance: internal arrays are turned into native Python
      --  lists, likewise for array items, etc.), iterators are lazy, so the
      --  deep conversion is not possible.

      Elements : Bare_Gpr_Node_Array_Access;
      Index    : Positive;
   end record;

   Empty_Internal_Bare_Gpr_Node_Iterator : aliased Internal_Bare_Gpr_Node_Iterator :=
     (Ref_Count  => -1,
      Safety_Net => No_Iterator_Safety_Net,
      Elements   => No_Bare_Gpr_Node_Array_Type,
      Index      => 1);
   No_Bare_Gpr_Node_Iterator_Type : constant Bare_Gpr_Node_Iterator_Access :=
      Empty_Internal_Bare_Gpr_Node_Iterator'Access;

   function Next
     (T       : Bare_Gpr_Node_Iterator_Access;
      Element : out Bare_Gpr_Node) return Boolean;

   procedure Inc_Ref (T : Bare_Gpr_Node_Iterator_Access);
   procedure Dec_Ref (T : in out Bare_Gpr_Node_Iterator_Access);

      function Trace_Image (A : Bare_Gpr_Node_Iterator_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Bare_Gpr_Node_Iterator, Bare_Gpr_Node_Iterator_Access);

         

   

   type Internal_Internal_Entity_Iterator is record
      Ref_Count : Integer;
      --  Reference count. The iterator is freed when this drops to zero.
      --  Negative values are interpreted as "always living singleton".

      Safety_Net : Iterator_Safety_Net;
      --  Safety net for the iterator. Used to check that values produced by
      --  the iterator are still valid. Unlike for other types, we put the
      --  safety net in the internal type so that it can be used in all other
      --  APIs (Python, ...).
      --
      --  While other types (except nodes) are "deeply" converted to native
      --  APIs (for instance: internal arrays are turned into native Python
      --  lists, likewise for array items, etc.), iterators are lazy, so the
      --  deep conversion is not possible.

      Elements : Internal_Entity_Array_Access;
      Index    : Positive;
   end record;

   Empty_Internal_Internal_Entity_Iterator : aliased Internal_Internal_Entity_Iterator :=
     (Ref_Count  => -1,
      Safety_Net => No_Iterator_Safety_Net,
      Elements   => No_Internal_Entity_Array_Type,
      Index      => 1);
   No_Internal_Entity_Iterator_Type : constant Internal_Entity_Iterator_Access :=
      Empty_Internal_Internal_Entity_Iterator'Access;

   function Next
     (T       : Internal_Entity_Iterator_Access;
      Element : out Internal_Entity) return Boolean;

   procedure Inc_Ref (T : Internal_Entity_Iterator_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Iterator_Access);

      function Trace_Image (A : Internal_Entity_Iterator_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Internal_Entity_Iterator, Internal_Entity_Iterator_Access);


   ------------------------
   -- Named environments --
   ------------------------

   --  The goal of named environments is to provide a sound mechanism to
   --  associate nodes and environments across analysis units: nodes whose
   --  Self_Env comes from another unit ("foreign env"), environments whose
   --  parent comes from another unit (also foreign env), or that contain
   --  symbol/node mappings for nodes coming from other units ("foreign
   --  nodes").
   --
   --  This mechanism comes with the following requirements:
   --
   --  * Ensure that, after unit reparsing, all cross-unit associations are
   --    still valid. For instance, no node's Self_Env can refer to a lexical
   --    environment that has been deallocated.
   --
   --  * Ensure that regardless of the sequence of unit parsing/reparsing that
   --    led to a given set of units (considering only unit filename and source
   --    buffer), the node/env graph (i.e. the result of PLE) is always the
   --    same, i.e. make incremental PLE idempotent.
   --
   --  Note that even though the end goal for named envs is to replace the
   --  previous mechanism (proved to be unsound, as violating the second
   --  requirement), both still coexist during the transition period.
   --
   --  Here is how this mechanism works:
   --
   --  1. Environments can be assigned zero, one or several names (i.e. one or
   --     several symbols). Name(s) assignment happens at environment
   --     construction.
   --
   --  2. As a consequence, multiple environments can be associated to a given
   --     env name. Using a total and deterministic ordering predicate, only
   --     one of them is said to have "precedence": looking up an environment
   --     using that name will return this unique environment.
   --
   --  3. For a given env name, we keep track of all uses of the environment
   --     that is looked up by its name: environment parent link, symbol/node
   --     mapping addition, node's Self_Env assignment. This info is
   --     tracked using the Named_Env_Descriptor record type below, often
   --     abbreviated NED. Note that this tracking happens even when there is
   --     no environment associated to the env name, as we need to do such
   --     updates when an environment gets associated to that env name.
   --
   --  4. Unit reparsing can destroy existing environments and/or create new
   --     ones. This means that, depending on their "ranking" using the
   --     ordering predicate, environments can earn or lose precedence for a
   --     given name.
   --
   --  5. When the precedence changes for a given name, we use the info
   --     collected as per 3. to perform relocation: relevant environment
   --     parent links are updated, symbol/node mappings are removed from the
   --     env that lost precedence and added to the env that earned precedence,
   --     etc.

   --  Tables to populate lexical entries in named envs

   package NED_Assoc_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Internal_Map_Node_Vectors.Vector,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => Internal_Map_Node_Vectors."=");
   --  Symbol/lexical env entry mappings for a given named env descriptor.
   --  Symbols are not unique in all mappings, so the lexical env entries are
   --  stored in a vector.

   procedure Add
     (Self : in out NED_Assoc_Maps.Map;
      Key  : Symbol_Type;
      Node : AST_Envs.Internal_Map_Node);
   --  Add a symbol/lexical env entry mapping in Self

   procedure Remove
     (Self : in out NED_Assoc_Maps.Map;
      Key  : Symbol_Type;
      Node : Bare_Gpr_Node);
   --  Remove a symbol/lexical env entry mapping from Self

   --  Global table for named environments

   package Sorted_Env_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Bare_Gpr_Node,
      Element_Type => Lexical_Env);
   --  List of lexical environments, sorted by owning node. This means that the
   --  following must be true for all cursors in such maps::
   --
   --     Key (Cur) = Element (Cur).Env.Node

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Bare_Gpr_Node,
      Hash                => Hash,
      Equivalent_Elements => "=");

   type Named_Env_Descriptor is record
      Name : Symbol_Type;
      --  Name corresponding to this descriptor. Useful during debugging.

      Envs : Sorted_Env_Maps.Map;
      --  For each env name, we can have one or several environments
      --  (concurrent definitions). Just like foreign nodes in lexical
      --  environments, we keep them sorted by node to preserve determinism:
      --  given a set of loaded units, we will always have the same set of
      --  name:env associations sorted in the same order and thus always the
      --  same results at lookup time.

      Env_With_Precedence : Lexical_Env;
      --  Named environment that has precedence for this name.
      --
      --  Most of the time, if Envs is empty, this is Empty_Env and otherwise,
      --  shortcut to Envs.First_Element. However, when a change in Envs
      --  invalidates Env_With_Precedence, we reset it to Empty_Env momentarily
      --  during PLE as a way to tag the temprorary inconsistency. Later on, we
      --  recompute it and perform the needed relocations.

      Foreign_Nodes : NED_Assoc_Maps.Map;
      --  This maps symbols to lists of env entries for all the foreign nodes
      --  in Env_With_Precedence.
      --
      --  This set allows efficient relocation of env entries when
      --  Env_With_Precedence changes.

      Foreign_Envs : Sorted_Env_Maps.Map;
      --  This maps the owning node to env mapping for all lexical environments
      --  whose parent must be Env_With_Precedence. Envs are indexed by owning
      --  node for quick lookup during updates.
      --
      --  This set allows efficient env parent link updates when
      --  Env_With_Precedence changes.

      Nodes_With_Foreign_Env : Node_Sets.Set;
      --  Set of nodes whose env (Self_Env) must be Env_With_Precedence.
      --
      --  This set allows efficient Self_Env updates when Env_With_Precedence
      --  changes.

      --  Note that during the updating process of a reparsed unit
      --  (Update_After_Reparse procedure), these data structures become
      --  temporarily inconsistent: Env_With_Precedence can become Empty_Env
      --  even though Envs is not empty.  This is fine, because when it does,
      --  Update_After_Reparse keeps track of it as to be updated
      --  (Named_Envs_Needing_Update map).
   end record;
   type Named_Env_Descriptor_Access is access Named_Env_Descriptor;
   procedure Destroy is new Ada.Unchecked_Deallocation
     (Named_Env_Descriptor, Named_Env_Descriptor_Access);

   package NED_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Named_Env_Descriptor_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");
   --  Context-wide table that tracks for all env names the set of lexical envs
   --  that define it.

   type Exiled_Entry_In_NED is record
      Named_Env : Named_Env_Descriptor_Access;
      --  Named env descriptor in which Node is registered

      Key : Symbol_Type;
      --  Key in that Env's internal map that leads to the env descriptor that
      --  contains Node.

      Node : Bare_Gpr_Node;
      --  Exiled node
   end record;

   package Exiled_Entry_In_NED_Vectors is new
      Gpr_Parser_Support.Vectors (Exiled_Entry_In_NED);

   type Exiled_Env is record
      Named_Env : Named_Env_Descriptor_Access;
      --  Named env descriptor in which Env is registered

      Env : Lexical_Env;
      --  Exiled environment
   end record;

   package Exiled_Env_Vectors is new Gpr_Parser_Support.Vectors (Exiled_Env);

   type Named_Env_Pair is record
      Name : Symbol_Type;
      --  Name on the lexical environment

      Env  : Lexical_Env;
      --  Named lexical environment
   end record;

   package Named_Env_Vectors is new Gpr_Parser_Support.Vectors (Named_Env_Pair);

   --  High-level primitives to handle the life cycle of named environment

   function Get_Named_Env_Descriptor
     (Context : Internal_Context;
      Name    : Symbol_Type) return Named_Env_Descriptor_Access;
   --  Return the named env descriptor in Context corresponding to Name. Create
   --  it first, if needed.

   procedure Register_Named_Env
     (Context                   : Internal_Context;
      Name                      : Symbol_Type;
      Env                       : Lexical_Env;
      Named_Envs_Needing_Update : in out NED_Maps.Map);
   --  Register Name as the environment name for Env. If Env takes the
   --  precedence for this name, add Name/its named env descriptor to
   --  Named_Envs_Needing_Update.

   procedure Update_Named_Envs
     (Context : Internal_Context; Named_Envs : NED_Maps.Map);
   --  For each named environment in Named_Envs, update Env_With_Precedence and
   --  do the necessary adjustments: relocate exiled entries, etc.

   -------------------------------
   -- Tree traversal operations --
   -------------------------------

   Kind_To_Node_Children_Count : constant array (Gpr_Node_Kind_Type) of Integer :=
     (Gpr_Ada_Access_Subp => 2, 
Gpr_Ada_Pragma => 1, 
Gpr_Ada_Use => 1, 
Gpr_Ada_With => 3, 
Gpr_Ada_Entity_Kind_Function => 0, 
Gpr_Ada_Entity_Kind_Package => 0, 
Gpr_Ada_Entity_Kind_Procedure => 0, 
Gpr_Ada_Generic => 1, 
Gpr_Ada_Library_Item => 3, 
Gpr_Ada_Pkg => 2, 
Gpr_Ada_Pkg_Body => 1, 
Gpr_Ada_Subp => 2, 
Gpr_Ada_Prelude => 2, 
Gpr_Ada_Separate => 1, 
Gpr_Ada_Skip => 0, 
Gpr_Ada_With_Formal => 2, 
Gpr_All_Qualifier_Absent => 0, 
Gpr_All_Qualifier_Present => 0, 
Gpr_Attribute_Decl => 3, 
Gpr_Attribute_Reference => 2, 
Gpr_Ada_Context_Clause_List => -1, 
Gpr_Ada_Prelude_Node_List => -1, 
Gpr_Ada_Skip_List => -1, 
Gpr_Case_Item_List => -1, 
Gpr_Expr_List => -1, 
Gpr_Gpr_Node_List => -1, 
Gpr_Choices => -1, 
Gpr_Term_List => -1, 
Gpr_Identifier_List => -1, 
Gpr_String_Literal_List => -1, 
Gpr_Term_List_List => -1, 
Gpr_With_Decl_List => -1, 
Gpr_Builtin_Function_Call => 2, 
Gpr_Case_Construction => 2, 
Gpr_Case_Item => 2, 
Gpr_Compilation_Unit => 1, 
Gpr_Empty_Decl => 0, 
Gpr_Prefix => 2, 
Gpr_Identifier => 0, 
Gpr_Num_Literal => 0, 
Gpr_String_Literal => 0, 
Gpr_Limited_Absent => 0, 
Gpr_Limited_Present => 0, 
Gpr_Others_Designator => 0, 
Gpr_Package_Decl => 2, 
Gpr_Package_Extension => 1, 
Gpr_Package_Renaming => 1, 
Gpr_Package_Spec => 3, 
Gpr_Private_Absent => 0, 
Gpr_Private_Present => 0, 
Gpr_Project => 2, 
Gpr_Project_Declaration => 5, 
Gpr_Project_Extension => 2, 
Gpr_Project_Qualifier_Abstract => 0, 
Gpr_Project_Qualifier_Aggregate => 0, 
Gpr_Project_Qualifier_Aggregate_Library => 0, 
Gpr_Project_Qualifier_Configuration => 0, 
Gpr_Project_Qualifier_Library => 0, 
Gpr_Project_Qualifier_Standard => 0, 
Gpr_Project_Reference => 1, 
Gpr_String_Literal_At => 2, 
Gpr_Terms => 1, 
Gpr_Type_Reference => 1, 
Gpr_Typed_String_Decl => 2, 
Gpr_Variable_Decl => 3, 
Gpr_Variable_Reference => 2, 
Gpr_With_Decl => 2);
   --  For each AST node kind, this array gives the number of AST node children
   --  it has. For AST node lists, this is -1 as this number varies from one
   --  list instance to another.

   function First_Child_Index (Node : Bare_Gpr_Node) return Natural;
   --  Return the index of the first child Node has

   function Last_Child_Index (Node : Bare_Gpr_Node) return Natural;
   --  Return the index of the last child Node has, or 0 if there is no child

   function Children_Count (Node : Bare_Gpr_Node) return Natural;
   --  Return the number of children that Node has

   procedure Get_Child
     (Node            : Bare_Gpr_Node;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Bare_Gpr_Node);
   --  Return the Index'th child of node, storing it into Result.
   --
   --  Child indexing is 1-based. Store in Index_In_Bounds whether Node had
   --  such a child: if not (i.e. ``Index`` is out-of-bounds), set ``Result``
   --  to a null node.

   function Child
     (Node  : Bare_Gpr_Node;
      Index : Positive) return Bare_Gpr_Node;
   --  Return the Index'th child of Node, or null if Node has no such child

   function Children
     (Node : Bare_Gpr_Node) return Internal_Bare_Gpr_Node_Array;
   --  Return an array containing all the children of Node.
   --  This is an alternative to the Child/Children_Count pair, useful if you
   --  want the convenience of Ada arrays, and you don't care about the small
   --  performance hit of creating an array.

   function Parents
     (Node      : Bare_Gpr_Node;
      With_Self : Boolean := True)
      return Bare_Gpr_Node_Array_Access;
   --  Return an array that contains the lexical parents, this node included
   --  iff ``with_self`` is True. Nearer parents are first in the list.

   function Parent (Node : Bare_Gpr_Node) return Bare_Gpr_Node;

   function Fetch_Sibling
     (Node   : Bare_Gpr_Node;
      Offset : Integer) return Bare_Gpr_Node;
   function Fetch_Sibling
     (Node   : Bare_Gpr_Node;
      E_Info : Internal_Entity_Info;
      Offset : Integer) return Internal_Entity;
   --  Assuming Node is the Nth child of its parent, return the (N + Offset)'th
   --  child of the same parent, or null/No_Entity if there is no such sibling.

   function Traverse
     (Node  : Bare_Gpr_Node;
      Visit : access function (Node : Bare_Gpr_Node) return Visit_Status)
      return Visit_Status;
   --  Given the parent node for a subtree, traverse all syntactic nodes of
   --  this tree, calling the given function on each node in prefix order (i.e.
   --  top-down). The order of traversing subtrees follows the order of
   --  declaration of the corresponding attributes in the grammar. The
   --  traversal is controlled as follows by the result returned by Visit:
   --
   --     Into   The traversal continues normally with the syntactic
   --            children of the node just processed.
   --
   --     Over   The children of the node just processed are skipped and
   --            excluded from the traversal, but otherwise processing
   --            continues elsewhere in the tree.
   --
   --     Stop   The entire traversal is immediately abandoned, and the
   --            original call to Traverse returns Stop.

   procedure Traverse
     (Node  : Bare_Gpr_Node;
      Visit : access function (Node : Bare_Gpr_Node)
                               return Visit_Status);
   --  This is the same as Traverse function except that no result is returned
   --  i.e. the Traverse function is called and the result is simply discarded.

   generic
      type Data_Type is private;
      Reset_After_Traversal : Boolean := False;
   function Traverse_With_Data
     (Node  : Bare_Gpr_Node;
      Visit : access function (Node : Bare_Gpr_Node;
                               Data : in out Data_Type)
                               return Visit_Status;
      Data  : in out Data_Type)
      return Visit_Status;
   --  This is the same as the first Traverse function except it accepts an
   --  argument that is passed to all Visit calls.
   --
   --  If Reset_After_Traversal is True, the Data formal is left unchanged when
   --  Traverse_With_Data returns no matter what Visit does. Visit can change
   --  it otherwise.

   ----------------------------------------
   -- Source location-related operations --
   ----------------------------------------

   function Sloc_Range
     (Node : Bare_Gpr_Node) return Source_Location_Range;
   --  Return the source location range corresponding to the set of tokens from
   --  which Node was parsed.

   function Compare
     (Node : Bare_Gpr_Node;
      Sloc : Source_Location) return Relative_Position;
   --  Compare Sloc to the sloc range of Node

   function Lookup
     (Node : Bare_Gpr_Node;
      Sloc : Source_Location) return Bare_Gpr_Node;
   --  Look for the bottom-most AST node whose sloc range contains Sloc. Return
   --  it, or null if no such node was found.

   function Compare
     (Self, Left, Right : Bare_Gpr_Node;
      Relation          : Comparison_Relation) return Boolean;
   --  If ``Left`` and ``Right`` don't belong to the same analysis units or if
   --  one of them is null, raise a ``Property_Error`` (use ``Self`` to provide
   --  error context). Otherwise, return the comparison of their starting
   --  source location according to Relation.

   -------------------
   -- Debug helpers --
   -------------------

   function Image (Value : Boolean) return String;
   --  Image for a Boolean, for debugging/logging purposes

   procedure Print
     (Node        : Bare_Gpr_Node;
      Show_Slocs  : Boolean;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children.
   --  Line_Prefix is prepended to each output line.

   procedure PP_Trivia
     (Node        : Bare_Gpr_Node;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children along
   --  with the trivia associated to them. Line_Prefix is prepended to each
   --  output line.

   procedure Assign_Names_To_Logic_Vars (Node : Bare_Gpr_Node);
   --  Debug helper: Assign names to every logical variable in the root node,
   --  so that we can trace logical variables.

   -------------------------------
   -- Root AST node (internals) --
   -------------------------------

   type Root_Node_Record (Kind : Gpr_Node_Kind_Type) is record
      Parent : Bare_Gpr_Node;
      --  Reference to the parent node, or null if this is the root one

      Unit : Internal_Unit;
      --  Reference to the analysis unit that owns this node

      Token_Start_Index : Token_Index;
      Token_End_Index   : Token_Index;
      --  Reference to the start and end token that constitutes this node. If
      --  this node is a ghost, Token_Start_Index is the token that this AST
      --  node relates to and Token_End_Index is No_Token_Index. Otherwise,
      --  both tokens are inclusive, i.e. they both belong to this node.

      Self_Env : Lexical_Env;
      --  Hold the environment this node defines, or the parent environment
      --  otherwise.

      Last_Attempted_Child : Integer;
      --  0-based index for the last child we tried to parse for this node. -1
      --  if parsing for all children was successful.

      

      
         



         


            case Kind is
                  when Gpr_Ada_Prelude_Node =>
                     
         



         


            case Kind is
                  when Gpr_Ada_Access_Subp_Range =>
                     
         


            Ada_Access_Subp_F_Subp_Kind : aliased Bare_Ada_Entity_Kind :=
               No_Bare_Gpr_Node;
            Ada_Access_Subp_F_Skips : aliased Bare_Ada_Skip_List :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Ada_Context_Clause =>
                     
         



         


            case Kind is
                  when Gpr_Ada_Pragma_Range =>
                     
         


            Ada_Pragma_F_Skips : aliased Bare_Ada_Skip_List :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Ada_Use_Range =>
                     
         


            Ada_Use_F_Skips : aliased Bare_Ada_Skip_List :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Ada_With_Range =>
                     
         


            Ada_With_F_Has_Limited : aliased Bare_Limited_Node :=
               No_Bare_Gpr_Node;
            Ada_With_F_Has_Private : aliased Bare_Private_Node :=
               No_Bare_Gpr_Node;
            Ada_With_F_Packages : aliased Bare_Expr_List :=
               No_Bare_Gpr_Node;

         



      
               when others => null;
            end case;

      
                  when Gpr_Ada_Entity_Kind =>
                     
         



         


            case Kind is
                  when Gpr_Ada_Entity_Kind_Function_Range =>
                     
         



         



            null;
      
                  when Gpr_Ada_Entity_Kind_Package_Range =>
                     
         



         



            null;
      
                  when Gpr_Ada_Entity_Kind_Procedure_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Gpr_Ada_Generic_Range =>
                     
         


            Ada_Generic_F_Skips : aliased Bare_Gpr_Node :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Ada_Library_Item_Range =>
                     
         


            Ada_Library_Item_F_Generic_Stub : aliased Bare_Ada_Generic :=
               No_Bare_Gpr_Node;
            Ada_Library_Item_F_Separate : aliased Bare_Ada_Separate :=
               No_Bare_Gpr_Node;
            Ada_Library_Item_F_Main : aliased Bare_Ada_Main :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Ada_Main =>
                     
         



         


            case Kind is
                  when Gpr_Ada_Pkg_Range =>
                     
         


            Ada_Pkg_F_Has_Private : aliased Bare_Private_Node :=
               No_Bare_Gpr_Node;
            Ada_Pkg_F_Name : aliased Bare_Expr :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Ada_Pkg_Body_Range =>
                     
         


            Ada_Pkg_Body_F_Name : aliased Bare_Expr :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Ada_Subp_Range =>
                     
         


            Ada_Subp_F_Subp_Kind : aliased Bare_Ada_Entity_Kind :=
               No_Bare_Gpr_Node;
            Ada_Subp_F_Name : aliased Bare_Expr :=
               No_Bare_Gpr_Node;

         



      
               when others => null;
            end case;

      
                  when Gpr_Ada_Prelude_Range =>
                     
         


            Ada_Prelude_F_Context_Clauses : aliased Bare_Ada_Context_Clause_List :=
               No_Bare_Gpr_Node;
            Ada_Prelude_F_Library_Item : aliased Bare_Ada_Library_Item :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Ada_Separate_Range =>
                     
         


            Ada_Separate_F_Parent_Name : aliased Bare_Expr :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Ada_Skip_Range =>
                     
         



         



            null;
      
                  when Gpr_Ada_With_Formal_Range =>
                     
         


            Ada_With_Formal_F_Kind : aliased Bare_Ada_Entity_Kind :=
               No_Bare_Gpr_Node;
            Ada_With_Formal_F_Skips : aliased Bare_Ada_Skip_List :=
               No_Bare_Gpr_Node;

         



      
               when others => null;
            end case;

      
                  when Gpr_All_Qualifier =>
                     
         



         


            case Kind is
                  when Gpr_All_Qualifier_Absent_Range =>
                     
         



         



            null;
      
                  when Gpr_All_Qualifier_Present_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Gpr_Attribute_Decl_Range =>
                     
         


            Attribute_Decl_F_Attr_Name : aliased Bare_Identifier :=
               No_Bare_Gpr_Node;
            Attribute_Decl_F_Attr_Index : aliased Bare_Gpr_Node :=
               No_Bare_Gpr_Node;
            Attribute_Decl_F_Expr : aliased Bare_Term_List :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Attribute_Reference_Range =>
                     
         


            Attribute_Reference_F_Attribute_Name : aliased Bare_Identifier :=
               No_Bare_Gpr_Node;
            Attribute_Reference_F_Attribute_Index : aliased Bare_Gpr_Node :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Base_List =>
                     
         

            Count : Natural;
            Nodes : Alloc_AST_List_Array.Element_Array_Access;


         


            case Kind is
                  when Gpr_Ada_Context_Clause_List_Range =>
                     
         



         



            null;
      
                  when Gpr_Ada_Prelude_Node_List_Range =>
                     
         



         



            null;
      
                  when Gpr_Ada_Skip_List_Range =>
                     
         



         



            null;
      
                  when Gpr_Case_Item_List_Range =>
                     
         



         



            null;
      
                  when Gpr_Expr_List_Range =>
                     
         



         



            null;
      
                  when Gpr_Gpr_Node_List_Range =>
                     
         



         


            case Kind is
                  when Gpr_Choices_Range =>
                     
         



         



            null;
      
                  when Gpr_Term_List_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Gpr_Identifier_List_Range =>
                     
         



         



            null;
      
                  when Gpr_String_Literal_List_Range =>
                     
         



         



            null;
      
                  when Gpr_Term_List_List_Range =>
                     
         



         



            null;
      
                  when Gpr_With_Decl_List_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Gpr_Builtin_Function_Call_Range =>
                     
         


            Builtin_Function_Call_F_Function_Name : aliased Bare_Identifier :=
               No_Bare_Gpr_Node;
            Builtin_Function_Call_F_Parameters : aliased Bare_Terms :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Case_Construction_Range =>
                     
         


            Case_Construction_F_Var_Ref : aliased Bare_Variable_Reference :=
               No_Bare_Gpr_Node;
            Case_Construction_F_Items : aliased Bare_Case_Item_List :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Case_Item_Range =>
                     
         


            Case_Item_F_Choice : aliased Bare_Choices :=
               No_Bare_Gpr_Node;
            Case_Item_F_Decls : aliased Bare_Gpr_Node_List :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Compilation_Unit_Range =>
                     
         


            Compilation_Unit_F_Project : aliased Bare_Project :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Empty_Decl_Range =>
                     
         



         



            null;
      
                  when Gpr_Expr =>
                     
         



         


            case Kind is
                  when Gpr_Prefix_Range =>
                     
         


            Prefix_F_Prefix : aliased Bare_Expr :=
               No_Bare_Gpr_Node;
            Prefix_F_Suffix : aliased Bare_Identifier :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Single_Tok_Node =>
                     
         



         


            case Kind is
                  when Gpr_Identifier_Range =>
                     
         



         



            null;
      
                  when Gpr_Num_Literal_Range =>
                     
         



         



            null;
      
                  when Gpr_String_Literal_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
               when others => null;
            end case;

      
                  when Gpr_Limited_Node =>
                     
         



         


            case Kind is
                  when Gpr_Limited_Absent_Range =>
                     
         



         



            null;
      
                  when Gpr_Limited_Present_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Gpr_Others_Designator_Range =>
                     
         



         



            null;
      
                  when Gpr_Package_Decl_Range =>
                     
         


            Package_Decl_F_Pkg_Name : aliased Bare_Identifier :=
               No_Bare_Gpr_Node;
            Package_Decl_F_Pkg_Spec : aliased Bare_Gpr_Node :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Package_Extension_Range =>
                     
         


            Package_Extension_F_Extended_Name : aliased Bare_Identifier_List :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Package_Renaming_Range =>
                     
         


            Package_Renaming_F_Renamed_Name : aliased Bare_Identifier_List :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Package_Spec_Range =>
                     
         


            Package_Spec_F_Extension : aliased Bare_Package_Extension :=
               No_Bare_Gpr_Node;
            Package_Spec_F_Decls : aliased Bare_Gpr_Node_List :=
               No_Bare_Gpr_Node;
            Package_Spec_F_End_Name : aliased Bare_Identifier :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Private_Node =>
                     
         



         


            case Kind is
                  when Gpr_Private_Absent_Range =>
                     
         



         



            null;
      
                  when Gpr_Private_Present_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Gpr_Project_Range =>
                     
         


            Project_F_Context_Clauses : aliased Bare_With_Decl_List :=
               No_Bare_Gpr_Node;
            Project_F_Project_Decl : aliased Bare_Project_Declaration :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Project_Declaration_Range =>
                     
         


            Project_Declaration_F_Qualifier : aliased Bare_Project_Qualifier :=
               No_Bare_Gpr_Node;
            Project_Declaration_F_Project_Name : aliased Bare_Expr :=
               No_Bare_Gpr_Node;
            Project_Declaration_F_Extension : aliased Bare_Project_Extension :=
               No_Bare_Gpr_Node;
            Project_Declaration_F_Decls : aliased Bare_Gpr_Node_List :=
               No_Bare_Gpr_Node;
            Project_Declaration_F_End_Name : aliased Bare_Expr :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Project_Extension_Range =>
                     
         


            Project_Extension_F_Is_All : aliased Bare_All_Qualifier :=
               No_Bare_Gpr_Node;
            Project_Extension_F_Path_Name : aliased Bare_String_Literal :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Project_Qualifier =>
                     
         



         


            case Kind is
                  when Gpr_Project_Qualifier_Abstract_Range =>
                     
         



         



            null;
      
                  when Gpr_Project_Qualifier_Aggregate_Range =>
                     
         



         



            null;
      
                  when Gpr_Project_Qualifier_Aggregate_Library_Range =>
                     
         



         



            null;
      
                  when Gpr_Project_Qualifier_Configuration_Range =>
                     
         



         



            null;
      
                  when Gpr_Project_Qualifier_Library_Range =>
                     
         



         



            null;
      
                  when Gpr_Project_Qualifier_Standard_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Gpr_Project_Reference_Range =>
                     
         


            Project_Reference_F_Attr_Ref : aliased Bare_Attribute_Reference :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_String_Literal_At_Range =>
                     
         


            String_Literal_At_F_Str_Lit : aliased Bare_String_Literal :=
               No_Bare_Gpr_Node;
            String_Literal_At_F_At_Lit : aliased Bare_Num_Literal :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Terms_Range =>
                     
         


            Terms_F_Terms : aliased Bare_Term_List_List :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Type_Reference_Range =>
                     
         


            Type_Reference_F_Var_Type_Name : aliased Bare_Identifier_List :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Typed_String_Decl_Range =>
                     
         


            Typed_String_Decl_F_Type_Id : aliased Bare_Identifier :=
               No_Bare_Gpr_Node;
            Typed_String_Decl_F_String_Literals : aliased Bare_String_Literal_List :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Variable_Decl_Range =>
                     
         


            Variable_Decl_F_Var_Name : aliased Bare_Identifier :=
               No_Bare_Gpr_Node;
            Variable_Decl_F_Var_Type : aliased Bare_Type_Reference :=
               No_Bare_Gpr_Node;
            Variable_Decl_F_Expr : aliased Bare_Term_List :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_Variable_Reference_Range =>
                     
         


            Variable_Reference_F_Variable_Name : aliased Bare_Identifier_List :=
               No_Bare_Gpr_Node;
            Variable_Reference_F_Attribute_Ref : aliased Bare_Attribute_Reference :=
               No_Bare_Gpr_Node;

         



      
                  when Gpr_With_Decl_Range =>
                     
         


            With_Decl_F_Is_Limited : aliased Bare_Limited_Node :=
               No_Bare_Gpr_Node;
            With_Decl_F_Path_Names : aliased Bare_String_Literal_List :=
               No_Bare_Gpr_Node;

         



      
               when others => null;
            end case;

      
   end record;

   procedure Initialize
     (Self              : Bare_Gpr_Node;
      Kind              : Gpr_Node_Kind_Type;
      Unit              : Internal_Unit;
      Token_Start_Index : Token_Index;
      Token_End_Index   : Token_Index;
      Parent            : Bare_Gpr_Node := null;
      Self_Env          : Lexical_Env := AST_Envs.Empty_Env);
   --  Helper for parsers, to initialize a freshly allocated node

   type PLE_Unit_State is record
      Named_Envs_Needing_Update : NED_Maps.Map;
      --  Set of named env entries whose Env_With_Precedence needs to be
      --  updated.
   end record;
   --  State of PLE on a specific unit

   type PLE_Unit_State_Access is access all PLE_Unit_State;

   type PLE_Node_State is record
      Unit_State : PLE_Unit_State_Access;
      --  State of PLE on the unit that owns this node

      Current_Env : Lexical_Env;
      --  Current environment when processing the node: initially inheritted
      --  from the Current_Env of the parent node (or Root_Scope on the root
      --  node), SetInitialEnv actions can change this.
      --
      --  Other environment actions such as AddEnv or AddToEnv can use this.

      Current_NED : Named_Env_Descriptor_Access;
      --  If the current environment was looked up by name, reference to the
      --  named environment descriptor. Null otherwise.
   end record;
   --  State of PLE on a specific node

   procedure Use_Direct_Env (State : in out PLE_Node_State; Env : Lexical_Env);
   --  Change State so that the current environment is Env, and record that it
   --  was *not* looked up by name.

   procedure Use_Named_Env
     (State   : in out PLE_Node_State;
      Context : Internal_Context;
      Name    : Symbol_Type);
   --  Change State so that the current environment comes from the named
   --  environment looked up with Name.

   procedure Set_Initial_Env
     (Self         : Bare_Gpr_Node;
      State        : in out PLE_Node_State;
      Env          : Internal_Designated_Env;
      DSL_Location : String);
   --  Helper for ``Populate_Lexical_Env``: fetch the initial environment for
   --  ``Self`` according to ``Env`` and update ``State`` accordingly.

   procedure Add_To_Env
     (Self         : Bare_Gpr_Node;
      State        : PLE_Node_State;
      Key          : Symbol_Type;
      Value        : Bare_Gpr_Node;
      Md           : Internal_Metadata;
      Resolver     : Entity_Resolver;
      Dest_Env     : Internal_Designated_Env;
      DSL_Location : String);
   --  Helper for Populate_Lexical_Env: insert the Key/Value/MD/Resolver entry
   --  in the appropriate lexical env.
   --
   --  The destination environment is:
   --
   --  * If Dest_Env_Name is not null, this is the corresponding named
   --    environment.
   --
   --  * Otherwise, use Dest_Env_Fallback if is not the empty environment.
   --
   --  * Finally, use State's current environment.
   --
   --  If the destination environment is foreign and not fetched from its name
   --  while DSL_Location is not empty, raise a Property_Error.

   procedure Ref_Env
     (Self                : Bare_Gpr_Node;
      Dest_Env            : Lexical_Env;
      Ref_Env_Nodes       : in out Bare_Gpr_Node_Array_Access;
      Resolver            : Lexical_Env_Resolver;
      Kind                : Ref_Kind;
      Cats                : Ref_Categories;
      Shed_Rebindings     : Boolean);
   --  Helper for Populate_Lexical_Env: add referenced environments to
   --  Dest_Env. Calling this takes an ownership share for Ref_Env_Nodes.

   procedure Add_Env
     (Self              : Bare_Gpr_Node;
      State             : in out PLE_Node_State;
      No_Parent         : Boolean;
      Transitive_Parent : Boolean;
      Names             : in out Symbol_Type_Array_Access);
   --  Helper for Populate_Lexical_Env: create a new environment for Self, and
   --  update State accordingly.
   --
   --  State and No_Parent participate to the computation of the parent for
   --  this new environment. Transitive_Parent is directly forwarded to the
   --  lexical environment constructor.
   --
   --  If Names is not null, this also registers the new environment as a named
   --  env for all the given names. For PLE code brevity, Add_Env takes care of
   --  freeing Names before returning.

   procedure Pre_Env_Actions
     (Self            : Bare_Gpr_Node;
      State           : in out PLE_Node_State;
      Add_To_Env_Only : Boolean := False);
   --  Internal procedure that will execute all necessary lexical env actions
   --  for Node. This is meant to be called by Populate_Lexical_Env, and not by
   --  the user.

   procedure Post_Env_Actions
     (Self : Bare_Gpr_Node; State : in out PLE_Node_State);
   --  Internal procedure that will execute all post add to env actions for
   --  Node. This is meant to be called by Populate_Lexical_Env.

   function Get_Symbol (Node : Bare_Gpr_Node) return Symbol_Type
      with Pre => Node = null or else Is_Token_Node (Node);
   --  Assuming Node is a token node, return the corresponding symbol for the
   --  token it contains.

   function Image (Self : Symbol_Type) return String_Type;
   --  Transform a Symbol into an internal String

   function Text (Node : Bare_Gpr_Node) return Text_Type;
   --  Retun the fragment of text from which Node was parsed

   ------------------------------
   -- Root AST node properties --
   ------------------------------


   -----------------------
   -- Generic list type --
   -----------------------


   function Length (Node : Bare_Base_List) return Natural;

   function Children
     (Node : Bare_Gpr_Node) return Bare_Gpr_Node_Array_Access;
   --  Return an array containing all the children of Node.
   --  This is an alternative to the Child/Children_Count pair, useful if you
   --  want the convenience of ada arrays, and you don't care about the small
   --  performance hit of creating an array.

   function Item
     (Node  : Bare_Base_List;
      Index : Positive) return Bare_Gpr_Node renames Child;

   function Get
     (Self    : Bare_Gpr_Node;
      Node    : Bare_Base_List;
      Index   : Integer;
      Or_Null : Boolean := False) return Bare_Gpr_Node;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --
   --  ``Self`` is used to provide context to the ``Property_Error`` that is
   --  raised when the index is invalid.

   procedure Free_User_Fields (Node : Bare_Gpr_Node);
   --  Free resources associated to user fields in ``Node``

   procedure Set_Parents (Node, Parent : Bare_Gpr_Node);
   --  Set Node.Parent to Parent, and initialize recursively the parent of all
   --  child nodes.

   procedure Destroy (Node : Bare_Gpr_Node);
   --  Free the resources allocated to this node and all its children

   --------------------------------------
   -- Environments handling (internal) --
   --------------------------------------

   function Create_Static_Lexical_Env
     (Parent            : Lexical_Env;
      Node              : Bare_Gpr_Node;
      Transitive_Parent : Boolean := False) return Lexical_Env;
   --  Wrapper around AST_Envs.Create_Lexical_Env. Create the environment and,
   --  if Node is not null, register the result for destruction in Node's
   --  analysis unit.

   function Get
     (Self  : Bare_Gpr_Node;
      A     : AST_Envs.Entity_Array;
      Index : Integer) return Internal_Entity;
   --  Simple getter that raises a ``Property_Error`` on out-of-bound accesses
   --  (using ``Self`` to provide context for this error). Useful for code
   --  generation.

   function Group
     (Envs   : Lexical_Env_Array_Access;
      Env_Md : Internal_Metadata := No_Metadata) return Lexical_Env;
   --  Convenience wrapper for uniform types handling in code generation

   package Bare_Gpr_Node_Vectors is
      new Gpr_Parser_Support.Vectors (Bare_Gpr_Node);

   function Is_Visible_From
     (Self                     : Bare_Gpr_Node;
      Referenced_Env, Base_Env : Lexical_Env) return Boolean;
   --  Return whether the unit that ``Referenced_Env`` belongs to is visible
   --  from the unit that Base_Env belongs to. If at least one of these two
   --  lexical environments does not belong to a particular analysis unit, this
   --  raises a ``Property_Error``.
   --
   --  ``Self`` is used to give context to the error in case of failure.

   function Populate_Lexical_Env (Node : Bare_Gpr_Node) return Boolean;
   --  Populate the lexical environment for node and all its children. Return
   --  whether a Property_Error error occurred in the process.

   -----------------------------------
   -- Lexical utilities (internals) --
   -----------------------------------

   function Token
     (Node  : Bare_Gpr_Node;
      Index : Token_Index) return Token_Reference;
   --  Helper for properties. This is used to turn token indexes as stored in
   --  AST nodes into Token_Reference values.

   function Stored_Token
     (Node  : Bare_Gpr_Node;
      Token : Token_Reference) return Token_Index;
   --  Helper for properties. This is used to turn a Token_Reference value into
   --  a Token_Index value that can be stored as a field in Node. This raises a
   --  Property_Error if Node and Token don't belong to the same analysis unit
   --  or if Token is actually a Trivia.

   type Bare_Child_Record (Kind : Child_Or_Trivia := Child) is record
      case Kind is
         when Child =>
            Node : Bare_Gpr_Node;
         when Trivia =>
            Trivia : Token_Reference;
      end case;
   end record;
   --  Variant that holds either an node or a token

   type Bare_Children_Array is array (Positive range <>) of Bare_Child_Record;

   function Children_And_Trivia
     (Node : Bare_Gpr_Node) return Bare_Children_Array;
   --  Implementation for Analysis.Children_And_Trivia

      

   



         



 function Node_Env
   
  (Node : Bare_Gpr_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  For nodes that introduce a new environment, return the parent lexical
--  environment. Return the "inherited" environment otherwise.

         



 function Children_Env
   
  (Node : Bare_Gpr_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  For nodes that introduce a new environment, return it. Return the
--  "inherited" environment otherwise.

         



 function Parent
   
  (Node : Bare_Gpr_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity
   ;
--  Return the syntactic parent for this node. Return null for the root node.

         



 function Parents
   
  (Node : Bare_Gpr_Node
      ; With_Self : Boolean
         := True
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Array_Access
   ;
--  Return an array that contains the lexical parents, this node included iff
--  ``with_self`` is True. Nearer parents are first in the list.

         



 function Children
   
  (Node : Bare_Gpr_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Array_Access
   ;
--  Return an array that contains the direct lexical children.
--
--  .. warning:: This constructs a whole array every-time you call it, and as
--     such is less efficient than calling the ``Child`` built-in.

         



 function Token_Start
   
  (Node : Bare_Gpr_Node
  )

   return Token_Reference
   ;
--  Return the first token used to parse this node.

         



 function Token_End
   
  (Node : Bare_Gpr_Node
  )

   return Token_Reference
   ;
--  Return the last token used to parse this node.

         



 function Child_Index
   
  (Node : Bare_Gpr_Node
  )

   return Integer
   ;
--  Return the 0-based index for Node in its parent's children.

         



 function Previous_Sibling
   
  (Node : Bare_Gpr_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity
   ;
--  Return the node's previous sibling, or null if there is no such sibling.

         



 function Next_Sibling
   
  (Node : Bare_Gpr_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity
   ;
--  Return the node's next sibling, or null if there is no such sibling.

         



 function Unit
   
  (Node : Bare_Gpr_Node
  )

   return Internal_Unit
   ;
--  Return the analysis unit owning this node.

         



 function Is_Ghost
   
  (Node : Bare_Gpr_Node
  )

   return Boolean
   ;
--  Return whether the node is a ghost.
--
--  Unlike regular nodes, ghost nodes cover no token in the input source: they
--  are logically located instead between two tokens. Both the ``token_start``
--  and the ``token_end`` of all ghost nodes is the token right after this
--  logical position.

         



 function Text
   
  (Node : Bare_Gpr_Node
  )

   return String_Type
   ;
--  Return the text corresponding to this node. Private property (for internal
--  DSL use).

         



 function Full_Sloc_Image
   
  (Node : Bare_Gpr_Node
  )

   return String_Type
   ;
--  Return a string containing the filename + the sloc in GNU conformant
--  format. Useful to create diagnostics from a node.


   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Ada_Access_Subp
        (Self : Bare_Ada_Access_Subp
         ; Ada_Access_Subp_F_Subp_Kind : Bare_Ada_Entity_Kind
         ; Ada_Access_Subp_F_Skips : Bare_Ada_Skip_List
        );

      
   function Ada_Access_Subp_F_Subp_Kind
     (Node : Bare_Ada_Access_Subp) return Bare_Ada_Entity_Kind;

      
   function Ada_Access_Subp_F_Skips
     (Node : Bare_Ada_Access_Subp) return Bare_Ada_Skip_List;



   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Ada_Pragma
        (Self : Bare_Ada_Pragma
         ; Ada_Pragma_F_Skips : Bare_Ada_Skip_List
        );

      
   function Ada_Pragma_F_Skips
     (Node : Bare_Ada_Pragma) return Bare_Ada_Skip_List;



   




      

   

      
      procedure Initialize_Fields_For_Ada_Use
        (Self : Bare_Ada_Use
         ; Ada_Use_F_Skips : Bare_Ada_Skip_List
        );

      
   function Ada_Use_F_Skips
     (Node : Bare_Ada_Use) return Bare_Ada_Skip_List;



   




      

   

      
      procedure Initialize_Fields_For_Ada_With
        (Self : Bare_Ada_With
         ; Ada_With_F_Has_Limited : Bare_Limited_Node
         ; Ada_With_F_Has_Private : Bare_Private_Node
         ; Ada_With_F_Packages : Bare_Expr_List
        );

      
   function Ada_With_F_Has_Limited
     (Node : Bare_Ada_With) return Bare_Limited_Node;

      
   function Ada_With_F_Has_Private
     (Node : Bare_Ada_With) return Bare_Private_Node;

      
   function Ada_With_F_Packages
     (Node : Bare_Ada_With) return Bare_Expr_List;



   




      

   




   




      

   




   




      

   




   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Ada_Generic
        (Self : Bare_Ada_Generic
         ; Ada_Generic_F_Skips : Bare_Gpr_Node
        );

      
   function Ada_Generic_F_Skips
     (Node : Bare_Ada_Generic) return Bare_Gpr_Node;



   




      

   

      
      procedure Initialize_Fields_For_Ada_Library_Item
        (Self : Bare_Ada_Library_Item
         ; Ada_Library_Item_F_Generic_Stub : Bare_Ada_Generic
         ; Ada_Library_Item_F_Separate : Bare_Ada_Separate
         ; Ada_Library_Item_F_Main : Bare_Ada_Main
        );

      
   function Ada_Library_Item_F_Generic_Stub
     (Node : Bare_Ada_Library_Item) return Bare_Ada_Generic;

      
   function Ada_Library_Item_F_Separate
     (Node : Bare_Ada_Library_Item) return Bare_Ada_Separate;

      
   function Ada_Library_Item_F_Main
     (Node : Bare_Ada_Library_Item) return Bare_Ada_Main;



   




      

   


      
   function Ada_Main_F_Name
     (Node : Bare_Ada_Main) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Ada_Pkg
        (Self : Bare_Ada_Pkg
         ; Ada_Pkg_F_Has_Private : Bare_Private_Node
         ; Ada_Pkg_F_Name : Bare_Expr
        );

      
   function Ada_Pkg_F_Has_Private
     (Node : Bare_Ada_Pkg) return Bare_Private_Node;



   




      

   

      
      procedure Initialize_Fields_For_Ada_Pkg_Body
        (Self : Bare_Ada_Pkg_Body
         ; Ada_Pkg_Body_F_Name : Bare_Expr
        );



   




      

   

      
      procedure Initialize_Fields_For_Ada_Subp
        (Self : Bare_Ada_Subp
         ; Ada_Subp_F_Subp_Kind : Bare_Ada_Entity_Kind
         ; Ada_Subp_F_Name : Bare_Expr
        );

      
   function Ada_Subp_F_Subp_Kind
     (Node : Bare_Ada_Subp) return Bare_Ada_Entity_Kind;



   




      

   

      
      procedure Initialize_Fields_For_Ada_Prelude
        (Self : Bare_Ada_Prelude
         ; Ada_Prelude_F_Context_Clauses : Bare_Ada_Context_Clause_List
         ; Ada_Prelude_F_Library_Item : Bare_Ada_Library_Item
        );

      
   function Ada_Prelude_F_Context_Clauses
     (Node : Bare_Ada_Prelude) return Bare_Ada_Context_Clause_List;

      
   function Ada_Prelude_F_Library_Item
     (Node : Bare_Ada_Prelude) return Bare_Ada_Library_Item;



   




      

   

      
      procedure Initialize_Fields_For_Ada_Separate
        (Self : Bare_Ada_Separate
         ; Ada_Separate_F_Parent_Name : Bare_Expr
        );

      
   function Ada_Separate_F_Parent_Name
     (Node : Bare_Ada_Separate) return Bare_Expr;



   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Ada_With_Formal
        (Self : Bare_Ada_With_Formal
         ; Ada_With_Formal_F_Kind : Bare_Ada_Entity_Kind
         ; Ada_With_Formal_F_Skips : Bare_Ada_Skip_List
        );

      
   function Ada_With_Formal_F_Kind
     (Node : Bare_Ada_With_Formal) return Bare_Ada_Entity_Kind;

      
   function Ada_With_Formal_F_Skips
     (Node : Bare_Ada_With_Formal) return Bare_Ada_Skip_List;



   




      

   



         



 function Dispatcher_All_Qualifier_P_As_Bool
   
  (Node : Bare_All_Qualifier
  )

   return Boolean
   with Inline_Always
   ;
--  Return whether this is an instance of AllQualifierPresent


   




      

   



         



 function All_Qualifier_Absent_P_As_Bool
   
  (Node : Bare_All_Qualifier_Absent
  )

   return Boolean
   ;



   




      

   



         



 function All_Qualifier_Present_P_As_Bool
   
  (Node : Bare_All_Qualifier_Present
  )

   return Boolean
   ;



   




      

   

      
      procedure Initialize_Fields_For_Attribute_Decl
        (Self : Bare_Attribute_Decl
         ; Attribute_Decl_F_Attr_Name : Bare_Identifier
         ; Attribute_Decl_F_Attr_Index : Bare_Gpr_Node
         ; Attribute_Decl_F_Expr : Bare_Term_List
        );

      
   function Attribute_Decl_F_Attr_Name
     (Node : Bare_Attribute_Decl) return Bare_Identifier;

      
   function Attribute_Decl_F_Attr_Index
     (Node : Bare_Attribute_Decl) return Bare_Gpr_Node;

      
   function Attribute_Decl_F_Expr
     (Node : Bare_Attribute_Decl) return Bare_Term_List;



   




      

   

      
      procedure Initialize_Fields_For_Attribute_Reference
        (Self : Bare_Attribute_Reference
         ; Attribute_Reference_F_Attribute_Name : Bare_Identifier
         ; Attribute_Reference_F_Attribute_Index : Bare_Gpr_Node
        );

      
   function Attribute_Reference_F_Attribute_Name
     (Node : Bare_Attribute_Reference) return Bare_Identifier;

      
   function Attribute_Reference_F_Attribute_Index
     (Node : Bare_Attribute_Reference) return Bare_Gpr_Node;



   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Builtin_Function_Call
        (Self : Bare_Builtin_Function_Call
         ; Builtin_Function_Call_F_Function_Name : Bare_Identifier
         ; Builtin_Function_Call_F_Parameters : Bare_Terms
        );

      
   function Builtin_Function_Call_F_Function_Name
     (Node : Bare_Builtin_Function_Call) return Bare_Identifier;

      
   function Builtin_Function_Call_F_Parameters
     (Node : Bare_Builtin_Function_Call) return Bare_Terms;



   




      

   

      
      procedure Initialize_Fields_For_Case_Construction
        (Self : Bare_Case_Construction
         ; Case_Construction_F_Var_Ref : Bare_Variable_Reference
         ; Case_Construction_F_Items : Bare_Case_Item_List
        );

      
   function Case_Construction_F_Var_Ref
     (Node : Bare_Case_Construction) return Bare_Variable_Reference;

      
   function Case_Construction_F_Items
     (Node : Bare_Case_Construction) return Bare_Case_Item_List;



   




      

   

      
      procedure Initialize_Fields_For_Case_Item
        (Self : Bare_Case_Item
         ; Case_Item_F_Choice : Bare_Choices
         ; Case_Item_F_Decls : Bare_Gpr_Node_List
        );

      
   function Case_Item_F_Choice
     (Node : Bare_Case_Item) return Bare_Choices;

      
   function Case_Item_F_Decls
     (Node : Bare_Case_Item) return Bare_Gpr_Node_List;



   




      

   

      
      procedure Initialize_Fields_For_Compilation_Unit
        (Self : Bare_Compilation_Unit
         ; Compilation_Unit_F_Project : Bare_Project
        );

      
   function Compilation_Unit_F_Project
     (Node : Bare_Compilation_Unit) return Bare_Project;



   




      

   




   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Prefix
        (Self : Bare_Prefix
         ; Prefix_F_Prefix : Bare_Expr
         ; Prefix_F_Suffix : Bare_Identifier
        );

      
   function Prefix_F_Prefix
     (Node : Bare_Prefix) return Bare_Expr;

      
   function Prefix_F_Suffix
     (Node : Bare_Prefix) return Bare_Identifier;



   




      

   




   




      

   




   




      

   




   




      

   




   




      

   



         



 function Dispatcher_Limited_Node_P_As_Bool
   
  (Node : Bare_Limited_Node
  )

   return Boolean
   with Inline_Always
   ;
--  Return whether this is an instance of LimitedPresent


   




      

   



         



 function Limited_Absent_P_As_Bool
   
  (Node : Bare_Limited_Absent
  )

   return Boolean
   ;



   




      

   



         



 function Limited_Present_P_As_Bool
   
  (Node : Bare_Limited_Present
  )

   return Boolean
   ;



   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Package_Decl
        (Self : Bare_Package_Decl
         ; Package_Decl_F_Pkg_Name : Bare_Identifier
         ; Package_Decl_F_Pkg_Spec : Bare_Gpr_Node
        );

      
   function Package_Decl_F_Pkg_Name
     (Node : Bare_Package_Decl) return Bare_Identifier;

      
   function Package_Decl_F_Pkg_Spec
     (Node : Bare_Package_Decl) return Bare_Gpr_Node;



   




      

   

      
      procedure Initialize_Fields_For_Package_Extension
        (Self : Bare_Package_Extension
         ; Package_Extension_F_Extended_Name : Bare_Identifier_List
        );

      
   function Package_Extension_F_Extended_Name
     (Node : Bare_Package_Extension) return Bare_Identifier_List;



   




      

   

      
      procedure Initialize_Fields_For_Package_Renaming
        (Self : Bare_Package_Renaming
         ; Package_Renaming_F_Renamed_Name : Bare_Identifier_List
        );

      
   function Package_Renaming_F_Renamed_Name
     (Node : Bare_Package_Renaming) return Bare_Identifier_List;



   




      

   

      
      procedure Initialize_Fields_For_Package_Spec
        (Self : Bare_Package_Spec
         ; Package_Spec_F_Extension : Bare_Package_Extension
         ; Package_Spec_F_Decls : Bare_Gpr_Node_List
         ; Package_Spec_F_End_Name : Bare_Identifier
        );

      
   function Package_Spec_F_Extension
     (Node : Bare_Package_Spec) return Bare_Package_Extension;

      
   function Package_Spec_F_Decls
     (Node : Bare_Package_Spec) return Bare_Gpr_Node_List;

      
   function Package_Spec_F_End_Name
     (Node : Bare_Package_Spec) return Bare_Identifier;



   




      

   



         



 function Dispatcher_Private_Node_P_As_Bool
   
  (Node : Bare_Private_Node
  )

   return Boolean
   with Inline_Always
   ;
--  Return whether this is an instance of PrivatePresent


   




      

   



         



 function Private_Absent_P_As_Bool
   
  (Node : Bare_Private_Absent
  )

   return Boolean
   ;



   




      

   



         



 function Private_Present_P_As_Bool
   
  (Node : Bare_Private_Present
  )

   return Boolean
   ;



   




      

   

      
      procedure Initialize_Fields_For_Project
        (Self : Bare_Project
         ; Project_F_Context_Clauses : Bare_With_Decl_List
         ; Project_F_Project_Decl : Bare_Project_Declaration
        );

      
   function Project_F_Context_Clauses
     (Node : Bare_Project) return Bare_With_Decl_List;

      
   function Project_F_Project_Decl
     (Node : Bare_Project) return Bare_Project_Declaration;



   




      

   

      
      procedure Initialize_Fields_For_Project_Declaration
        (Self : Bare_Project_Declaration
         ; Project_Declaration_F_Qualifier : Bare_Project_Qualifier
         ; Project_Declaration_F_Project_Name : Bare_Expr
         ; Project_Declaration_F_Extension : Bare_Project_Extension
         ; Project_Declaration_F_Decls : Bare_Gpr_Node_List
         ; Project_Declaration_F_End_Name : Bare_Expr
        );

      
   function Project_Declaration_F_Qualifier
     (Node : Bare_Project_Declaration) return Bare_Project_Qualifier;

      
   function Project_Declaration_F_Project_Name
     (Node : Bare_Project_Declaration) return Bare_Expr;

      
   function Project_Declaration_F_Extension
     (Node : Bare_Project_Declaration) return Bare_Project_Extension;

      
   function Project_Declaration_F_Decls
     (Node : Bare_Project_Declaration) return Bare_Gpr_Node_List;

      
   function Project_Declaration_F_End_Name
     (Node : Bare_Project_Declaration) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Project_Extension
        (Self : Bare_Project_Extension
         ; Project_Extension_F_Is_All : Bare_All_Qualifier
         ; Project_Extension_F_Path_Name : Bare_String_Literal
        );

      
   function Project_Extension_F_Is_All
     (Node : Bare_Project_Extension) return Bare_All_Qualifier;

      
   function Project_Extension_F_Path_Name
     (Node : Bare_Project_Extension) return Bare_String_Literal;



   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Project_Reference
        (Self : Bare_Project_Reference
         ; Project_Reference_F_Attr_Ref : Bare_Attribute_Reference
        );

      
   function Project_Reference_F_Attr_Ref
     (Node : Bare_Project_Reference) return Bare_Attribute_Reference;



   




      

   

      
      procedure Initialize_Fields_For_String_Literal_At
        (Self : Bare_String_Literal_At
         ; String_Literal_At_F_Str_Lit : Bare_String_Literal
         ; String_Literal_At_F_At_Lit : Bare_Num_Literal
        );

      
   function String_Literal_At_F_Str_Lit
     (Node : Bare_String_Literal_At) return Bare_String_Literal;

      
   function String_Literal_At_F_At_Lit
     (Node : Bare_String_Literal_At) return Bare_Num_Literal;



   




      

   

      
      procedure Initialize_Fields_For_Terms
        (Self : Bare_Terms
         ; Terms_F_Terms : Bare_Term_List_List
        );

      
   function Terms_F_Terms
     (Node : Bare_Terms) return Bare_Term_List_List;



   




      

   

      
      procedure Initialize_Fields_For_Type_Reference
        (Self : Bare_Type_Reference
         ; Type_Reference_F_Var_Type_Name : Bare_Identifier_List
        );

      
   function Type_Reference_F_Var_Type_Name
     (Node : Bare_Type_Reference) return Bare_Identifier_List;



   




      

   

      
      procedure Initialize_Fields_For_Typed_String_Decl
        (Self : Bare_Typed_String_Decl
         ; Typed_String_Decl_F_Type_Id : Bare_Identifier
         ; Typed_String_Decl_F_String_Literals : Bare_String_Literal_List
        );

      
   function Typed_String_Decl_F_Type_Id
     (Node : Bare_Typed_String_Decl) return Bare_Identifier;

      
   function Typed_String_Decl_F_String_Literals
     (Node : Bare_Typed_String_Decl) return Bare_String_Literal_List;



   




      

   

      
      procedure Initialize_Fields_For_Variable_Decl
        (Self : Bare_Variable_Decl
         ; Variable_Decl_F_Var_Name : Bare_Identifier
         ; Variable_Decl_F_Var_Type : Bare_Type_Reference
         ; Variable_Decl_F_Expr : Bare_Term_List
        );

      
   function Variable_Decl_F_Var_Name
     (Node : Bare_Variable_Decl) return Bare_Identifier;

      
   function Variable_Decl_F_Var_Type
     (Node : Bare_Variable_Decl) return Bare_Type_Reference;

      
   function Variable_Decl_F_Expr
     (Node : Bare_Variable_Decl) return Bare_Term_List;



   




      

   

      
      procedure Initialize_Fields_For_Variable_Reference
        (Self : Bare_Variable_Reference
         ; Variable_Reference_F_Variable_Name : Bare_Identifier_List
         ; Variable_Reference_F_Attribute_Ref : Bare_Attribute_Reference
        );

      
   function Variable_Reference_F_Variable_Name
     (Node : Bare_Variable_Reference) return Bare_Identifier_List;

      
   function Variable_Reference_F_Attribute_Ref
     (Node : Bare_Variable_Reference) return Bare_Attribute_Reference;



   




      

   

      
      procedure Initialize_Fields_For_With_Decl
        (Self : Bare_With_Decl
         ; With_Decl_F_Is_Limited : Bare_Limited_Node
         ; With_Decl_F_Path_Names : Bare_String_Literal_List
        );

      
   function With_Decl_F_Is_Limited
     (Node : Bare_With_Decl) return Bare_Limited_Node;

      
   function With_Decl_F_Path_Names
     (Node : Bare_With_Decl) return Bare_String_Literal_List;



   





   function "<" (Left, Right : Internal_Unit) return Boolean;

   type Exiled_Entry is record
      Env  : Lexical_Env;
      Key  : Symbol_Type;
      Node : Bare_Gpr_Node;
   end record;
   --  Tuple of values passed to AST_Envs.Add. Used in the lexical
   --  environment rerooting machinery: see Remove_Exiled_Entries and
   --  Reroot_Foreign_Nodes.

   package Exiled_Entry_Vectors is new Gpr_Parser_Support.Vectors (Exiled_Entry);

   type Foreign_Node_Entry is record
      Node : Bare_Gpr_Node;
      --  The foreign node that has been added to an analysis unit's lexical
      --  environment.

      Unit : Internal_Unit;
      --  Analysis unit that owns Node
   end record;

   package Foreign_Node_Entry_Vectors is new Gpr_Parser_Support.Vectors
     (Foreign_Node_Entry);

   procedure Register_Destroyable
     (Unit : Internal_Unit; Node : Bare_Gpr_Node);
   --  Register Node to be destroyed when Unit is deallocated/reparsed

   procedure Register_Destroyable
     (Unit : Internal_Unit; Env : AST_Envs.Lexical_Env_Access);
   --  Register Env to be destroyed when Unit is deallocated/reparsed


   -----------------------------
   -- Miscellanous operations --
   -----------------------------

   type Destroy_Procedure is access procedure (Object : System.Address);

   type Destroyable_Type is record
      Object  : System.Address;
      --  Object to destroy

      Destroy : Destroy_Procedure;
      --  Procedure to destroy Object
   end record;
   --  Simple holder to associate an object to destroy and the procedure to
   --  perform the destruction.

   package Destroyable_Vectors is new Gpr_Parser_Support.Vectors
     (Destroyable_Type);

   package Analysis_Unit_Sets is new Gpr_Parser_Support.Cheap_Sets
     (Internal_Unit, null);

   package Units_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => GNATCOLL.VFS.Virtual_File,
      Element_Type    => Internal_Unit,
      Hash            => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Keys => GNATCOLL.VFS."=");

   function Token_Data (Unit : Internal_Unit) return Token_Data_Handler_Access;

   function Lookup_Symbol
     (Context : Internal_Context; Symbol : Text_Type) return Symbol_Type;
   --  Return the given symbol text as a symbol for this context. Raise an
   --  Invalid_Symbol_Error if it is invalid.

   function Create_Special_Unit
     (Context             : Internal_Context;
      Normalized_Filename : GNATCOLL.VFS.Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Internal_Unit;
   --  Create a new special analysis unit, i.e. a unit that is not registered
   --  in Context's unit map.

   function Templates_Unit (Context : Internal_Context) return Internal_Unit;
   --  Return the analysis unit to be used to parse tree rewriting templates.
   --  This creates it if it does not exists yet.

   procedure Set_Rule (Unit : Internal_Unit; Rule : Grammar_Rule);

   package Virtual_File_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => GNATCOLL.VFS.Virtual_File,
      Equivalent_Keys => "=",
      "="             => GNATCOLL.VFS."=",
      Hash            => Ada.Strings.Unbounded.Hash);

   function Normalized_Unit_Filename
     (Context : Internal_Context; Filename : String)
      return GNATCOLL.VFS.Virtual_File;
   --  Try to return a canonical filename. This is used to have an
   --  as-unique-as-possible analysis unit identifier.

   ------------------------------------
   -- File reader internal interface --
   ------------------------------------

   type Internal_File_Reader is limited interface;
   type Internal_File_Reader_Access is access all Internal_File_Reader'Class;
   pragma No_Strict_Aliasing (Internal_File_Reader_Access);

   procedure Inc_Ref (Self : in out Internal_File_Reader) is abstract;
   --  Create an ownership share for this file reader.

   function Dec_Ref (Self : in out Internal_File_Reader) return Boolean
   is abstract;
   --  Release an ownership share for this file reader. This destroys the file
   --  reader if there are no shares left.
   --
   --  Return whether there are no ownership shares left.

   procedure Read
     (Self        : Internal_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector) is abstract;
   --  Read the content of the source at the given filename, decoding it using
   --  the given charset and decoding the byte order mark if ``Read_BOM`` is
   --  true.
   --
   --  If there is an error during this process, append an error message to
   --  Diagnostics. In that case, Contents is considered uninitialized.
   --
   --  Otherwise, allocate a Text_Type buffer, fill it and initialize Contents
   --  to refer to it.

   procedure Dec_Ref (File_Reader : in out Internal_File_Reader_Access);
   --  Call Dec_Ref on File_Reader.all and, if the ref-count reaches 0,
   --  dealloacte it.

   --------------------------------------
   -- Unit provider internal interface --
   --------------------------------------

   type Internal_Unit_Provider is limited interface;
   type Internal_Unit_Provider_Access is
      access all Internal_Unit_Provider'Class;
   pragma No_Strict_Aliasing (Internal_Unit_Provider_Access);

   procedure Inc_Ref (Provider : in out Internal_Unit_Provider) is abstract;
   --  Create an ownership share for this unit provider.

   function Dec_Ref (Provider : in out Internal_Unit_Provider) return Boolean
   is abstract;
   --  Release an ownership share for this unit provider. This destroys the
   --  unit provider if there are no shares left.
   --
   --  Return whether there are no ownership shares left.

   function Get_Unit_Filename
     (Provider : Internal_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String is abstract;
   --  Return the filename corresponding to the given unit name/unit kind.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   function Get_Unit
     (Provider    : Internal_Unit_Provider;
      Context     : Internal_Context;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Internal_Unit is abstract;
   --  Fetch and return the analysis unit referenced by the given unit name.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   procedure Dec_Ref (Provider : in out Internal_Unit_Provider_Access);

   --------------------------------------
   -- Event handler internal interface --
   --------------------------------------

   type Internal_Event_Handler is limited interface;
   type Internal_Event_Handler_Access is
      access all Internal_Event_Handler'Class;
   pragma No_Strict_Aliasing (Internal_Event_Handler_Access);

   procedure Inc_Ref (Self : in out Internal_Event_Handler) is abstract;
   --  Create an ownership share for this event handler.

   function Dec_Ref (Self : in out Internal_Event_Handler) return Boolean
   is abstract;
   --  Release an ownership share for this event handler. This destroys the
   --  event handler if there are no shares left.
   --
   --  Return whether there are no ownership shares left.

   procedure Unit_Requested_Callback
     (Self               : in out Internal_Event_Handler;
      Context            : Internal_Context;
      Name               : Text_Type;
      From               : Internal_Unit;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean) is null;

   procedure Unit_Parsed_Callback
     (Self     : in out Internal_Event_Handler;
      Context  : Internal_Context;
      Unit     : Internal_Unit;
      Reparsed : Boolean) is null;

   procedure Dec_Ref (Self : in out Internal_Event_Handler_Access);

   ---------------------------------
   -- Analysis context definition --
   ---------------------------------

   type Analysis_Context_Type is limited record
      --  Start of ABI area. In order to perform fast checks from foreign
      --  languages, we maintain minimal ABI for analysis context: this allows
      --  us in language bindings to directly peek in this record rather than
      --  rely on (slow) calls to getters.

      Serial_Number : Version_Number;
      --  Serial number that is incremented each time this context allocation
      --  is released.

      --  End of ABI area

      Ref_Count : Natural;

      Units : Units_Maps.Map;
      --  Collection of analysis units loaded in this context

      Filenames : Virtual_File_Maps.Map;
      --  Cache for GNATCOLL.VFS.Virtual_File we create for String filenames.
      --  Re-using older Virtual_File values is useful as this reduces the need
      --  to normalize paths, which is a costly operation.

      Symbols : Symbol_Table;
      --  Symbol table used in this whole context

      Charset : Unbounded_String;
      --  Default charset to use in analysis units

      Tab_Stop : aliased Positive;
      --  Tab stop for the lexer to correctly interpret ASCII.HT input
      --  characters.

      With_Trivia : Boolean;
      --  Whether Trivia nodes were parsed and included in analysis units

      Root_Scope : Lexical_Env;
      --  The lexical scope that is shared amongst every compilation unit. Used
      --  to resolve cross file references.

      Named_Envs : NED_Maps.Map;
      --  Map env names to the corresponding named environment descriptors

      File_Reader : Internal_File_Reader_Access;
      --  Object to override the reading and decoding of source files

      Event_Handler : Internal_Event_Handler_Access;
      --  Object to provide event callbacks

      Unit_Provider : Internal_Unit_Provider_Access;
      --  Object to translate unit names to file names

      Parser : Parser_Type;
      --  Main parser type. TODO: If we want to parse in several tasks, we'll
      --  replace that by an array of parsers.

      Discard_Errors_In_Populate_Lexical_Env : Boolean;
      --  See the eponym procedure

      In_Populate_Lexical_Env : Boolean;
      --  Flag to tell whether we are running the Populate_Lexical_Env pass.
      --  When it's on, we must not use the memoization map as the hash of
      --  lexical environment changes when their content changes.

      Logic_Resolution_Timeout : Natural;
      --  If zero, inefficient. Otherwise, designates the maximal number of
      --  steps allowed in the resolution of logic equations before
      --  interrupting the resolution because of timeout. See the
      --  Set_Logic_Resolution_Timeout procedure.

      Cache_Version : Version_Number;
      --  Version number used to invalidate memoization caches in a lazy
      --  fashion. If an analysis unit's version number is strictly inferior to
      --  this, its memoization map should be cleared.

      Reparse_Cache_Version : Version_Number;
      --  Version number used to invalidate referenced envs caches. It is
      --  incremented only when a unit is reparsed in the context.

      Rewriting_Handle : Rewriting_Handle_Pointer :=
         No_Rewriting_Handle_Pointer;
      --  Rewriting handle for this context's current rewriting session.
      --  No_Rewriting_Handle_Pointer if there is no such session currently.

      Templates_Unit : Internal_Unit := No_Analysis_Unit;
      --  Special analysis unit used only as a containing unit to parse
      --  templates in the context of tree rewriting.

      Available_Rebindings : Env_Rebindings_Vectors.Vector;
      --  List of allocated-but-unused Env_Rebinding_Type records.
      --
      --  Each rebinding we allocate for an analysis context is deallocated
      --  only when the whole context is released, so when this list is not
      --  empty, we pick one of its element instead of allocating another
      --  rebinding (see the Acquire_Rebindings and Release_Rebindings
      --  subprograms).
      --
      --  Thanks to this mechanism, we have a very simple way to implement
      --  rebindings validity checking for nodes: once we have established that
      --  the node reference is valid regarding its context, we know that the
      --  rebindings pointer is valid, and thus we can just check the rebinding
      --  version number.

      

   end record;

   package Node_To_Named_Env_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Bare_Gpr_Node,
      Element_Type    => Named_Env_Descriptor_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Analysis_Unit_Type is limited record
      --  Start of ABI area. In order to perform fast checks from foreign
      --  languages, we maintain minimal ABI for analysis context: this allows
      --  us in language bindings to directly peek in this record rather than
      --  rely on (slow) calls to getters.

      Unit_Version : Version_Number := 0;
      --  Version for this particular unit. This will be incremented every time
      --  a reparse occurs.

      --  End of ABI area

      Context : Internal_Context;
      --  The owning context for this analysis unit

      Is_Internal : Boolean;
      --  Whether this unit is internal.
      --
      --  The use of file readers for parsing is disabled for internal units,
      --  which allows in-memory parsing for them even when a file reader is
      --  active.
      --
      --  It is illegal for users of public APIs to reparse an internal unit.
      --  Setting this flag allows generated libraries to create internal units
      --  to implement language internals and forbid library users to mess with
      --  this unit.

      Ast_Root : Bare_Gpr_Node;

      Filename : GNATCOLL.VFS.Virtual_File;
      --  The originating name for this analysis unit. This should be set even
      --  if the analysis unit was parsed from a buffer.

      Charset : Unbounded_String;
      --  The parsing charset for this analysis unit, as a string. If the
      --  charset used actually came from a byte order mark, this is
      --  nevertheless set to the one the user requested.

      TDH : aliased Token_Data_Handler;
      --  The token data handler that handles all token data during parsing and
      --  owns it afterwards.

      Diagnostics : Diagnostics_Vectors.Vector;
      --  The list of diagnostics produced for this analysis unit

      Rule : Grammar_Rule;
      --  The grammar rule used to parse this unit

      Ast_Mem_Pool : Bump_Ptr_Pool;
      --  This memory pool shall only be used for AST parsing. Stored here
      --  because it is more convenient, but one shall not allocate from it.

      Destroyables : Destroyable_Vectors.Vector;
      --  Collection of objects to destroy when destroying the analysis unit

      Referenced_Units : Analysis_Unit_Sets.Set;
      --  Units that are referenced from this one. Useful for
      --  visibility/computation of the reference graph.

      Is_Env_Populated : Boolean;
      --  Whether Populate_Lexical_Env was called on this unit. Used not to
      --  populate multiple times the same unit and hence avoid infinite
      --  populate recursions for circular dependencies.

      Exiled_Entries : Exiled_Entry_Vectors.Vector;
      --  Lexical env population for this unit may have added AST nodes it owns
      --  to the lexical environments that belong to other units ("exiled"
      --  entries). For each of these AST nodes, this vector contains an entry
      --  that records the target environment, the AST node and the
      --  corresponding symbol.

      Foreign_Nodes : Foreign_Node_Entry_Vectors.Vector;
      --  This unit owns a set of lexical environments. This vector contains
      --  the list of AST nodes that were added to these environments and that
      --  come from other units.

      Exiled_Entries_In_NED : Exiled_Entry_In_NED_Vectors.Vector;
      --  Like Exiled_Entries, but for symbol/node associations exclusively
      --  handled by the named environments mechanism.
      --
      --  This list allows efficient removal of these entries from
      --  Named_Env_Descriptor.Foreign_Nodes components when unloading this
      --  unit.

      Exiled_Envs : Exiled_Env_Vectors.Vector;
      --  List of lexical environments created in this unit and whose parent is
      --  a named environment.
      --
      --  This list allows efficient removal for these envs from
      --  Named_Env_Descriptor.Foreign_Envs components when unloading this
      --  unit.

      Named_Envs : Named_Env_Vectors.Vector;
      --  List of named environment created in this unit.
      --
      --  This list allows efficient removal for these envs from the
      --  Named_Env_Descriptor.Envs components when unloading this unit.

      Nodes_With_Foreign_Env : Node_To_Named_Env_Maps.Map;
      --  Mapping from a node to its Self_Env's named env descriptor, for each
      --  node in this unit whose Self_Env is a named environment.
      --
      --  This mapping allows efficient removal for these nodes from the
      --  Named_Env_Descriptor.Nodes_With_Foreign_Env components when unloading
      --  this unit.

      Rebindings : aliased Env_Rebindings_Vectors.Vector;
      --  List of rebindings for which Old_Env and/or New_Env belong to this
      --  unit. When this unit gets destroyed or reparsed, these rebindings
      --  need to be destroyed too (see Destroy_Rebindings).


      Cache_Version : Version_Number := 0;
      --  See the eponym field in Analysis_Context_Type

      

   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Context_Type, Internal_Context);

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Unit_Type, Internal_Unit);

   type Reparsed_Unit is record
      TDH          : Token_Data_Handler;
      Diagnostics  : Diagnostics_Vectors.Vector;
      Ast_Mem_Pool : Bump_Ptr_Pool;
      Ast_Root     : Bare_Gpr_Node;
   end record;
   --  Holder for fields affected by an analysis unit reparse. This makes it
   --  possible to separate the "reparsing" and the "replace" steps.

   procedure Destroy (Reparsed : in out Reparsed_Unit);
   --  Free all resources in Reparsed

   function Basename (Filename : String) return String;
   --  Return the base filename for String

   ----------------------------------------------------
   -- Implementation for analysis context primitives --
   ----------------------------------------------------

   function Allocate_Context return Internal_Context;
   --  Allocate a new analysis context.

   procedure Initialize_Context
     (Context        : Internal_Context;
      Charset        : String;
      File_Reader    : Internal_File_Reader_Access;
      Unit_Provider  : Internal_Unit_Provider_Access;
      Event_Handler  : Internal_Event_Handler_Access;
      With_Trivia    : Boolean;
      Tab_Stop       : Positive);
   --  Initialize an analysis context. Must be called right after
   --  ``Allocate_Context`` on its result.
   --
   --  Having separate primitives for allocation/initialization allows library
   --  bindings to have a context wrapper (created between the two calls) ready
   --  when callbacks that happen during context initialization (for instance
   --  "unit parsed" events).
   --  Implementation for ``Analysis.Create_Context``: call
   --  ``Allocate_Context`` to allocate an ``Internal_Context`` value, then
   --  call ``Initialize_Context`` to initialize it.
   --
   --  Having separate primitives for allocation/initialization allows library
   --  bindings to have a context wrapper (created between the two calls) ready
   --  when callbacks that happen during context initialization (for instance
   --  "unit parsed" events).

   function Create_Unit
     (Context             : Internal_Context;
      Normalized_Filename : GNATCOLL.VFS.Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Internal_Unit
      with Pre => not Has_Unit (Context, +Normalized_Filename.Full_Name);
   --  Create a new analysis unit and register it in Context

   function Get_Unit
     (Context           : Internal_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Input             : Internal_Lexer_Input;
      Rule              : Grammar_Rule;
      Is_Internal       : Boolean := False) return Internal_Unit;
   --  Helper for Get_From_File and Get_From_Buffer. Return the resulting
   --  analysis unit.
   --
   --  If ``Is_Internal`` is True, allow parsing from buffer even if
   --  ``Context`` has a file reader, and forbid later calls to
   --  Get_From_File/Get_From_Buffer/Reparse on the returned unit.

   function Has_Unit
     (Context : Internal_Context; Unit_Filename : String) return Boolean;
   --  Implementation for Analysis.Has_Unit

   function Get_From_File
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Reparse  : Boolean;
      Rule     : Grammar_Rule) return Internal_Unit;
   --  Implementation for Analysis.Get_From_File

   function Get_From_Buffer
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Buffer   : String;
      Rule     : Grammar_Rule) return Internal_Unit;
   --  Implementation for Analysis.Get_From_Buffer

   function Get_With_Error
     (Context  : Internal_Context;
      Filename : String;
      Error    : Text_Type;
      Charset  : String;
      Rule     : Grammar_Rule) return Internal_Unit;
   --  Implementation for Analysis.Get_With_Error


   function Unit_Provider
     (Context : Internal_Context) return Internal_Unit_Provider_Access;
   --  Implementation for Analysis.Unit_Provider

   function Hash (Context : Internal_Context) return Hash_Type;
   --  Implementation for Analysis.Hash

   function Has_With_Trivia (Context : Internal_Context) return Boolean;
   --  Implementation for Analysis.Has_With_Trivia

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Internal_Context; Discard : Boolean);
   --  Implementation for Analysis.Discard_Errors_In_Populate_Lexical_Env

   procedure Set_Logic_Resolution_Timeout
     (Context : Internal_Context; Timeout : Natural);
   --  Implementation for Analysis.Set_Logic_Resolution_Timeout

   function Has_Rewriting_Handle (Context : Internal_Context) return Boolean;
   --  Implementation for Analysis.Has_Rewriting_Handle

   procedure Inc_Ref (Context : Internal_Context);
   --  Increment the ref-count of Context. This does nothing if Context is
   --  null.

   procedure Dec_Ref (Context : in out Internal_Context);
   --  Decrement the ref-count of Context, destroying it if the ref-count
   --  reaches zero. This does nothing if Context is null.

   procedure Destroy (Context : in out Internal_Context)
      with Pre => not Has_Rewriting_Handle (Context);
   --  Free all resources allocated for Context

   -------------------------------------------------
   -- Implementation for analysis unit primitives --
   -------------------------------------------------

   function Context (Unit : Internal_Unit) return Internal_Context;
   --  Implementation for Analysis.Context

   function Hash (Unit : Internal_Unit) return Hash_Type;
   --  Implementation for Analysis.Hash

   procedure Reparse (Unit : Internal_Unit; Charset : String);
   --  Implementation for Analysis.Reparse

   procedure Reparse
     (Unit : Internal_Unit; Charset : String; Buffer  : String);
   --  Implementation for Analysis.Reparse

   procedure Populate_Lexical_Env (Unit : Internal_Unit);
   --  Implementation for Analysis.Populate_Lexical_Env

   function Get_Filename (Unit : Internal_Unit) return String;
   --  Implementation for Analysis.Get_Filename

   function Get_Charset (Unit : Internal_Unit) return String;
   --  Implementation for Analysis.Get_Charset

   function Has_Diagnostics (Unit : Internal_Unit) return Boolean;
   --  Implementation for Analysis.Has_Diagnostics

   function Diagnostics (Unit : Internal_Unit) return Diagnostics_Array;
   --  Implementation for Analysis.Diagnostics

   function Format_GNU_Diagnostic
     (Unit : Internal_Unit; D : Diagnostic) return String;
   --  Implementation for Analysis.Format_GNU_Diagnostic

   function Root (Unit : Internal_Unit) return Bare_Gpr_Node;
   --  Implementation for Analysis.Root

   function First_Token (Unit : Internal_Unit) return Token_Reference;
   --  Implementation for Analysis.First_Token

   function Last_Token (Unit : Internal_Unit) return Token_Reference;
   --  Implementation for Analysis.Last_Token

   function Token_Count (Unit : Internal_Unit) return Natural;
   --  Implementation for Analysis.Token_Count

   function Trivia_Count (Unit : Internal_Unit) return Natural;
   --  Implementation for Analysis.Trivia_Count

   function Text (Unit : Internal_Unit) return Text_Type;
   --  Implementation for Analysis.Text

   function Lookup_Token
     (Unit : Internal_Unit; Sloc : Source_Location) return Token_Reference;
   --  Implementation for Analysis.Lookup_Token

   procedure Dump_Lexical_Env (Unit : Internal_Unit);
   --  Implementation for Analysis.Dump_Lexical_Env

   procedure Print (Unit : Internal_Unit; Show_Slocs : Boolean);
   --  Implementation for Analysis.Print

   procedure PP_Trivia (Unit : Internal_Unit);
   --  Implementation for Analysis.PP_Trivia

   procedure Destroy (Unit : in out Internal_Unit);
   --  TODO???

   function Basename (Unit : Internal_Unit) return String;
   --  Return the base filename for Unit

   procedure Invalidate_Caches
     (Context : Internal_Context; Invalidate_Envs : Boolean);
   --  Invalidate memoization caches. If Invalidate_Envs is true, also
   --  invalidate referenced envs caches.

   procedure Reset_Caches (Unit : Internal_Unit);
   --  Destroy Unit's memoization cache. This resets Unit's version number to
   --  Unit.Context.Cache_Version.

   procedure Reference_Unit (From, Referenced : Internal_Unit);
   --  Set the Referenced unit as being referenced from the From unit. This is
   --  useful for visibility purposes, and is mainly meant to be used in the
   --  env hooks.

   function Get_Line
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type;
   --  Return the line of text at line number ``Line_Number``

   function Is_Referenced_From
     (Self, Unit : Internal_Unit) return Boolean;

   procedure Do_Parsing
     (Unit   : Internal_Unit;
      Input  : Internal_Lexer_Input;
      Result : out Reparsed_Unit);
   --  Parse text for Unit using Input and store the result in Result. This
   --  leaves Unit unchanged.

   procedure Update_After_Reparse
     (Unit : Internal_Unit; Reparsed : in out Reparsed_Unit);
   --  Update Unit's AST from Reparsed and update stale lexical environment
   --  data after the reparsing of Unit.

   procedure Destroy_Unit_Destroyables (Unit : Internal_Unit);
   --  Destroy all destroyables objects in Unit and clear this list in Unit

   procedure Remove_Exiled_Entries (Unit : Internal_Unit);
   --  Remove lexical environment entries referencing nodes in Unit from
   --  lexical environments Unit does not own. Remove foreign node entries in
   --  foreign units that correspond to these exiled entries. Clear
   --  Unit.Exiled_Entries afterwards.

   procedure Remove_Named_Envs
     (Unit                      : Internal_Unit;
      Named_Envs_Needing_Update : in out NED_Maps.Map);
   --  Remove envs that belong to Unit from all relevant NEDs, and keep track
   --  in Named_Env_Needing_Update of the env names whose env with precedence
   --  must change because of this.

   procedure Extract_Foreign_Nodes
     (Unit          : Internal_Unit;
      Foreign_Nodes : in out Bare_Gpr_Node_Vectors.Vector);
   --  Collect in Foreign_Nodes all foreign nodes in Unit's lexical
   --  environments (i.e. lexical env entries that refer to nodes which belongs
   --  to other analysis units). Remove the exiled entries in foreign units
   --  that correspond to these foreign nodes. Clear Unit.Foreign_Nodes
   --  afterwards.

   procedure Reroot_Foreign_Node (Node : Bare_Gpr_Node);
   --  Re-create the lexical env entry for Node. This is to be used in
   --  Flush_Populate_Lexical_Env_Queue, after reparsing removed the target
   --  lexical environment.

   procedure Destroy_Rebindings
     (Rebindings : access Env_Rebindings_Vectors.Vector);
   --  Destroy all rebindings in Rebindings, plus their child rebindings. Note
   --  that children can belong to various analysis units, so this takes care
   --  of removing the destroyed rebindings from each concerned analysis unit's
   --  Rebindings vector.
   --
   --  This require an access parameter in order to avoid aliasing issues in
   --  the body.

   function Get_Rewriting_Handle
     (Context : Internal_Context) return Rewriting_Handle_Pointer;
   --  Return the Rewriting_Handle component of Context

   procedure Set_Rewriting_Handle
     (Context : Internal_Context; Handle : Rewriting_Handle_Pointer);
   --  Set the Rewriting_Handle component of Context

   type Node_Safety_Net is record
      Context        : Internal_Context;
      Context_Serial : Version_Number;
      --  Analysis context and serial number at the time this safety net was
      --  produced.

      Unit         : Internal_Unit;
      Unit_Version : Version_Number;
      --  Analysis unit and unit version at the time this safety net was
      --  produced.

      Rebindings_Version : Version_Number;
      --  Version of the associated rebinding at the time this safety net was
      --  procuded.
   end record;
   --  Information to embed in public APIs, used to check before accessing data
   --  that the said-data is still valid.

   No_Node_Safety_Net : constant Node_Safety_Net := (null, 0, null, 0, 0);

   function String_To_Symbol
     (Self    : Bare_Gpr_Node;
      Context : Internal_Context;
      S       : String_Type) return Symbol_Type;
   --  Convert ``S`` into the corresponding symbol, raising a
   --  ``Property_Error`` if symbol canonicalization fails (using ``Self`` to
   --  provide context for this error). If ``S`` is empty, just return
   --  ``null``.

   function Solve_Wrapper
     (R            : Solver.Relation;
      Context_Node : Bare_Gpr_Node) return Boolean;
   --  Wrapper for Gpr_Parser_Support.Adalog.Solve; will handle setting the debug
   --  strings in the equation if in debug mode.

   generic
      type T (<>) is limited private;
      type T_Access is access all T;
      with procedure Destroy (Object : in out T_Access);
   procedure Register_Destroyable_Gen
     (Unit : Internal_Unit; Object : T_Access);
   --  Generic procedure to register an object so that it is automatically
   --  destroyed when Unit is destroyed.

   function New_Unit_String
     (Unit : Internal_Unit; Str : String) return String_Access;
   --  This function allocates a string whose lifetime will be associated with
   --  ``Unit``.

private
   --  We only have a private part to defer the initialization of struct
   --  constants. This allows us to circumvent circularity problems between
   --  arrays and structs.

         
      


      No_Designated_Env : constant Internal_Designated_Env :=
      (
               Kind => None, 
               Env_Name => null, 
               Direct_Env => Empty_Env
      );

         

         

         
      


      No_Entity_Ada_Prelude_Node : constant Internal_Entity_Ada_Prelude_Node :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Access_Subp : constant Internal_Entity_Ada_Access_Subp :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Context_Clause : constant Internal_Entity_Ada_Context_Clause :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Base_List : constant Internal_Entity_Base_List :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Context_Clause_List : constant Internal_Entity_Ada_Context_Clause_List :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Entity_Kind : constant Internal_Entity_Ada_Entity_Kind :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Entity_Kind_Function : constant Internal_Entity_Ada_Entity_Kind_Function :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Entity_Kind_Package : constant Internal_Entity_Ada_Entity_Kind_Package :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Entity_Kind_Procedure : constant Internal_Entity_Ada_Entity_Kind_Procedure :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Generic : constant Internal_Entity_Ada_Generic :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Library_Item : constant Internal_Entity_Ada_Library_Item :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Main : constant Internal_Entity_Ada_Main :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Pkg : constant Internal_Entity_Ada_Pkg :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Pkg_Body : constant Internal_Entity_Ada_Pkg_Body :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Pragma : constant Internal_Entity_Ada_Pragma :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Prelude : constant Internal_Entity_Ada_Prelude :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Prelude_Node_List : constant Internal_Entity_Ada_Prelude_Node_List :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Separate : constant Internal_Entity_Ada_Separate :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Skip : constant Internal_Entity_Ada_Skip :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Skip_List : constant Internal_Entity_Ada_Skip_List :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Subp : constant Internal_Entity_Ada_Subp :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_Use : constant Internal_Entity_Ada_Use :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_With : constant Internal_Entity_Ada_With :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ada_With_Formal : constant Internal_Entity_Ada_With_Formal :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_All_Qualifier : constant Internal_Entity_All_Qualifier :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_All_Qualifier_Absent : constant Internal_Entity_All_Qualifier_Absent :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_All_Qualifier_Present : constant Internal_Entity_All_Qualifier_Present :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Attribute_Decl : constant Internal_Entity_Attribute_Decl :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Attribute_Reference : constant Internal_Entity_Attribute_Reference :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Builtin_Function_Call : constant Internal_Entity_Builtin_Function_Call :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Case_Construction : constant Internal_Entity_Case_Construction :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Case_Item : constant Internal_Entity_Case_Item :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Case_Item_List : constant Internal_Entity_Case_Item_List :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Gpr_Node_List : constant Internal_Entity_Gpr_Node_List :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Choices : constant Internal_Entity_Choices :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Compilation_Unit : constant Internal_Entity_Compilation_Unit :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Empty_Decl : constant Internal_Entity_Empty_Decl :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Expr : constant Internal_Entity_Expr :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Expr_List : constant Internal_Entity_Expr_List :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Single_Tok_Node : constant Internal_Entity_Single_Tok_Node :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Identifier : constant Internal_Entity_Identifier :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Identifier_List : constant Internal_Entity_Identifier_List :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Limited_Node : constant Internal_Entity_Limited_Node :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Limited_Absent : constant Internal_Entity_Limited_Absent :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Limited_Present : constant Internal_Entity_Limited_Present :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Num_Literal : constant Internal_Entity_Num_Literal :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Others_Designator : constant Internal_Entity_Others_Designator :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Package_Decl : constant Internal_Entity_Package_Decl :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Package_Extension : constant Internal_Entity_Package_Extension :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Package_Renaming : constant Internal_Entity_Package_Renaming :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Package_Spec : constant Internal_Entity_Package_Spec :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Prefix : constant Internal_Entity_Prefix :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Private_Node : constant Internal_Entity_Private_Node :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Private_Absent : constant Internal_Entity_Private_Absent :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Private_Present : constant Internal_Entity_Private_Present :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Project : constant Internal_Entity_Project :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Project_Declaration : constant Internal_Entity_Project_Declaration :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Project_Extension : constant Internal_Entity_Project_Extension :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Project_Qualifier : constant Internal_Entity_Project_Qualifier :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Project_Qualifier_Abstract : constant Internal_Entity_Project_Qualifier_Abstract :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Project_Qualifier_Aggregate : constant Internal_Entity_Project_Qualifier_Aggregate :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Project_Qualifier_Aggregate_Library : constant Internal_Entity_Project_Qualifier_Aggregate_Library :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Project_Qualifier_Configuration : constant Internal_Entity_Project_Qualifier_Configuration :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Project_Qualifier_Library : constant Internal_Entity_Project_Qualifier_Library :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Project_Qualifier_Standard : constant Internal_Entity_Project_Qualifier_Standard :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Project_Reference : constant Internal_Entity_Project_Reference :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_String_Literal : constant Internal_Entity_String_Literal :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_String_Literal_At : constant Internal_Entity_String_Literal_At :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_String_Literal_List : constant Internal_Entity_String_Literal_List :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Term_List : constant Internal_Entity_Term_List :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Term_List_List : constant Internal_Entity_Term_List_List :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Terms : constant Internal_Entity_Terms :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Type_Reference : constant Internal_Entity_Type_Reference :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Typed_String_Decl : constant Internal_Entity_Typed_String_Decl :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Variable_Decl : constant Internal_Entity_Variable_Decl :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Variable_Reference : constant Internal_Entity_Variable_Reference :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_With_Decl : constant Internal_Entity_With_Decl :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_With_Decl_List : constant Internal_Entity_With_Decl_List :=
      (
               Node => No_Bare_Gpr_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Env_Assoc : constant Internal_Env_Assoc :=
      (
               Key => null, 
               Value => No_Bare_Gpr_Node, 
               Dest_Env => No_Designated_Env, 
               Metadata => No_Metadata
      );


end Gpr_Parser.Implementation;
