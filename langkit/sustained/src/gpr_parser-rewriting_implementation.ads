
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Gpr_Parser_Support.Bump_Ptr; use Gpr_Parser_Support.Bump_Ptr;
with Gpr_Parser_Support.Bump_Ptr_Vectors;

with Gpr_Parser.Common;   use Gpr_Parser.Common;
with Gpr_Parser.Implementation; use Gpr_Parser.Implementation;

--  Internal package: low-level primitives to implement syntax-based source
--  rewriting.

private package Gpr_Parser.Rewriting_Implementation is

   use Support.Diagnostics, Support.Text;

   type Rewriting_Handle_Type;
   type Unit_Rewriting_Handle_Type;
   type Node_Rewriting_Handle_Type;

   type Rewriting_Handle is access Rewriting_Handle_Type;
   --  Internal handle for an analysis context rewriting session

   type Unit_Rewriting_Handle is access Unit_Rewriting_Handle_Type;
   --  Internal handle for the process of rewriting an analysis unit

   type Node_Rewriting_Handle is access Node_Rewriting_Handle_Type;
   --  Internal handle for the process of rewriting an analysis unit

   pragma No_Strict_Aliasing (Rewriting_Handle);
   pragma No_Strict_Aliasing (Unit_Rewriting_Handle);
   pragma No_Strict_Aliasing (Node_Rewriting_Handle);

   package Unit_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Unit_Rewriting_Handle,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   package Node_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Bare_Gpr_Node,
      Element_Type    => Node_Rewriting_Handle,
      Hash            => Named_Hash,
      Equivalent_Keys => "=");

   package Nodes_Pools is new Gpr_Parser_Support.Bump_Ptr_Vectors
     (Node_Rewriting_Handle);

   type Rewriting_Handle_Type is record
      Context : Internal_Context;
      --  Analysis context this rewriting handle relates to

      Units : Unit_Maps.Map;
      --  Keep track of rewriting handles we create all units that Context owns

      Pool      : Bump_Ptr_Pool;
      New_Nodes : Nodes_Pools.Vector;
      --  Keep track of all node rewriting handles that don't map to original
      --  nodes, i.e. all nodes that were created during this rewriting
      --  session.
   end record;

   type Unit_Rewriting_Handle_Type is record
      Context_Handle : Rewriting_Handle;
      --  Rewriting handle for the analysis context this relates to

      Unit : Internal_Unit;
      --  Analysis unit this relates to

      Root : Node_Rewriting_Handle;
      --  Handle for the node that will become the root node of this analysis
      --  unit.

      Nodes : Node_Maps.Map;
      --  Keep track of rewriting handles we create for base AST nodes that
      --  Unit owns.
   end record;

   package Node_Vectors is new Ada.Containers.Vectors
     (Positive, Node_Rewriting_Handle);

   type Node_Children_Kind is (
      Unexpanded,
      --  Dummy node rewriting handle: children don't have their own handle yet

      Expanded_Regular,
      --  Expanded node rewriting handle: children have their own handle. Note
      --  that this is for all but token nodes.

      Expanded_Token_Node
      --  Expanded node rewriting handle, specific for token nodes: there is no
      --  children, only some associated text.
   );

   type Node_Children (Kind : Node_Children_Kind := Unexpanded) is record
      case Kind is
         when Unexpanded          => null;
         when Expanded_Regular    => Vector : Node_Vectors.Vector;
         when Expanded_Token_Node => Text   : Unbounded_Wide_Wide_String;
      end case;
   end record;
   --  Lazily evaluated vector of children for a Node_Rewriting_Handle.
   --
   --  In order to avoid constructing the whole tree of Node_Rewriting_Handle
   --  for some analysis unit at once, we build them in a lazy fashion.

   Unexpanded_Children : constant Node_Children := (Kind => Unexpanded);

   type Node_Rewriting_Handle_Type is record
      Context_Handle : Rewriting_Handle;
      --  Rewriting handle for the analysis context that owns Node

      Node : Bare_Gpr_Node;
      --  Bare AST node which this rewriting handle relates to

      Parent : Node_Rewriting_Handle;
      --  Rewriting handle for Node's parent, or No_Node_Rewriting_Handle if
      --  Node is a root node.

      Kind : Gpr_Node_Kind_Type;
      --  Kind for the node this handle represents. When Node is not null (i.e.
      --  when this represents an already existing node, rather than a new
      --  one), this must be equal to Node.Kind.

      Tied : Boolean;
      --  Whether this node is tied to an analysis unit tree. It can be
      --  assigned as a child to another node iff it is not tied.

      Root_Of : Unit_Rewriting_Handle;
      --  If the node this handle represents is the root of a rewritten unit,
      --  this references this unit. No_Unit_Rewriting_Handle in all other
      --  cases.

      Children : Node_Children;
      --  Lazily evaluated vector of children for the rewritten node
   end record;

   type Unit_Rewriting_Handle_Array is
      array (Positive range <>) of Unit_Rewriting_Handle;

   type Node_Rewriting_Handle_Array is
      array (Positive range <>) of Node_Rewriting_Handle;

   No_Rewriting_Handle      : constant Rewriting_Handle      := null;
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle := null;
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle := null;

   --------------------------------------------------
   -- Implementation of context rewriting routines --
   --------------------------------------------------

   function Handle (Context : Internal_Context) return Rewriting_Handle;
   --  Implementation for Rewriting.Handle

   function Context (Handle : Rewriting_Handle) return Internal_Context;
   --  Implementation for Rewriting.Context

   function Start_Rewriting
     (Context : Internal_Context) return Rewriting_Handle
      with Post => Handle (Context) /= No_Rewriting_Handle
                   and then Has_With_Trivia (Context)
                   and then Start_Rewriting'Result = Handle (Context)
                   and then Gpr_Parser.Rewriting_Implementation.Context
                             (Start_Rewriting'Result) = Context;
   --  Implementation for Rewriting.Start_Rewriting

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle)
      with Post => Handle = No_Rewriting_Handle;
   --  Implementation for Rewriting.Abort_Rewriting

   type Apply_Result (Success : Boolean := True) is record
      case Success is
         when False =>
            Unit : Internal_Unit;
            --  Reference to the analysis unit that was being processed when
            --  the error occurred.

            Diagnostics : Diagnostics_Vectors.Vector;
            --  Corresponding list of error messages
         when True => null;
      end case;
   end record;

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result
      with Post => (if Apply'Result.Success
                    then Handle = No_Rewriting_Handle
                    else Handle = Handle'Old);
   --  Implementation for Rewriting.Apply

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array;
   --  Implementation for Rewriting.Unit_Handles

   ---------------------------------------
   -- Implementation for unit rewriting --
   ---------------------------------------

   function Handle (Unit : Internal_Unit) return Unit_Rewriting_Handle;
   --  Implementation for Rewriting.Handle

   function Unit (Handle : Unit_Rewriting_Handle) return Internal_Unit;
   --  Implementation for Rewriting.Unit

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Root

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Set_Root

   function Unparse (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type;
   --  Implementation for Rewriting.Unparse

   ---------------------------------------
   -- Implementation for node rewriting --
   ---------------------------------------

   function Handle
     (Node : Bare_Gpr_Node) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Handle

   function Node
     (Handle : Node_Rewriting_Handle) return Bare_Gpr_Node;
   --  Implementation for Rewriting.Node

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle;
   --  Implementation for Rewriting.Context

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Implementation for Rewriting.Unparse

   function Kind (Handle : Node_Rewriting_Handle) return Gpr_Node_Kind_Type;
   --  Implementation for Rewriting.Kind

   function Tied (Handle : Node_Rewriting_Handle) return Boolean;
   --  Implementation for Rewriting.Tied

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Parent

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural;
   --  Implementation for Rewriting.Children_Count

   function Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Child

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Set_Child

   function Text (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Implementation for Rewriting.Text

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type);
   --  Implementation for Rewriting.Set_Text

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Replace

   --------------------------------------------
   -- Implementation for list node rewriting --
   --------------------------------------------

   procedure Insert_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle)
      with Post => Rewriting_Implementation.Child
                     (Handle, Index) = Child;
   --  Implementation for Rewriting.Insert_Child

   procedure Append_Child
     (Handle : Node_Rewriting_Handle;
      Child  : Node_Rewriting_Handle)
      with Post => Rewriting_Implementation.Child
                     (Handle, Children_Count (Handle)) = Child;
   --  Implementation for Rewriting.Append_Child

   procedure Remove_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive);
   --  Implementation for Rewriting.Remove_Child

   --------------------------------------
   -- Implementation for node creation --
   --------------------------------------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Clone

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : Gpr_Node_Kind_Type) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_Node

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : Gpr_Node_Kind_Type;
      Text   : Text_Type) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_Token_Node

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : Gpr_Node_Kind_Type;
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_Regular_Node

   ----------------------------------
   -- Implementation for templates --
   ----------------------------------

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_From_Template

   -----------------------------
   -- Node creation shortcuts --
   -----------------------------



         function Create_Ada_Access_Subp
           (Handle : Rewriting_Handle
               ; Ada_Access_Subp_F_Subp_Kind : Node_Rewriting_Handle
               ; Ada_Access_Subp_F_Skips : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Pragma
           (Handle : Rewriting_Handle
               ; Ada_Pragma_F_Skips : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Use
           (Handle : Rewriting_Handle
               ; Ada_Use_F_Skips : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_With
           (Handle : Rewriting_Handle
               ; Ada_With_F_Has_Limited : Node_Rewriting_Handle
               ; Ada_With_F_Has_Private : Node_Rewriting_Handle
               ; Ada_With_F_Packages : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Generic
           (Handle : Rewriting_Handle
               ; Ada_Generic_F_Skips : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Library_Item
           (Handle : Rewriting_Handle
               ; Ada_Library_Item_F_Generic_Stub : Node_Rewriting_Handle
               ; Ada_Library_Item_F_Separate : Node_Rewriting_Handle
               ; Ada_Library_Item_F_Main : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Pkg
           (Handle : Rewriting_Handle
               ; Ada_Pkg_F_Has_Private : Node_Rewriting_Handle
               ; Ada_Pkg_F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Pkg_Body
           (Handle : Rewriting_Handle
               ; Ada_Pkg_Body_F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Subp
           (Handle : Rewriting_Handle
               ; Ada_Subp_F_Subp_Kind : Node_Rewriting_Handle
               ; Ada_Subp_F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Prelude
           (Handle : Rewriting_Handle
               ; Ada_Prelude_F_Context_Clauses : Node_Rewriting_Handle
               ; Ada_Prelude_F_Library_Item : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Separate
           (Handle : Rewriting_Handle
               ; Ada_Separate_F_Parent_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_With_Formal
           (Handle : Rewriting_Handle
               ; Ada_With_Formal_F_Kind : Node_Rewriting_Handle
               ; Ada_With_Formal_F_Skips : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Attribute_Decl
           (Handle : Rewriting_Handle
               ; Attribute_Decl_F_Attr_Name : Node_Rewriting_Handle
               ; Attribute_Decl_F_Attr_Index : Node_Rewriting_Handle
               ; Attribute_Decl_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Attribute_Reference
           (Handle : Rewriting_Handle
               ; Attribute_Reference_F_Attribute_Name : Node_Rewriting_Handle
               ; Attribute_Reference_F_Attribute_Index : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Builtin_Function_Call
           (Handle : Rewriting_Handle
               ; Builtin_Function_Call_F_Function_Name : Node_Rewriting_Handle
               ; Builtin_Function_Call_F_Parameters : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Case_Construction
           (Handle : Rewriting_Handle
               ; Case_Construction_F_Var_Ref : Node_Rewriting_Handle
               ; Case_Construction_F_Items : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Case_Item
           (Handle : Rewriting_Handle
               ; Case_Item_F_Choice : Node_Rewriting_Handle
               ; Case_Item_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Compilation_Unit
           (Handle : Rewriting_Handle
               ; Compilation_Unit_F_Project : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Prefix
           (Handle : Rewriting_Handle
               ; Prefix_F_Prefix : Node_Rewriting_Handle
               ; Prefix_F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Package_Decl
           (Handle : Rewriting_Handle
               ; Package_Decl_F_Pkg_Name : Node_Rewriting_Handle
               ; Package_Decl_F_Pkg_Spec : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Package_Extension
           (Handle : Rewriting_Handle
               ; Package_Extension_F_Extended_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Package_Renaming
           (Handle : Rewriting_Handle
               ; Package_Renaming_F_Renamed_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Package_Spec
           (Handle : Rewriting_Handle
               ; Package_Spec_F_Extension : Node_Rewriting_Handle
               ; Package_Spec_F_Decls : Node_Rewriting_Handle
               ; Package_Spec_F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Project
           (Handle : Rewriting_Handle
               ; Project_F_Context_Clauses : Node_Rewriting_Handle
               ; Project_F_Project_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Project_Declaration
           (Handle : Rewriting_Handle
               ; Project_Declaration_F_Qualifier : Node_Rewriting_Handle
               ; Project_Declaration_F_Project_Name : Node_Rewriting_Handle
               ; Project_Declaration_F_Extension : Node_Rewriting_Handle
               ; Project_Declaration_F_Decls : Node_Rewriting_Handle
               ; Project_Declaration_F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Project_Extension
           (Handle : Rewriting_Handle
               ; Project_Extension_F_Is_All : Node_Rewriting_Handle
               ; Project_Extension_F_Path_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_String_Literal_At
           (Handle : Rewriting_Handle
               ; String_Literal_At_F_Str_Lit : Node_Rewriting_Handle
               ; String_Literal_At_F_At_Lit : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Terms
           (Handle : Rewriting_Handle
               ; Terms_F_Terms : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Type_Reference
           (Handle : Rewriting_Handle
               ; Type_Reference_F_Var_Type_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Typed_String_Decl
           (Handle : Rewriting_Handle
               ; Typed_String_Decl_F_Type_Id : Node_Rewriting_Handle
               ; Typed_String_Decl_F_String_Literals : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Variable_Decl
           (Handle : Rewriting_Handle
               ; Variable_Decl_F_Var_Name : Node_Rewriting_Handle
               ; Variable_Decl_F_Var_Type : Node_Rewriting_Handle
               ; Variable_Decl_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Variable_Reference
           (Handle : Rewriting_Handle
               ; Variable_Reference_F_Variable_Name : Node_Rewriting_Handle
               ; Variable_Reference_F_Attribute_Ref : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_With_Decl
           (Handle : Rewriting_Handle
               ; With_Decl_F_Is_Limited : Node_Rewriting_Handle
               ; With_Decl_F_Path_Names : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


end Gpr_Parser.Rewriting_Implementation;
