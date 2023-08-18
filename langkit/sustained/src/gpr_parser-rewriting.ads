
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


--  This package provides support for tree-based source code rewriting.
--
--  .. ATTENTION:: This is an experimental feature, so even if it is exposed to
--  allow experiments, it is totally unsupported and the API is very likely to
--  change in the future.

with System;

with Gpr_Parser_Support.Generic_API.Introspection;
use Gpr_Parser_Support.Generic_API.Introspection;

with Gpr_Parser.Analysis; use Gpr_Parser.Analysis;
with Gpr_Parser.Common;   use Gpr_Parser.Common;
with Gpr_Parser.Generic_API.Introspection;
use Gpr_Parser.Generic_API.Introspection;

package Gpr_Parser.Rewriting is

   use Support.Diagnostics, Support.Text;

   type Rewriting_Handle is private;
   --  Handle for an analysis context rewriting session

   type Unit_Rewriting_Handle is private;
   --  Handle for the process of rewriting an analysis unit. Such handles are
   --  owned by a Rewriting_Handle instance.

   type Node_Rewriting_Handle is private;
   --  Handle for the process of rewriting an AST node. Such handles are owned
   --  by a Rewriting_Handle instance.

   No_Rewriting_Handle      : constant Rewriting_Handle;
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle;
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle;

   type Unit_Rewriting_Handle_Array is
      array (Positive range <>) of Unit_Rewriting_Handle;

   type Node_Rewriting_Handle_Array is
      array (Positive range <>) of Node_Rewriting_Handle;

   -----------------------
   -- Context rewriting --
   -----------------------

   function Handle (Context : Analysis_Context) return Rewriting_Handle;
   --  Return the rewriting handle associated to Context, or
   --  No_Rewriting_Handle if Context is not being rewritten.

   function Context (Handle : Rewriting_Handle) return Analysis_Context;
   --  Return the analysis context associated to Handle

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle;
   --  Start a rewriting session for Context.
   --
   --  This handle will keep track of all changes to do on Context's analysis
   --  units. Once the set of changes is complete, call the Apply procedure to
   --  actually update Context. This makes it possible to inspect the "old"
   --  Context state while creating the list of changes.
   --
   --  There can be only one rewriting session per analysis context, so this
   --  will raise an Existing_Rewriting_Handle_Error exception if Context
   --  already has a living rewriting session.

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle);
   --  Discard all modifications registered in Handle and close Handle. This
   --  invalidates all related unit/node handles.

   type Apply_Result (Success : Boolean := True) is record
      case Success is
         when False =>
            Unit : Analysis_Unit;
            --  Reference to the analysis unit that was being processed when
            --  the error occurred.

            Diagnostics : Diagnostics_Vectors.Vector;
            --  Corresponding list of error messages
         when True => null;
      end case;
   end record;

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result;
   --  Apply all modifications to Handle's analysis context. If that worked,
   --  close Handle and return (Success => True). Otherwise, reparsing did not
   --  work, so keep Handle and its Context unchanged and return details about
   --  the error that happened.
   --
   --  Note that on success, this invalidates all related unit/node handles.

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array;
   --  Return the list of unit rewriting handles in the given context handle
   --  for units that the Apply primitive will modify.

   --------------------
   -- Unit rewriting --
   --------------------

   function Handle (Unit : Analysis_Unit) return Unit_Rewriting_Handle;
   --  Return the rewriting handle corresponding to Unit

   function Unit (Handle : Unit_Rewriting_Handle) return Analysis_Unit;
   --  Return the unit corresponding to Handle

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Return the node handle corresponding to the root of the unit which
   --  Handle designates.

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle);
   --  Set the root node for the unit Handle to Root. This unties the previous
   --  root handle. If Root is not No_Node_Rewriting_Handle, this also ties
   --  Root to Handle.
   --
   --  Root must not already be tied to another analysis unit handle.

   function Unparse (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type;
   --  Return the text associated to the given unit.

   --------------------
   -- Node rewriting --
   --------------------

   function Handle
     (Node : Gpr_Node'Class) return Node_Rewriting_Handle;
   --  Return the rewriting handle corresponding to Node.
   --
   --  The owning unit of Node must be free of diagnostics.

   function Node
     (Handle : Node_Rewriting_Handle) return Gpr_Node;
   --  Return the node which the given rewriting Handle relates to. This can be
   --  the null entity if this handle designates a new node.

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle;
   --  Return a handle for the rewriting context to which Handle belongs

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Turn the given rewritten node Handles designates into text. This is the
   --  text that is used in Apply in order to re-create an analysis unit.

   function Kind (Handle : Node_Rewriting_Handle) return Gpr_Node_Kind_Type;
   --  Return the kind corresponding to Handle's node

   function Type_Of (Handle : Node_Rewriting_Handle) return Type_Ref
   is (Kind_To_Type (Kind (Handle)));
   --  Return the introspection type reference corresponding to ``Handle``'s
   --  node.

   function Tied (Handle : Node_Rewriting_Handle) return Boolean;
   --  Return whether this node handle is tied to an analysis unit. If it is
   --  not, it can be passed as the Child parameter to Set_Child.

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Return a handle for the node that is the parent of Handle's node. This
   --  is ``No_Rewriting_Handle`` for a node that is not tied to any tree yet.

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural;
   --  Return the number of children the node represented by Handle has

   function Child_Index
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref) return Positive
   is (Syntax_Field_Index (Field, Type_Of (Handle)));
   --  Return the index of ``Handle``'s ``Child`` that correspond to the given
   --  ``Field``.

   function Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive) return Node_Rewriting_Handle;
   --  Return a handle corresponding to the Index'th child of the node that
   --  Handle represents. Index is 1-based.

   function Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref) return Node_Rewriting_Handle;
   --  Return the node that is in the syntax ``Field`` for ``Handle``

   function Child
     (Handle : Node_Rewriting_Handle;
      Fields : Struct_Member_Ref_Array) return Node_Rewriting_Handle;
   --  Return a child deep in the tree ``Handle``.
   --
   --  Assuming ``Fields'Range`` is ``1 .. N``, this is a shortcut for:
   --
   --  .. code::
   --
   --     C1 := Child (Handle, Fields (1));
   --     C2 := Child (C1, Fields (2));
   --     ...
   --     CN_1 := Child (CN_2, Fields (N - 1));
   --     CN := Child (CN_1, Fields (N));

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle);
   --  If Child is ``No_Rewriting_Node``, untie the Handle's ``Index``'th child
   --  to this tree, so it can be attached to another one. Otherwise, Child
   --  must have no parent as it will be tied to ``Handle``'s tree.

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref;
      Child  : Node_Rewriting_Handle);
   --  If ``Child`` is ``No_Rewriting_Node``, untie the syntax field in
   --  ``Handle`` corresponding to ``Field``, so it can be attached to another
   --  one. Otherwise, ``Child`` must have no parent as it will be tied to
   --  ``Handle``'s tree.

   function Text (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Return the text associated to the given token node.

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type);
   --  Override text associated to the given token node.

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle);
   --  If Handle is the root of an analysis unit, untie it and set New_Node as
   --  its new root. Otherwise, replace Handle with New_Node in Handle's parent
   --  node.
   --
   --  Note that: * Handle must be tied to an existing analysis unit handle. *
   --  New_Node must not already be tied to another analysis unit handle.

   -------------------------
   -- List node rewriting --
   -------------------------

   procedure Insert_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle);
   --  Assuming Handle refers to a list node, insert the given Child node to be
   --  in the children list at the given index.
   --
   --  The given Child node must not be tied to any analysis unit.

   procedure Append_Child
     (Handle : Node_Rewriting_Handle;
      Child  : Node_Rewriting_Handle);
   --  Assuming Handle refers to a list node, append the given Child node to
   --  the children list.
   --
   --  The given Child node must not be tied to any analysis unit.

   procedure Remove_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive);
   --  Assuming Handle refers to a list node, remove the child at the given
   --  Index from the children list.

   -------------------
   -- Node creation --
   -------------------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Create a clone of the Handle node tree. The result is not tied to any
   --  analysis unit tree.

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : Gpr_Node_Kind_Type) return Node_Rewriting_Handle;
   --  Create a new node of the given Kind, with empty text (for token nodes)
   --  or children (for regular nodes).

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : Gpr_Node_Kind_Type;
      Text   : Text_Type) return Node_Rewriting_Handle;
   --  Create a new token node with the given Kind and Text

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : Gpr_Node_Kind_Type;
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle;
   --  Create a new regular node of the given Kind and assign it the given
   --  Children.
   --
   --  Except for lists, which can have any number of children, the size of
   --  Children must match the number of children associated to the given Kind.
   --  Besides, all given children must not be tied.

   ---------------
   -- Templates --
   ---------------

   --  Templating is a way to create trees of node rewriting handles. It is
   --  intended to be more convenient than calling node constructors for each
   --  individual node in a tree.
   --
   --  A template is text that represents source code, including zero or
   --  multiple placeholders (stray "{}").
   --
   --  Create a tree of new nodes from a template is called instantiating a
   --  template: just call Create_From_Template, passing to it the template
   --  itself, a sequence of nodes (the template arguments) to fill the
   --  template placeholders and a grammar rule to parse the resulting source
   --  code. This will unparse given nodes to replace placeholders in the
   --  template text, and then parse the resulting source code in order to
   --  create a tree of node rewriting handles.
   --
   --  In order not to interfer with the template DSL, stray "{" and "}"
   --  characters in the source code must be doubled: for instance "{{"
   --  represent "{" in the source code to be parsed.

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule) return Node_Rewriting_Handle;
   --  Create a tree of new nodes from the given Template string, replacing
   --  placeholders with nodes in Arguments and parsed according to the given
   --  grammar Rule.

   -----------------------------
   -- Node creation shortcuts --
   -----------------------------



         function Create_Ada_Access_Subp
           (Handle : Rewriting_Handle
               ; F_Subp_Kind : Node_Rewriting_Handle
               ; F_Skips : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Pragma
           (Handle : Rewriting_Handle
               ; F_Skips : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Use
           (Handle : Rewriting_Handle
               ; F_Skips : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_With
           (Handle : Rewriting_Handle
               ; F_Has_Limited : Node_Rewriting_Handle
               ; F_Has_Private : Node_Rewriting_Handle
               ; F_Packages : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Generic
           (Handle : Rewriting_Handle
               ; F_Skips : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Library_Item
           (Handle : Rewriting_Handle
               ; F_Generic_Stub : Node_Rewriting_Handle
               ; F_Separate : Node_Rewriting_Handle
               ; F_Main : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Pkg
           (Handle : Rewriting_Handle
               ; F_Has_Private : Node_Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Pkg_Body
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Subp
           (Handle : Rewriting_Handle
               ; F_Subp_Kind : Node_Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Prelude
           (Handle : Rewriting_Handle
               ; F_Context_Clauses : Node_Rewriting_Handle
               ; F_Library_Item : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_Separate
           (Handle : Rewriting_Handle
               ; F_Parent_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ada_With_Formal
           (Handle : Rewriting_Handle
               ; F_Kind : Node_Rewriting_Handle
               ; F_Skips : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Attribute_Decl
           (Handle : Rewriting_Handle
               ; F_Attr_Name : Node_Rewriting_Handle
               ; F_Attr_Index : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Attribute_Reference
           (Handle : Rewriting_Handle
               ; F_Attribute_Name : Node_Rewriting_Handle
               ; F_Attribute_Index : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Builtin_Function_Call
           (Handle : Rewriting_Handle
               ; F_Function_Name : Node_Rewriting_Handle
               ; F_Parameters : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Case_Construction
           (Handle : Rewriting_Handle
               ; F_Var_Ref : Node_Rewriting_Handle
               ; F_Items : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Case_Item
           (Handle : Rewriting_Handle
               ; F_Choice : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Compilation_Unit
           (Handle : Rewriting_Handle
               ; F_Project : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Prefix
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Package_Decl
           (Handle : Rewriting_Handle
               ; F_Pkg_Name : Node_Rewriting_Handle
               ; F_Pkg_Spec : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Package_Extension
           (Handle : Rewriting_Handle
               ; F_Extended_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Package_Renaming
           (Handle : Rewriting_Handle
               ; F_Renamed_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Package_Spec
           (Handle : Rewriting_Handle
               ; F_Extension : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Project
           (Handle : Rewriting_Handle
               ; F_Context_Clauses : Node_Rewriting_Handle
               ; F_Project_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Project_Declaration
           (Handle : Rewriting_Handle
               ; F_Qualifier : Node_Rewriting_Handle
               ; F_Project_Name : Node_Rewriting_Handle
               ; F_Extension : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Project_Extension
           (Handle : Rewriting_Handle
               ; F_Is_All : Node_Rewriting_Handle
               ; F_Path_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_String_Literal_At
           (Handle : Rewriting_Handle
               ; F_Str_Lit : Node_Rewriting_Handle
               ; F_At_Lit : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Terms
           (Handle : Rewriting_Handle
               ; F_Terms : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Type_Reference
           (Handle : Rewriting_Handle
               ; F_Var_Type_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Typed_String_Decl
           (Handle : Rewriting_Handle
               ; F_Type_Id : Node_Rewriting_Handle
               ; F_String_Literals : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Variable_Decl
           (Handle : Rewriting_Handle
               ; F_Var_Name : Node_Rewriting_Handle
               ; F_Var_Type : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Variable_Reference
           (Handle : Rewriting_Handle
               ; F_Variable_Name : Node_Rewriting_Handle
               ; F_Attribute_Ref : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_With_Decl
           (Handle : Rewriting_Handle
               ; F_Is_Limited : Node_Rewriting_Handle
               ; F_Path_Names : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


private

   --  Workaround S114-026 by not deriving from Impl.Rewriting_Handle directly.
   --  TODO: Cleanup once S114-026 is fixed.
   type Rewriting_Handle is new System.Address;
   type Unit_Rewriting_Handle is new System.Address;
   type Node_Rewriting_Handle is new System.Address;

   No_Rewriting_Handle : constant Rewriting_Handle :=
      Rewriting_Handle (System.Null_Address);
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle :=
      Unit_Rewriting_Handle (System.Null_Address);
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle :=
      Node_Rewriting_Handle (System.Null_Address);

end Gpr_Parser.Rewriting;
