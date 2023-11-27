
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


with Ada.Unchecked_Conversion;

with Gpr_Parser.Common;

with Gpr_Parser.Public_Converters; use Gpr_Parser.Public_Converters;
with Gpr_Parser.Rewriting_Implementation;

package body Gpr_Parser.Rewriting is

   package Impl renames Gpr_Parser.Rewriting_Implementation;

   function Unwrap_RH is new Ada.Unchecked_Conversion
     (Rewriting_Handle, Impl.Rewriting_Handle);
   function Wrap_RH is new Ada.Unchecked_Conversion
     (Impl.Rewriting_Handle, Rewriting_Handle);

   function Unwrap_Node_RH is new Ada.Unchecked_Conversion
     (Node_Rewriting_Handle, Impl.Node_Rewriting_Handle);
   function Wrap_Node_RH is new Ada.Unchecked_Conversion
     (Impl.Node_Rewriting_Handle, Node_Rewriting_Handle);

   function Unwrap_Unit_RH is new Ada.Unchecked_Conversion
     (Unit_Rewriting_Handle, Impl.Unit_Rewriting_Handle);
   function Wrap_Unit_RH is new Ada.Unchecked_Conversion
     (Impl.Unit_Rewriting_Handle, Unit_Rewriting_Handle);

   function Wrap_Apply_Result
     (Res : Impl.Apply_Result) return Apply_Result;

   function Wrap_Unit_RH_Array
     (Arr : Impl.Unit_Rewriting_Handle_Array)
      return Unit_Rewriting_Handle_Array;

   function Wrap_Node_RH_Array
     (Arr : Impl.Node_Rewriting_Handle_Array)
      return Node_Rewriting_Handle_Array;
   function Unwrap_Node_RH_Array
     (Arr : Node_Rewriting_Handle_Array)
      return Impl.Node_Rewriting_Handle_Array;

   function Wrap_Apply_Result
     (Res : Impl.Apply_Result) return Apply_Result is
   begin
      if Res.Success then
         return (Success => True);
      else
         return
           (Success     => False,
            Unit        => Wrap_Unit (Res.Unit),
            Diagnostics => Res.Diagnostics);
      end if;
   end Wrap_Apply_Result;

   ------------------------
   -- Wrap_Unit_RH_Array --
   ------------------------

   function Wrap_Unit_RH_Array
     (Arr : Impl.Unit_Rewriting_Handle_Array)
      return Unit_Rewriting_Handle_Array
   is
      Res : Unit_Rewriting_Handle_Array (Arr'Range);
   begin
      for I in Arr'Range loop
         Res (I) := Wrap_Unit_RH (Arr (I));
      end loop;
      return Res;
   end Wrap_Unit_RH_Array;

   ------------------------
   -- Wrap_Node_RH_Array --
   ------------------------

   function Wrap_Node_RH_Array
     (Arr : Impl.Node_Rewriting_Handle_Array)
      return Node_Rewriting_Handle_Array
   is
      Res : Node_Rewriting_Handle_Array (Arr'Range);
   begin
      for I in Arr'Range loop
         Res (I) := Wrap_Node_RH (Arr (I));
      end loop;
      return Res;
   end Wrap_Node_RH_Array;

   --------------------------
   -- Unwrap_Node_RH_Array --
   --------------------------

   function Unwrap_Node_RH_Array
     (Arr : Node_Rewriting_Handle_Array)
      return Impl.Node_Rewriting_Handle_Array
   is
      Res : Impl.Node_Rewriting_Handle_Array (Arr'Range);
   begin
      for I in Arr'Range loop
         Res (I) := Unwrap_Node_RH (Arr (I));
      end loop;
      return Res;
   end Unwrap_Node_RH_Array;

   ------------
   -- Handle --
   ------------

   function Handle (Context : Analysis_Context) return Rewriting_Handle is
   begin
      return Wrap_RH (Impl.Handle (Unwrap_Context (Context)));
   end Handle;

   -------------
   -- Context --
   -------------

   function Context (Handle : Rewriting_Handle) return Analysis_Context is
   begin
      return Wrap_Context (Impl.Context (Unwrap_RH (Handle)));
   end Context;

   ---------------------
   -- Start_Rewriting --
   ---------------------

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle is
   begin
      return Wrap_RH (Impl.Start_Rewriting (Unwrap_Context (Context)));
   end Start_Rewriting;

   ---------------------
   -- Abort_Rewriting --
   ---------------------

   procedure Abort_Rewriting
     (Handle          : in out Rewriting_Handle)
   is
      Internal_Handle : Impl.Rewriting_Handle := Unwrap_RH (Handle);
   begin
      Impl.Abort_Rewriting (Internal_Handle);
      Handle := Wrap_RH (Internal_Handle);
   end Abort_Rewriting;

   -----------
   -- Apply --
   -----------

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result is
      Internal_Handle : Impl.Rewriting_Handle := Unwrap_RH (Handle);
      Res             : Impl.Apply_Result := Impl.Apply (Internal_Handle);
   begin
      Handle := Wrap_RH (Internal_Handle);
      return Wrap_Apply_Result (Res);
   end Apply;

   ------------------
   -- Unit_Handles --
   ------------------

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array is
   begin
      return Wrap_Unit_RH_Array (Impl.Unit_Handles (Unwrap_RH (Handle)));
   end Unit_Handles;

   ------------
   -- Handle --
   ------------

   function Handle (Unit : Analysis_Unit) return Unit_Rewriting_Handle is
   begin
      return Wrap_Unit_RH (Impl.Handle (Unwrap_Unit (Unit)));
   end Handle;

   ----------
   -- Unit --
   ----------

   function Unit (Handle : Unit_Rewriting_Handle) return Analysis_Unit is
   begin
      return Wrap_Unit (Impl.Unit (Unwrap_Unit_RH (Handle)));
   end Unit;

   ----------
   -- Root --
   ----------

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH (Impl.Root (Unwrap_Unit_RH (Handle)));
   end Root;

   --------------
   -- Set_Root --
   --------------

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle) is
   begin
      Impl.Set_Root (Unwrap_Unit_RH (Handle), Unwrap_Node_RH (Root));
   end Set_Root;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type is
   begin
      return Impl.Unparse (Unwrap_Unit_RH (Handle));
   end Unparse;

   ------------
   -- Handle --
   ------------

   function Handle
     (Node : Gpr_Node'Class) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Handle (Unwrap_Node (Node)));
   end Handle;

   ----------
   -- Node --
   ----------

   function Node
     (Handle : Node_Rewriting_Handle) return Gpr_Node is
   begin
      return Wrap_Node (Impl.Node (Unwrap_Node_RH (Handle)));
   end Node;

   -------------
   -- Context --
   -------------

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle is
   begin
      return Wrap_RH (Impl.Context (Unwrap_Node_RH (Handle)));
   end Context;

   -------------
   -- Unparse --
   -------------

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      return Impl.Unparse (Unwrap_Node_RH (Handle));
   end Unparse;

   ----------
   -- Kind --
   ----------

   function Kind (Handle : Node_Rewriting_Handle) return Gpr_Node_Kind_Type is
   begin
      return Impl.Kind (Unwrap_Node_RH (Handle));
   end Kind;

   -----------
   -- Image --
   -----------

   function Image (Handle : Node_Rewriting_Handle) return String is
   begin
      return Impl.Image (Unwrap_Node_RH (Handle));
   end Image;

   ----------
   -- Tied --
   ----------

   function Tied (Handle : Node_Rewriting_Handle) return Boolean is
   begin
      return Impl.Tied (Unwrap_Node_RH (Handle));
   end Tied;

   ------------
   -- Parent --
   ------------

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Parent (Unwrap_Node_RH (Handle)));
   end Parent;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural is
   begin
      return Impl.Children_Count (Unwrap_Node_RH (Handle));
   end Children_Count;

   -----------
   -- Child --
   -----------

   function Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Child (Unwrap_Node_RH (Handle), Field));
   end Child;

   -----------
   -- Child --
   -----------

   function Child
     (Handle : Node_Rewriting_Handle;
      Fields : Struct_Member_Ref_Array) return Node_Rewriting_Handle is
   begin
      return Result : Node_Rewriting_Handle := Handle do
         for F of Fields loop
            Result := Child (Result, F);
         end loop;
      end return;
   end Child;

   --------------
   -- Children --
   --------------

   function Children
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle_Array
   is
   begin
      return Wrap_Node_RH_Array (Impl.Children (Unwrap_Node_RH (Handle)));
   end Children;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref;
      Child  : Node_Rewriting_Handle)
   is
   begin
      Impl.Set_Child (Unwrap_Node_RH (Handle), Field, Unwrap_Node_RH (Child));
   end Set_Child;

   ----------
   -- Text --
   ----------

   function Text (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      return Impl.Text (Unwrap_Node_RH (Handle));
   end Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type) is
   begin
      Impl.Set_Text (Unwrap_Node_RH (Handle), Text);
   end Set_Text;

   -------------
   -- Replace --
   -------------

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle) is
   begin
      Impl.Replace (Unwrap_Node_RH (Handle), Unwrap_Node_RH (New_Node));
   end Replace;

   ------------
   -- Rotate --
   ------------

   procedure Rotate (Handles : Node_Rewriting_Handle_Array) is
   begin
      Impl.Rotate (Unwrap_Node_RH_Array (Handles));
   end Rotate;

   ------------------
   -- Is_List_Node --
   ------------------

   function Is_List_Node (Handle : Node_Rewriting_Handle) return Boolean is
   begin
      return Impl.Is_List_Node (Unwrap_Node_RH (Handle));
   end Is_List_Node;

   -----------------
   -- First_Child --
   -----------------

   function First_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.First_Child (Unwrap_Node_RH (Handle)));
   end First_Child;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Last_Child (Unwrap_Node_RH (Handle)));
   end Last_Child;

   ----------------
   -- Next_Child --
   ----------------

   function Next_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Next_Child (Unwrap_Node_RH (Handle)));
   end Next_Child;

   --------------------
   -- Previous_Child --
   --------------------

   function Previous_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Previous_Child (Unwrap_Node_RH (Handle)));
   end Previous_Child;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before
     (Handle, New_Sibling : Node_Rewriting_Handle) is
   begin
      Impl.Insert_Before
        (Unwrap_Node_RH (Handle), Unwrap_Node_RH (New_Sibling));
   end Insert_Before;

   ------------------
   -- Insert_After --
   ------------------

   procedure Insert_After
     (Handle, New_Sibling : Node_Rewriting_Handle) is
   begin
      Impl.Insert_After
        (Unwrap_Node_RH (Handle), Unwrap_Node_RH (New_Sibling));
   end Insert_After;

   ------------------
   -- Insert_First --
   ------------------

   procedure Insert_First (Handle, New_Child : Node_Rewriting_Handle) is
   begin
      Impl.Insert_First
        (Unwrap_Node_RH (Handle), Unwrap_Node_RH (New_Child));
   end Insert_First;

   -----------------
   -- Insert_Last --
   -----------------

   procedure Insert_Last (Handle, New_Child : Node_Rewriting_Handle) is
   begin
      Impl.Insert_Last
        (Unwrap_Node_RH (Handle), Unwrap_Node_RH (New_Child));
   end Insert_Last;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child (Handle : Node_Rewriting_Handle) is
   begin
      Impl.Remove_Child (Unwrap_Node_RH (Handle));
   end Remove_Child;

   -----------
   -- Clone --
   -----------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH (Impl.Clone (Unwrap_Node_RH (Handle)));
   end Clone;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : Gpr_Node_Kind_Type) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Create_Node (Unwrap_RH (Handle), Kind));
   end Create_Node;

   -----------------------
   -- Create_Token_Node --
   -----------------------

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : Gpr_Node_Kind_Type;
      Text   : Text_Type) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH
        (Impl.Create_Token_Node (Unwrap_RH (Handle), Kind, Text));
   end Create_Token_Node;

   -------------------------
   -- Create_Regular_Node --
   -------------------------

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : Gpr_Node_Kind_Type;
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Create_Regular_Node
        (Unwrap_RH (Handle), Kind, Unwrap_Node_RH_Array (Children)));
   end Create_Regular_Node;

   --------------------------
   -- Create_From_Template --
   --------------------------

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Create_From_Template
        (Unwrap_RH (Handle),
         Template,
         Unwrap_Node_RH_Array (Arguments),
         Rule));
   end Create_From_Template;


         function Create_Attribute_Decl
           (Handle : Rewriting_Handle
               ; F_Attr_Name : Node_Rewriting_Handle
               ; F_Attr_Index : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Attribute_Decl
               (Unwrap_RH (Handle),
                Attribute_Decl_F_Attr_Name => Unwrap_Node_RH (F_Attr_Name), Attribute_Decl_F_Attr_Index => Unwrap_Node_RH (F_Attr_Index), Attribute_Decl_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Attribute_Reference
           (Handle : Rewriting_Handle
               ; F_Attribute_Name : Node_Rewriting_Handle
               ; F_Attribute_Index : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Attribute_Reference
               (Unwrap_RH (Handle),
                Attribute_Reference_F_Attribute_Name => Unwrap_Node_RH (F_Attribute_Name), Attribute_Reference_F_Attribute_Index => Unwrap_Node_RH (F_Attribute_Index)));
         end;


         function Create_Builtin_Function_Call
           (Handle : Rewriting_Handle
               ; F_Function_Name : Node_Rewriting_Handle
               ; F_Parameters : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Builtin_Function_Call
               (Unwrap_RH (Handle),
                Builtin_Function_Call_F_Function_Name => Unwrap_Node_RH (F_Function_Name), Builtin_Function_Call_F_Parameters => Unwrap_Node_RH (F_Parameters)));
         end;


         function Create_Case_Construction
           (Handle : Rewriting_Handle
               ; F_Var_Ref : Node_Rewriting_Handle
               ; F_Items : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Case_Construction
               (Unwrap_RH (Handle),
                Case_Construction_F_Var_Ref => Unwrap_Node_RH (F_Var_Ref), Case_Construction_F_Items => Unwrap_Node_RH (F_Items)));
         end;


         function Create_Case_Item
           (Handle : Rewriting_Handle
               ; F_Choice : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Case_Item
               (Unwrap_RH (Handle),
                Case_Item_F_Choice => Unwrap_Node_RH (F_Choice), Case_Item_F_Decls => Unwrap_Node_RH (F_Decls)));
         end;


         function Create_Compilation_Unit
           (Handle : Rewriting_Handle
               ; F_Project : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Compilation_Unit
               (Unwrap_RH (Handle),
                Compilation_Unit_F_Project => Unwrap_Node_RH (F_Project)));
         end;


         function Create_Prefix
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Prefix
               (Unwrap_RH (Handle),
                Prefix_F_Prefix => Unwrap_Node_RH (F_Prefix), Prefix_F_Suffix => Unwrap_Node_RH (F_Suffix)));
         end;


         function Create_Package_Decl
           (Handle : Rewriting_Handle
               ; F_Pkg_Name : Node_Rewriting_Handle
               ; F_Pkg_Spec : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Package_Decl
               (Unwrap_RH (Handle),
                Package_Decl_F_Pkg_Name => Unwrap_Node_RH (F_Pkg_Name), Package_Decl_F_Pkg_Spec => Unwrap_Node_RH (F_Pkg_Spec)));
         end;


         function Create_Package_Extension
           (Handle : Rewriting_Handle
               ; F_Extended_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Package_Extension
               (Unwrap_RH (Handle),
                Package_Extension_F_Extended_Name => Unwrap_Node_RH (F_Extended_Name)));
         end;


         function Create_Package_Renaming
           (Handle : Rewriting_Handle
               ; F_Renamed_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Package_Renaming
               (Unwrap_RH (Handle),
                Package_Renaming_F_Renamed_Name => Unwrap_Node_RH (F_Renamed_Name)));
         end;


         function Create_Package_Spec
           (Handle : Rewriting_Handle
               ; F_Extension : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Package_Spec
               (Unwrap_RH (Handle),
                Package_Spec_F_Extension => Unwrap_Node_RH (F_Extension), Package_Spec_F_Decls => Unwrap_Node_RH (F_Decls), Package_Spec_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_Project
           (Handle : Rewriting_Handle
               ; F_Context_Clauses : Node_Rewriting_Handle
               ; F_Project_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Project
               (Unwrap_RH (Handle),
                Project_F_Context_Clauses => Unwrap_Node_RH (F_Context_Clauses), Project_F_Project_Decl => Unwrap_Node_RH (F_Project_Decl)));
         end;


         function Create_Project_Declaration
           (Handle : Rewriting_Handle
               ; F_Qualifier : Node_Rewriting_Handle
               ; F_Project_Name : Node_Rewriting_Handle
               ; F_Extension : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Project_Declaration
               (Unwrap_RH (Handle),
                Project_Declaration_F_Qualifier => Unwrap_Node_RH (F_Qualifier), Project_Declaration_F_Project_Name => Unwrap_Node_RH (F_Project_Name), Project_Declaration_F_Extension => Unwrap_Node_RH (F_Extension), Project_Declaration_F_Decls => Unwrap_Node_RH (F_Decls), Project_Declaration_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_Project_Extension
           (Handle : Rewriting_Handle
               ; F_Is_All : Node_Rewriting_Handle
               ; F_Path_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Project_Extension
               (Unwrap_RH (Handle),
                Project_Extension_F_Is_All => Unwrap_Node_RH (F_Is_All), Project_Extension_F_Path_Name => Unwrap_Node_RH (F_Path_Name)));
         end;


         function Create_String_Literal_At
           (Handle : Rewriting_Handle
               ; F_Str_Lit : Node_Rewriting_Handle
               ; F_At_Lit : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_String_Literal_At
               (Unwrap_RH (Handle),
                String_Literal_At_F_Str_Lit => Unwrap_Node_RH (F_Str_Lit), String_Literal_At_F_At_Lit => Unwrap_Node_RH (F_At_Lit)));
         end;


         function Create_Terms
           (Handle : Rewriting_Handle
               ; F_Terms : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Terms
               (Unwrap_RH (Handle),
                Terms_F_Terms => Unwrap_Node_RH (F_Terms)));
         end;


         function Create_Type_Reference
           (Handle : Rewriting_Handle
               ; F_Var_Type_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Type_Reference
               (Unwrap_RH (Handle),
                Type_Reference_F_Var_Type_Name => Unwrap_Node_RH (F_Var_Type_Name)));
         end;


         function Create_Typed_String_Decl
           (Handle : Rewriting_Handle
               ; F_Type_Id : Node_Rewriting_Handle
               ; F_String_Literals : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Typed_String_Decl
               (Unwrap_RH (Handle),
                Typed_String_Decl_F_Type_Id => Unwrap_Node_RH (F_Type_Id), Typed_String_Decl_F_String_Literals => Unwrap_Node_RH (F_String_Literals)));
         end;


         function Create_Variable_Decl
           (Handle : Rewriting_Handle
               ; F_Var_Name : Node_Rewriting_Handle
               ; F_Var_Type : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Variable_Decl
               (Unwrap_RH (Handle),
                Variable_Decl_F_Var_Name => Unwrap_Node_RH (F_Var_Name), Variable_Decl_F_Var_Type => Unwrap_Node_RH (F_Var_Type), Variable_Decl_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Variable_Reference
           (Handle : Rewriting_Handle
               ; F_Variable_Name : Node_Rewriting_Handle
               ; F_Attribute_Ref : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Variable_Reference
               (Unwrap_RH (Handle),
                Variable_Reference_F_Variable_Name => Unwrap_Node_RH (F_Variable_Name), Variable_Reference_F_Attribute_Ref => Unwrap_Node_RH (F_Attribute_Ref)));
         end;


         function Create_With_Decl
           (Handle : Rewriting_Handle
               ; F_Is_Limited : Node_Rewriting_Handle
               ; F_Path_Names : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_With_Decl
               (Unwrap_RH (Handle),
                With_Decl_F_Is_Limited => Unwrap_Node_RH (F_Is_Limited), With_Decl_F_Path_Names => Unwrap_Node_RH (F_Path_Names)));
         end;


end Gpr_Parser.Rewriting;
