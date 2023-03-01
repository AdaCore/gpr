
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--




package body Gpr_Parser.Introspection_Implementation is

   ----------------
   -- As_Boolean --
   ----------------

   function As_Boolean (Self : Internal_Value) return Boolean is
   begin
      return Self.Boolean_Value;
   end As_Boolean;

   ----------------
   -- As_Integer --
   ----------------

   function As_Integer (Self : Internal_Value) return Integer is
   begin
      return Self.Integer_Value;
   end As_Integer;

   ------------------
   -- As_Character --
   ------------------

   function As_Character (Self : Internal_Value) return Character_Type is
   begin
      return Self.Character_Value;
   end As_Character;

   ---------------
   -- As_String --
   ---------------

   function As_String (Self : Internal_Value) return String_Type is
   begin
      return Self.String_Value;
   end As_String;

   -------------
   -- As_Node --
   -------------

   function As_Node (Self : Internal_Value) return Internal_Entity is
   begin
      return Self.Node_Value;
   end As_Node;

      function As_Analysis_Unit_Kind
        (Self : Internal_Value) return Analysis_Unit_Kind is
      begin
         return Self.Analysis_Unit_Kind_Value;
      end As_Analysis_Unit_Kind;

      function As_Lookup_Kind
        (Self : Internal_Value) return Lookup_Kind is
      begin
         return Self.Lookup_Kind_Value;
      end As_Lookup_Kind;

      function As_Designated_Env_Kind
        (Self : Internal_Value) return Designated_Env_Kind is
      begin
         return Self.Designated_Env_Kind_Value;
      end As_Designated_Env_Kind;

      function As_Grammar_Rule
        (Self : Internal_Value) return Grammar_Rule is
      begin
         return Self.Grammar_Rule_Value;
      end As_Grammar_Rule;


   --  Now we can emit descriptor tables

   ----------------------
   -- Struct_Type_Desc --
   ----------------------

   function Struct_Type_Desc
     (Kind : Struct_Value_Kind) return Struct_Type_Descriptor_Access
   is
   begin
         pragma Unreferenced (Kind);
         return (raise Program_Error);
   end Struct_Type_Desc;

   -----------------------
   -- Struct_Field_Name --
   -----------------------

   function Struct_Field_Name (Field : Struct_Field_Reference) return Text_Type
   is
   begin
      pragma Warnings (Off, "value not in range of subtype");
      return To_Text (Struct_Field_Descriptors (Field).Name);
      pragma Warnings (On, "value not in range of subtype");
   end Struct_Field_Name;

   -----------------------
   -- Struct_Field_Type --
   -----------------------

   function Struct_Field_Type
     (Field : Struct_Field_Reference) return Type_Constraint is
   begin
      pragma Warnings (Off, "value not in range of subtype");
      return Struct_Field_Descriptors (Field).Field_Type;
      pragma Warnings (On, "value not in range of subtype");
   end Struct_Field_Type;

   -------------------
   -- Struct_Fields --
   -------------------

   pragma Warnings (Off, "referenced");
   function Struct_Fields
     (Kind : Struct_Value_Kind) return Struct_Field_Reference_Array
   is
      pragma Warnings (On, "referenced");
   begin
         return (raise Program_Error);
   end Struct_Fields;

   --------------
   -- DSL_Name --
   --------------

   function DSL_Name (Id : Node_Type_Id) return Text_Type is
   begin
      return To_Text (To_String (Node_Type_Descriptors (Id).DSL_Name));
   end DSL_Name;

   ---------------------
   -- Lookup_DSL_Name --
   ---------------------

   function Lookup_DSL_Name (Name : Text_Type) return Any_Node_Type_Id is
      use Node_Type_Id_Maps;

      Position : constant Cursor :=
         DSL_Name_To_Node_Type.Find (To_Unbounded_String (Image (Name)));
   begin
      if Has_Element (Position) then
         return Element (Position);
      else
         return None;
      end if;
   end Lookup_DSL_Name;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (Id : Node_Type_Id) return Boolean is
   begin
      return Node_Type_Descriptors (Id).Is_Abstract;
   end Is_Abstract;

   --------------
   -- Kind_For --
   --------------

   function Kind_For (Id : Node_Type_Id) return Gpr_Node_Kind_Type is
      Desc : Node_Type_Descriptor renames Node_Type_Descriptors (Id).all;
   begin
      if Desc.Is_Abstract then
         raise Bad_Type_Error with "trying to get kind for abstract node";
      end if;
      return Desc.Kind;
   end Kind_For;

   --------------------
   -- First_Kind_For --
   --------------------

   function First_Kind_For (Id : Node_Type_Id) return Gpr_Node_Kind_Type is

      --  Look for the leftmost leaf derivation of an abstract node. Langkit
      --  disallows abstract nodes with no concrete derivation, so each time we
      --  see an an abstract node, we know there are concrete derivations down
      --  the tree.
      --
      --  Note that we have to stop at the first concrete node we see because
      --  of the way we sort kinds: the kind of concrete root comes before the
      --  kinds of all its derivations.

      Cur : Node_Type_Id := Id;
   begin
      loop
         declare
            Desc : Node_Type_Descriptor renames
               Node_Type_Descriptors (Cur).all;
         begin
            exit when not Desc.Is_Abstract or else Desc.Derivations'Length = 0;
            Cur := Desc.Derivations (Desc.Derivations'First);
         end;
      end loop;
      return Kind_For (Cur);
   end First_Kind_For;

   -------------------
   -- Last_Kind_For --
   -------------------

   function Last_Kind_For (Id : Node_Type_Id) return Gpr_Node_Kind_Type is

      --  Look for the rightmost leaf derivation. Langkit disallows abstract
      --  nodes with no concrete derivation, so we know that the result is
      --  concrete.

      Cur : Node_Type_Id := Id;
   begin
      loop
         declare
            Desc : Node_Type_Descriptor renames
               Node_Type_Descriptors (Cur).all;
         begin
            exit when Desc.Derivations'Length = 0;
            Cur := Desc.Derivations (Desc.Derivations'Last);
         end;
      end loop;
      return Kind_For (Cur);
   end Last_Kind_For;

   -----------------
   -- Id_For_Kind --
   -----------------

   function Id_For_Kind (Kind : Gpr_Node_Kind_Type) return Node_Type_Id is
   begin
      return Kind_To_Id (Kind);
   end Id_For_Kind;

   ------------------
   -- Is_Root_Node --
   ------------------

   function Is_Root_Node (Id : Node_Type_Id) return Boolean is
   begin
      return Id = Common.Gpr_Node_Type_Id;
   end Is_Root_Node;

   ---------------
   -- Base_Type --
   ---------------

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id is
   begin
      if Is_Root_Node (Id) then
         raise Bad_Type_Error with "trying to get base type of root node";
      end if;
      return Node_Type_Descriptors (Id).Base_Type;
   end Base_Type;

   -------------------
   -- Derived_Types --
   -------------------

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array is
   begin
      return Node_Type_Descriptors (Id).Derivations;
   end Derived_Types;

   ---------------------
   -- Is_Derived_From --
   ---------------------

   function Is_Derived_From (Id, Parent : Node_Type_Id) return Boolean is
      Cursor : Any_Node_Type_Id := Id;
   begin
      while Cursor /= None loop
         if Cursor = Parent then
            return True;
         end if;

         Cursor := Node_Type_Descriptors (Cursor).Base_Type;
      end loop;
      return False;
   end Is_Derived_From;

   -----------------
   -- Member_Name --
   -----------------

   function Member_Name (Member : Member_Reference) return Text_Type is
   begin
      case Member is
         when Struct_Field_Reference =>
            pragma Warnings (Off, "value not in range of type");
            return Struct_Field_Name (Member);
            pragma Warnings (On, "value not in range of type");

         when Syntax_Field_Reference =>
            pragma Warnings (Off, "value not in range of type");
            return Syntax_Field_Name (Member);
            pragma Warnings (On, "value not in range of type");

         when Property_Reference =>
            return Property_Name (Member);
      end case;
   end Member_Name;

   -----------------
   -- Member_Type --
   -----------------

   function Member_Type (Member : Member_Reference) return Type_Constraint is
   begin
      case Member is
         when Struct_Field_Reference =>
            pragma Warnings (Off, "value not in range of type");
            return Struct_Field_Type (Member);
            pragma Warnings (On, "value not in range of type");

         when Syntax_Field_Reference =>
            pragma Warnings (Off, "value not in range of type");
            return (Kind      => Node_Value,
                    Node_Type => Syntax_Field_Type (Member));
            pragma Warnings (On, "value not in range of type");

         when Property_Reference =>
            return Property_Return_Type (Member);
      end case;
   end Member_Type;

   --------------------------
   -- Lookup_Member_Struct --
   --------------------------

   function Lookup_Member_Struct
     (Kind : Struct_Value_Kind;
      Name : Text_Type) return Any_Member_Reference
   is
      pragma Warnings (Off, "value not in range of type");
      Desc : Struct_Type_Descriptor renames Struct_Type_Desc (Kind).all;
      pragma Warnings (On, "value not in range of type");
   begin
      for F of Desc.Fields loop
         if To_Text (F.Name) = Name then
            return F.Reference;
         end if;
      end loop;

      return None;
   end Lookup_Member_Struct;

   ------------------------
   -- Lookup_Member_Node --
   ------------------------

   function Lookup_Member_Node
     (Id   : Node_Type_Id;
      Name : Text_Type) return Any_Member_Reference
   is
      Cursor : Any_Node_Type_Id := Id;
   begin
      --  Go through the derivation chain for Id and look for any field or
      --  property whose name matches Name.

      while Cursor /= None loop
         declare
            Node_Desc : Node_Type_Descriptor renames
               Node_Type_Descriptors (Cursor).all;
         begin
            for F of Node_Desc.Fields loop
               pragma Warnings (Off, "value not in range of type");
               if Syntax_Field_Name (F.Field) = Name then
                  return F.Field;
               end if;
               pragma Warnings (On, "value not in range of type");
            end loop;

            for P of Node_Desc.Properties loop
               if Property_Name (P) = Name then
                  return P;
               end if;
            end loop;

            Cursor := Node_Desc.Base_Type;
         end;
      end loop;
      return None;
   end Lookup_Member_Node;

   -----------------------
   -- Syntax_Field_Name --
   -----------------------

   function Syntax_Field_Name (Field : Syntax_Field_Reference) return Text_Type
   is
   begin
      pragma Warnings (Off, "value not in range of subtype");
      return To_Text (Syntax_Field_Descriptors (Field).Name);
      pragma Warnings (On, "value not in range of subtype");
   end Syntax_Field_Name;

   -----------------------
   -- Syntax_Field_Type --
   -----------------------

   function Syntax_Field_Type
     (Field : Syntax_Field_Reference) return Node_Type_Id is
   begin
      pragma Warnings (Off, "value not in range of subtype");
      return Syntax_Field_Descriptors (Field).Field_Type;
      pragma Warnings (On, "value not in range of subtype");
   end Syntax_Field_Type;

   -----------------------
   -- Eval_Syntax_Field --
   -----------------------

   function Eval_Syntax_Field
     (Node  : Bare_Gpr_Node;
      Field : Syntax_Field_Reference) return Bare_Gpr_Node
   is
      Kind : constant Gpr_Node_Kind_Type := Node.Kind;
   begin
      
      case Gpr_Gpr_Node (Kind) is
when Gpr_Ada_Access_Subp_Range =>
declare
N_Bare_Ada_Access_Subp : constant Bare_Ada_Access_Subp := Node;
begin
case Field is
when Ada_Access_Subp_F_Subp_Kind => return Ada_Access_Subp_F_Subp_Kind (N_Bare_Ada_Access_Subp);
when Ada_Access_Subp_F_Skips => return Ada_Access_Subp_F_Skips (N_Bare_Ada_Access_Subp);
when others => null;
end case;
end;
when Gpr_Ada_Pragma_Range =>
declare
N_Bare_Ada_Pragma : constant Bare_Ada_Pragma := Node;
begin
case Field is
when Ada_Pragma_F_Skips => return Ada_Pragma_F_Skips (N_Bare_Ada_Pragma);
when others => null;
end case;
end;
when Gpr_Ada_Use_Range =>
declare
N_Bare_Ada_Use : constant Bare_Ada_Use := Node;
begin
case Field is
when Ada_Use_F_Skips => return Ada_Use_F_Skips (N_Bare_Ada_Use);
when others => null;
end case;
end;
when Gpr_Ada_With_Range =>
declare
N_Bare_Ada_With : constant Bare_Ada_With := Node;
begin
case Field is
when Ada_With_F_Has_Limited => return Ada_With_F_Has_Limited (N_Bare_Ada_With);
when Ada_With_F_Has_Private => return Ada_With_F_Has_Private (N_Bare_Ada_With);
when Ada_With_F_Packages => return Ada_With_F_Packages (N_Bare_Ada_With);
when others => null;
end case;
end;
when Gpr_Ada_Generic_Range =>
declare
N_Bare_Ada_Generic : constant Bare_Ada_Generic := Node;
begin
case Field is
when Ada_Generic_F_Skips => return Ada_Generic_F_Skips (N_Bare_Ada_Generic);
when others => null;
end case;
end;
when Gpr_Ada_Library_Item_Range =>
declare
N_Bare_Ada_Library_Item : constant Bare_Ada_Library_Item := Node;
begin
case Field is
when Ada_Library_Item_F_Generic_Stub => return Ada_Library_Item_F_Generic_Stub (N_Bare_Ada_Library_Item);
when Ada_Library_Item_F_Separate => return Ada_Library_Item_F_Separate (N_Bare_Ada_Library_Item);
when Ada_Library_Item_F_Main => return Ada_Library_Item_F_Main (N_Bare_Ada_Library_Item);
when others => null;
end case;
end;
when Gpr_Ada_Main =>
declare
N_Bare_Ada_Main : constant Bare_Ada_Main := Node;
begin
case Field is
when Ada_Main_F_Name => return Ada_Main_F_Name (N_Bare_Ada_Main);
when others => null;
end case;
case Gpr_Ada_Main (Kind) is
when Gpr_Ada_Pkg_Range =>
declare
N_Bare_Ada_Pkg : constant Bare_Ada_Pkg := N_Bare_Ada_Main;
begin
case Field is
when Ada_Pkg_F_Has_Private => return Ada_Pkg_F_Has_Private (N_Bare_Ada_Pkg);
when others => null;
end case;
end;
when Gpr_Ada_Subp_Range =>
declare
N_Bare_Ada_Subp : constant Bare_Ada_Subp := N_Bare_Ada_Main;
begin
case Field is
when Ada_Subp_F_Subp_Kind => return Ada_Subp_F_Subp_Kind (N_Bare_Ada_Subp);
when others => null;
end case;
end;
when others => null;
end case;
end;
when Gpr_Ada_Prelude_Range =>
declare
N_Bare_Ada_Prelude : constant Bare_Ada_Prelude := Node;
begin
case Field is
when Ada_Prelude_F_Context_Clauses => return Ada_Prelude_F_Context_Clauses (N_Bare_Ada_Prelude);
when Ada_Prelude_F_Library_Item => return Ada_Prelude_F_Library_Item (N_Bare_Ada_Prelude);
when others => null;
end case;
end;
when Gpr_Ada_Separate_Range =>
declare
N_Bare_Ada_Separate : constant Bare_Ada_Separate := Node;
begin
case Field is
when Ada_Separate_F_Parent_Name => return Ada_Separate_F_Parent_Name (N_Bare_Ada_Separate);
when others => null;
end case;
end;
when Gpr_Ada_With_Formal_Range =>
declare
N_Bare_Ada_With_Formal : constant Bare_Ada_With_Formal := Node;
begin
case Field is
when Ada_With_Formal_F_Kind => return Ada_With_Formal_F_Kind (N_Bare_Ada_With_Formal);
when Ada_With_Formal_F_Skips => return Ada_With_Formal_F_Skips (N_Bare_Ada_With_Formal);
when others => null;
end case;
end;
when Gpr_Attribute_Decl_Range =>
declare
N_Bare_Attribute_Decl : constant Bare_Attribute_Decl := Node;
begin
case Field is
when Attribute_Decl_F_Attr_Name => return Attribute_Decl_F_Attr_Name (N_Bare_Attribute_Decl);
when Attribute_Decl_F_Attr_Index => return Attribute_Decl_F_Attr_Index (N_Bare_Attribute_Decl);
when Attribute_Decl_F_Expr => return Attribute_Decl_F_Expr (N_Bare_Attribute_Decl);
when others => null;
end case;
end;
when Gpr_Attribute_Reference_Range =>
declare
N_Bare_Attribute_Reference : constant Bare_Attribute_Reference := Node;
begin
case Field is
when Attribute_Reference_F_Attribute_Name => return Attribute_Reference_F_Attribute_Name (N_Bare_Attribute_Reference);
when Attribute_Reference_F_Attribute_Index => return Attribute_Reference_F_Attribute_Index (N_Bare_Attribute_Reference);
when others => null;
end case;
end;
when Gpr_Builtin_Function_Call_Range =>
declare
N_Bare_Builtin_Function_Call : constant Bare_Builtin_Function_Call := Node;
begin
case Field is
when Builtin_Function_Call_F_Function_Name => return Builtin_Function_Call_F_Function_Name (N_Bare_Builtin_Function_Call);
when Builtin_Function_Call_F_Parameters => return Builtin_Function_Call_F_Parameters (N_Bare_Builtin_Function_Call);
when others => null;
end case;
end;
when Gpr_Case_Construction_Range =>
declare
N_Bare_Case_Construction : constant Bare_Case_Construction := Node;
begin
case Field is
when Case_Construction_F_Var_Ref => return Case_Construction_F_Var_Ref (N_Bare_Case_Construction);
when Case_Construction_F_Items => return Case_Construction_F_Items (N_Bare_Case_Construction);
when others => null;
end case;
end;
when Gpr_Case_Item_Range =>
declare
N_Bare_Case_Item : constant Bare_Case_Item := Node;
begin
case Field is
when Case_Item_F_Choice => return Case_Item_F_Choice (N_Bare_Case_Item);
when Case_Item_F_Decls => return Case_Item_F_Decls (N_Bare_Case_Item);
when others => null;
end case;
end;
when Gpr_Compilation_Unit_Range =>
declare
N_Bare_Compilation_Unit : constant Bare_Compilation_Unit := Node;
begin
case Field is
when Compilation_Unit_F_Project => return Compilation_Unit_F_Project (N_Bare_Compilation_Unit);
when others => null;
end case;
end;
when Gpr_Prefix_Range =>
declare
N_Bare_Prefix : constant Bare_Prefix := Node;
begin
case Field is
when Prefix_F_Prefix => return Prefix_F_Prefix (N_Bare_Prefix);
when Prefix_F_Suffix => return Prefix_F_Suffix (N_Bare_Prefix);
when others => null;
end case;
end;
when Gpr_Package_Decl_Range =>
declare
N_Bare_Package_Decl : constant Bare_Package_Decl := Node;
begin
case Field is
when Package_Decl_F_Pkg_Name => return Package_Decl_F_Pkg_Name (N_Bare_Package_Decl);
when Package_Decl_F_Pkg_Spec => return Package_Decl_F_Pkg_Spec (N_Bare_Package_Decl);
when others => null;
end case;
end;
when Gpr_Package_Extension_Range =>
declare
N_Bare_Package_Extension : constant Bare_Package_Extension := Node;
begin
case Field is
when Package_Extension_F_Extended_Name => return Package_Extension_F_Extended_Name (N_Bare_Package_Extension);
when others => null;
end case;
end;
when Gpr_Package_Renaming_Range =>
declare
N_Bare_Package_Renaming : constant Bare_Package_Renaming := Node;
begin
case Field is
when Package_Renaming_F_Renamed_Name => return Package_Renaming_F_Renamed_Name (N_Bare_Package_Renaming);
when others => null;
end case;
end;
when Gpr_Package_Spec_Range =>
declare
N_Bare_Package_Spec : constant Bare_Package_Spec := Node;
begin
case Field is
when Package_Spec_F_Extension => return Package_Spec_F_Extension (N_Bare_Package_Spec);
when Package_Spec_F_Decls => return Package_Spec_F_Decls (N_Bare_Package_Spec);
when Package_Spec_F_End_Name => return Package_Spec_F_End_Name (N_Bare_Package_Spec);
when others => null;
end case;
end;
when Gpr_Project_Range =>
declare
N_Bare_Project : constant Bare_Project := Node;
begin
case Field is
when Project_F_Context_Clauses => return Project_F_Context_Clauses (N_Bare_Project);
when Project_F_Project_Decl => return Project_F_Project_Decl (N_Bare_Project);
when others => null;
end case;
end;
when Gpr_Project_Declaration_Range =>
declare
N_Bare_Project_Declaration : constant Bare_Project_Declaration := Node;
begin
case Field is
when Project_Declaration_F_Qualifier => return Project_Declaration_F_Qualifier (N_Bare_Project_Declaration);
when Project_Declaration_F_Project_Name => return Project_Declaration_F_Project_Name (N_Bare_Project_Declaration);
when Project_Declaration_F_Extension => return Project_Declaration_F_Extension (N_Bare_Project_Declaration);
when Project_Declaration_F_Decls => return Project_Declaration_F_Decls (N_Bare_Project_Declaration);
when Project_Declaration_F_End_Name => return Project_Declaration_F_End_Name (N_Bare_Project_Declaration);
when others => null;
end case;
end;
when Gpr_Project_Extension_Range =>
declare
N_Bare_Project_Extension : constant Bare_Project_Extension := Node;
begin
case Field is
when Project_Extension_F_Is_All => return Project_Extension_F_Is_All (N_Bare_Project_Extension);
when Project_Extension_F_Path_Name => return Project_Extension_F_Path_Name (N_Bare_Project_Extension);
when others => null;
end case;
end;
when Gpr_Project_Reference_Range =>
declare
N_Bare_Project_Reference : constant Bare_Project_Reference := Node;
begin
case Field is
when Project_Reference_F_Attr_Ref => return Project_Reference_F_Attr_Ref (N_Bare_Project_Reference);
when others => null;
end case;
end;
when Gpr_String_Literal_At_Range =>
declare
N_Bare_String_Literal_At : constant Bare_String_Literal_At := Node;
begin
case Field is
when String_Literal_At_F_Str_Lit => return String_Literal_At_F_Str_Lit (N_Bare_String_Literal_At);
when String_Literal_At_F_At_Lit => return String_Literal_At_F_At_Lit (N_Bare_String_Literal_At);
when others => null;
end case;
end;
when Gpr_Terms_Range =>
declare
N_Bare_Terms : constant Bare_Terms := Node;
begin
case Field is
when Terms_F_Terms => return Terms_F_Terms (N_Bare_Terms);
when others => null;
end case;
end;
when Gpr_Type_Reference_Range =>
declare
N_Bare_Type_Reference : constant Bare_Type_Reference := Node;
begin
case Field is
when Type_Reference_F_Var_Type_Name => return Type_Reference_F_Var_Type_Name (N_Bare_Type_Reference);
when others => null;
end case;
end;
when Gpr_Typed_String_Decl_Range =>
declare
N_Bare_Typed_String_Decl : constant Bare_Typed_String_Decl := Node;
begin
case Field is
when Typed_String_Decl_F_Type_Id => return Typed_String_Decl_F_Type_Id (N_Bare_Typed_String_Decl);
when Typed_String_Decl_F_String_Literals => return Typed_String_Decl_F_String_Literals (N_Bare_Typed_String_Decl);
when others => null;
end case;
end;
when Gpr_Variable_Decl_Range =>
declare
N_Bare_Variable_Decl : constant Bare_Variable_Decl := Node;
begin
case Field is
when Variable_Decl_F_Var_Name => return Variable_Decl_F_Var_Name (N_Bare_Variable_Decl);
when Variable_Decl_F_Var_Type => return Variable_Decl_F_Var_Type (N_Bare_Variable_Decl);
when Variable_Decl_F_Expr => return Variable_Decl_F_Expr (N_Bare_Variable_Decl);
when others => null;
end case;
end;
when Gpr_Variable_Reference_Range =>
declare
N_Bare_Variable_Reference : constant Bare_Variable_Reference := Node;
begin
case Field is
when Variable_Reference_F_Variable_Name => return Variable_Reference_F_Variable_Name (N_Bare_Variable_Reference);
when Variable_Reference_F_Attribute_Ref => return Variable_Reference_F_Attribute_Ref (N_Bare_Variable_Reference);
when others => null;
end case;
end;
when Gpr_With_Decl_Range =>
declare
N_Bare_With_Decl : constant Bare_With_Decl := Node;
begin
case Field is
when With_Decl_F_Is_Limited => return With_Decl_F_Is_Limited (N_Bare_With_Decl);
when With_Decl_F_Path_Names => return With_Decl_F_Path_Names (N_Bare_With_Decl);
when others => null;
end case;
end;
when others => null;
end case;

      return (raise Bad_Type_Error with "no such field on this node");
   end Eval_Syntax_Field;

   -----------
   -- Index --
   -----------

   function Index
     (Kind : Gpr_Node_Kind_Type; Field : Syntax_Field_Reference) return Positive is
   begin
         
         case Kind is
               when Gpr_Ada_Access_Subp =>
               return (case Field is
                       when Ada_Access_Subp_F_Subp_Kind => 1,
                       when Ada_Access_Subp_F_Skips => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Pragma =>
               return (case Field is
                       when Ada_Pragma_F_Skips => 1,
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Use =>
               return (case Field is
                       when Ada_Use_F_Skips => 1,
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_With =>
               return (case Field is
                       when Ada_With_F_Has_Limited => 1,
                       when Ada_With_F_Has_Private => 2,
                       when Ada_With_F_Packages => 3,
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Entity_Kind_Function =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Entity_Kind_Package =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Entity_Kind_Procedure =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Generic =>
               return (case Field is
                       when Ada_Generic_F_Skips => 1,
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Library_Item =>
               return (case Field is
                       when Ada_Library_Item_F_Generic_Stub => 1,
                       when Ada_Library_Item_F_Separate => 2,
                       when Ada_Library_Item_F_Main => 3,
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Pkg =>
               return (case Field is
                       when Ada_Pkg_F_Has_Private => 1,
                       when Ada_Main_F_Name => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Pkg_Body =>
               return (case Field is
                       when Ada_Main_F_Name => 1,
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Subp =>
               return (case Field is
                       when Ada_Subp_F_Subp_Kind => 1,
                       when Ada_Main_F_Name => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Prelude =>
               return (case Field is
                       when Ada_Prelude_F_Context_Clauses => 1,
                       when Ada_Prelude_F_Library_Item => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Separate =>
               return (case Field is
                       when Ada_Separate_F_Parent_Name => 1,
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Skip =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_With_Formal =>
               return (case Field is
                       when Ada_With_Formal_F_Kind => 1,
                       when Ada_With_Formal_F_Skips => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_All_Qualifier_Absent =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_All_Qualifier_Present =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Attribute_Decl =>
               return (case Field is
                       when Attribute_Decl_F_Attr_Name => 1,
                       when Attribute_Decl_F_Attr_Index => 2,
                       when Attribute_Decl_F_Expr => 3,
                       when others => raise Bad_Type_Error);
               when Gpr_Attribute_Reference =>
               return (case Field is
                       when Attribute_Reference_F_Attribute_Name => 1,
                       when Attribute_Reference_F_Attribute_Index => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Context_Clause_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Prelude_Node_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Ada_Skip_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Case_Item_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Expr_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Gpr_Node_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Choices =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Term_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Identifier_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_String_Literal_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Term_List_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_With_Decl_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Builtin_Function_Call =>
               return (case Field is
                       when Builtin_Function_Call_F_Function_Name => 1,
                       when Builtin_Function_Call_F_Parameters => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_Case_Construction =>
               return (case Field is
                       when Case_Construction_F_Var_Ref => 1,
                       when Case_Construction_F_Items => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_Case_Item =>
               return (case Field is
                       when Case_Item_F_Choice => 1,
                       when Case_Item_F_Decls => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_Compilation_Unit =>
               return (case Field is
                       when Compilation_Unit_F_Project => 1,
                       when others => raise Bad_Type_Error);
               when Gpr_Empty_Decl =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Prefix =>
               return (case Field is
                       when Prefix_F_Prefix => 1,
                       when Prefix_F_Suffix => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_Identifier =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Num_Literal =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_String_Literal =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Limited_Absent =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Limited_Present =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Others_Designator =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Package_Decl =>
               return (case Field is
                       when Package_Decl_F_Pkg_Name => 1,
                       when Package_Decl_F_Pkg_Spec => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_Package_Extension =>
               return (case Field is
                       when Package_Extension_F_Extended_Name => 1,
                       when others => raise Bad_Type_Error);
               when Gpr_Package_Renaming =>
               return (case Field is
                       when Package_Renaming_F_Renamed_Name => 1,
                       when others => raise Bad_Type_Error);
               when Gpr_Package_Spec =>
               return (case Field is
                       when Package_Spec_F_Extension => 1,
                       when Package_Spec_F_Decls => 2,
                       when Package_Spec_F_End_Name => 3,
                       when others => raise Bad_Type_Error);
               when Gpr_Private_Absent =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Private_Present =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Project =>
               return (case Field is
                       when Project_F_Context_Clauses => 1,
                       when Project_F_Project_Decl => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_Project_Declaration =>
               return (case Field is
                       when Project_Declaration_F_Qualifier => 1,
                       when Project_Declaration_F_Project_Name => 2,
                       when Project_Declaration_F_Extension => 3,
                       when Project_Declaration_F_Decls => 4,
                       when Project_Declaration_F_End_Name => 5,
                       when others => raise Bad_Type_Error);
               when Gpr_Project_Extension =>
               return (case Field is
                       when Project_Extension_F_Is_All => 1,
                       when Project_Extension_F_Path_Name => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_Project_Qualifier_Abstract =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Project_Qualifier_Aggregate =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Project_Qualifier_Aggregate_Library =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Project_Qualifier_Configuration =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Project_Qualifier_Library =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Project_Qualifier_Standard =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Gpr_Project_Reference =>
               return (case Field is
                       when Project_Reference_F_Attr_Ref => 1,
                       when others => raise Bad_Type_Error);
               when Gpr_String_Literal_At =>
               return (case Field is
                       when String_Literal_At_F_Str_Lit => 1,
                       when String_Literal_At_F_At_Lit => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_Terms =>
               return (case Field is
                       when Terms_F_Terms => 1,
                       when others => raise Bad_Type_Error);
               when Gpr_Type_Reference =>
               return (case Field is
                       when Type_Reference_F_Var_Type_Name => 1,
                       when others => raise Bad_Type_Error);
               when Gpr_Typed_String_Decl =>
               return (case Field is
                       when Typed_String_Decl_F_Type_Id => 1,
                       when Typed_String_Decl_F_String_Literals => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_Variable_Decl =>
               return (case Field is
                       when Variable_Decl_F_Var_Name => 1,
                       when Variable_Decl_F_Var_Type => 2,
                       when Variable_Decl_F_Expr => 3,
                       when others => raise Bad_Type_Error);
               when Gpr_Variable_Reference =>
               return (case Field is
                       when Variable_Reference_F_Variable_Name => 1,
                       when Variable_Reference_F_Attribute_Ref => 2,
                       when others => raise Bad_Type_Error);
               when Gpr_With_Decl =>
               return (case Field is
                       when With_Decl_F_Is_Limited => 1,
                       when With_Decl_F_Path_Names => 2,
                       when others => raise Bad_Type_Error);
         end case;

   end Index;

   ---------------------------------------
   -- Syntax_Field_Reference_From_Index --
   ---------------------------------------

   function Syntax_Field_Reference_From_Index
     (Kind : Gpr_Node_Kind_Type; Index : Positive) return Syntax_Field_Reference is
   begin
      
      case Gpr_Gpr_Node (Kind) is
when Gpr_Ada_Access_Subp_Range =>
case Index is
when 1 => return Ada_Access_Subp_F_Subp_Kind;
when 2 => return Ada_Access_Subp_F_Skips;
when others => null;
end case;
when Gpr_Ada_Pragma_Range =>
case Index is
when 1 => return Ada_Pragma_F_Skips;
when others => null;
end case;
when Gpr_Ada_Use_Range =>
case Index is
when 1 => return Ada_Use_F_Skips;
when others => null;
end case;
when Gpr_Ada_With_Range =>
case Index is
when 1 => return Ada_With_F_Has_Limited;
when 2 => return Ada_With_F_Has_Private;
when 3 => return Ada_With_F_Packages;
when others => null;
end case;
when Gpr_Ada_Generic_Range =>
case Index is
when 1 => return Ada_Generic_F_Skips;
when others => null;
end case;
when Gpr_Ada_Library_Item_Range =>
case Index is
when 1 => return Ada_Library_Item_F_Generic_Stub;
when 2 => return Ada_Library_Item_F_Separate;
when 3 => return Ada_Library_Item_F_Main;
when others => null;
end case;
when Gpr_Ada_Pkg_Range =>
case Index is
when 1 => return Ada_Pkg_F_Has_Private;
when 2 => return Ada_Main_F_Name;
when others => null;
end case;
when Gpr_Ada_Pkg_Body_Range =>
case Index is
when 1 => return Ada_Main_F_Name;
when others => null;
end case;
when Gpr_Ada_Subp_Range =>
case Index is
when 1 => return Ada_Subp_F_Subp_Kind;
when 2 => return Ada_Main_F_Name;
when others => null;
end case;
when Gpr_Ada_Prelude_Range =>
case Index is
when 1 => return Ada_Prelude_F_Context_Clauses;
when 2 => return Ada_Prelude_F_Library_Item;
when others => null;
end case;
when Gpr_Ada_Separate_Range =>
case Index is
when 1 => return Ada_Separate_F_Parent_Name;
when others => null;
end case;
when Gpr_Ada_With_Formal_Range =>
case Index is
when 1 => return Ada_With_Formal_F_Kind;
when 2 => return Ada_With_Formal_F_Skips;
when others => null;
end case;
when Gpr_Attribute_Decl_Range =>
case Index is
when 1 => return Attribute_Decl_F_Attr_Name;
when 2 => return Attribute_Decl_F_Attr_Index;
when 3 => return Attribute_Decl_F_Expr;
when others => null;
end case;
when Gpr_Attribute_Reference_Range =>
case Index is
when 1 => return Attribute_Reference_F_Attribute_Name;
when 2 => return Attribute_Reference_F_Attribute_Index;
when others => null;
end case;
when Gpr_Base_List =>
raise Bad_Type_Error with "List AST nodes have no field";
when Gpr_Builtin_Function_Call_Range =>
case Index is
when 1 => return Builtin_Function_Call_F_Function_Name;
when 2 => return Builtin_Function_Call_F_Parameters;
when others => null;
end case;
when Gpr_Case_Construction_Range =>
case Index is
when 1 => return Case_Construction_F_Var_Ref;
when 2 => return Case_Construction_F_Items;
when others => null;
end case;
when Gpr_Case_Item_Range =>
case Index is
when 1 => return Case_Item_F_Choice;
when 2 => return Case_Item_F_Decls;
when others => null;
end case;
when Gpr_Compilation_Unit_Range =>
case Index is
when 1 => return Compilation_Unit_F_Project;
when others => null;
end case;
when Gpr_Prefix_Range =>
case Index is
when 1 => return Prefix_F_Prefix;
when 2 => return Prefix_F_Suffix;
when others => null;
end case;
when Gpr_Package_Decl_Range =>
case Index is
when 1 => return Package_Decl_F_Pkg_Name;
when 2 => return Package_Decl_F_Pkg_Spec;
when others => null;
end case;
when Gpr_Package_Extension_Range =>
case Index is
when 1 => return Package_Extension_F_Extended_Name;
when others => null;
end case;
when Gpr_Package_Renaming_Range =>
case Index is
when 1 => return Package_Renaming_F_Renamed_Name;
when others => null;
end case;
when Gpr_Package_Spec_Range =>
case Index is
when 1 => return Package_Spec_F_Extension;
when 2 => return Package_Spec_F_Decls;
when 3 => return Package_Spec_F_End_Name;
when others => null;
end case;
when Gpr_Project_Range =>
case Index is
when 1 => return Project_F_Context_Clauses;
when 2 => return Project_F_Project_Decl;
when others => null;
end case;
when Gpr_Project_Declaration_Range =>
case Index is
when 1 => return Project_Declaration_F_Qualifier;
when 2 => return Project_Declaration_F_Project_Name;
when 3 => return Project_Declaration_F_Extension;
when 4 => return Project_Declaration_F_Decls;
when 5 => return Project_Declaration_F_End_Name;
when others => null;
end case;
when Gpr_Project_Extension_Range =>
case Index is
when 1 => return Project_Extension_F_Is_All;
when 2 => return Project_Extension_F_Path_Name;
when others => null;
end case;
when Gpr_Project_Reference_Range =>
case Index is
when 1 => return Project_Reference_F_Attr_Ref;
when others => null;
end case;
when Gpr_String_Literal_At_Range =>
case Index is
when 1 => return String_Literal_At_F_Str_Lit;
when 2 => return String_Literal_At_F_At_Lit;
when others => null;
end case;
when Gpr_Terms_Range =>
case Index is
when 1 => return Terms_F_Terms;
when others => null;
end case;
when Gpr_Type_Reference_Range =>
case Index is
when 1 => return Type_Reference_F_Var_Type_Name;
when others => null;
end case;
when Gpr_Typed_String_Decl_Range =>
case Index is
when 1 => return Typed_String_Decl_F_Type_Id;
when 2 => return Typed_String_Decl_F_String_Literals;
when others => null;
end case;
when Gpr_Variable_Decl_Range =>
case Index is
when 1 => return Variable_Decl_F_Var_Name;
when 2 => return Variable_Decl_F_Var_Type;
when 3 => return Variable_Decl_F_Expr;
when others => null;
end case;
when Gpr_Variable_Reference_Range =>
case Index is
when 1 => return Variable_Reference_F_Variable_Name;
when 2 => return Variable_Reference_F_Attribute_Ref;
when others => null;
end case;
when Gpr_With_Decl_Range =>
case Index is
when 1 => return With_Decl_F_Is_Limited;
when 2 => return With_Decl_F_Path_Names;
when others => null;
end case;
when others => null;
end case;

      pragma Warnings (Off, "value not in range of type");
      return (raise Bad_Type_Error with "Index is out of bounds");
      pragma Warnings (On, "value not in range of type");
   end Syntax_Field_Reference_From_Index;

   -------------------
   -- Syntax_Fields --
   -------------------

   function Syntax_Fields
     (Kind : Gpr_Node_Kind_Type) return Syntax_Field_Reference_Array is
   begin
         return Syntax_Fields (Id_For_Kind (Kind), Concrete_Only => True);
   end Syntax_Fields;

   -------------------
   -- Syntax_Fields --
   -------------------

   function Syntax_Fields
     (Id            : Node_Type_Id;
      Concrete_Only : Boolean) return Syntax_Field_Reference_Array
   is
      Cursor : Any_Node_Type_Id := Id;

      Added_Fields : array (Syntax_Field_Reference) of Boolean :=
        (others => False);
      --  Set of field references that were added to Result

      Result : Syntax_Field_Reference_Array (1 .. Added_Fields'Length);
      --  Temporary to hold the result. We return Result (1 .. Last).

      Last : Natural := 0;
      --  Index of the last element in Result to return
   begin

         --  Go through the derivation chain for Id and collect fields. Do
         --  it in reverse order as we process base types last.
         while Cursor /= None loop
            declare
               Node_Desc : Node_Type_Descriptor renames
                  Node_Type_Descriptors (Cursor).all;
            begin
               for Field_Index in reverse Node_Desc.Fields'Range loop
                  declare
                     Field_Desc : Node_Field_Descriptor renames
                        Node_Desc.Fields (Field_Index).all;
                     Field      : Syntax_Field_Reference renames
                        Field_Desc.Field;
                  begin
                     --  Abstract fields share the same Syntax_Field_Reference
                     --  value with the corresponding concrete fields, so
                     --  collect fields only once. We process fields in reverse
                     --  order, so we know that concrete ones will be processed
                     --  before the abstract fields they override.
                     if not (Concrete_Only
                             and then Field_Desc.Is_Abstract_Or_Null)
                        and then not Added_Fields (Field)
                     then
                        Added_Fields (Field) := True;
                        Last := Last + 1;
                        Result (Last) := Field;
                     end if;
                  end;
               end loop;
               Cursor := Node_Desc.Base_Type;
            end;
         end loop;

         --  At this point, Result contains elements in the opposite order as
         --  expected, so reverse it.

         for I in 1 .. Last / 2 loop
            declare
               Other_I : constant Positive := Last - I + 1;
               Swap    : constant Syntax_Field_Reference := Result (I);
            begin
               Result (I) := Result (Other_I);
               Result (Other_I) := Swap;
            end;
         end loop;

         return Result (1 .. Last);

   end Syntax_Fields;

   -------------------
   -- Syntax_Fields --
   -------------------

   function Syntax_Fields
     (Id : Node_Type_Id) return Syntax_Field_Reference_Array is
   begin
      return Syntax_Fields (Id, Concrete_Only => False);
   end Syntax_Fields;


   -------------------
   -- Property_Name --
   -------------------

   function Property_Name (Property : Property_Reference) return Text_Type is
   begin
      return To_Text (Property_Descriptors (Property).Name);
   end Property_Name;

   --------------------------
   -- Property_Return_Type --
   --------------------------

   function Property_Return_Type
     (Property : Property_Reference) return Type_Constraint is
   begin
      return Property_Descriptors (Property).Return_Type;
   end Property_Return_Type;

   ---------------------------
   -- Check_Argument_Number --
   ---------------------------

   procedure Check_Argument_Number
     (Desc : Property_Descriptor; Argument_Number : Positive) is
   begin
      if Argument_Number not in Desc.Argument_Names'Range then
         raise Property_Error with "out-of-bounds argument number";
      end if;
   end Check_Argument_Number;

   -----------------------------
   -- Property_Argument_Types --
   -----------------------------

   function Property_Argument_Types
     (Property : Property_Reference) return Type_Constraint_Array is
   begin
      return Property_Descriptors (Property).Argument_Types;
   end Property_Argument_Types;

   ----------------------------
   -- Property_Argument_Name --
   ----------------------------

   function Property_Argument_Name
     (Property        : Property_Reference;
      Argument_Number : Positive) return Text_Type
   is
      Desc : Property_Descriptor renames Property_Descriptors (Property).all;
   begin
      Check_Argument_Number (Desc, Argument_Number);
      return To_Text
        (Property_Descriptors (Property).Argument_Names (Argument_Number).all);
   end Property_Argument_Name;

   -------------------------------------
   -- Property_Argument_Default_Value --
   -------------------------------------

   function Property_Argument_Default_Value
     (Property        : Property_Reference;
      Argument_Number : Positive) return Internal_Value
   is
      Desc : Property_Descriptor renames Property_Descriptors (Property).all;
   begin
      Check_Argument_Number (Desc, Argument_Number);
      return Desc.Argument_Default_Values (Argument_Number);
   end Property_Argument_Default_Value;

   ----------------
   -- Properties --
   ----------------

   function Properties (Kind : Gpr_Node_Kind_Type) return Property_Reference_Array
   is
   begin
      return Properties (Id_For_Kind (Kind));
   end Properties;

   ----------------
   -- Properties --
   ----------------

   function Properties (Id : Node_Type_Id) return Property_Reference_Array is
      Cursor : Any_Node_Type_Id := Id;

      Result : Property_Reference_Array (1 .. Property_Descriptors'Length);
      --  Temporary to hold the result. We return Result (1 .. Last).

      Last : Natural := 0;
      --  Index of the last element in Result to return
   begin
      --  Go through the derivation chain for Id and collect properties. Do
      --  it in reverse order as we process base types last.

      while Cursor /= None loop
         declare
            Node_Desc : Node_Type_Descriptor renames
               Node_Type_Descriptors (Cursor).all;
         begin
            for Prop_Desc of reverse Node_Desc.Properties loop
               Last := Last + 1;
               Result (Last) := Prop_Desc;
            end loop;
            Cursor := Node_Desc.Base_Type;
         end;
      end loop;

      --  At this point, Result contains elements in the opposite order as
      --  expected, so reverse it.

      for I in 1 .. Last / 2 loop
         declare
            Other_I : constant Positive := Last - I + 1;
            Swap    : constant Property_Reference := Result (I);
         begin
            Result (I) := Result (Other_I);
            Result (Other_I) := Swap;
         end;
      end loop;

      return Result (1 .. Last);
   end Properties;


   ---------------------
   -- Token_Node_Kind --
   ---------------------

   function Token_Node_Kind (Kind : Gpr_Node_Kind_Type) return Token_Kind is
      
   begin
         case Kind is
               when Gpr_Identifier =>
                  return Gpr_Identifier;
               when Gpr_Num_Literal =>
                  return Gpr_Number;
               when Gpr_String_Literal =>
                  return Gpr_String;

            when others =>
               --  Kind is not a token node, and thus the precondition does not
               --  hold.
               return (raise Program_Error);
         end case;

   end Token_Node_Kind;

begin
   for D in Node_Type_Descriptors'Range loop
      DSL_Name_To_Node_Type.Insert (Node_Type_Descriptors (D).DSL_Name, D);
   end loop;
end Gpr_Parser.Introspection_Implementation;
