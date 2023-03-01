
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with Gpr_Parser.Implementation; use Gpr_Parser.Implementation;
with Gpr_Parser.Common;         use Gpr_Parser.Common;

private package Gpr_Parser.Introspection_Implementation is

   use Support.Text;

   ------------------------
   -- Polymorphic values --
   ------------------------

   --  TODO: for now, support only value types that are required to represent
   --  default values for property arguments.

   subtype Internal_Value_Kind is Any_Value_Kind
      with Static_Predicate => Internal_Value_Kind in
         None | Boolean_Value | Integer_Value | Character_Value | String_Value
       | Analysis_Unit_Kind_Value
       | Lookup_Kind_Value
       | Designated_Env_Kind_Value
       | Grammar_Rule_Value
       | Node_Value;

   type Internal_Value (Kind : Internal_Value_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Boolean_Value =>
            Boolean_Value : Boolean;

         when Integer_Value =>
            Integer_Value : Integer;

         when Character_Value =>
            Character_Value : Character_Type;

         when String_Value =>
            String_Value : String_Type;

         when Analysis_Unit_Kind_Value =>
            Analysis_Unit_Kind_Value : Analysis_Unit_Kind;
         when Lookup_Kind_Value =>
            Lookup_Kind_Value : Lookup_Kind;
         when Designated_Env_Kind_Value =>
            Designated_Env_Kind_Value : Designated_Env_Kind;
         when Grammar_Rule_Value =>
            Grammar_Rule_Value : Grammar_Rule;

         when Node_Value =>
            Node_Value : Internal_Entity;
      end case;
   end record;

   No_Internal_Value : constant Internal_Value := (Kind => None);

   type Internal_Value_Array is array (Positive range <>) of Internal_Value;

   function As_Boolean (Self : Internal_Value) return Boolean;
   function Create_Boolean (Value : Boolean) return Internal_Value is
     ((Kind => Boolean_Value, Boolean_Value => Value));

   function As_Integer (Self : Internal_Value) return Integer;
   function Create_Integer (Value : Integer) return Internal_Value is
     ((Kind => Integer_Value, Integer_Value => Value));

   function As_Character (Self : Internal_Value) return Character_Type;
   function Create_Character (Value : Character_Type) return Internal_Value is
     ((Kind => Character_Value, Character_Value => Value));

   function As_String (Self : Internal_Value) return String_Type;
   function Create_String (Value : String_Type) return Internal_Value is
     ((Kind => String_Value, String_Value => Value));

   function As_Node (Self : Internal_Value) return Internal_Entity;
   function Create_Node (Value : Internal_Entity) return Internal_Value is
     ((Kind => Node_Value, Node_Value => Value));

      function As_Analysis_Unit_Kind
        (Self : Internal_Value) return Analysis_Unit_Kind;
      function Create_Analysis_Unit_Kind
        (Value : Analysis_Unit_Kind) return Internal_Value
      is ((Kind => Analysis_Unit_Kind_Value,
           Analysis_Unit_Kind_Value => Value));
      function As_Lookup_Kind
        (Self : Internal_Value) return Lookup_Kind;
      function Create_Lookup_Kind
        (Value : Lookup_Kind) return Internal_Value
      is ((Kind => Lookup_Kind_Value,
           Lookup_Kind_Value => Value));
      function As_Designated_Env_Kind
        (Self : Internal_Value) return Designated_Env_Kind;
      function Create_Designated_Env_Kind
        (Value : Designated_Env_Kind) return Internal_Value
      is ((Kind => Designated_Env_Kind_Value,
           Designated_Env_Kind_Value => Value));
      function As_Grammar_Rule
        (Self : Internal_Value) return Grammar_Rule;
      function Create_Grammar_Rule
        (Value : Grammar_Rule) return Internal_Value
      is ((Kind => Grammar_Rule_Value,
           Grammar_Rule_Value => Value));

   -----------------------
   -- Descriptor tables --
   -----------------------

   type String_Access is access constant String;
   type String_Array is array (Positive range <>) of String_Access;

   ------------------------------
   -- Struct field descriptors --
   ------------------------------

   type Struct_Field_Descriptor (Name_Length : Natural) is record
      Reference : Struct_Field_Reference;
      --  Enum value that designates this field

      Field_Type : Type_Constraint;
      --  Type for this field

      Name : String (1 .. Name_Length);
      --  Lower-case name for this field
   end record;
   --  General description of a struct field

   type Struct_Field_Descriptor_Access is
      access constant Struct_Field_Descriptor;
   type Struct_Field_Descriptor_Array is
      array (Positive range <>) of Struct_Field_Descriptor_Access;

   -----------------------------
   -- Struct type descriptors --
   -----------------------------

   type Struct_Type_Descriptor (Fields_Count : Natural) is record
      Fields : Struct_Field_Descriptor_Array (1 .. Fields_Count);
   end record;

   type Struct_Type_Descriptor_Access is
      access constant Struct_Type_Descriptor;

   ------------------------------
   -- Syntax field descriptors --
   ------------------------------

   type Syntax_Field_Descriptor (Name_Length : Natural) is record
      Field_Type : Node_Type_Id;
      Name       : String (1 .. Name_Length);
   end record;
   --  General description of a field (independent of field implementations)

   type Syntax_Field_Descriptor_Access is
      access constant Syntax_Field_Descriptor;

   --  Descriptors for syntax fields

      
      Desc_For_Ada_Access_Subp_F_Subp_Kind : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Ada_Entity_Kind_Type_Id,
            Name        => "f_subp_kind"
         );
      
      Desc_For_Ada_Access_Subp_F_Skips : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Ada_Skip_List_Type_Id,
            Name        => "f_skips"
         );
      
      Desc_For_Ada_Pragma_F_Skips : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Ada_Skip_List_Type_Id,
            Name        => "f_skips"
         );
      
      Desc_For_Ada_Use_F_Skips : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Ada_Skip_List_Type_Id,
            Name        => "f_skips"
         );
      
      Desc_For_Ada_With_F_Has_Limited : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 13,
            Field_Type  => Common.Limited_Node_Type_Id,
            Name        => "f_has_limited"
         );
      
      Desc_For_Ada_With_F_Has_Private : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 13,
            Field_Type  => Common.Private_Node_Type_Id,
            Name        => "f_has_private"
         );
      
      Desc_For_Ada_With_F_Packages : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_packages"
         );
      
      Desc_For_Ada_Generic_F_Skips : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Gpr_Node_Type_Id,
            Name        => "f_skips"
         );
      
      Desc_For_Ada_Library_Item_F_Generic_Stub : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 14,
            Field_Type  => Common.Ada_Generic_Type_Id,
            Name        => "f_generic_stub"
         );
      
      Desc_For_Ada_Library_Item_F_Separate : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Ada_Separate_Type_Id,
            Name        => "f_separate"
         );
      
      Desc_For_Ada_Library_Item_F_Main : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Ada_Main_Type_Id,
            Name        => "f_main"
         );
      
      Desc_For_Ada_Main_F_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_name"
         );
      
      Desc_For_Ada_Pkg_F_Has_Private : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 13,
            Field_Type  => Common.Private_Node_Type_Id,
            Name        => "f_has_private"
         );
      
      Desc_For_Ada_Subp_F_Subp_Kind : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Ada_Entity_Kind_Type_Id,
            Name        => "f_subp_kind"
         );
      
      Desc_For_Ada_Prelude_F_Context_Clauses : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 17,
            Field_Type  => Common.Ada_Context_Clause_List_Type_Id,
            Name        => "f_context_clauses"
         );
      
      Desc_For_Ada_Prelude_F_Library_Item : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 14,
            Field_Type  => Common.Ada_Library_Item_Type_Id,
            Name        => "f_library_item"
         );
      
      Desc_For_Ada_Separate_F_Parent_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 13,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_parent_name"
         );
      
      Desc_For_Ada_With_Formal_F_Kind : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Ada_Entity_Kind_Type_Id,
            Name        => "f_kind"
         );
      
      Desc_For_Ada_With_Formal_F_Skips : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Ada_Skip_List_Type_Id,
            Name        => "f_skips"
         );
      
      Desc_For_Attribute_Decl_F_Attr_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Identifier_Type_Id,
            Name        => "f_attr_name"
         );
      
      Desc_For_Attribute_Decl_F_Attr_Index : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Gpr_Node_Type_Id,
            Name        => "f_attr_index"
         );
      
      Desc_For_Attribute_Decl_F_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Term_List_Type_Id,
            Name        => "f_expr"
         );
      
      Desc_For_Attribute_Reference_F_Attribute_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 16,
            Field_Type  => Common.Identifier_Type_Id,
            Name        => "f_attribute_name"
         );
      
      Desc_For_Attribute_Reference_F_Attribute_Index : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 17,
            Field_Type  => Common.Gpr_Node_Type_Id,
            Name        => "f_attribute_index"
         );
      
      Desc_For_Builtin_Function_Call_F_Function_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 15,
            Field_Type  => Common.Identifier_Type_Id,
            Name        => "f_function_name"
         );
      
      Desc_For_Builtin_Function_Call_F_Parameters : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Terms_Type_Id,
            Name        => "f_parameters"
         );
      
      Desc_For_Case_Construction_F_Var_Ref : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 9,
            Field_Type  => Common.Variable_Reference_Type_Id,
            Name        => "f_var_ref"
         );
      
      Desc_For_Case_Construction_F_Items : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Case_Item_List_Type_Id,
            Name        => "f_items"
         );
      
      Desc_For_Case_Item_F_Choice : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 8,
            Field_Type  => Common.Choices_Type_Id,
            Name        => "f_choice"
         );
      
      Desc_For_Case_Item_F_Decls : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Gpr_Node_List_Type_Id,
            Name        => "f_decls"
         );
      
      Desc_For_Compilation_Unit_F_Project : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 9,
            Field_Type  => Common.Project_Type_Id,
            Name        => "f_project"
         );
      
      Desc_For_Prefix_F_Prefix : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 8,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_prefix"
         );
      
      Desc_For_Prefix_F_Suffix : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 8,
            Field_Type  => Common.Identifier_Type_Id,
            Name        => "f_suffix"
         );
      
      Desc_For_Package_Decl_F_Pkg_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Identifier_Type_Id,
            Name        => "f_pkg_name"
         );
      
      Desc_For_Package_Decl_F_Pkg_Spec : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Gpr_Node_Type_Id,
            Name        => "f_pkg_spec"
         );
      
      Desc_For_Package_Extension_F_Extended_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 15,
            Field_Type  => Common.Identifier_List_Type_Id,
            Name        => "f_extended_name"
         );
      
      Desc_For_Package_Renaming_F_Renamed_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 14,
            Field_Type  => Common.Identifier_List_Type_Id,
            Name        => "f_renamed_name"
         );
      
      Desc_For_Package_Spec_F_Extension : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Package_Extension_Type_Id,
            Name        => "f_extension"
         );
      
      Desc_For_Package_Spec_F_Decls : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Gpr_Node_List_Type_Id,
            Name        => "f_decls"
         );
      
      Desc_For_Package_Spec_F_End_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Identifier_Type_Id,
            Name        => "f_end_name"
         );
      
      Desc_For_Project_F_Context_Clauses : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 17,
            Field_Type  => Common.With_Decl_List_Type_Id,
            Name        => "f_context_clauses"
         );
      
      Desc_For_Project_F_Project_Decl : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 14,
            Field_Type  => Common.Project_Declaration_Type_Id,
            Name        => "f_project_decl"
         );
      
      Desc_For_Project_Declaration_F_Qualifier : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Project_Qualifier_Type_Id,
            Name        => "f_qualifier"
         );
      
      Desc_For_Project_Declaration_F_Project_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 14,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_project_name"
         );
      
      Desc_For_Project_Declaration_F_Extension : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Project_Extension_Type_Id,
            Name        => "f_extension"
         );
      
      Desc_For_Project_Declaration_F_Decls : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Gpr_Node_List_Type_Id,
            Name        => "f_decls"
         );
      
      Desc_For_Project_Declaration_F_End_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_end_name"
         );
      
      Desc_For_Project_Extension_F_Is_All : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 8,
            Field_Type  => Common.All_Qualifier_Type_Id,
            Name        => "f_is_all"
         );
      
      Desc_For_Project_Extension_F_Path_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.String_Literal_Type_Id,
            Name        => "f_path_name"
         );
      
      Desc_For_Project_Reference_F_Attr_Ref : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Attribute_Reference_Type_Id,
            Name        => "f_attr_ref"
         );
      
      Desc_For_String_Literal_At_F_Str_Lit : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 9,
            Field_Type  => Common.String_Literal_Type_Id,
            Name        => "f_str_lit"
         );
      
      Desc_For_String_Literal_At_F_At_Lit : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 8,
            Field_Type  => Common.Num_Literal_Type_Id,
            Name        => "f_at_lit"
         );
      
      Desc_For_Terms_F_Terms : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Term_List_List_Type_Id,
            Name        => "f_terms"
         );
      
      Desc_For_Type_Reference_F_Var_Type_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 15,
            Field_Type  => Common.Identifier_List_Type_Id,
            Name        => "f_var_type_name"
         );
      
      Desc_For_Typed_String_Decl_F_Type_Id : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 9,
            Field_Type  => Common.Identifier_Type_Id,
            Name        => "f_type_id"
         );
      
      Desc_For_Typed_String_Decl_F_String_Literals : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 17,
            Field_Type  => Common.String_Literal_List_Type_Id,
            Name        => "f_string_literals"
         );
      
      Desc_For_Variable_Decl_F_Var_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Identifier_Type_Id,
            Name        => "f_var_name"
         );
      
      Desc_For_Variable_Decl_F_Var_Type : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Type_Reference_Type_Id,
            Name        => "f_var_type"
         );
      
      Desc_For_Variable_Decl_F_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Term_List_Type_Id,
            Name        => "f_expr"
         );
      
      Desc_For_Variable_Reference_F_Variable_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 15,
            Field_Type  => Common.Identifier_List_Type_Id,
            Name        => "f_variable_name"
         );
      
      Desc_For_Variable_Reference_F_Attribute_Ref : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 15,
            Field_Type  => Common.Attribute_Reference_Type_Id,
            Name        => "f_attribute_ref"
         );
      
      Desc_For_With_Decl_F_Is_Limited : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Limited_Node_Type_Id,
            Name        => "f_is_limited"
         );
      
      Desc_For_With_Decl_F_Path_Names : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.String_Literal_List_Type_Id,
            Name        => "f_path_names"
         );

   Syntax_Field_Descriptors : constant
      array (Syntax_Field_Reference) of Syntax_Field_Descriptor_Access := (
         Ada_Access_Subp_F_Subp_Kind => Desc_For_Ada_Access_Subp_F_Subp_Kind'Access, Ada_Access_Subp_F_Skips => Desc_For_Ada_Access_Subp_F_Skips'Access, Ada_Pragma_F_Skips => Desc_For_Ada_Pragma_F_Skips'Access, Ada_Use_F_Skips => Desc_For_Ada_Use_F_Skips'Access, Ada_With_F_Has_Limited => Desc_For_Ada_With_F_Has_Limited'Access, Ada_With_F_Has_Private => Desc_For_Ada_With_F_Has_Private'Access, Ada_With_F_Packages => Desc_For_Ada_With_F_Packages'Access, Ada_Generic_F_Skips => Desc_For_Ada_Generic_F_Skips'Access, Ada_Library_Item_F_Generic_Stub => Desc_For_Ada_Library_Item_F_Generic_Stub'Access, Ada_Library_Item_F_Separate => Desc_For_Ada_Library_Item_F_Separate'Access, Ada_Library_Item_F_Main => Desc_For_Ada_Library_Item_F_Main'Access, Ada_Main_F_Name => Desc_For_Ada_Main_F_Name'Access, Ada_Pkg_F_Has_Private => Desc_For_Ada_Pkg_F_Has_Private'Access, Ada_Subp_F_Subp_Kind => Desc_For_Ada_Subp_F_Subp_Kind'Access, Ada_Prelude_F_Context_Clauses => Desc_For_Ada_Prelude_F_Context_Clauses'Access, Ada_Prelude_F_Library_Item => Desc_For_Ada_Prelude_F_Library_Item'Access, Ada_Separate_F_Parent_Name => Desc_For_Ada_Separate_F_Parent_Name'Access, Ada_With_Formal_F_Kind => Desc_For_Ada_With_Formal_F_Kind'Access, Ada_With_Formal_F_Skips => Desc_For_Ada_With_Formal_F_Skips'Access, Attribute_Decl_F_Attr_Name => Desc_For_Attribute_Decl_F_Attr_Name'Access, Attribute_Decl_F_Attr_Index => Desc_For_Attribute_Decl_F_Attr_Index'Access, Attribute_Decl_F_Expr => Desc_For_Attribute_Decl_F_Expr'Access, Attribute_Reference_F_Attribute_Name => Desc_For_Attribute_Reference_F_Attribute_Name'Access, Attribute_Reference_F_Attribute_Index => Desc_For_Attribute_Reference_F_Attribute_Index'Access, Builtin_Function_Call_F_Function_Name => Desc_For_Builtin_Function_Call_F_Function_Name'Access, Builtin_Function_Call_F_Parameters => Desc_For_Builtin_Function_Call_F_Parameters'Access, Case_Construction_F_Var_Ref => Desc_For_Case_Construction_F_Var_Ref'Access, Case_Construction_F_Items => Desc_For_Case_Construction_F_Items'Access, Case_Item_F_Choice => Desc_For_Case_Item_F_Choice'Access, Case_Item_F_Decls => Desc_For_Case_Item_F_Decls'Access, Compilation_Unit_F_Project => Desc_For_Compilation_Unit_F_Project'Access, Prefix_F_Prefix => Desc_For_Prefix_F_Prefix'Access, Prefix_F_Suffix => Desc_For_Prefix_F_Suffix'Access, Package_Decl_F_Pkg_Name => Desc_For_Package_Decl_F_Pkg_Name'Access, Package_Decl_F_Pkg_Spec => Desc_For_Package_Decl_F_Pkg_Spec'Access, Package_Extension_F_Extended_Name => Desc_For_Package_Extension_F_Extended_Name'Access, Package_Renaming_F_Renamed_Name => Desc_For_Package_Renaming_F_Renamed_Name'Access, Package_Spec_F_Extension => Desc_For_Package_Spec_F_Extension'Access, Package_Spec_F_Decls => Desc_For_Package_Spec_F_Decls'Access, Package_Spec_F_End_Name => Desc_For_Package_Spec_F_End_Name'Access, Project_F_Context_Clauses => Desc_For_Project_F_Context_Clauses'Access, Project_F_Project_Decl => Desc_For_Project_F_Project_Decl'Access, Project_Declaration_F_Qualifier => Desc_For_Project_Declaration_F_Qualifier'Access, Project_Declaration_F_Project_Name => Desc_For_Project_Declaration_F_Project_Name'Access, Project_Declaration_F_Extension => Desc_For_Project_Declaration_F_Extension'Access, Project_Declaration_F_Decls => Desc_For_Project_Declaration_F_Decls'Access, Project_Declaration_F_End_Name => Desc_For_Project_Declaration_F_End_Name'Access, Project_Extension_F_Is_All => Desc_For_Project_Extension_F_Is_All'Access, Project_Extension_F_Path_Name => Desc_For_Project_Extension_F_Path_Name'Access, Project_Reference_F_Attr_Ref => Desc_For_Project_Reference_F_Attr_Ref'Access, String_Literal_At_F_Str_Lit => Desc_For_String_Literal_At_F_Str_Lit'Access, String_Literal_At_F_At_Lit => Desc_For_String_Literal_At_F_At_Lit'Access, Terms_F_Terms => Desc_For_Terms_F_Terms'Access, Type_Reference_F_Var_Type_Name => Desc_For_Type_Reference_F_Var_Type_Name'Access, Typed_String_Decl_F_Type_Id => Desc_For_Typed_String_Decl_F_Type_Id'Access, Typed_String_Decl_F_String_Literals => Desc_For_Typed_String_Decl_F_String_Literals'Access, Variable_Decl_F_Var_Name => Desc_For_Variable_Decl_F_Var_Name'Access, Variable_Decl_F_Var_Type => Desc_For_Variable_Decl_F_Var_Type'Access, Variable_Decl_F_Expr => Desc_For_Variable_Decl_F_Expr'Access, Variable_Reference_F_Variable_Name => Desc_For_Variable_Reference_F_Variable_Name'Access, Variable_Reference_F_Attribute_Ref => Desc_For_Variable_Reference_F_Attribute_Ref'Access, With_Decl_F_Is_Limited => Desc_For_With_Decl_F_Is_Limited'Access, With_Decl_F_Path_Names => Desc_For_With_Decl_F_Path_Names'Access
   );

   --------------------------
   -- Property descriptors --
   --------------------------

   type Property_Descriptor (
      Name_Length : Natural;
      --  Length of the proprety name

      Arity : Natural
      --  Number of arguments this property takes (exclude the ``Self``
      --  argument).
   )
   is record
      Name : String (1 .. Name_Length);
      --  Lower-case name for this property

      Return_Type : Type_Constraint;
      --  Return type for this property

      Argument_Types : Type_Constraint_Array (1 .. Arity);
      --  Types of the arguments that this property takes

      Argument_Names : String_Array (1 .. Arity);
      --  Lower-case names for arguments that this property takes

      Argument_Default_Values : Internal_Value_Array (1 .. Arity);
      --  Default values (if any, otherwise ``No_Internal_Value``) for
      --  arguments that this property takes.
   end record;

   type Property_Descriptor_Access is access constant Property_Descriptor;

   --  Descriptors for properties

   
   Name_For_with_self : aliased constant String := "with_self";

      
      Desc_For_Gpr_Node_Parent : aliased constant
         Property_Descriptor := (
            Name_Length => 6,
            Arity       => 0,

            Name => "parent",

            Return_Type    => (Kind => Node_Value, Node_Type => Common.Gpr_Node_Type_Id),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Gpr_Node_Parents : aliased constant
         Property_Descriptor := (
            Name_Length => 7,
            Arity       => 1,

            Name => "parents",

            Return_Type    => (Kind => Gpr_Node_Array_Value),
            Argument_Types => (
                  1 => (Kind => Boolean_Value)
            ),
            Argument_Names => (
                  1 => Name_For_with_self'Access
            ),
            Argument_Default_Values => (
                  1 => Create_Boolean (True)
            )
         );
      
      Desc_For_Gpr_Node_Children : aliased constant
         Property_Descriptor := (
            Name_Length => 8,
            Arity       => 0,

            Name => "children",

            Return_Type    => (Kind => Gpr_Node_Array_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Gpr_Node_Token_Start : aliased constant
         Property_Descriptor := (
            Name_Length => 11,
            Arity       => 0,

            Name => "token_start",

            Return_Type    => (Kind => Token_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Gpr_Node_Token_End : aliased constant
         Property_Descriptor := (
            Name_Length => 9,
            Arity       => 0,

            Name => "token_end",

            Return_Type    => (Kind => Token_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Gpr_Node_Child_Index : aliased constant
         Property_Descriptor := (
            Name_Length => 11,
            Arity       => 0,

            Name => "child_index",

            Return_Type    => (Kind => Integer_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Gpr_Node_Previous_Sibling : aliased constant
         Property_Descriptor := (
            Name_Length => 16,
            Arity       => 0,

            Name => "previous_sibling",

            Return_Type    => (Kind => Node_Value, Node_Type => Common.Gpr_Node_Type_Id),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Gpr_Node_Next_Sibling : aliased constant
         Property_Descriptor := (
            Name_Length => 12,
            Arity       => 0,

            Name => "next_sibling",

            Return_Type    => (Kind => Node_Value, Node_Type => Common.Gpr_Node_Type_Id),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Gpr_Node_Unit : aliased constant
         Property_Descriptor := (
            Name_Length => 4,
            Arity       => 0,

            Name => "unit",

            Return_Type    => (Kind => Analysis_Unit_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Gpr_Node_Is_Ghost : aliased constant
         Property_Descriptor := (
            Name_Length => 8,
            Arity       => 0,

            Name => "is_ghost",

            Return_Type    => (Kind => Boolean_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Gpr_Node_Full_Sloc_Image : aliased constant
         Property_Descriptor := (
            Name_Length => 15,
            Arity       => 0,

            Name => "full_sloc_image",

            Return_Type    => (Kind => String_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_All_Qualifier_P_As_Bool : aliased constant
         Property_Descriptor := (
            Name_Length => 9,
            Arity       => 0,

            Name => "p_as_bool",

            Return_Type    => (Kind => Boolean_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Limited_Node_P_As_Bool : aliased constant
         Property_Descriptor := (
            Name_Length => 9,
            Arity       => 0,

            Name => "p_as_bool",

            Return_Type    => (Kind => Boolean_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Private_Node_P_As_Bool : aliased constant
         Property_Descriptor := (
            Name_Length => 9,
            Arity       => 0,

            Name => "p_as_bool",

            Return_Type    => (Kind => Boolean_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );

      Property_Descriptors : constant
         array (Property_Reference) of Property_Descriptor_Access := (
            Desc_For_Gpr_Node_Parent'Access, Desc_For_Gpr_Node_Parents'Access, Desc_For_Gpr_Node_Children'Access, Desc_For_Gpr_Node_Token_Start'Access, Desc_For_Gpr_Node_Token_End'Access, Desc_For_Gpr_Node_Child_Index'Access, Desc_For_Gpr_Node_Previous_Sibling'Access, Desc_For_Gpr_Node_Next_Sibling'Access, Desc_For_Gpr_Node_Unit'Access, Desc_For_Gpr_Node_Is_Ghost'Access, Desc_For_Gpr_Node_Full_Sloc_Image'Access, Desc_For_All_Qualifier_P_As_Bool'Access, Desc_For_Limited_Node_P_As_Bool'Access, Desc_For_Private_Node_P_As_Bool'Access
      );

   ---------------------------
   -- Node type descriptors --
   ---------------------------

   type Node_Field_Descriptor (Is_Abstract_Or_Null : Boolean) is record
      Field : Syntax_Field_Reference;
      --  Reference to the field this describes

      --  Only non-null concrete fields are assigned an index

      case Is_Abstract_Or_Null is
         when False =>
            Index : Positive;
            --  Index for this field

         when True =>
            null;
      end case;
   end record;
   --  Description of a field as implemented by a specific node

   type Node_Field_Descriptor_Access is access constant Node_Field_Descriptor;
   type Node_Field_Descriptor_Array is
      array (Positive range <>) of Node_Field_Descriptor_Access;

   type Node_Type_Descriptor
     (Is_Abstract       : Boolean;
      Derivations_Count : Natural;
      Fields_Count      : Natural;
      Properties_Count  : Natural)
   is record
      Base_Type : Any_Node_Type_Id;
      --  Reference to the node type from which this derives

      Derivations : Node_Type_Id_Array (1 .. Derivations_Count);
      --  List of references for all node types that derives from this

      DSL_Name : Unbounded_String;
      --  Name for this type in the Langkit DSL

      Inherited_Fields : Natural;
      --  Number of syntax field inherited from the base type

      Fields : Node_Field_Descriptor_Array (1 .. Fields_Count);
      --  For regular node types, list of syntax fields that are specific to
      --  this derivation (i.e. excluding fields from the base type).

      Properties : Property_Reference_Array (1 .. Properties_Count);
      --  List of properties that this node provides that are specific to this
      --  derivation (i.e. excluding fields from the base type).

      --  Only concrete nodes are assigned a node kind

      case Is_Abstract is
         when False =>
            Kind : Gpr_Node_Kind_Type;
            --  Kind corresponding this this node type

         when True =>
            null;
      end case;
   end record;

   type Node_Type_Descriptor_Access is access constant Node_Type_Descriptor;

   --  Descriptors for struct types and their fields


   Struct_Field_Descriptors : constant
      array (Struct_Field_Reference) of Struct_Field_Descriptor_Access := (
         Struct_Field_Reference => <>
   );


   --  Descriptors for node types and their syntax fields

   


   Desc_For_Gpr_Node : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 30,
      Fields_Count      => 0,
      Properties_Count  => 11,

      Base_Type   => None,
      Derivations =>
         (1 => Common.Ada_Prelude_Node_Type_Id, 2 => Common.All_Qualifier_Type_Id, 3 => Common.Attribute_Decl_Type_Id, 4 => Common.Attribute_Reference_Type_Id, 5 => Common.Base_List_Type_Id, 6 => Common.Builtin_Function_Call_Type_Id, 7 => Common.Case_Construction_Type_Id, 8 => Common.Case_Item_Type_Id, 9 => Common.Compilation_Unit_Type_Id, 10 => Common.Empty_Decl_Type_Id, 11 => Common.Expr_Type_Id, 12 => Common.Limited_Node_Type_Id, 13 => Common.Others_Designator_Type_Id, 14 => Common.Package_Decl_Type_Id, 15 => Common.Package_Extension_Type_Id, 16 => Common.Package_Renaming_Type_Id, 17 => Common.Package_Spec_Type_Id, 18 => Common.Private_Node_Type_Id, 19 => Common.Project_Type_Id, 20 => Common.Project_Declaration_Type_Id, 21 => Common.Project_Extension_Type_Id, 22 => Common.Project_Qualifier_Type_Id, 23 => Common.Project_Reference_Type_Id, 24 => Common.String_Literal_At_Type_Id, 25 => Common.Terms_Type_Id, 26 => Common.Type_Reference_Type_Id, 27 => Common.Typed_String_Decl_Type_Id, 28 => Common.Variable_Decl_Type_Id, 29 => Common.Variable_Reference_Type_Id, 30 => Common.With_Decl_Type_Id),

      DSL_Name => To_Unbounded_String ("GprNode"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 => Gpr_Node_Parent, 2 => Gpr_Node_Parents, 3 => Gpr_Node_Children, 4 => Gpr_Node_Token_Start, 5 => Gpr_Node_Token_End, 6 => Gpr_Node_Child_Index, 7 => Gpr_Node_Previous_Sibling, 8 => Gpr_Node_Next_Sibling, 9 => Gpr_Node_Unit, 10 => Gpr_Node_Is_Ghost, 11 => Gpr_Node_Full_Sloc_Image
      )

   );
   


   Desc_For_Ada_Prelude_Node : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 10,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 => Common.Ada_Access_Subp_Type_Id, 2 => Common.Ada_Context_Clause_Type_Id, 3 => Common.Ada_Entity_Kind_Type_Id, 4 => Common.Ada_Generic_Type_Id, 5 => Common.Ada_Library_Item_Type_Id, 6 => Common.Ada_Main_Type_Id, 7 => Common.Ada_Prelude_Type_Id, 8 => Common.Ada_Separate_Type_Id, 9 => Common.Ada_Skip_Type_Id, 10 => Common.Ada_With_Formal_Type_Id),

      DSL_Name => To_Unbounded_String ("AdaPreludeNode"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   

   Ada_Access_Subp_F_Subp_Kind_For_Ada_Access_Subp : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Access_Subp_F_Subp_Kind

         , Index => 1
   );
   Ada_Access_Subp_F_Skips_For_Ada_Access_Subp : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Access_Subp_F_Skips

         , Index => 2
   );

   Desc_For_Ada_Access_Subp : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Prelude_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaAccessSubp"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Ada_Access_Subp_F_Subp_Kind_For_Ada_Access_Subp'Access, 2 => Ada_Access_Subp_F_Skips_For_Ada_Access_Subp'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Access_Subp
   );
   


   Desc_For_Ada_Context_Clause : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 3,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Prelude_Node_Type_Id,
      Derivations =>
         (1 => Common.Ada_Pragma_Type_Id, 2 => Common.Ada_Use_Type_Id, 3 => Common.Ada_With_Type_Id),

      DSL_Name => To_Unbounded_String ("AdaContextClause"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   

   Ada_Pragma_F_Skips_For_Ada_Pragma : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Pragma_F_Skips

         , Index => 1
   );

   Desc_For_Ada_Pragma : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Context_Clause_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaPragma"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Ada_Pragma_F_Skips_For_Ada_Pragma'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Pragma
   );
   

   Ada_Use_F_Skips_For_Ada_Use : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Use_F_Skips

         , Index => 1
   );

   Desc_For_Ada_Use : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Context_Clause_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaUse"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Ada_Use_F_Skips_For_Ada_Use'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Use
   );
   

   Ada_With_F_Has_Limited_For_Ada_With : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_With_F_Has_Limited

         , Index => 1
   );
   Ada_With_F_Has_Private_For_Ada_With : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_With_F_Has_Private

         , Index => 2
   );
   Ada_With_F_Packages_For_Ada_With : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_With_F_Packages

         , Index => 3
   );

   Desc_For_Ada_With : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 3,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Context_Clause_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaWith"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Ada_With_F_Has_Limited_For_Ada_With'Access, 2 => Ada_With_F_Has_Private_For_Ada_With'Access, 3 => Ada_With_F_Packages_For_Ada_With'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_With
   );
   


   Desc_For_Ada_Entity_Kind : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 3,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Prelude_Node_Type_Id,
      Derivations =>
         (1 => Common.Ada_Entity_Kind_Function_Type_Id, 2 => Common.Ada_Entity_Kind_Package_Type_Id, 3 => Common.Ada_Entity_Kind_Procedure_Type_Id),

      DSL_Name => To_Unbounded_String ("AdaEntityKind"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   


   Desc_For_Ada_Entity_Kind_Function : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Entity_Kind_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaEntityKind.Function"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Entity_Kind_Function
   );
   


   Desc_For_Ada_Entity_Kind_Package : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Entity_Kind_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaEntityKind.Package"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Entity_Kind_Package
   );
   


   Desc_For_Ada_Entity_Kind_Procedure : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Entity_Kind_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaEntityKind.Procedure"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Entity_Kind_Procedure
   );
   

   Ada_Generic_F_Skips_For_Ada_Generic : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Generic_F_Skips

         , Index => 1
   );

   Desc_For_Ada_Generic : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Prelude_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaGeneric"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Ada_Generic_F_Skips_For_Ada_Generic'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Generic
   );
   

   Ada_Library_Item_F_Generic_Stub_For_Ada_Library_Item : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Library_Item_F_Generic_Stub

         , Index => 1
   );
   Ada_Library_Item_F_Separate_For_Ada_Library_Item : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Library_Item_F_Separate

         , Index => 2
   );
   Ada_Library_Item_F_Main_For_Ada_Library_Item : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Library_Item_F_Main

         , Index => 3
   );

   Desc_For_Ada_Library_Item : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 3,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Prelude_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaLibraryItem"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Ada_Library_Item_F_Generic_Stub_For_Ada_Library_Item'Access, 2 => Ada_Library_Item_F_Separate_For_Ada_Library_Item'Access, 3 => Ada_Library_Item_F_Main_For_Ada_Library_Item'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Library_Item
   );
   

   Ada_Main_F_Name_For_Ada_Main : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => True,
      Field               => Ada_Main_F_Name

   );

   Desc_For_Ada_Main : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 3,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Prelude_Node_Type_Id,
      Derivations =>
         (1 => Common.Ada_Pkg_Type_Id, 2 => Common.Ada_Pkg_Body_Type_Id, 3 => Common.Ada_Subp_Type_Id),

      DSL_Name => To_Unbounded_String ("AdaMain"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Ada_Main_F_Name_For_Ada_Main'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   

   Ada_Pkg_F_Has_Private_For_Ada_Pkg : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Pkg_F_Has_Private

         , Index => 1
   );
   Ada_Pkg_F_Name_For_Ada_Pkg : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Main_F_Name

         , Index => 2
   );

   Desc_For_Ada_Pkg : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Main_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaPkg"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Ada_Pkg_F_Has_Private_For_Ada_Pkg'Access, 2 => Ada_Pkg_F_Name_For_Ada_Pkg'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Pkg
   );
   

   Ada_Pkg_Body_F_Name_For_Ada_Pkg_Body : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Main_F_Name

         , Index => 1
   );

   Desc_For_Ada_Pkg_Body : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Main_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaPkgBody"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Ada_Pkg_Body_F_Name_For_Ada_Pkg_Body'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Pkg_Body
   );
   

   Ada_Subp_F_Subp_Kind_For_Ada_Subp : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Subp_F_Subp_Kind

         , Index => 1
   );
   Ada_Subp_F_Name_For_Ada_Subp : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Main_F_Name

         , Index => 2
   );

   Desc_For_Ada_Subp : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Main_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaSubp"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Ada_Subp_F_Subp_Kind_For_Ada_Subp'Access, 2 => Ada_Subp_F_Name_For_Ada_Subp'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Subp
   );
   

   Ada_Prelude_F_Context_Clauses_For_Ada_Prelude : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Prelude_F_Context_Clauses

         , Index => 1
   );
   Ada_Prelude_F_Library_Item_For_Ada_Prelude : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Prelude_F_Library_Item

         , Index => 2
   );

   Desc_For_Ada_Prelude : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Prelude_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaPrelude"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Ada_Prelude_F_Context_Clauses_For_Ada_Prelude'Access, 2 => Ada_Prelude_F_Library_Item_For_Ada_Prelude'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Prelude
   );
   

   Ada_Separate_F_Parent_Name_For_Ada_Separate : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_Separate_F_Parent_Name

         , Index => 1
   );

   Desc_For_Ada_Separate : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Prelude_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaSeparate"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Ada_Separate_F_Parent_Name_For_Ada_Separate'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Separate
   );
   


   Desc_For_Ada_Skip : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Prelude_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaSkip"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Skip
   );
   

   Ada_With_Formal_F_Kind_For_Ada_With_Formal : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_With_Formal_F_Kind

         , Index => 1
   );
   Ada_With_Formal_F_Skips_For_Ada_With_Formal : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ada_With_Formal_F_Skips

         , Index => 2
   );

   Desc_For_Ada_With_Formal : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Ada_Prelude_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaWithFormal"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Ada_With_Formal_F_Kind_For_Ada_With_Formal'Access, 2 => Ada_With_Formal_F_Skips_For_Ada_With_Formal'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_With_Formal
   );
   


   Desc_For_All_Qualifier : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 2,
      Fields_Count      => 0,
      Properties_Count  => 1,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 => Common.All_Qualifier_Absent_Type_Id, 2 => Common.All_Qualifier_Present_Type_Id),

      DSL_Name => To_Unbounded_String ("AllQualifier"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 => All_Qualifier_P_As_Bool
      )

   );
   


   Desc_For_All_Qualifier_Absent : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.All_Qualifier_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AllQualifier.Absent"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_All_Qualifier_Absent
   );
   


   Desc_For_All_Qualifier_Present : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.All_Qualifier_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AllQualifier.Present"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_All_Qualifier_Present
   );
   

   Attribute_Decl_F_Attr_Name_For_Attribute_Decl : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Attribute_Decl_F_Attr_Name

         , Index => 1
   );
   Attribute_Decl_F_Attr_Index_For_Attribute_Decl : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Attribute_Decl_F_Attr_Index

         , Index => 2
   );
   Attribute_Decl_F_Expr_For_Attribute_Decl : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Attribute_Decl_F_Expr

         , Index => 3
   );

   Desc_For_Attribute_Decl : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 3,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AttributeDecl"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Attribute_Decl_F_Attr_Name_For_Attribute_Decl'Access, 2 => Attribute_Decl_F_Attr_Index_For_Attribute_Decl'Access, 3 => Attribute_Decl_F_Expr_For_Attribute_Decl'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Attribute_Decl
   );
   

   Attribute_Reference_F_Attribute_Name_For_Attribute_Reference : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Attribute_Reference_F_Attribute_Name

         , Index => 1
   );
   Attribute_Reference_F_Attribute_Index_For_Attribute_Reference : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Attribute_Reference_F_Attribute_Index

         , Index => 2
   );

   Desc_For_Attribute_Reference : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AttributeReference"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Attribute_Reference_F_Attribute_Name_For_Attribute_Reference'Access, 2 => Attribute_Reference_F_Attribute_Index_For_Attribute_Reference'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Attribute_Reference
   );
   


   Desc_For_Base_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 10,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 => Common.Ada_Context_Clause_List_Type_Id, 2 => Common.Ada_Prelude_Node_List_Type_Id, 3 => Common.Ada_Skip_List_Type_Id, 4 => Common.Case_Item_List_Type_Id, 5 => Common.Expr_List_Type_Id, 6 => Common.Gpr_Node_List_Type_Id, 7 => Common.Identifier_List_Type_Id, 8 => Common.String_Literal_List_Type_Id, 9 => Common.Term_List_List_Type_Id, 10 => Common.With_Decl_List_Type_Id),

      DSL_Name => To_Unbounded_String ("BaseList"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   


   Desc_For_Ada_Context_Clause_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaContextClause.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Context_Clause_List
   );
   


   Desc_For_Ada_Prelude_Node_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaPreludeNode.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Prelude_Node_List
   );
   


   Desc_For_Ada_Skip_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AdaSkip.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Ada_Skip_List
   );
   


   Desc_For_Case_Item_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CaseItem.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Case_Item_List
   );
   


   Desc_For_Expr_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Expr.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Expr_List
   );
   


   Desc_For_Gpr_Node_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 2,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Base_List_Type_Id,
      Derivations =>
         (1 => Common.Choices_Type_Id, 2 => Common.Term_List_Type_Id),

      DSL_Name => To_Unbounded_String ("GprNode.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Gpr_Node_List
   );
   


   Desc_For_Choices : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Choices"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Choices
   );
   


   Desc_For_Term_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("TermList"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Term_List
   );
   


   Desc_For_Identifier_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Identifier.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Identifier_List
   );
   


   Desc_For_String_Literal_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("StringLiteral.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_String_Literal_List
   );
   


   Desc_For_Term_List_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("TermList.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Term_List_List
   );
   


   Desc_For_With_Decl_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("WithDecl.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_With_Decl_List
   );
   

   Builtin_Function_Call_F_Function_Name_For_Builtin_Function_Call : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Builtin_Function_Call_F_Function_Name

         , Index => 1
   );
   Builtin_Function_Call_F_Parameters_For_Builtin_Function_Call : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Builtin_Function_Call_F_Parameters

         , Index => 2
   );

   Desc_For_Builtin_Function_Call : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("BuiltinFunctionCall"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Builtin_Function_Call_F_Function_Name_For_Builtin_Function_Call'Access, 2 => Builtin_Function_Call_F_Parameters_For_Builtin_Function_Call'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Builtin_Function_Call
   );
   

   Case_Construction_F_Var_Ref_For_Case_Construction : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Case_Construction_F_Var_Ref

         , Index => 1
   );
   Case_Construction_F_Items_For_Case_Construction : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Case_Construction_F_Items

         , Index => 2
   );

   Desc_For_Case_Construction : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CaseConstruction"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Case_Construction_F_Var_Ref_For_Case_Construction'Access, 2 => Case_Construction_F_Items_For_Case_Construction'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Case_Construction
   );
   

   Case_Item_F_Choice_For_Case_Item : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Case_Item_F_Choice

         , Index => 1
   );
   Case_Item_F_Decls_For_Case_Item : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Case_Item_F_Decls

         , Index => 2
   );

   Desc_For_Case_Item : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CaseItem"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Case_Item_F_Choice_For_Case_Item'Access, 2 => Case_Item_F_Decls_For_Case_Item'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Case_Item
   );
   

   Compilation_Unit_F_Project_For_Compilation_Unit : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Compilation_Unit_F_Project

         , Index => 1
   );

   Desc_For_Compilation_Unit : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompilationUnit"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Compilation_Unit_F_Project_For_Compilation_Unit'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Compilation_Unit
   );
   


   Desc_For_Empty_Decl : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("EmptyDecl"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Empty_Decl
   );
   


   Desc_For_Expr : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 2,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 => Common.Prefix_Type_Id, 2 => Common.Single_Tok_Node_Type_Id),

      DSL_Name => To_Unbounded_String ("Expr"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   

   Prefix_F_Prefix_For_Prefix : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Prefix_F_Prefix

         , Index => 1
   );
   Prefix_F_Suffix_For_Prefix : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Prefix_F_Suffix

         , Index => 2
   );

   Desc_For_Prefix : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Prefix"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Prefix_F_Prefix_For_Prefix'Access, 2 => Prefix_F_Suffix_For_Prefix'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Prefix
   );
   


   Desc_For_Single_Tok_Node : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 3,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 => Common.Identifier_Type_Id, 2 => Common.Num_Literal_Type_Id, 3 => Common.String_Literal_Type_Id),

      DSL_Name => To_Unbounded_String ("SingleTokNode"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   


   Desc_For_Identifier : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Single_Tok_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Identifier"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Identifier
   );
   


   Desc_For_Num_Literal : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Single_Tok_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("NumLiteral"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Num_Literal
   );
   


   Desc_For_String_Literal : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Single_Tok_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("StringLiteral"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_String_Literal
   );
   


   Desc_For_Limited_Node : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 2,
      Fields_Count      => 0,
      Properties_Count  => 1,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 => Common.Limited_Absent_Type_Id, 2 => Common.Limited_Present_Type_Id),

      DSL_Name => To_Unbounded_String ("Limited"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 => Limited_Node_P_As_Bool
      )

   );
   


   Desc_For_Limited_Absent : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Limited_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Limited.Absent"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Limited_Absent
   );
   


   Desc_For_Limited_Present : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Limited_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Limited.Present"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Limited_Present
   );
   


   Desc_For_Others_Designator : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("OthersDesignator"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Others_Designator
   );
   

   Package_Decl_F_Pkg_Name_For_Package_Decl : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Package_Decl_F_Pkg_Name

         , Index => 1
   );
   Package_Decl_F_Pkg_Spec_For_Package_Decl : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Package_Decl_F_Pkg_Spec

         , Index => 2
   );

   Desc_For_Package_Decl : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("PackageDecl"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Package_Decl_F_Pkg_Name_For_Package_Decl'Access, 2 => Package_Decl_F_Pkg_Spec_For_Package_Decl'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Package_Decl
   );
   

   Package_Extension_F_Extended_Name_For_Package_Extension : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Package_Extension_F_Extended_Name

         , Index => 1
   );

   Desc_For_Package_Extension : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("PackageExtension"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Package_Extension_F_Extended_Name_For_Package_Extension'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Package_Extension
   );
   

   Package_Renaming_F_Renamed_Name_For_Package_Renaming : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Package_Renaming_F_Renamed_Name

         , Index => 1
   );

   Desc_For_Package_Renaming : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("PackageRenaming"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Package_Renaming_F_Renamed_Name_For_Package_Renaming'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Package_Renaming
   );
   

   Package_Spec_F_Extension_For_Package_Spec : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Package_Spec_F_Extension

         , Index => 1
   );
   Package_Spec_F_Decls_For_Package_Spec : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Package_Spec_F_Decls

         , Index => 2
   );
   Package_Spec_F_End_Name_For_Package_Spec : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Package_Spec_F_End_Name

         , Index => 3
   );

   Desc_For_Package_Spec : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 3,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("PackageSpec"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Package_Spec_F_Extension_For_Package_Spec'Access, 2 => Package_Spec_F_Decls_For_Package_Spec'Access, 3 => Package_Spec_F_End_Name_For_Package_Spec'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Package_Spec
   );
   


   Desc_For_Private_Node : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 2,
      Fields_Count      => 0,
      Properties_Count  => 1,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 => Common.Private_Absent_Type_Id, 2 => Common.Private_Present_Type_Id),

      DSL_Name => To_Unbounded_String ("Private"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 => Private_Node_P_As_Bool
      )

   );
   


   Desc_For_Private_Absent : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Private_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Private.Absent"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Private_Absent
   );
   


   Desc_For_Private_Present : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Private_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Private.Present"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Private_Present
   );
   

   Project_F_Context_Clauses_For_Project : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Project_F_Context_Clauses

         , Index => 1
   );
   Project_F_Project_Decl_For_Project : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Project_F_Project_Decl

         , Index => 2
   );

   Desc_For_Project : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Project"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Project_F_Context_Clauses_For_Project'Access, 2 => Project_F_Project_Decl_For_Project'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Project
   );
   

   Project_Declaration_F_Qualifier_For_Project_Declaration : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Project_Declaration_F_Qualifier

         , Index => 1
   );
   Project_Declaration_F_Project_Name_For_Project_Declaration : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Project_Declaration_F_Project_Name

         , Index => 2
   );
   Project_Declaration_F_Extension_For_Project_Declaration : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Project_Declaration_F_Extension

         , Index => 3
   );
   Project_Declaration_F_Decls_For_Project_Declaration : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Project_Declaration_F_Decls

         , Index => 4
   );
   Project_Declaration_F_End_Name_For_Project_Declaration : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Project_Declaration_F_End_Name

         , Index => 5
   );

   Desc_For_Project_Declaration : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 5,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ProjectDeclaration"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Project_Declaration_F_Qualifier_For_Project_Declaration'Access, 2 => Project_Declaration_F_Project_Name_For_Project_Declaration'Access, 3 => Project_Declaration_F_Extension_For_Project_Declaration'Access, 4 => Project_Declaration_F_Decls_For_Project_Declaration'Access, 5 => Project_Declaration_F_End_Name_For_Project_Declaration'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Project_Declaration
   );
   

   Project_Extension_F_Is_All_For_Project_Extension : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Project_Extension_F_Is_All

         , Index => 1
   );
   Project_Extension_F_Path_Name_For_Project_Extension : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Project_Extension_F_Path_Name

         , Index => 2
   );

   Desc_For_Project_Extension : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ProjectExtension"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Project_Extension_F_Is_All_For_Project_Extension'Access, 2 => Project_Extension_F_Path_Name_For_Project_Extension'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Project_Extension
   );
   


   Desc_For_Project_Qualifier : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 6,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 => Common.Project_Qualifier_Abstract_Type_Id, 2 => Common.Project_Qualifier_Aggregate_Type_Id, 3 => Common.Project_Qualifier_Aggregate_Library_Type_Id, 4 => Common.Project_Qualifier_Configuration_Type_Id, 5 => Common.Project_Qualifier_Library_Type_Id, 6 => Common.Project_Qualifier_Standard_Type_Id),

      DSL_Name => To_Unbounded_String ("ProjectQualifier"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   


   Desc_For_Project_Qualifier_Abstract : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Project_Qualifier_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ProjectQualifier.Abstract"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Project_Qualifier_Abstract
   );
   


   Desc_For_Project_Qualifier_Aggregate : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Project_Qualifier_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ProjectQualifier.Aggregate"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Project_Qualifier_Aggregate
   );
   


   Desc_For_Project_Qualifier_Aggregate_Library : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Project_Qualifier_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ProjectQualifier.AggregateLibrary"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Project_Qualifier_Aggregate_Library
   );
   


   Desc_For_Project_Qualifier_Configuration : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Project_Qualifier_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ProjectQualifier.Configuration"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Project_Qualifier_Configuration
   );
   


   Desc_For_Project_Qualifier_Library : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Project_Qualifier_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ProjectQualifier.Library"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Project_Qualifier_Library
   );
   


   Desc_For_Project_Qualifier_Standard : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Project_Qualifier_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ProjectQualifier.Standard"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Project_Qualifier_Standard
   );
   

   Project_Reference_F_Attr_Ref_For_Project_Reference : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Project_Reference_F_Attr_Ref

         , Index => 1
   );

   Desc_For_Project_Reference : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ProjectReference"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Project_Reference_F_Attr_Ref_For_Project_Reference'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Project_Reference
   );
   

   String_Literal_At_F_Str_Lit_For_String_Literal_At : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => String_Literal_At_F_Str_Lit

         , Index => 1
   );
   String_Literal_At_F_At_Lit_For_String_Literal_At : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => String_Literal_At_F_At_Lit

         , Index => 2
   );

   Desc_For_String_Literal_At : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("StringLiteralAt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => String_Literal_At_F_Str_Lit_For_String_Literal_At'Access, 2 => String_Literal_At_F_At_Lit_For_String_Literal_At'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_String_Literal_At
   );
   

   Terms_F_Terms_For_Terms : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Terms_F_Terms

         , Index => 1
   );

   Desc_For_Terms : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Terms"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Terms_F_Terms_For_Terms'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Terms
   );
   

   Type_Reference_F_Var_Type_Name_For_Type_Reference : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Type_Reference_F_Var_Type_Name

         , Index => 1
   );

   Desc_For_Type_Reference : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("TypeReference"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Type_Reference_F_Var_Type_Name_For_Type_Reference'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Type_Reference
   );
   

   Typed_String_Decl_F_Type_Id_For_Typed_String_Decl : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Typed_String_Decl_F_Type_Id

         , Index => 1
   );
   Typed_String_Decl_F_String_Literals_For_Typed_String_Decl : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Typed_String_Decl_F_String_Literals

         , Index => 2
   );

   Desc_For_Typed_String_Decl : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("TypedStringDecl"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Typed_String_Decl_F_Type_Id_For_Typed_String_Decl'Access, 2 => Typed_String_Decl_F_String_Literals_For_Typed_String_Decl'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Typed_String_Decl
   );
   

   Variable_Decl_F_Var_Name_For_Variable_Decl : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Variable_Decl_F_Var_Name

         , Index => 1
   );
   Variable_Decl_F_Var_Type_For_Variable_Decl : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Variable_Decl_F_Var_Type

         , Index => 2
   );
   Variable_Decl_F_Expr_For_Variable_Decl : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Variable_Decl_F_Expr

         , Index => 3
   );

   Desc_For_Variable_Decl : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 3,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("VariableDecl"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Variable_Decl_F_Var_Name_For_Variable_Decl'Access, 2 => Variable_Decl_F_Var_Type_For_Variable_Decl'Access, 3 => Variable_Decl_F_Expr_For_Variable_Decl'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Variable_Decl
   );
   

   Variable_Reference_F_Variable_Name_For_Variable_Reference : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Variable_Reference_F_Variable_Name

         , Index => 1
   );
   Variable_Reference_F_Attribute_Ref_For_Variable_Reference : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Variable_Reference_F_Attribute_Ref

         , Index => 2
   );

   Desc_For_Variable_Reference : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("VariableReference"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Variable_Reference_F_Variable_Name_For_Variable_Reference'Access, 2 => Variable_Reference_F_Attribute_Ref_For_Variable_Reference'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_Variable_Reference
   );
   

   With_Decl_F_Is_Limited_For_With_Decl : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => With_Decl_F_Is_Limited

         , Index => 1
   );
   With_Decl_F_Path_Names_For_With_Decl : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => With_Decl_F_Path_Names

         , Index => 2
   );

   Desc_For_With_Decl : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Gpr_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("WithDecl"),

      Inherited_Fields => 0,
      Fields           => (
            1 => With_Decl_F_Is_Limited_For_With_Decl'Access, 2 => With_Decl_F_Path_Names_For_With_Decl'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Gpr_With_Decl
   );

   Node_Type_Descriptors : constant
      array (Node_Type_Id) of Node_Type_Descriptor_Access
   := (Desc_For_Gpr_Node'Access, Desc_For_Ada_Prelude_Node'Access, Desc_For_Ada_Access_Subp'Access, Desc_For_Ada_Context_Clause'Access, Desc_For_Ada_Pragma'Access, Desc_For_Ada_Use'Access, Desc_For_Ada_With'Access, Desc_For_Ada_Entity_Kind'Access, Desc_For_Ada_Entity_Kind_Function'Access, Desc_For_Ada_Entity_Kind_Package'Access, Desc_For_Ada_Entity_Kind_Procedure'Access, Desc_For_Ada_Generic'Access, Desc_For_Ada_Library_Item'Access, Desc_For_Ada_Main'Access, Desc_For_Ada_Pkg'Access, Desc_For_Ada_Pkg_Body'Access, Desc_For_Ada_Subp'Access, Desc_For_Ada_Prelude'Access, Desc_For_Ada_Separate'Access, Desc_For_Ada_Skip'Access, Desc_For_Ada_With_Formal'Access, Desc_For_All_Qualifier'Access, Desc_For_All_Qualifier_Absent'Access, Desc_For_All_Qualifier_Present'Access, Desc_For_Attribute_Decl'Access, Desc_For_Attribute_Reference'Access, Desc_For_Base_List'Access, Desc_For_Ada_Context_Clause_List'Access, Desc_For_Ada_Prelude_Node_List'Access, Desc_For_Ada_Skip_List'Access, Desc_For_Case_Item_List'Access, Desc_For_Expr_List'Access, Desc_For_Gpr_Node_List'Access, Desc_For_Choices'Access, Desc_For_Term_List'Access, Desc_For_Identifier_List'Access, Desc_For_String_Literal_List'Access, Desc_For_Term_List_List'Access, Desc_For_With_Decl_List'Access, Desc_For_Builtin_Function_Call'Access, Desc_For_Case_Construction'Access, Desc_For_Case_Item'Access, Desc_For_Compilation_Unit'Access, Desc_For_Empty_Decl'Access, Desc_For_Expr'Access, Desc_For_Prefix'Access, Desc_For_Single_Tok_Node'Access, Desc_For_Identifier'Access, Desc_For_Num_Literal'Access, Desc_For_String_Literal'Access, Desc_For_Limited_Node'Access, Desc_For_Limited_Absent'Access, Desc_For_Limited_Present'Access, Desc_For_Others_Designator'Access, Desc_For_Package_Decl'Access, Desc_For_Package_Extension'Access, Desc_For_Package_Renaming'Access, Desc_For_Package_Spec'Access, Desc_For_Private_Node'Access, Desc_For_Private_Absent'Access, Desc_For_Private_Present'Access, Desc_For_Project'Access, Desc_For_Project_Declaration'Access, Desc_For_Project_Extension'Access, Desc_For_Project_Qualifier'Access, Desc_For_Project_Qualifier_Abstract'Access, Desc_For_Project_Qualifier_Aggregate'Access, Desc_For_Project_Qualifier_Aggregate_Library'Access, Desc_For_Project_Qualifier_Configuration'Access, Desc_For_Project_Qualifier_Library'Access, Desc_For_Project_Qualifier_Standard'Access, Desc_For_Project_Reference'Access, Desc_For_String_Literal_At'Access, Desc_For_Terms'Access, Desc_For_Type_Reference'Access, Desc_For_Typed_String_Decl'Access, Desc_For_Variable_Decl'Access, Desc_For_Variable_Reference'Access, Desc_For_With_Decl'Access);

   ----------------------
   -- Various mappings --
   ----------------------

   package Node_Type_Id_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Node_Type_Id,
      Equivalent_Keys => "=",
      Hash            => Hash);

   DSL_Name_To_Node_Type : Node_Type_Id_Maps.Map;
   --  Lookup table for DSL names to node type references. Created at
   --  elaboration time and never updated after.

   Kind_To_Id : constant array (Gpr_Node_Kind_Type) of Node_Type_Id := (
      Gpr_Ada_Access_Subp => Common.Ada_Access_Subp_Type_Id, Gpr_Ada_Pragma => Common.Ada_Pragma_Type_Id, Gpr_Ada_Use => Common.Ada_Use_Type_Id, Gpr_Ada_With => Common.Ada_With_Type_Id, Gpr_Ada_Entity_Kind_Function => Common.Ada_Entity_Kind_Function_Type_Id, Gpr_Ada_Entity_Kind_Package => Common.Ada_Entity_Kind_Package_Type_Id, Gpr_Ada_Entity_Kind_Procedure => Common.Ada_Entity_Kind_Procedure_Type_Id, Gpr_Ada_Generic => Common.Ada_Generic_Type_Id, Gpr_Ada_Library_Item => Common.Ada_Library_Item_Type_Id, Gpr_Ada_Pkg => Common.Ada_Pkg_Type_Id, Gpr_Ada_Pkg_Body => Common.Ada_Pkg_Body_Type_Id, Gpr_Ada_Subp => Common.Ada_Subp_Type_Id, Gpr_Ada_Prelude => Common.Ada_Prelude_Type_Id, Gpr_Ada_Separate => Common.Ada_Separate_Type_Id, Gpr_Ada_Skip => Common.Ada_Skip_Type_Id, Gpr_Ada_With_Formal => Common.Ada_With_Formal_Type_Id, Gpr_All_Qualifier_Absent => Common.All_Qualifier_Absent_Type_Id, Gpr_All_Qualifier_Present => Common.All_Qualifier_Present_Type_Id, Gpr_Attribute_Decl => Common.Attribute_Decl_Type_Id, Gpr_Attribute_Reference => Common.Attribute_Reference_Type_Id, Gpr_Ada_Context_Clause_List => Common.Ada_Context_Clause_List_Type_Id, Gpr_Ada_Prelude_Node_List => Common.Ada_Prelude_Node_List_Type_Id, Gpr_Ada_Skip_List => Common.Ada_Skip_List_Type_Id, Gpr_Case_Item_List => Common.Case_Item_List_Type_Id, Gpr_Expr_List => Common.Expr_List_Type_Id, Gpr_Gpr_Node_List => Common.Gpr_Node_List_Type_Id, Gpr_Choices => Common.Choices_Type_Id, Gpr_Term_List => Common.Term_List_Type_Id, Gpr_Identifier_List => Common.Identifier_List_Type_Id, Gpr_String_Literal_List => Common.String_Literal_List_Type_Id, Gpr_Term_List_List => Common.Term_List_List_Type_Id, Gpr_With_Decl_List => Common.With_Decl_List_Type_Id, Gpr_Builtin_Function_Call => Common.Builtin_Function_Call_Type_Id, Gpr_Case_Construction => Common.Case_Construction_Type_Id, Gpr_Case_Item => Common.Case_Item_Type_Id, Gpr_Compilation_Unit => Common.Compilation_Unit_Type_Id, Gpr_Empty_Decl => Common.Empty_Decl_Type_Id, Gpr_Prefix => Common.Prefix_Type_Id, Gpr_Identifier => Common.Identifier_Type_Id, Gpr_Num_Literal => Common.Num_Literal_Type_Id, Gpr_String_Literal => Common.String_Literal_Type_Id, Gpr_Limited_Absent => Common.Limited_Absent_Type_Id, Gpr_Limited_Present => Common.Limited_Present_Type_Id, Gpr_Others_Designator => Common.Others_Designator_Type_Id, Gpr_Package_Decl => Common.Package_Decl_Type_Id, Gpr_Package_Extension => Common.Package_Extension_Type_Id, Gpr_Package_Renaming => Common.Package_Renaming_Type_Id, Gpr_Package_Spec => Common.Package_Spec_Type_Id, Gpr_Private_Absent => Common.Private_Absent_Type_Id, Gpr_Private_Present => Common.Private_Present_Type_Id, Gpr_Project => Common.Project_Type_Id, Gpr_Project_Declaration => Common.Project_Declaration_Type_Id, Gpr_Project_Extension => Common.Project_Extension_Type_Id, Gpr_Project_Qualifier_Abstract => Common.Project_Qualifier_Abstract_Type_Id, Gpr_Project_Qualifier_Aggregate => Common.Project_Qualifier_Aggregate_Type_Id, Gpr_Project_Qualifier_Aggregate_Library => Common.Project_Qualifier_Aggregate_Library_Type_Id, Gpr_Project_Qualifier_Configuration => Common.Project_Qualifier_Configuration_Type_Id, Gpr_Project_Qualifier_Library => Common.Project_Qualifier_Library_Type_Id, Gpr_Project_Qualifier_Standard => Common.Project_Qualifier_Standard_Type_Id, Gpr_Project_Reference => Common.Project_Reference_Type_Id, Gpr_String_Literal_At => Common.String_Literal_At_Type_Id, Gpr_Terms => Common.Terms_Type_Id, Gpr_Type_Reference => Common.Type_Reference_Type_Id, Gpr_Typed_String_Decl => Common.Typed_String_Decl_Type_Id, Gpr_Variable_Decl => Common.Variable_Decl_Type_Id, Gpr_Variable_Reference => Common.Variable_Reference_Type_Id, Gpr_With_Decl => Common.With_Decl_Type_Id
   );

   ------------------
   -- Struct types --
   ------------------

   function Struct_Type_Desc
     (Kind : Struct_Value_Kind) return Struct_Type_Descriptor_Access;
   --  Return the type descriptor corresponding to the given struct type

   function Struct_Field_Name
     (Field : Struct_Field_Reference) return Text_Type;
   --  Helper for Member_Name: take care of structs

   function Struct_Field_Type
     (Field : Struct_Field_Reference) return Type_Constraint;
   --  Helper for Member_Type: take care of structs

   function Struct_Fields
     (Kind : Struct_Value_Kind) return Struct_Field_Reference_Array;
   --  Implementation for Introspection.Struct_Fields

   ----------------
   -- Node types --
   ----------------

   function DSL_Name (Id : Node_Type_Id) return Text_Type;
   --  Implementation for Introspection.DSL_Name

   function Lookup_DSL_Name (Name : Text_Type) return Any_Node_Type_Id;
   --  Implementation for Introspection.Lookup_DSL_Name

   function Is_Abstract (Id : Node_Type_Id) return Boolean;
   --  Implementation for Introspection.Is_Abstract

   function Is_Concrete (Id : Node_Type_Id) return Boolean
   is (not Is_Abstract (Id));

   function Kind_For (Id : Node_Type_Id) return Gpr_Node_Kind_Type;
   --  Implementation for Introspection.Kind_For

   function First_Kind_For (Id : Node_Type_Id) return Gpr_Node_Kind_Type;
   --  Implementation for Introspection.First_Kind_For

   function Last_Kind_For (Id : Node_Type_Id) return Gpr_Node_Kind_Type;
   --  Implementation for Introspection.Last_Kind_For

   function Id_For_Kind (Kind : Gpr_Node_Kind_Type) return Node_Type_Id;
   --  Implementation for Introspection.Id_For_Kind

   function Is_Root_Node (Id : Node_Type_Id) return Boolean;
   --  Implementation for Introspection.Is_Root_NOde

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id;
   --  Implementation for Introspection.Base_Type

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array;
   --  Implementation for Introspection.Derived_Types

   function Is_Derived_From (Id, Parent : Node_Type_Id) return Boolean;
   --  Implementation for Introspection.Is_Derived_From

   ------------
   -- Member --
   ------------

   function Member_Name (Member : Member_Reference) return Text_Type;
   --  Implementation for Introspection.Member_Name

   function Member_Type (Member : Member_Reference) return Type_Constraint;
   --  Implementation for Introspection.Member_Type

   function Lookup_Member_Struct
     (Kind : Struct_Value_Kind;
      Name : Text_Type) return Any_Member_Reference;
   --  Helper for Introspection.Lookup_Member: take care of struct types

   function Lookup_Member_Node
     (Id   : Node_Type_Id;
      Name : Text_Type) return Any_Member_Reference;
   --  Helper for Introspection.Lookup_Member: take care of nodes

   -------------------
   -- Syntax fields --
   -------------------

   function Syntax_Field_Name
     (Field : Syntax_Field_Reference) return Text_Type;
   --  Helper for Member_Name: take care of syntax fields

   function Syntax_Field_Type
     (Field : Syntax_Field_Reference) return Node_Type_Id;
   --  Helper for Member_Type: take care of syntax fields

   function Eval_Syntax_Field
     (Node  : Bare_Gpr_Node;
      Field : Syntax_Field_Reference) return Bare_Gpr_Node;
   --  Implementation for Introspection.Eval_Field

   function Index
     (Kind : Gpr_Node_Kind_Type; Field : Syntax_Field_Reference) return Positive;
   --  Implementation for Introspection.Index

   function Syntax_Field_Reference_From_Index
     (Kind : Gpr_Node_Kind_Type; Index : Positive) return Syntax_Field_Reference;
   --  Implementation for Introspection.Syntax_Field_Reference_From_Index

   function Syntax_Fields
     (Id            : Node_Type_Id;
      Concrete_Only : Boolean) return Syntax_Field_Reference_Array;
   --  Return the list of fields associated to ``Id``. If ``Concrete_Only`` is
   --  true, collect only non-null and concrete fields. Otherwise, collect all
   --  fields.

   function Syntax_Fields
     (Kind : Gpr_Node_Kind_Type) return Syntax_Field_Reference_Array;
   --  Implementation for Introspection.Fields

   function Syntax_Fields
     (Id : Node_Type_Id) return Syntax_Field_Reference_Array;
   --  Implementation for Introspection.Fields

   ----------------
   -- Properties --
   ----------------

   function Property_Name (Property : Property_Reference) return Text_Type;
   --  Helper for Member_Name: take care of properties

   function Property_Return_Type
     (Property : Property_Reference) return Type_Constraint;
   --  Helper for Member_Type: take care of properties

   function Property_Argument_Types
     (Property : Property_Reference) return Type_Constraint_Array;
   --  Implementation for Introspection.Property_Argument_Types

   function Property_Argument_Name
     (Property        : Property_Reference;
      Argument_Number : Positive) return Text_Type;
   --  Implementation for Introspection.Property_Argument_Name

   function Property_Argument_Default_Value
     (Property        : Property_Reference;
      Argument_Number : Positive) return Internal_Value;
   --  Implementation for Introspection.Property_Argument_Default_Value

   function Properties (Kind : Gpr_Node_Kind_Type) return Property_Reference_Array;
   --  Implementation for Introspection.Properties

   function Properties (Id : Node_Type_Id) return Property_Reference_Array;
   --  Implementation for Introspection.Properties

   procedure Check_Argument_Number
     (Desc : Property_Descriptor; Argument_Number : Positive);
   --  Raise a ``Property_Error`` if ``Argument_Number`` is not valid for the
   --  property that ``Desc`` describes. Do nothing otherwise.


   ------------
   -- Tokens --
   ------------

   function Token_Node_Kind (Kind : Gpr_Node_Kind_Type) return Token_Kind;
   --  Implementation for Introspection.Token_Node_Kind

end Gpr_Parser.Introspection_Implementation;
