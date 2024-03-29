
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


with Ada.Unchecked_Deallocation;

with Gpr_Parser_Support.Generic_API; use Gpr_Parser_Support.Generic_API;
pragma Warnings (Off, "referenced");
with Gpr_Parser_Support.Generic_API.Analysis;
use Gpr_Parser_Support.Generic_API.Analysis;
pragma Warnings (On, "referenced");
with Gpr_Parser_Support.Generic_API.Introspection;
use Gpr_Parser_Support.Generic_API.Introspection;
with Gpr_Parser_Support.Internal.Introspection;
use Gpr_Parser_Support.Internal.Introspection;
with Gpr_Parser_Support.Text;        use Gpr_Parser_Support.Text;

with Gpr_Parser.Analysis; use Gpr_Parser.Analysis;
with Gpr_Parser.Common;   use Gpr_Parser.Common;

--  This package provides description tables to enable the generic
--  introspection API in Gpr_Parser_Support to work with this Langkit-generated
--  library.

private package Gpr_Parser.Generic_Introspection is

   

   --------------------------
   -- Type index constants --
   --------------------------

      Type_Index_For_Analysis_Unit : constant Type_Index := 1;
      Type_Index_For_Big_Int : constant Type_Index := 2;
      Type_Index_For_Bool : constant Type_Index := 3;
      Type_Index_For_Character : constant Type_Index := 4;
      Type_Index_For_Int : constant Type_Index := 5;
      Type_Index_For_Source_Location_Range : constant Type_Index := 6;
      Type_Index_For_String : constant Type_Index := 7;
      Type_Index_For_Token : constant Type_Index := 8;
      Type_Index_For_Symbol : constant Type_Index := 9;
      Type_Index_For_Analysis_Unit_Kind : constant Type_Index := 10;
      Type_Index_For_Lookup_Kind : constant Type_Index := 11;
      Type_Index_For_Designated_Env_Kind : constant Type_Index := 12;
      Type_Index_For_Grammar_Rule : constant Type_Index := 13;
      Type_Index_For_Gpr_Node_Array : constant Type_Index := 14;
      Type_Index_For_Gpr_Node : constant Type_Index := 15;
      Type_Index_For_All_Qualifier : constant Type_Index := 16;
      Type_Index_For_All_Qualifier_Absent : constant Type_Index := 17;
      Type_Index_For_All_Qualifier_Present : constant Type_Index := 18;
      Type_Index_For_Attribute_Decl : constant Type_Index := 19;
      Type_Index_For_Attribute_Reference : constant Type_Index := 20;
      Type_Index_For_Base_List : constant Type_Index := 21;
      Type_Index_For_Case_Item_List : constant Type_Index := 22;
      Type_Index_For_Gpr_Node_List : constant Type_Index := 23;
      Type_Index_For_Choices : constant Type_Index := 24;
      Type_Index_For_Term_List : constant Type_Index := 25;
      Type_Index_For_Identifier_List : constant Type_Index := 26;
      Type_Index_For_String_Literal_List : constant Type_Index := 27;
      Type_Index_For_Term_List_List : constant Type_Index := 28;
      Type_Index_For_With_Decl_List : constant Type_Index := 29;
      Type_Index_For_Builtin_Function_Call : constant Type_Index := 30;
      Type_Index_For_Case_Construction : constant Type_Index := 31;
      Type_Index_For_Case_Item : constant Type_Index := 32;
      Type_Index_For_Compilation_Unit : constant Type_Index := 33;
      Type_Index_For_Empty_Decl : constant Type_Index := 34;
      Type_Index_For_Expr : constant Type_Index := 35;
      Type_Index_For_Prefix : constant Type_Index := 36;
      Type_Index_For_Single_Tok_Node : constant Type_Index := 37;
      Type_Index_For_Identifier : constant Type_Index := 38;
      Type_Index_For_Num_Literal : constant Type_Index := 39;
      Type_Index_For_String_Literal : constant Type_Index := 40;
      Type_Index_For_Limited : constant Type_Index := 41;
      Type_Index_For_Limited_Absent : constant Type_Index := 42;
      Type_Index_For_Limited_Present : constant Type_Index := 43;
      Type_Index_For_Others_Designator : constant Type_Index := 44;
      Type_Index_For_Package_Decl : constant Type_Index := 45;
      Type_Index_For_Package_Extension : constant Type_Index := 46;
      Type_Index_For_Package_Renaming : constant Type_Index := 47;
      Type_Index_For_Package_Spec : constant Type_Index := 48;
      Type_Index_For_Project : constant Type_Index := 49;
      Type_Index_For_Project_Declaration : constant Type_Index := 50;
      Type_Index_For_Project_Extension : constant Type_Index := 51;
      Type_Index_For_Project_Qualifier : constant Type_Index := 52;
      Type_Index_For_Project_Qualifier_Abstract : constant Type_Index := 53;
      Type_Index_For_Project_Qualifier_Aggregate : constant Type_Index := 54;
      Type_Index_For_Project_Qualifier_Aggregate_Library : constant Type_Index := 55;
      Type_Index_For_Project_Qualifier_Configuration : constant Type_Index := 56;
      Type_Index_For_Project_Qualifier_Library : constant Type_Index := 57;
      Type_Index_For_Project_Qualifier_Standard : constant Type_Index := 58;
      Type_Index_For_String_Literal_At : constant Type_Index := 59;
      Type_Index_For_Terms : constant Type_Index := 60;
      Type_Index_For_Type_Reference : constant Type_Index := 61;
      Type_Index_For_Typed_String_Decl : constant Type_Index := 62;
      Type_Index_For_Variable_Decl : constant Type_Index := 63;
      Type_Index_For_Variable_Reference : constant Type_Index := 64;
      Type_Index_For_With_Decl : constant Type_Index := 65;

   ----------------------------
   -- Member index constants --
   ----------------------------

      Member_Index_For_Attribute_Decl_F_Attr_Name : constant Struct_Member_Index := 1;
      Member_Index_For_Attribute_Decl_F_Attr_Index : constant Struct_Member_Index := 2;
      Member_Index_For_Attribute_Decl_F_Expr : constant Struct_Member_Index := 3;
      Member_Index_For_Attribute_Reference_F_Attribute_Name : constant Struct_Member_Index := 4;
      Member_Index_For_Attribute_Reference_F_Attribute_Index : constant Struct_Member_Index := 5;
      Member_Index_For_Builtin_Function_Call_F_Function_Name : constant Struct_Member_Index := 6;
      Member_Index_For_Builtin_Function_Call_F_Parameters : constant Struct_Member_Index := 7;
      Member_Index_For_Case_Construction_F_Var_Ref : constant Struct_Member_Index := 8;
      Member_Index_For_Case_Construction_F_Items : constant Struct_Member_Index := 9;
      Member_Index_For_Case_Item_F_Choice : constant Struct_Member_Index := 10;
      Member_Index_For_Case_Item_F_Decls : constant Struct_Member_Index := 11;
      Member_Index_For_Compilation_Unit_F_Project : constant Struct_Member_Index := 12;
      Member_Index_For_Prefix_F_Prefix : constant Struct_Member_Index := 13;
      Member_Index_For_Prefix_F_Suffix : constant Struct_Member_Index := 14;
      Member_Index_For_Package_Decl_F_Pkg_Name : constant Struct_Member_Index := 15;
      Member_Index_For_Package_Decl_F_Pkg_Spec : constant Struct_Member_Index := 16;
      Member_Index_For_Package_Extension_F_Extended_Name : constant Struct_Member_Index := 17;
      Member_Index_For_Package_Renaming_F_Renamed_Name : constant Struct_Member_Index := 18;
      Member_Index_For_Package_Spec_F_Extension : constant Struct_Member_Index := 19;
      Member_Index_For_Package_Spec_F_Decls : constant Struct_Member_Index := 20;
      Member_Index_For_Package_Spec_F_End_Name : constant Struct_Member_Index := 21;
      Member_Index_For_Project_F_Context_Clauses : constant Struct_Member_Index := 22;
      Member_Index_For_Project_F_Project_Decl : constant Struct_Member_Index := 23;
      Member_Index_For_Project_Declaration_F_Qualifier : constant Struct_Member_Index := 24;
      Member_Index_For_Project_Declaration_F_Project_Name : constant Struct_Member_Index := 25;
      Member_Index_For_Project_Declaration_F_Extension : constant Struct_Member_Index := 26;
      Member_Index_For_Project_Declaration_F_Decls : constant Struct_Member_Index := 27;
      Member_Index_For_Project_Declaration_F_End_Name : constant Struct_Member_Index := 28;
      Member_Index_For_Project_Extension_F_Is_All : constant Struct_Member_Index := 29;
      Member_Index_For_Project_Extension_F_Path_Name : constant Struct_Member_Index := 30;
      Member_Index_For_String_Literal_At_F_Str_Lit : constant Struct_Member_Index := 31;
      Member_Index_For_String_Literal_At_F_At_Lit : constant Struct_Member_Index := 32;
      Member_Index_For_Terms_F_Terms : constant Struct_Member_Index := 33;
      Member_Index_For_Type_Reference_F_Var_Type_Name : constant Struct_Member_Index := 34;
      Member_Index_For_Typed_String_Decl_F_Type_Id : constant Struct_Member_Index := 35;
      Member_Index_For_Typed_String_Decl_F_String_Literals : constant Struct_Member_Index := 36;
      Member_Index_For_Variable_Decl_F_Var_Name : constant Struct_Member_Index := 37;
      Member_Index_For_Variable_Decl_F_Var_Type : constant Struct_Member_Index := 38;
      Member_Index_For_Variable_Decl_F_Expr : constant Struct_Member_Index := 39;
      Member_Index_For_Variable_Reference_F_Variable_Name : constant Struct_Member_Index := 40;
      Member_Index_For_Variable_Reference_F_Attribute_Ref : constant Struct_Member_Index := 41;
      Member_Index_For_With_Decl_F_Is_Limited : constant Struct_Member_Index := 42;
      Member_Index_For_With_Decl_F_Path_Names : constant Struct_Member_Index := 43;
      Member_Index_For_Parent : constant Struct_Member_Index := 44;
      Member_Index_For_Parents : constant Struct_Member_Index := 45;
      Member_Index_For_Children : constant Struct_Member_Index := 46;
      Member_Index_For_Token_Start : constant Struct_Member_Index := 47;
      Member_Index_For_Token_End : constant Struct_Member_Index := 48;
      Member_Index_For_Child_Index : constant Struct_Member_Index := 49;
      Member_Index_For_Previous_Sibling : constant Struct_Member_Index := 50;
      Member_Index_For_Next_Sibling : constant Struct_Member_Index := 51;
      Member_Index_For_Unit : constant Struct_Member_Index := 52;
      Member_Index_For_Is_Ghost : constant Struct_Member_Index := 53;
      Member_Index_For_Full_Sloc_Image : constant Struct_Member_Index := 54;
      Member_Index_For_All_Qualifier_P_As_Bool : constant Struct_Member_Index := 55;
      Member_Index_For_Limited_Node_P_As_Bool : constant Struct_Member_Index := 56;

   ------------------------------
   -- Grammar rule descriptors --
   ------------------------------

   
      
      Rule_Name_1 : aliased constant Text_Type :=
        "Project_Qualifier";
      Rule_Doc_1 : aliased constant Text_Type :=
        "";
      Rule_Desc_1 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_1'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_1'Access,
         Return_Type => Type_Index_For_Project_Qualifier);
      
      Rule_Name_2 : aliased constant Text_Type :=
        "Project_Extension";
      Rule_Doc_2 : aliased constant Text_Type :=
        "";
      Rule_Desc_2 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_2'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_2'Access,
         Return_Type => Type_Index_For_Project_Extension);
      
      Rule_Name_3 : aliased constant Text_Type :=
        "Project_Declaration";
      Rule_Doc_3 : aliased constant Text_Type :=
        "";
      Rule_Desc_3 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_3'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_3'Access,
         Return_Type => Type_Index_For_Project_Declaration);
      
      Rule_Name_4 : aliased constant Text_Type :=
        "Project";
      Rule_Doc_4 : aliased constant Text_Type :=
        "";
      Rule_Desc_4 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_4'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_4'Access,
         Return_Type => Type_Index_For_Project);
      
      Rule_Name_5 : aliased constant Text_Type :=
        "Declarative_Items";
      Rule_Doc_5 : aliased constant Text_Type :=
        "";
      Rule_Desc_5 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_5'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_5'Access,
         Return_Type => Type_Index_For_Gpr_Node_List);
      
      Rule_Name_6 : aliased constant Text_Type :=
        "Declarative_Item";
      Rule_Doc_6 : aliased constant Text_Type :=
        "";
      Rule_Desc_6 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_6'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_6'Access,
         Return_Type => Type_Index_For_Gpr_Node);
      
      Rule_Name_7 : aliased constant Text_Type :=
        "Simple_Declarative_Items";
      Rule_Doc_7 : aliased constant Text_Type :=
        "";
      Rule_Desc_7 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_7'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_7'Access,
         Return_Type => Type_Index_For_Gpr_Node_List);
      
      Rule_Name_8 : aliased constant Text_Type :=
        "Simple_Declarative_Item";
      Rule_Doc_8 : aliased constant Text_Type :=
        "";
      Rule_Desc_8 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_8'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_8'Access,
         Return_Type => Type_Index_For_Gpr_Node);
      
      Rule_Name_9 : aliased constant Text_Type :=
        "Variable_Decl";
      Rule_Doc_9 : aliased constant Text_Type :=
        "";
      Rule_Desc_9 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_9'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_9'Access,
         Return_Type => Type_Index_For_Variable_Decl);
      
      Rule_Name_10 : aliased constant Text_Type :=
        "Attribute_Decl";
      Rule_Doc_10 : aliased constant Text_Type :=
        "";
      Rule_Desc_10 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_10'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_10'Access,
         Return_Type => Type_Index_For_Attribute_Decl);
      
      Rule_Name_11 : aliased constant Text_Type :=
        "Associative_Array_Index";
      Rule_Doc_11 : aliased constant Text_Type :=
        "";
      Rule_Desc_11 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_11'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_11'Access,
         Return_Type => Type_Index_For_Gpr_Node);
      
      Rule_Name_12 : aliased constant Text_Type :=
        "Package_Decl";
      Rule_Doc_12 : aliased constant Text_Type :=
        "";
      Rule_Desc_12 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_12'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_12'Access,
         Return_Type => Type_Index_For_Package_Decl);
      
      Rule_Name_13 : aliased constant Text_Type :=
        "Package_Renaming";
      Rule_Doc_13 : aliased constant Text_Type :=
        "";
      Rule_Desc_13 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_13'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_13'Access,
         Return_Type => Type_Index_For_Package_Renaming);
      
      Rule_Name_14 : aliased constant Text_Type :=
        "Package_Extension";
      Rule_Doc_14 : aliased constant Text_Type :=
        "";
      Rule_Desc_14 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_14'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_14'Access,
         Return_Type => Type_Index_For_Package_Extension);
      
      Rule_Name_15 : aliased constant Text_Type :=
        "Package_Spec";
      Rule_Doc_15 : aliased constant Text_Type :=
        "";
      Rule_Desc_15 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_15'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_15'Access,
         Return_Type => Type_Index_For_Package_Spec);
      
      Rule_Name_16 : aliased constant Text_Type :=
        "Empty_Declaration";
      Rule_Doc_16 : aliased constant Text_Type :=
        "";
      Rule_Desc_16 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_16'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_16'Access,
         Return_Type => Type_Index_For_Empty_Decl);
      
      Rule_Name_17 : aliased constant Text_Type :=
        "Case_Construction";
      Rule_Doc_17 : aliased constant Text_Type :=
        "";
      Rule_Desc_17 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_17'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_17'Access,
         Return_Type => Type_Index_For_Case_Construction);
      
      Rule_Name_18 : aliased constant Text_Type :=
        "Case_Item";
      Rule_Doc_18 : aliased constant Text_Type :=
        "";
      Rule_Desc_18 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_18'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_18'Access,
         Return_Type => Type_Index_For_Case_Item);
      
      Rule_Name_19 : aliased constant Text_Type :=
        "Others_Designator";
      Rule_Doc_19 : aliased constant Text_Type :=
        "";
      Rule_Desc_19 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_19'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_19'Access,
         Return_Type => Type_Index_For_Others_Designator);
      
      Rule_Name_20 : aliased constant Text_Type :=
        "Choice";
      Rule_Doc_20 : aliased constant Text_Type :=
        "";
      Rule_Desc_20 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_20'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_20'Access,
         Return_Type => Type_Index_For_Gpr_Node);
      
      Rule_Name_21 : aliased constant Text_Type :=
        "Discrete_Choice_List";
      Rule_Doc_21 : aliased constant Text_Type :=
        "";
      Rule_Desc_21 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_21'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_21'Access,
         Return_Type => Type_Index_For_Choices);
      
      Rule_Name_22 : aliased constant Text_Type :=
        "With_Decl";
      Rule_Doc_22 : aliased constant Text_Type :=
        "";
      Rule_Desc_22 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_22'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_22'Access,
         Return_Type => Type_Index_For_With_Decl);
      
      Rule_Name_23 : aliased constant Text_Type :=
        "Context_Clauses";
      Rule_Doc_23 : aliased constant Text_Type :=
        "";
      Rule_Desc_23 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_23'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_23'Access,
         Return_Type => Type_Index_For_With_Decl_List);
      
      Rule_Name_24 : aliased constant Text_Type :=
        "Typed_String_Decl";
      Rule_Doc_24 : aliased constant Text_Type :=
        "";
      Rule_Desc_24 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_24'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_24'Access,
         Return_Type => Type_Index_For_Typed_String_Decl);
      
      Rule_Name_25 : aliased constant Text_Type :=
        "Identifier";
      Rule_Doc_25 : aliased constant Text_Type :=
        "";
      Rule_Desc_25 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_25'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_25'Access,
         Return_Type => Type_Index_For_Identifier);
      
      Rule_Name_26 : aliased constant Text_Type :=
        "String_Literal";
      Rule_Doc_26 : aliased constant Text_Type :=
        "";
      Rule_Desc_26 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_26'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_26'Access,
         Return_Type => Type_Index_For_String_Literal);
      
      Rule_Name_27 : aliased constant Text_Type :=
        "Num_Literal";
      Rule_Doc_27 : aliased constant Text_Type :=
        "";
      Rule_Desc_27 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_27'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_27'Access,
         Return_Type => Type_Index_For_Num_Literal);
      
      Rule_Name_28 : aliased constant Text_Type :=
        "Static_Name";
      Rule_Doc_28 : aliased constant Text_Type :=
        "";
      Rule_Desc_28 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_28'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_28'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_29 : aliased constant Text_Type :=
        "Attribute_Reference";
      Rule_Doc_29 : aliased constant Text_Type :=
        "";
      Rule_Desc_29 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_29'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_29'Access,
         Return_Type => Type_Index_For_Attribute_Reference);
      
      Rule_Name_30 : aliased constant Text_Type :=
        "Variable_Reference";
      Rule_Doc_30 : aliased constant Text_Type :=
        "";
      Rule_Desc_30 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_30'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_30'Access,
         Return_Type => Type_Index_For_Variable_Reference);
      
      Rule_Name_31 : aliased constant Text_Type :=
        "Type_Reference";
      Rule_Doc_31 : aliased constant Text_Type :=
        "";
      Rule_Desc_31 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_31'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_31'Access,
         Return_Type => Type_Index_For_Type_Reference);
      
      Rule_Name_32 : aliased constant Text_Type :=
        "Builtin_Function_Call";
      Rule_Doc_32 : aliased constant Text_Type :=
        "";
      Rule_Desc_32 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_32'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_32'Access,
         Return_Type => Type_Index_For_Builtin_Function_Call);
      
      Rule_Name_33 : aliased constant Text_Type :=
        "Expression";
      Rule_Doc_33 : aliased constant Text_Type :=
        "";
      Rule_Desc_33 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_33'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_33'Access,
         Return_Type => Type_Index_For_Term_List);
      
      Rule_Name_34 : aliased constant Text_Type :=
        "Expression_List";
      Rule_Doc_34 : aliased constant Text_Type :=
        "";
      Rule_Desc_34 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_34'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_34'Access,
         Return_Type => Type_Index_For_Terms);
      
      Rule_Name_35 : aliased constant Text_Type :=
        "String_Literal_At";
      Rule_Doc_35 : aliased constant Text_Type :=
        "";
      Rule_Desc_35 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_35'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_35'Access,
         Return_Type => Type_Index_For_String_Literal_At);
      
      Rule_Name_36 : aliased constant Text_Type :=
        "Term";
      Rule_Doc_36 : aliased constant Text_Type :=
        "";
      Rule_Desc_36 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_36'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_36'Access,
         Return_Type => Type_Index_For_Gpr_Node);
      
      Rule_Name_37 : aliased constant Text_Type :=
        "Compilation_Unit";
      Rule_Doc_37 : aliased constant Text_Type :=
        "";
      Rule_Desc_37 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_37'Access,
         Is_Public   => True,
         Doc         => Rule_Doc_37'Access,
         Return_Type => Type_Index_For_Compilation_Unit);

   Grammar_Rules : aliased constant Grammar_Rule_Descriptor_Array := (
      1 => Rule_Desc_1'Access,
2 => Rule_Desc_2'Access,
3 => Rule_Desc_3'Access,
4 => Rule_Desc_4'Access,
5 => Rule_Desc_5'Access,
6 => Rule_Desc_6'Access,
7 => Rule_Desc_7'Access,
8 => Rule_Desc_8'Access,
9 => Rule_Desc_9'Access,
10 => Rule_Desc_10'Access,
11 => Rule_Desc_11'Access,
12 => Rule_Desc_12'Access,
13 => Rule_Desc_13'Access,
14 => Rule_Desc_14'Access,
15 => Rule_Desc_15'Access,
16 => Rule_Desc_16'Access,
17 => Rule_Desc_17'Access,
18 => Rule_Desc_18'Access,
19 => Rule_Desc_19'Access,
20 => Rule_Desc_20'Access,
21 => Rule_Desc_21'Access,
22 => Rule_Desc_22'Access,
23 => Rule_Desc_23'Access,
24 => Rule_Desc_24'Access,
25 => Rule_Desc_25'Access,
26 => Rule_Desc_26'Access,
27 => Rule_Desc_27'Access,
28 => Rule_Desc_28'Access,
29 => Rule_Desc_29'Access,
30 => Rule_Desc_30'Access,
31 => Rule_Desc_31'Access,
32 => Rule_Desc_32'Access,
33 => Rule_Desc_33'Access,
34 => Rule_Desc_34'Access,
35 => Rule_Desc_35'Access,
36 => Rule_Desc_36'Access,
37 => Rule_Desc_37'Access
   );

   ------------------------------------
   -- General value type descriptors --
   ------------------------------------

   
      
      Debug_Name_For_Internal_Unit : aliased constant String :=
        "AnalysisUnit";
      Desc_For_Internal_Unit : aliased constant Type_Descriptor :=
        (Category   => Analysis_Unit_Category,
         Debug_Name => Debug_Name_For_Internal_Unit'Access);
      
      Debug_Name_For_Big_Integer_Type : aliased constant String :=
        "BigInt";
      Desc_For_Big_Integer_Type : aliased constant Type_Descriptor :=
        (Category   => Big_Int_Category,
         Debug_Name => Debug_Name_For_Big_Integer_Type'Access);
      
      Debug_Name_For_Boolean : aliased constant String :=
        "Bool";
      Desc_For_Boolean : aliased constant Type_Descriptor :=
        (Category   => Bool_Category,
         Debug_Name => Debug_Name_For_Boolean'Access);
      
      Debug_Name_For_Character_Type : aliased constant String :=
        "Character";
      Desc_For_Character_Type : aliased constant Type_Descriptor :=
        (Category   => Char_Category,
         Debug_Name => Debug_Name_For_Character_Type'Access);
      
      Debug_Name_For_Integer : aliased constant String :=
        "Int";
      Desc_For_Integer : aliased constant Type_Descriptor :=
        (Category   => Int_Category,
         Debug_Name => Debug_Name_For_Integer'Access);
      
      Debug_Name_For_Source_Location_Range : aliased constant String :=
        "SourceLocationRange";
      Desc_For_Source_Location_Range : aliased constant Type_Descriptor :=
        (Category   => Source_Location_Range_Category,
         Debug_Name => Debug_Name_For_Source_Location_Range'Access);
      
      Debug_Name_For_String_Type : aliased constant String :=
        "String";
      Desc_For_String_Type : aliased constant Type_Descriptor :=
        (Category   => String_Category,
         Debug_Name => Debug_Name_For_String_Type'Access);
      
      Debug_Name_For_Token_Reference : aliased constant String :=
        "Token";
      Desc_For_Token_Reference : aliased constant Type_Descriptor :=
        (Category   => Token_Category,
         Debug_Name => Debug_Name_For_Token_Reference'Access);
      
      Debug_Name_For_Symbol_Type : aliased constant String :=
        "Symbol";
      Desc_For_Symbol_Type : aliased constant Type_Descriptor :=
        (Category   => Symbol_Category,
         Debug_Name => Debug_Name_For_Symbol_Type'Access);
      
      Debug_Name_For_Analysis_Unit_Kind : aliased constant String :=
        "AnalysisUnitKind";
      Desc_For_Analysis_Unit_Kind : aliased constant Type_Descriptor :=
        (Category   => Enum_Category,
         Debug_Name => Debug_Name_For_Analysis_Unit_Kind'Access);
      
      Debug_Name_For_Lookup_Kind : aliased constant String :=
        "LookupKind";
      Desc_For_Lookup_Kind : aliased constant Type_Descriptor :=
        (Category   => Enum_Category,
         Debug_Name => Debug_Name_For_Lookup_Kind'Access);
      
      Debug_Name_For_Designated_Env_Kind : aliased constant String :=
        "DesignatedEnvKind";
      Desc_For_Designated_Env_Kind : aliased constant Type_Descriptor :=
        (Category   => Enum_Category,
         Debug_Name => Debug_Name_For_Designated_Env_Kind'Access);
      
      Debug_Name_For_Grammar_Rule : aliased constant String :=
        "GrammarRule";
      Desc_For_Grammar_Rule : aliased constant Type_Descriptor :=
        (Category   => Enum_Category,
         Debug_Name => Debug_Name_For_Grammar_Rule'Access);
      
      Debug_Name_For_Internal_Entity_Array_Access : aliased constant String :=
        "GprNode.array";
      Desc_For_Internal_Entity_Array_Access : aliased constant Type_Descriptor :=
        (Category   => Array_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Array_Access'Access);
      
      Debug_Name_For_Internal_Entity : aliased constant String :=
        "GprNode";
      Desc_For_Internal_Entity : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity'Access);
      
      Debug_Name_For_Internal_Entity_All_Qualifier : aliased constant String :=
        "AllQualifier";
      Desc_For_Internal_Entity_All_Qualifier : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_All_Qualifier'Access);
      
      Debug_Name_For_Internal_Entity_All_Qualifier_Absent : aliased constant String :=
        "AllQualifier.Absent";
      Desc_For_Internal_Entity_All_Qualifier_Absent : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_All_Qualifier_Absent'Access);
      
      Debug_Name_For_Internal_Entity_All_Qualifier_Present : aliased constant String :=
        "AllQualifier.Present";
      Desc_For_Internal_Entity_All_Qualifier_Present : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_All_Qualifier_Present'Access);
      
      Debug_Name_For_Internal_Entity_Attribute_Decl : aliased constant String :=
        "AttributeDecl";
      Desc_For_Internal_Entity_Attribute_Decl : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Attribute_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Attribute_Reference : aliased constant String :=
        "AttributeReference";
      Desc_For_Internal_Entity_Attribute_Reference : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Attribute_Reference'Access);
      
      Debug_Name_For_Internal_Entity_Base_List : aliased constant String :=
        "BaseList";
      Desc_For_Internal_Entity_Base_List : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Base_List'Access);
      
      Debug_Name_For_Internal_Entity_Case_Item_List : aliased constant String :=
        "CaseItem.list";
      Desc_For_Internal_Entity_Case_Item_List : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Case_Item_List'Access);
      
      Debug_Name_For_Internal_Entity_Gpr_Node_List : aliased constant String :=
        "GprNode.list";
      Desc_For_Internal_Entity_Gpr_Node_List : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Gpr_Node_List'Access);
      
      Debug_Name_For_Internal_Entity_Choices : aliased constant String :=
        "Choices";
      Desc_For_Internal_Entity_Choices : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Choices'Access);
      
      Debug_Name_For_Internal_Entity_Term_List : aliased constant String :=
        "TermList";
      Desc_For_Internal_Entity_Term_List : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Term_List'Access);
      
      Debug_Name_For_Internal_Entity_Identifier_List : aliased constant String :=
        "Identifier.list";
      Desc_For_Internal_Entity_Identifier_List : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Identifier_List'Access);
      
      Debug_Name_For_Internal_Entity_String_Literal_List : aliased constant String :=
        "StringLiteral.list";
      Desc_For_Internal_Entity_String_Literal_List : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_String_Literal_List'Access);
      
      Debug_Name_For_Internal_Entity_Term_List_List : aliased constant String :=
        "TermList.list";
      Desc_For_Internal_Entity_Term_List_List : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Term_List_List'Access);
      
      Debug_Name_For_Internal_Entity_With_Decl_List : aliased constant String :=
        "WithDecl.list";
      Desc_For_Internal_Entity_With_Decl_List : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_With_Decl_List'Access);
      
      Debug_Name_For_Internal_Entity_Builtin_Function_Call : aliased constant String :=
        "BuiltinFunctionCall";
      Desc_For_Internal_Entity_Builtin_Function_Call : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Builtin_Function_Call'Access);
      
      Debug_Name_For_Internal_Entity_Case_Construction : aliased constant String :=
        "CaseConstruction";
      Desc_For_Internal_Entity_Case_Construction : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Case_Construction'Access);
      
      Debug_Name_For_Internal_Entity_Case_Item : aliased constant String :=
        "CaseItem";
      Desc_For_Internal_Entity_Case_Item : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Case_Item'Access);
      
      Debug_Name_For_Internal_Entity_Compilation_Unit : aliased constant String :=
        "CompilationUnit";
      Desc_For_Internal_Entity_Compilation_Unit : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Compilation_Unit'Access);
      
      Debug_Name_For_Internal_Entity_Empty_Decl : aliased constant String :=
        "EmptyDecl";
      Desc_For_Internal_Entity_Empty_Decl : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Empty_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Expr : aliased constant String :=
        "Expr";
      Desc_For_Internal_Entity_Expr : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Prefix : aliased constant String :=
        "Prefix";
      Desc_For_Internal_Entity_Prefix : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Prefix'Access);
      
      Debug_Name_For_Internal_Entity_Single_Tok_Node : aliased constant String :=
        "SingleTokNode";
      Desc_For_Internal_Entity_Single_Tok_Node : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Single_Tok_Node'Access);
      
      Debug_Name_For_Internal_Entity_Identifier : aliased constant String :=
        "Identifier";
      Desc_For_Internal_Entity_Identifier : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Identifier'Access);
      
      Debug_Name_For_Internal_Entity_Num_Literal : aliased constant String :=
        "NumLiteral";
      Desc_For_Internal_Entity_Num_Literal : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Num_Literal'Access);
      
      Debug_Name_For_Internal_Entity_String_Literal : aliased constant String :=
        "StringLiteral";
      Desc_For_Internal_Entity_String_Literal : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_String_Literal'Access);
      
      Debug_Name_For_Internal_Entity_Limited_Node : aliased constant String :=
        "Limited";
      Desc_For_Internal_Entity_Limited_Node : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Limited_Node'Access);
      
      Debug_Name_For_Internal_Entity_Limited_Absent : aliased constant String :=
        "Limited.Absent";
      Desc_For_Internal_Entity_Limited_Absent : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Limited_Absent'Access);
      
      Debug_Name_For_Internal_Entity_Limited_Present : aliased constant String :=
        "Limited.Present";
      Desc_For_Internal_Entity_Limited_Present : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Limited_Present'Access);
      
      Debug_Name_For_Internal_Entity_Others_Designator : aliased constant String :=
        "OthersDesignator";
      Desc_For_Internal_Entity_Others_Designator : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Others_Designator'Access);
      
      Debug_Name_For_Internal_Entity_Package_Decl : aliased constant String :=
        "PackageDecl";
      Desc_For_Internal_Entity_Package_Decl : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Package_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Package_Extension : aliased constant String :=
        "PackageExtension";
      Desc_For_Internal_Entity_Package_Extension : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Package_Extension'Access);
      
      Debug_Name_For_Internal_Entity_Package_Renaming : aliased constant String :=
        "PackageRenaming";
      Desc_For_Internal_Entity_Package_Renaming : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Package_Renaming'Access);
      
      Debug_Name_For_Internal_Entity_Package_Spec : aliased constant String :=
        "PackageSpec";
      Desc_For_Internal_Entity_Package_Spec : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Package_Spec'Access);
      
      Debug_Name_For_Internal_Entity_Project : aliased constant String :=
        "Project";
      Desc_For_Internal_Entity_Project : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Project'Access);
      
      Debug_Name_For_Internal_Entity_Project_Declaration : aliased constant String :=
        "ProjectDeclaration";
      Desc_For_Internal_Entity_Project_Declaration : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Project_Declaration'Access);
      
      Debug_Name_For_Internal_Entity_Project_Extension : aliased constant String :=
        "ProjectExtension";
      Desc_For_Internal_Entity_Project_Extension : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Project_Extension'Access);
      
      Debug_Name_For_Internal_Entity_Project_Qualifier : aliased constant String :=
        "ProjectQualifier";
      Desc_For_Internal_Entity_Project_Qualifier : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Project_Qualifier'Access);
      
      Debug_Name_For_Internal_Entity_Project_Qualifier_Abstract : aliased constant String :=
        "ProjectQualifier.Abstract";
      Desc_For_Internal_Entity_Project_Qualifier_Abstract : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Project_Qualifier_Abstract'Access);
      
      Debug_Name_For_Internal_Entity_Project_Qualifier_Aggregate : aliased constant String :=
        "ProjectQualifier.Aggregate";
      Desc_For_Internal_Entity_Project_Qualifier_Aggregate : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Project_Qualifier_Aggregate'Access);
      
      Debug_Name_For_Internal_Entity_Project_Qualifier_Aggregate_Library : aliased constant String :=
        "ProjectQualifier.AggregateLibrary";
      Desc_For_Internal_Entity_Project_Qualifier_Aggregate_Library : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Project_Qualifier_Aggregate_Library'Access);
      
      Debug_Name_For_Internal_Entity_Project_Qualifier_Configuration : aliased constant String :=
        "ProjectQualifier.Configuration";
      Desc_For_Internal_Entity_Project_Qualifier_Configuration : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Project_Qualifier_Configuration'Access);
      
      Debug_Name_For_Internal_Entity_Project_Qualifier_Library : aliased constant String :=
        "ProjectQualifier.Library";
      Desc_For_Internal_Entity_Project_Qualifier_Library : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Project_Qualifier_Library'Access);
      
      Debug_Name_For_Internal_Entity_Project_Qualifier_Standard : aliased constant String :=
        "ProjectQualifier.Standard";
      Desc_For_Internal_Entity_Project_Qualifier_Standard : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Project_Qualifier_Standard'Access);
      
      Debug_Name_For_Internal_Entity_String_Literal_At : aliased constant String :=
        "StringLiteralAt";
      Desc_For_Internal_Entity_String_Literal_At : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_String_Literal_At'Access);
      
      Debug_Name_For_Internal_Entity_Terms : aliased constant String :=
        "Terms";
      Desc_For_Internal_Entity_Terms : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Terms'Access);
      
      Debug_Name_For_Internal_Entity_Type_Reference : aliased constant String :=
        "TypeReference";
      Desc_For_Internal_Entity_Type_Reference : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Type_Reference'Access);
      
      Debug_Name_For_Internal_Entity_Typed_String_Decl : aliased constant String :=
        "TypedStringDecl";
      Desc_For_Internal_Entity_Typed_String_Decl : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Typed_String_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Variable_Decl : aliased constant String :=
        "VariableDecl";
      Desc_For_Internal_Entity_Variable_Decl : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Variable_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Variable_Reference : aliased constant String :=
        "VariableReference";
      Desc_For_Internal_Entity_Variable_Reference : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_Variable_Reference'Access);
      
      Debug_Name_For_Internal_Entity_With_Decl : aliased constant String :=
        "WithDecl";
      Desc_For_Internal_Entity_With_Decl : aliased constant Type_Descriptor :=
        (Category   => Struct_Category,
         Debug_Name => Debug_Name_For_Internal_Entity_With_Decl'Access);

   Types : aliased constant Type_Descriptor_Array := (
      Desc_For_Internal_Unit'Access,
Desc_For_Big_Integer_Type'Access,
Desc_For_Boolean'Access,
Desc_For_Character_Type'Access,
Desc_For_Integer'Access,
Desc_For_Source_Location_Range'Access,
Desc_For_String_Type'Access,
Desc_For_Token_Reference'Access,
Desc_For_Symbol_Type'Access,
Desc_For_Analysis_Unit_Kind'Access,
Desc_For_Lookup_Kind'Access,
Desc_For_Designated_Env_Kind'Access,
Desc_For_Grammar_Rule'Access,
Desc_For_Internal_Entity_Array_Access'Access,
Desc_For_Internal_Entity'Access,
Desc_For_Internal_Entity_All_Qualifier'Access,
Desc_For_Internal_Entity_All_Qualifier_Absent'Access,
Desc_For_Internal_Entity_All_Qualifier_Present'Access,
Desc_For_Internal_Entity_Attribute_Decl'Access,
Desc_For_Internal_Entity_Attribute_Reference'Access,
Desc_For_Internal_Entity_Base_List'Access,
Desc_For_Internal_Entity_Case_Item_List'Access,
Desc_For_Internal_Entity_Gpr_Node_List'Access,
Desc_For_Internal_Entity_Choices'Access,
Desc_For_Internal_Entity_Term_List'Access,
Desc_For_Internal_Entity_Identifier_List'Access,
Desc_For_Internal_Entity_String_Literal_List'Access,
Desc_For_Internal_Entity_Term_List_List'Access,
Desc_For_Internal_Entity_With_Decl_List'Access,
Desc_For_Internal_Entity_Builtin_Function_Call'Access,
Desc_For_Internal_Entity_Case_Construction'Access,
Desc_For_Internal_Entity_Case_Item'Access,
Desc_For_Internal_Entity_Compilation_Unit'Access,
Desc_For_Internal_Entity_Empty_Decl'Access,
Desc_For_Internal_Entity_Expr'Access,
Desc_For_Internal_Entity_Prefix'Access,
Desc_For_Internal_Entity_Single_Tok_Node'Access,
Desc_For_Internal_Entity_Identifier'Access,
Desc_For_Internal_Entity_Num_Literal'Access,
Desc_For_Internal_Entity_String_Literal'Access,
Desc_For_Internal_Entity_Limited_Node'Access,
Desc_For_Internal_Entity_Limited_Absent'Access,
Desc_For_Internal_Entity_Limited_Present'Access,
Desc_For_Internal_Entity_Others_Designator'Access,
Desc_For_Internal_Entity_Package_Decl'Access,
Desc_For_Internal_Entity_Package_Extension'Access,
Desc_For_Internal_Entity_Package_Renaming'Access,
Desc_For_Internal_Entity_Package_Spec'Access,
Desc_For_Internal_Entity_Project'Access,
Desc_For_Internal_Entity_Project_Declaration'Access,
Desc_For_Internal_Entity_Project_Extension'Access,
Desc_For_Internal_Entity_Project_Qualifier'Access,
Desc_For_Internal_Entity_Project_Qualifier_Abstract'Access,
Desc_For_Internal_Entity_Project_Qualifier_Aggregate'Access,
Desc_For_Internal_Entity_Project_Qualifier_Aggregate_Library'Access,
Desc_For_Internal_Entity_Project_Qualifier_Configuration'Access,
Desc_For_Internal_Entity_Project_Qualifier_Library'Access,
Desc_For_Internal_Entity_Project_Qualifier_Standard'Access,
Desc_For_Internal_Entity_String_Literal_At'Access,
Desc_For_Internal_Entity_Terms'Access,
Desc_For_Internal_Entity_Type_Reference'Access,
Desc_For_Internal_Entity_Typed_String_Decl'Access,
Desc_For_Internal_Entity_Variable_Decl'Access,
Desc_For_Internal_Entity_Variable_Reference'Access,
Desc_For_Internal_Entity_With_Decl'Access
   );

   ---------------------------
   -- Enum type descriptors --
   ---------------------------

   
      

         Enum_Name_For_Analysis_Unit_Kind_1 : aliased constant Text_Type :=
           "Unit_Specification";
         Enum_Name_For_Analysis_Unit_Kind_2 : aliased constant Text_Type :=
           "Unit_Body";

      Enum_Name_For_Analysis_Unit_Kind : aliased constant Text_Type :=
        "Analysis_Unit_Kind";
      Enum_Desc_For_Analysis_Unit_Kind : aliased constant Enum_Type_Descriptor := (
         Last_Value    => 2,
         Name          => Enum_Name_For_Analysis_Unit_Kind'Access,
         Default_Value => 0,
         Value_Names   => (
            1 => Enum_Name_For_Analysis_Unit_Kind_1'Access,
2 => Enum_Name_For_Analysis_Unit_Kind_2'Access
         )
      );
      

         Enum_Name_For_Lookup_Kind_1 : aliased constant Text_Type :=
           "Recursive";
         Enum_Name_For_Lookup_Kind_2 : aliased constant Text_Type :=
           "Flat";
         Enum_Name_For_Lookup_Kind_3 : aliased constant Text_Type :=
           "Minimal";

      Enum_Name_For_Lookup_Kind : aliased constant Text_Type :=
        "Lookup_Kind";
      Enum_Desc_For_Lookup_Kind : aliased constant Enum_Type_Descriptor := (
         Last_Value    => 3,
         Name          => Enum_Name_For_Lookup_Kind'Access,
         Default_Value => 0,
         Value_Names   => (
            1 => Enum_Name_For_Lookup_Kind_1'Access,
2 => Enum_Name_For_Lookup_Kind_2'Access,
3 => Enum_Name_For_Lookup_Kind_3'Access
         )
      );
      

         Enum_Name_For_Designated_Env_Kind_1 : aliased constant Text_Type :=
           "None";
         Enum_Name_For_Designated_Env_Kind_2 : aliased constant Text_Type :=
           "Current_Env";
         Enum_Name_For_Designated_Env_Kind_3 : aliased constant Text_Type :=
           "Named_Env";
         Enum_Name_For_Designated_Env_Kind_4 : aliased constant Text_Type :=
           "Direct_Env";

      Enum_Name_For_Designated_Env_Kind : aliased constant Text_Type :=
        "Designated_Env_Kind";
      Enum_Desc_For_Designated_Env_Kind : aliased constant Enum_Type_Descriptor := (
         Last_Value    => 4,
         Name          => Enum_Name_For_Designated_Env_Kind'Access,
         Default_Value => 1,
         Value_Names   => (
            1 => Enum_Name_For_Designated_Env_Kind_1'Access,
2 => Enum_Name_For_Designated_Env_Kind_2'Access,
3 => Enum_Name_For_Designated_Env_Kind_3'Access,
4 => Enum_Name_For_Designated_Env_Kind_4'Access
         )
      );
      

         Enum_Name_For_Grammar_Rule_1 : aliased constant Text_Type :=
           "Project_Qualifier_Rule";
         Enum_Name_For_Grammar_Rule_2 : aliased constant Text_Type :=
           "Project_Extension_Rule";
         Enum_Name_For_Grammar_Rule_3 : aliased constant Text_Type :=
           "Project_Declaration_Rule";
         Enum_Name_For_Grammar_Rule_4 : aliased constant Text_Type :=
           "Project_Rule";
         Enum_Name_For_Grammar_Rule_5 : aliased constant Text_Type :=
           "Declarative_Items_Rule";
         Enum_Name_For_Grammar_Rule_6 : aliased constant Text_Type :=
           "Declarative_Item_Rule";
         Enum_Name_For_Grammar_Rule_7 : aliased constant Text_Type :=
           "Simple_Declarative_Items_Rule";
         Enum_Name_For_Grammar_Rule_8 : aliased constant Text_Type :=
           "Simple_Declarative_Item_Rule";
         Enum_Name_For_Grammar_Rule_9 : aliased constant Text_Type :=
           "Variable_Decl_Rule";
         Enum_Name_For_Grammar_Rule_10 : aliased constant Text_Type :=
           "Attribute_Decl_Rule";
         Enum_Name_For_Grammar_Rule_11 : aliased constant Text_Type :=
           "Associative_Array_Index_Rule";
         Enum_Name_For_Grammar_Rule_12 : aliased constant Text_Type :=
           "Package_Decl_Rule";
         Enum_Name_For_Grammar_Rule_13 : aliased constant Text_Type :=
           "Package_Renaming_Rule";
         Enum_Name_For_Grammar_Rule_14 : aliased constant Text_Type :=
           "Package_Extension_Rule";
         Enum_Name_For_Grammar_Rule_15 : aliased constant Text_Type :=
           "Package_Spec_Rule";
         Enum_Name_For_Grammar_Rule_16 : aliased constant Text_Type :=
           "Empty_Declaration_Rule";
         Enum_Name_For_Grammar_Rule_17 : aliased constant Text_Type :=
           "Case_Construction_Rule";
         Enum_Name_For_Grammar_Rule_18 : aliased constant Text_Type :=
           "Case_Item_Rule";
         Enum_Name_For_Grammar_Rule_19 : aliased constant Text_Type :=
           "Others_Designator_Rule";
         Enum_Name_For_Grammar_Rule_20 : aliased constant Text_Type :=
           "Choice_Rule";
         Enum_Name_For_Grammar_Rule_21 : aliased constant Text_Type :=
           "Discrete_Choice_List_Rule";
         Enum_Name_For_Grammar_Rule_22 : aliased constant Text_Type :=
           "With_Decl_Rule";
         Enum_Name_For_Grammar_Rule_23 : aliased constant Text_Type :=
           "Context_Clauses_Rule";
         Enum_Name_For_Grammar_Rule_24 : aliased constant Text_Type :=
           "Typed_String_Decl_Rule";
         Enum_Name_For_Grammar_Rule_25 : aliased constant Text_Type :=
           "Identifier_Rule";
         Enum_Name_For_Grammar_Rule_26 : aliased constant Text_Type :=
           "String_Literal_Rule";
         Enum_Name_For_Grammar_Rule_27 : aliased constant Text_Type :=
           "Num_Literal_Rule";
         Enum_Name_For_Grammar_Rule_28 : aliased constant Text_Type :=
           "Static_Name_Rule";
         Enum_Name_For_Grammar_Rule_29 : aliased constant Text_Type :=
           "Attribute_Reference_Rule";
         Enum_Name_For_Grammar_Rule_30 : aliased constant Text_Type :=
           "Variable_Reference_Rule";
         Enum_Name_For_Grammar_Rule_31 : aliased constant Text_Type :=
           "Type_Reference_Rule";
         Enum_Name_For_Grammar_Rule_32 : aliased constant Text_Type :=
           "Builtin_Function_Call_Rule";
         Enum_Name_For_Grammar_Rule_33 : aliased constant Text_Type :=
           "Expression_Rule";
         Enum_Name_For_Grammar_Rule_34 : aliased constant Text_Type :=
           "Expression_List_Rule";
         Enum_Name_For_Grammar_Rule_35 : aliased constant Text_Type :=
           "String_Literal_At_Rule";
         Enum_Name_For_Grammar_Rule_36 : aliased constant Text_Type :=
           "Term_Rule";
         Enum_Name_For_Grammar_Rule_37 : aliased constant Text_Type :=
           "Compilation_Unit_Rule";

      Enum_Name_For_Grammar_Rule : aliased constant Text_Type :=
        "Grammar_Rule";
      Enum_Desc_For_Grammar_Rule : aliased constant Enum_Type_Descriptor := (
         Last_Value    => 37,
         Name          => Enum_Name_For_Grammar_Rule'Access,
         Default_Value => 0,
         Value_Names   => (
            1 => Enum_Name_For_Grammar_Rule_1'Access,
2 => Enum_Name_For_Grammar_Rule_2'Access,
3 => Enum_Name_For_Grammar_Rule_3'Access,
4 => Enum_Name_For_Grammar_Rule_4'Access,
5 => Enum_Name_For_Grammar_Rule_5'Access,
6 => Enum_Name_For_Grammar_Rule_6'Access,
7 => Enum_Name_For_Grammar_Rule_7'Access,
8 => Enum_Name_For_Grammar_Rule_8'Access,
9 => Enum_Name_For_Grammar_Rule_9'Access,
10 => Enum_Name_For_Grammar_Rule_10'Access,
11 => Enum_Name_For_Grammar_Rule_11'Access,
12 => Enum_Name_For_Grammar_Rule_12'Access,
13 => Enum_Name_For_Grammar_Rule_13'Access,
14 => Enum_Name_For_Grammar_Rule_14'Access,
15 => Enum_Name_For_Grammar_Rule_15'Access,
16 => Enum_Name_For_Grammar_Rule_16'Access,
17 => Enum_Name_For_Grammar_Rule_17'Access,
18 => Enum_Name_For_Grammar_Rule_18'Access,
19 => Enum_Name_For_Grammar_Rule_19'Access,
20 => Enum_Name_For_Grammar_Rule_20'Access,
21 => Enum_Name_For_Grammar_Rule_21'Access,
22 => Enum_Name_For_Grammar_Rule_22'Access,
23 => Enum_Name_For_Grammar_Rule_23'Access,
24 => Enum_Name_For_Grammar_Rule_24'Access,
25 => Enum_Name_For_Grammar_Rule_25'Access,
26 => Enum_Name_For_Grammar_Rule_26'Access,
27 => Enum_Name_For_Grammar_Rule_27'Access,
28 => Enum_Name_For_Grammar_Rule_28'Access,
29 => Enum_Name_For_Grammar_Rule_29'Access,
30 => Enum_Name_For_Grammar_Rule_30'Access,
31 => Enum_Name_For_Grammar_Rule_31'Access,
32 => Enum_Name_For_Grammar_Rule_32'Access,
33 => Enum_Name_For_Grammar_Rule_33'Access,
34 => Enum_Name_For_Grammar_Rule_34'Access,
35 => Enum_Name_For_Grammar_Rule_35'Access,
36 => Enum_Name_For_Grammar_Rule_36'Access,
37 => Enum_Name_For_Grammar_Rule_37'Access
         )
      );
   Enum_Types : aliased constant Enum_Type_Descriptor_Array := (
      Type_Index_For_Analysis_Unit_Kind => Enum_Desc_For_Analysis_Unit_Kind'Access,
Type_Index_For_Lookup_Kind => Enum_Desc_For_Lookup_Kind'Access,
Type_Index_For_Designated_Env_Kind => Enum_Desc_For_Designated_Env_Kind'Access,
Type_Index_For_Grammar_Rule => Enum_Desc_For_Grammar_Rule'Access
   );

   ------------------------------------
   -- Introspection values for enums --
   ------------------------------------

      
      type Internal_Rec_Analysis_Unit_Kind is new Base_Internal_Enum_Value with record
         Value : Analysis_Unit_Kind;
      end record;
      type Internal_Acc_Analysis_Unit_Kind is access all Internal_Rec_Analysis_Unit_Kind;

      overriding function "=" (Left, Right : Internal_Rec_Analysis_Unit_Kind) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Analysis_Unit_Kind) return Type_Index;
      overriding function Image (Value : Internal_Rec_Analysis_Unit_Kind) return String;
      overriding function Value_Index (Value : Internal_Rec_Analysis_Unit_Kind) return Enum_Value_Index;
      
      type Internal_Rec_Lookup_Kind is new Base_Internal_Enum_Value with record
         Value : Lookup_Kind;
      end record;
      type Internal_Acc_Lookup_Kind is access all Internal_Rec_Lookup_Kind;

      overriding function "=" (Left, Right : Internal_Rec_Lookup_Kind) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Lookup_Kind) return Type_Index;
      overriding function Image (Value : Internal_Rec_Lookup_Kind) return String;
      overriding function Value_Index (Value : Internal_Rec_Lookup_Kind) return Enum_Value_Index;
      
      type Internal_Rec_Designated_Env_Kind is new Base_Internal_Enum_Value with record
         Value : Designated_Env_Kind;
      end record;
      type Internal_Acc_Designated_Env_Kind is access all Internal_Rec_Designated_Env_Kind;

      overriding function "=" (Left, Right : Internal_Rec_Designated_Env_Kind) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Designated_Env_Kind) return Type_Index;
      overriding function Image (Value : Internal_Rec_Designated_Env_Kind) return String;
      overriding function Value_Index (Value : Internal_Rec_Designated_Env_Kind) return Enum_Value_Index;
      
      type Internal_Rec_Grammar_Rule is new Base_Internal_Enum_Value with record
         Value : Grammar_Rule;
      end record;
      type Internal_Acc_Grammar_Rule is access all Internal_Rec_Grammar_Rule;

      overriding function "=" (Left, Right : Internal_Rec_Grammar_Rule) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Grammar_Rule) return Type_Index;
      overriding function Image (Value : Internal_Rec_Grammar_Rule) return String;
      overriding function Value_Index (Value : Internal_Rec_Grammar_Rule) return Enum_Value_Index;

   function Create_Enum
     (Enum_Type   : Type_Index;
      Value_Index : Enum_Value_Index) return Internal_Value_Access;
   --  Implementation of the Create_Enum operation in the lanugage descriptor

   ----------------------------
   -- Array type descriptors --
   ----------------------------

   
   Array_Types : aliased constant Array_Type_Descriptor_Array := (
      Type_Index_For_Gpr_Node_Array => (Element_Type => Type_Index_For_Gpr_Node)
   );

   -------------------------------------
   -- Introspection values for arrays --
   -------------------------------------

      

      type Internal_Stored_Gpr_Node_Array is access all Gpr_Node_Array;
      procedure Free is new Ada.Unchecked_Deallocation (Gpr_Node_Array, Internal_Stored_Gpr_Node_Array);

      type Internal_Rec_Gpr_Node_Array is new Base_Internal_Array_Value with record
         Value : Internal_Stored_Gpr_Node_Array;
      end record;
      type Internal_Acc_Gpr_Node_Array is access all Internal_Rec_Gpr_Node_Array;

      overriding function "=" (Left, Right : Internal_Rec_Gpr_Node_Array) return Boolean;
      overriding procedure Destroy (Value : in out Internal_Rec_Gpr_Node_Array);
      overriding function Type_Of (Value : Internal_Rec_Gpr_Node_Array) return Type_Index;
      overriding function Array_Length (Value : Internal_Rec_Gpr_Node_Array) return Natural;
      overriding function Array_Item
        (Value : Internal_Rec_Gpr_Node_Array; Index : Positive) return Internal_Value_Access;

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Gpr_Node_Array;

   function Create_Array
     (Array_Type : Type_Index;
      Values     : Internal_Value_Array) return Internal_Value_Access;
   --  Implementation of the Create_Array operation in the language descriptor

   -------------------------------
   -- Iterator type descriptors --
   -------------------------------

   
   Iterator_Types : aliased constant Iterator_Type_Descriptor_Array := (
         1 .. 0 => <>
   );


   --------------------------------------
   -- Introspection values for structs --
   --------------------------------------


   function Create_Struct
     (Struct_Type : Type_Index;
      Values      : Internal_Value_Array) return Internal_Value_Access;
   --  Implementation for the Create_Struct operation in the language
   --  descriptor.

   -------------------------------
   -- Struct member descriptors --
   -------------------------------

   
      


      

      
         Indexes_For_Attribute_Decl_F_Attr_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Attribute_Decl => 1);

      Member_Name_For_Attribute_Decl_F_Attr_Name : aliased constant Text_Type :=
        "F_Attr_Name";
      Member_Desc_For_Attribute_Decl_F_Attr_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Attribute_Decl_F_Attr_Name'Access,
         Owner         => Type_Index_For_Attribute_Decl,
         Member_Type   => Type_Index_For_Identifier,
         Null_For      => null,
         Indexes       => Indexes_For_Attribute_Decl_F_Attr_Name'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Attribute_Decl_F_Attr_Index : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Attribute_Decl => 2);

      Member_Name_For_Attribute_Decl_F_Attr_Index : aliased constant Text_Type :=
        "F_Attr_Index";
      Member_Desc_For_Attribute_Decl_F_Attr_Index : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Attribute_Decl_F_Attr_Index'Access,
         Owner         => Type_Index_For_Attribute_Decl,
         Member_Type   => Type_Index_For_Gpr_Node,
         Null_For      => null,
         Indexes       => Indexes_For_Attribute_Decl_F_Attr_Index'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Attribute_Decl_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Attribute_Decl => 3);

      Member_Name_For_Attribute_Decl_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Desc_For_Attribute_Decl_F_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Attribute_Decl_F_Expr'Access,
         Owner         => Type_Index_For_Attribute_Decl,
         Member_Type   => Type_Index_For_Term_List,
         Null_For      => null,
         Indexes       => Indexes_For_Attribute_Decl_F_Expr'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Attribute_Reference_F_Attribute_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Attribute_Reference => 1);

      Member_Name_For_Attribute_Reference_F_Attribute_Name : aliased constant Text_Type :=
        "F_Attribute_Name";
      Member_Desc_For_Attribute_Reference_F_Attribute_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Attribute_Reference_F_Attribute_Name'Access,
         Owner         => Type_Index_For_Attribute_Reference,
         Member_Type   => Type_Index_For_Identifier,
         Null_For      => null,
         Indexes       => Indexes_For_Attribute_Reference_F_Attribute_Name'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Attribute_Reference_F_Attribute_Index : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Attribute_Reference => 2);

      Member_Name_For_Attribute_Reference_F_Attribute_Index : aliased constant Text_Type :=
        "F_Attribute_Index";
      Member_Desc_For_Attribute_Reference_F_Attribute_Index : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Attribute_Reference_F_Attribute_Index'Access,
         Owner         => Type_Index_For_Attribute_Reference,
         Member_Type   => Type_Index_For_Gpr_Node,
         Null_For      => null,
         Indexes       => Indexes_For_Attribute_Reference_F_Attribute_Index'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Builtin_Function_Call_F_Function_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Builtin_Function_Call => 1);

      Member_Name_For_Builtin_Function_Call_F_Function_Name : aliased constant Text_Type :=
        "F_Function_Name";
      Member_Desc_For_Builtin_Function_Call_F_Function_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Builtin_Function_Call_F_Function_Name'Access,
         Owner         => Type_Index_For_Builtin_Function_Call,
         Member_Type   => Type_Index_For_Identifier,
         Null_For      => null,
         Indexes       => Indexes_For_Builtin_Function_Call_F_Function_Name'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Builtin_Function_Call_F_Parameters : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Builtin_Function_Call => 2);

      Member_Name_For_Builtin_Function_Call_F_Parameters : aliased constant Text_Type :=
        "F_Parameters";
      Member_Desc_For_Builtin_Function_Call_F_Parameters : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Builtin_Function_Call_F_Parameters'Access,
         Owner         => Type_Index_For_Builtin_Function_Call,
         Member_Type   => Type_Index_For_Terms,
         Null_For      => null,
         Indexes       => Indexes_For_Builtin_Function_Call_F_Parameters'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Case_Construction_F_Var_Ref : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Case_Construction => 1);

      Member_Name_For_Case_Construction_F_Var_Ref : aliased constant Text_Type :=
        "F_Var_Ref";
      Member_Desc_For_Case_Construction_F_Var_Ref : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Case_Construction_F_Var_Ref'Access,
         Owner         => Type_Index_For_Case_Construction,
         Member_Type   => Type_Index_For_Variable_Reference,
         Null_For      => null,
         Indexes       => Indexes_For_Case_Construction_F_Var_Ref'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Case_Construction_F_Items : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Case_Construction => 2);

      Member_Name_For_Case_Construction_F_Items : aliased constant Text_Type :=
        "F_Items";
      Member_Desc_For_Case_Construction_F_Items : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Case_Construction_F_Items'Access,
         Owner         => Type_Index_For_Case_Construction,
         Member_Type   => Type_Index_For_Case_Item_List,
         Null_For      => null,
         Indexes       => Indexes_For_Case_Construction_F_Items'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Case_Item_F_Choice : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Case_Item => 1);

      Member_Name_For_Case_Item_F_Choice : aliased constant Text_Type :=
        "F_Choice";
      Member_Desc_For_Case_Item_F_Choice : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Case_Item_F_Choice'Access,
         Owner         => Type_Index_For_Case_Item,
         Member_Type   => Type_Index_For_Choices,
         Null_For      => null,
         Indexes       => Indexes_For_Case_Item_F_Choice'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Case_Item_F_Decls : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Case_Item => 2);

      Member_Name_For_Case_Item_F_Decls : aliased constant Text_Type :=
        "F_Decls";
      Member_Desc_For_Case_Item_F_Decls : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Case_Item_F_Decls'Access,
         Owner         => Type_Index_For_Case_Item,
         Member_Type   => Type_Index_For_Gpr_Node_List,
         Null_For      => null,
         Indexes       => Indexes_For_Case_Item_F_Decls'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Compilation_Unit_F_Project : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Compilation_Unit => 1);

      Member_Name_For_Compilation_Unit_F_Project : aliased constant Text_Type :=
        "F_Project";
      Member_Desc_For_Compilation_Unit_F_Project : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Compilation_Unit_F_Project'Access,
         Owner         => Type_Index_For_Compilation_Unit,
         Member_Type   => Type_Index_For_Project,
         Null_For      => null,
         Indexes       => Indexes_For_Compilation_Unit_F_Project'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Prefix_F_Prefix : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Prefix => 1);

      Member_Name_For_Prefix_F_Prefix : aliased constant Text_Type :=
        "F_Prefix";
      Member_Desc_For_Prefix_F_Prefix : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Prefix_F_Prefix'Access,
         Owner         => Type_Index_For_Prefix,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Prefix_F_Prefix'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Prefix_F_Suffix : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Prefix => 2);

      Member_Name_For_Prefix_F_Suffix : aliased constant Text_Type :=
        "F_Suffix";
      Member_Desc_For_Prefix_F_Suffix : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Prefix_F_Suffix'Access,
         Owner         => Type_Index_For_Prefix,
         Member_Type   => Type_Index_For_Identifier,
         Null_For      => null,
         Indexes       => Indexes_For_Prefix_F_Suffix'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Package_Decl_F_Pkg_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Package_Decl => 1);

      Member_Name_For_Package_Decl_F_Pkg_Name : aliased constant Text_Type :=
        "F_Pkg_Name";
      Member_Desc_For_Package_Decl_F_Pkg_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Package_Decl_F_Pkg_Name'Access,
         Owner         => Type_Index_For_Package_Decl,
         Member_Type   => Type_Index_For_Identifier,
         Null_For      => null,
         Indexes       => Indexes_For_Package_Decl_F_Pkg_Name'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Package_Decl_F_Pkg_Spec : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Package_Decl => 2);

      Member_Name_For_Package_Decl_F_Pkg_Spec : aliased constant Text_Type :=
        "F_Pkg_Spec";
      Member_Desc_For_Package_Decl_F_Pkg_Spec : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Package_Decl_F_Pkg_Spec'Access,
         Owner         => Type_Index_For_Package_Decl,
         Member_Type   => Type_Index_For_Gpr_Node,
         Null_For      => null,
         Indexes       => Indexes_For_Package_Decl_F_Pkg_Spec'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Package_Extension_F_Extended_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Package_Extension => 1);

      Member_Name_For_Package_Extension_F_Extended_Name : aliased constant Text_Type :=
        "F_Extended_Name";
      Member_Desc_For_Package_Extension_F_Extended_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Package_Extension_F_Extended_Name'Access,
         Owner         => Type_Index_For_Package_Extension,
         Member_Type   => Type_Index_For_Identifier_List,
         Null_For      => null,
         Indexes       => Indexes_For_Package_Extension_F_Extended_Name'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Package_Renaming_F_Renamed_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Package_Renaming => 1);

      Member_Name_For_Package_Renaming_F_Renamed_Name : aliased constant Text_Type :=
        "F_Renamed_Name";
      Member_Desc_For_Package_Renaming_F_Renamed_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Package_Renaming_F_Renamed_Name'Access,
         Owner         => Type_Index_For_Package_Renaming,
         Member_Type   => Type_Index_For_Identifier_List,
         Null_For      => null,
         Indexes       => Indexes_For_Package_Renaming_F_Renamed_Name'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Package_Spec_F_Extension : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Package_Spec => 1);

      Member_Name_For_Package_Spec_F_Extension : aliased constant Text_Type :=
        "F_Extension";
      Member_Desc_For_Package_Spec_F_Extension : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Package_Spec_F_Extension'Access,
         Owner         => Type_Index_For_Package_Spec,
         Member_Type   => Type_Index_For_Package_Extension,
         Null_For      => null,
         Indexes       => Indexes_For_Package_Spec_F_Extension'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Package_Spec_F_Decls : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Package_Spec => 2);

      Member_Name_For_Package_Spec_F_Decls : aliased constant Text_Type :=
        "F_Decls";
      Member_Desc_For_Package_Spec_F_Decls : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Package_Spec_F_Decls'Access,
         Owner         => Type_Index_For_Package_Spec,
         Member_Type   => Type_Index_For_Gpr_Node_List,
         Null_For      => null,
         Indexes       => Indexes_For_Package_Spec_F_Decls'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Package_Spec_F_End_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Package_Spec => 3);

      Member_Name_For_Package_Spec_F_End_Name : aliased constant Text_Type :=
        "F_End_Name";
      Member_Desc_For_Package_Spec_F_End_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Package_Spec_F_End_Name'Access,
         Owner         => Type_Index_For_Package_Spec,
         Member_Type   => Type_Index_For_Identifier,
         Null_For      => null,
         Indexes       => Indexes_For_Package_Spec_F_End_Name'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Project_F_Context_Clauses : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Project => 1);

      Member_Name_For_Project_F_Context_Clauses : aliased constant Text_Type :=
        "F_Context_Clauses";
      Member_Desc_For_Project_F_Context_Clauses : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Project_F_Context_Clauses'Access,
         Owner         => Type_Index_For_Project,
         Member_Type   => Type_Index_For_With_Decl_List,
         Null_For      => null,
         Indexes       => Indexes_For_Project_F_Context_Clauses'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Project_F_Project_Decl : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Project => 2);

      Member_Name_For_Project_F_Project_Decl : aliased constant Text_Type :=
        "F_Project_Decl";
      Member_Desc_For_Project_F_Project_Decl : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Project_F_Project_Decl'Access,
         Owner         => Type_Index_For_Project,
         Member_Type   => Type_Index_For_Project_Declaration,
         Null_For      => null,
         Indexes       => Indexes_For_Project_F_Project_Decl'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Project_Declaration_F_Qualifier : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Project_Declaration => 1);

      Member_Name_For_Project_Declaration_F_Qualifier : aliased constant Text_Type :=
        "F_Qualifier";
      Member_Desc_For_Project_Declaration_F_Qualifier : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Project_Declaration_F_Qualifier'Access,
         Owner         => Type_Index_For_Project_Declaration,
         Member_Type   => Type_Index_For_Project_Qualifier,
         Null_For      => null,
         Indexes       => Indexes_For_Project_Declaration_F_Qualifier'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Project_Declaration_F_Project_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Project_Declaration => 2);

      Member_Name_For_Project_Declaration_F_Project_Name : aliased constant Text_Type :=
        "F_Project_Name";
      Member_Desc_For_Project_Declaration_F_Project_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Project_Declaration_F_Project_Name'Access,
         Owner         => Type_Index_For_Project_Declaration,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Project_Declaration_F_Project_Name'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Project_Declaration_F_Extension : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Project_Declaration => 3);

      Member_Name_For_Project_Declaration_F_Extension : aliased constant Text_Type :=
        "F_Extension";
      Member_Desc_For_Project_Declaration_F_Extension : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Project_Declaration_F_Extension'Access,
         Owner         => Type_Index_For_Project_Declaration,
         Member_Type   => Type_Index_For_Project_Extension,
         Null_For      => null,
         Indexes       => Indexes_For_Project_Declaration_F_Extension'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Project_Declaration_F_Decls : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Project_Declaration => 4);

      Member_Name_For_Project_Declaration_F_Decls : aliased constant Text_Type :=
        "F_Decls";
      Member_Desc_For_Project_Declaration_F_Decls : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Project_Declaration_F_Decls'Access,
         Owner         => Type_Index_For_Project_Declaration,
         Member_Type   => Type_Index_For_Gpr_Node_List,
         Null_For      => null,
         Indexes       => Indexes_For_Project_Declaration_F_Decls'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Project_Declaration_F_End_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Project_Declaration => 5);

      Member_Name_For_Project_Declaration_F_End_Name : aliased constant Text_Type :=
        "F_End_Name";
      Member_Desc_For_Project_Declaration_F_End_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Project_Declaration_F_End_Name'Access,
         Owner         => Type_Index_For_Project_Declaration,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Project_Declaration_F_End_Name'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Project_Extension_F_Is_All : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Project_Extension => 1);

      Member_Name_For_Project_Extension_F_Is_All : aliased constant Text_Type :=
        "F_Is_All";
      Member_Desc_For_Project_Extension_F_Is_All : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Project_Extension_F_Is_All'Access,
         Owner         => Type_Index_For_Project_Extension,
         Member_Type   => Type_Index_For_All_Qualifier,
         Null_For      => null,
         Indexes       => Indexes_For_Project_Extension_F_Is_All'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Project_Extension_F_Path_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Project_Extension => 2);

      Member_Name_For_Project_Extension_F_Path_Name : aliased constant Text_Type :=
        "F_Path_Name";
      Member_Desc_For_Project_Extension_F_Path_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Project_Extension_F_Path_Name'Access,
         Owner         => Type_Index_For_Project_Extension,
         Member_Type   => Type_Index_For_String_Literal,
         Null_For      => null,
         Indexes       => Indexes_For_Project_Extension_F_Path_Name'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_String_Literal_At_F_Str_Lit : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_String_Literal_At => 1);

      Member_Name_For_String_Literal_At_F_Str_Lit : aliased constant Text_Type :=
        "F_Str_Lit";
      Member_Desc_For_String_Literal_At_F_Str_Lit : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_String_Literal_At_F_Str_Lit'Access,
         Owner         => Type_Index_For_String_Literal_At,
         Member_Type   => Type_Index_For_String_Literal,
         Null_For      => null,
         Indexes       => Indexes_For_String_Literal_At_F_Str_Lit'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_String_Literal_At_F_At_Lit : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_String_Literal_At => 2);

      Member_Name_For_String_Literal_At_F_At_Lit : aliased constant Text_Type :=
        "F_At_Lit";
      Member_Desc_For_String_Literal_At_F_At_Lit : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_String_Literal_At_F_At_Lit'Access,
         Owner         => Type_Index_For_String_Literal_At,
         Member_Type   => Type_Index_For_Num_Literal,
         Null_For      => null,
         Indexes       => Indexes_For_String_Literal_At_F_At_Lit'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Terms_F_Terms : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Terms => 1);

      Member_Name_For_Terms_F_Terms : aliased constant Text_Type :=
        "F_Terms";
      Member_Desc_For_Terms_F_Terms : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Terms_F_Terms'Access,
         Owner         => Type_Index_For_Terms,
         Member_Type   => Type_Index_For_Term_List_List,
         Null_For      => null,
         Indexes       => Indexes_For_Terms_F_Terms'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Type_Reference_F_Var_Type_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Type_Reference => 1);

      Member_Name_For_Type_Reference_F_Var_Type_Name : aliased constant Text_Type :=
        "F_Var_Type_Name";
      Member_Desc_For_Type_Reference_F_Var_Type_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Type_Reference_F_Var_Type_Name'Access,
         Owner         => Type_Index_For_Type_Reference,
         Member_Type   => Type_Index_For_Identifier_List,
         Null_For      => null,
         Indexes       => Indexes_For_Type_Reference_F_Var_Type_Name'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Typed_String_Decl_F_Type_Id : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Typed_String_Decl => 1);

      Member_Name_For_Typed_String_Decl_F_Type_Id : aliased constant Text_Type :=
        "F_Type_Id";
      Member_Desc_For_Typed_String_Decl_F_Type_Id : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Typed_String_Decl_F_Type_Id'Access,
         Owner         => Type_Index_For_Typed_String_Decl,
         Member_Type   => Type_Index_For_Identifier,
         Null_For      => null,
         Indexes       => Indexes_For_Typed_String_Decl_F_Type_Id'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Typed_String_Decl_F_String_Literals : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Typed_String_Decl => 2);

      Member_Name_For_Typed_String_Decl_F_String_Literals : aliased constant Text_Type :=
        "F_String_Literals";
      Member_Desc_For_Typed_String_Decl_F_String_Literals : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Typed_String_Decl_F_String_Literals'Access,
         Owner         => Type_Index_For_Typed_String_Decl,
         Member_Type   => Type_Index_For_String_Literal_List,
         Null_For      => null,
         Indexes       => Indexes_For_Typed_String_Decl_F_String_Literals'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Variable_Decl_F_Var_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Variable_Decl => 1);

      Member_Name_For_Variable_Decl_F_Var_Name : aliased constant Text_Type :=
        "F_Var_Name";
      Member_Desc_For_Variable_Decl_F_Var_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Variable_Decl_F_Var_Name'Access,
         Owner         => Type_Index_For_Variable_Decl,
         Member_Type   => Type_Index_For_Identifier,
         Null_For      => null,
         Indexes       => Indexes_For_Variable_Decl_F_Var_Name'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Variable_Decl_F_Var_Type : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Variable_Decl => 2);

      Member_Name_For_Variable_Decl_F_Var_Type : aliased constant Text_Type :=
        "F_Var_Type";
      Member_Desc_For_Variable_Decl_F_Var_Type : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Variable_Decl_F_Var_Type'Access,
         Owner         => Type_Index_For_Variable_Decl,
         Member_Type   => Type_Index_For_Type_Reference,
         Null_For      => null,
         Indexes       => Indexes_For_Variable_Decl_F_Var_Type'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Variable_Decl_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Variable_Decl => 3);

      Member_Name_For_Variable_Decl_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Desc_For_Variable_Decl_F_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Variable_Decl_F_Expr'Access,
         Owner         => Type_Index_For_Variable_Decl,
         Member_Type   => Type_Index_For_Term_List,
         Null_For      => null,
         Indexes       => Indexes_For_Variable_Decl_F_Expr'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Variable_Reference_F_Variable_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Variable_Reference => 1);

      Member_Name_For_Variable_Reference_F_Variable_Name : aliased constant Text_Type :=
        "F_Variable_Name";
      Member_Desc_For_Variable_Reference_F_Variable_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Variable_Reference_F_Variable_Name'Access,
         Owner         => Type_Index_For_Variable_Reference,
         Member_Type   => Type_Index_For_Identifier_List,
         Null_For      => null,
         Indexes       => Indexes_For_Variable_Reference_F_Variable_Name'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_Variable_Reference_F_Attribute_Ref : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Variable_Reference => 2);

      Member_Name_For_Variable_Reference_F_Attribute_Ref : aliased constant Text_Type :=
        "F_Attribute_Ref";
      Member_Desc_For_Variable_Reference_F_Attribute_Ref : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Variable_Reference_F_Attribute_Ref'Access,
         Owner         => Type_Index_For_Variable_Reference,
         Member_Type   => Type_Index_For_Attribute_Reference,
         Null_For      => null,
         Indexes       => Indexes_For_Variable_Reference_F_Attribute_Ref'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_With_Decl_F_Is_Limited : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_With_Decl => 1);

      Member_Name_For_With_Decl_F_Is_Limited : aliased constant Text_Type :=
        "F_Is_Limited";
      Member_Desc_For_With_Decl_F_Is_Limited : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_With_Decl_F_Is_Limited'Access,
         Owner         => Type_Index_For_With_Decl,
         Member_Type   => Type_Index_For_Limited,
         Null_For      => null,
         Indexes       => Indexes_For_With_Decl_F_Is_Limited'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      
         Indexes_For_With_Decl_F_Path_Names : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_With_Decl => 2);

      Member_Name_For_With_Decl_F_Path_Names : aliased constant Text_Type :=
        "F_Path_Names";
      Member_Desc_For_With_Decl_F_Path_Names : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_With_Decl_F_Path_Names'Access,
         Owner         => Type_Index_For_With_Decl,
         Member_Type   => Type_Index_For_String_Literal_List,
         Null_For      => null,
         Indexes       => Indexes_For_With_Decl_F_Path_Names'Access,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      

      Member_Name_For_Parent : aliased constant Text_Type :=
        "Parent";
      Member_Desc_For_Parent : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Parent'Access,
         Owner         => Type_Index_For_Gpr_Node,
         Member_Type   => Type_Index_For_Gpr_Node,
         Null_For      => null,
         Indexes       => null,
         Arguments     => (
               1 .. 0 => <>
        ));

      

         Arg_Name_1 : aliased constant Text_Type :=
           "With_Self";
         

      

      

      Member_Name_For_Parents : aliased constant Text_Type :=
        "Parents";
      Member_Desc_For_Parents : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 1,
         Name          => Member_Name_For_Parents'Access,
         Owner         => Type_Index_For_Gpr_Node,
         Member_Type   => Type_Index_For_Gpr_Node_Array,
         Null_For      => null,
         Indexes       => null,
         Arguments     => (
               1 => (Name          => Arg_Name_1'Access,  Argument_Type => Type_Index_For_Bool,  Default_Value => (Kind => Boolean_Value, Boolean_Value => True))
        ));

      


      

      

      Member_Name_For_Children : aliased constant Text_Type :=
        "Children";
      Member_Desc_For_Children : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Children'Access,
         Owner         => Type_Index_For_Gpr_Node,
         Member_Type   => Type_Index_For_Gpr_Node_Array,
         Null_For      => null,
         Indexes       => null,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      

      Member_Name_For_Token_Start : aliased constant Text_Type :=
        "Token_Start";
      Member_Desc_For_Token_Start : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Token_Start'Access,
         Owner         => Type_Index_For_Gpr_Node,
         Member_Type   => Type_Index_For_Token,
         Null_For      => null,
         Indexes       => null,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      

      Member_Name_For_Token_End : aliased constant Text_Type :=
        "Token_End";
      Member_Desc_For_Token_End : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Token_End'Access,
         Owner         => Type_Index_For_Gpr_Node,
         Member_Type   => Type_Index_For_Token,
         Null_For      => null,
         Indexes       => null,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      

      Member_Name_For_Child_Index : aliased constant Text_Type :=
        "Child_Index";
      Member_Desc_For_Child_Index : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Child_Index'Access,
         Owner         => Type_Index_For_Gpr_Node,
         Member_Type   => Type_Index_For_Int,
         Null_For      => null,
         Indexes       => null,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      

      Member_Name_For_Previous_Sibling : aliased constant Text_Type :=
        "Previous_Sibling";
      Member_Desc_For_Previous_Sibling : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Previous_Sibling'Access,
         Owner         => Type_Index_For_Gpr_Node,
         Member_Type   => Type_Index_For_Gpr_Node,
         Null_For      => null,
         Indexes       => null,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      

      Member_Name_For_Next_Sibling : aliased constant Text_Type :=
        "Next_Sibling";
      Member_Desc_For_Next_Sibling : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Next_Sibling'Access,
         Owner         => Type_Index_For_Gpr_Node,
         Member_Type   => Type_Index_For_Gpr_Node,
         Null_For      => null,
         Indexes       => null,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      

      Member_Name_For_Unit : aliased constant Text_Type :=
        "Unit";
      Member_Desc_For_Unit : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Unit'Access,
         Owner         => Type_Index_For_Gpr_Node,
         Member_Type   => Type_Index_For_Analysis_Unit,
         Null_For      => null,
         Indexes       => null,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      

      Member_Name_For_Is_Ghost : aliased constant Text_Type :=
        "Is_Ghost";
      Member_Desc_For_Is_Ghost : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Is_Ghost'Access,
         Owner         => Type_Index_For_Gpr_Node,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      

      Member_Name_For_Full_Sloc_Image : aliased constant Text_Type :=
        "Full_Sloc_Image";
      Member_Desc_For_Full_Sloc_Image : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Full_Sloc_Image'Access,
         Owner         => Type_Index_For_Gpr_Node,
         Member_Type   => Type_Index_For_String,
         Null_For      => null,
         Indexes       => null,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      

      Member_Name_For_All_Qualifier_P_As_Bool : aliased constant Text_Type :=
        "P_As_Bool";
      Member_Desc_For_All_Qualifier_P_As_Bool : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_All_Qualifier_P_As_Bool'Access,
         Owner         => Type_Index_For_All_Qualifier,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      

      

      Member_Name_For_Limited_Node_P_As_Bool : aliased constant Text_Type :=
        "P_As_Bool";
      Member_Desc_For_Limited_Node_P_As_Bool : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Limited_Node_P_As_Bool'Access,
         Owner         => Type_Index_For_Limited,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     => (
               1 .. 0 => <>
        ));


   Struct_Members : aliased constant Struct_Member_Descriptor_Array := (
      Member_Index_For_Attribute_Decl_F_Attr_Name => Member_Desc_For_Attribute_Decl_F_Attr_Name'Access,
Member_Index_For_Attribute_Decl_F_Attr_Index => Member_Desc_For_Attribute_Decl_F_Attr_Index'Access,
Member_Index_For_Attribute_Decl_F_Expr => Member_Desc_For_Attribute_Decl_F_Expr'Access,
Member_Index_For_Attribute_Reference_F_Attribute_Name => Member_Desc_For_Attribute_Reference_F_Attribute_Name'Access,
Member_Index_For_Attribute_Reference_F_Attribute_Index => Member_Desc_For_Attribute_Reference_F_Attribute_Index'Access,
Member_Index_For_Builtin_Function_Call_F_Function_Name => Member_Desc_For_Builtin_Function_Call_F_Function_Name'Access,
Member_Index_For_Builtin_Function_Call_F_Parameters => Member_Desc_For_Builtin_Function_Call_F_Parameters'Access,
Member_Index_For_Case_Construction_F_Var_Ref => Member_Desc_For_Case_Construction_F_Var_Ref'Access,
Member_Index_For_Case_Construction_F_Items => Member_Desc_For_Case_Construction_F_Items'Access,
Member_Index_For_Case_Item_F_Choice => Member_Desc_For_Case_Item_F_Choice'Access,
Member_Index_For_Case_Item_F_Decls => Member_Desc_For_Case_Item_F_Decls'Access,
Member_Index_For_Compilation_Unit_F_Project => Member_Desc_For_Compilation_Unit_F_Project'Access,
Member_Index_For_Prefix_F_Prefix => Member_Desc_For_Prefix_F_Prefix'Access,
Member_Index_For_Prefix_F_Suffix => Member_Desc_For_Prefix_F_Suffix'Access,
Member_Index_For_Package_Decl_F_Pkg_Name => Member_Desc_For_Package_Decl_F_Pkg_Name'Access,
Member_Index_For_Package_Decl_F_Pkg_Spec => Member_Desc_For_Package_Decl_F_Pkg_Spec'Access,
Member_Index_For_Package_Extension_F_Extended_Name => Member_Desc_For_Package_Extension_F_Extended_Name'Access,
Member_Index_For_Package_Renaming_F_Renamed_Name => Member_Desc_For_Package_Renaming_F_Renamed_Name'Access,
Member_Index_For_Package_Spec_F_Extension => Member_Desc_For_Package_Spec_F_Extension'Access,
Member_Index_For_Package_Spec_F_Decls => Member_Desc_For_Package_Spec_F_Decls'Access,
Member_Index_For_Package_Spec_F_End_Name => Member_Desc_For_Package_Spec_F_End_Name'Access,
Member_Index_For_Project_F_Context_Clauses => Member_Desc_For_Project_F_Context_Clauses'Access,
Member_Index_For_Project_F_Project_Decl => Member_Desc_For_Project_F_Project_Decl'Access,
Member_Index_For_Project_Declaration_F_Qualifier => Member_Desc_For_Project_Declaration_F_Qualifier'Access,
Member_Index_For_Project_Declaration_F_Project_Name => Member_Desc_For_Project_Declaration_F_Project_Name'Access,
Member_Index_For_Project_Declaration_F_Extension => Member_Desc_For_Project_Declaration_F_Extension'Access,
Member_Index_For_Project_Declaration_F_Decls => Member_Desc_For_Project_Declaration_F_Decls'Access,
Member_Index_For_Project_Declaration_F_End_Name => Member_Desc_For_Project_Declaration_F_End_Name'Access,
Member_Index_For_Project_Extension_F_Is_All => Member_Desc_For_Project_Extension_F_Is_All'Access,
Member_Index_For_Project_Extension_F_Path_Name => Member_Desc_For_Project_Extension_F_Path_Name'Access,
Member_Index_For_String_Literal_At_F_Str_Lit => Member_Desc_For_String_Literal_At_F_Str_Lit'Access,
Member_Index_For_String_Literal_At_F_At_Lit => Member_Desc_For_String_Literal_At_F_At_Lit'Access,
Member_Index_For_Terms_F_Terms => Member_Desc_For_Terms_F_Terms'Access,
Member_Index_For_Type_Reference_F_Var_Type_Name => Member_Desc_For_Type_Reference_F_Var_Type_Name'Access,
Member_Index_For_Typed_String_Decl_F_Type_Id => Member_Desc_For_Typed_String_Decl_F_Type_Id'Access,
Member_Index_For_Typed_String_Decl_F_String_Literals => Member_Desc_For_Typed_String_Decl_F_String_Literals'Access,
Member_Index_For_Variable_Decl_F_Var_Name => Member_Desc_For_Variable_Decl_F_Var_Name'Access,
Member_Index_For_Variable_Decl_F_Var_Type => Member_Desc_For_Variable_Decl_F_Var_Type'Access,
Member_Index_For_Variable_Decl_F_Expr => Member_Desc_For_Variable_Decl_F_Expr'Access,
Member_Index_For_Variable_Reference_F_Variable_Name => Member_Desc_For_Variable_Reference_F_Variable_Name'Access,
Member_Index_For_Variable_Reference_F_Attribute_Ref => Member_Desc_For_Variable_Reference_F_Attribute_Ref'Access,
Member_Index_For_With_Decl_F_Is_Limited => Member_Desc_For_With_Decl_F_Is_Limited'Access,
Member_Index_For_With_Decl_F_Path_Names => Member_Desc_For_With_Decl_F_Path_Names'Access,
Member_Index_For_Parent => Member_Desc_For_Parent'Access,
Member_Index_For_Parents => Member_Desc_For_Parents'Access,
Member_Index_For_Children => Member_Desc_For_Children'Access,
Member_Index_For_Token_Start => Member_Desc_For_Token_Start'Access,
Member_Index_For_Token_End => Member_Desc_For_Token_End'Access,
Member_Index_For_Child_Index => Member_Desc_For_Child_Index'Access,
Member_Index_For_Previous_Sibling => Member_Desc_For_Previous_Sibling'Access,
Member_Index_For_Next_Sibling => Member_Desc_For_Next_Sibling'Access,
Member_Index_For_Unit => Member_Desc_For_Unit'Access,
Member_Index_For_Is_Ghost => Member_Desc_For_Is_Ghost'Access,
Member_Index_For_Full_Sloc_Image => Member_Desc_For_Full_Sloc_Image'Access,
Member_Index_For_All_Qualifier_P_As_Bool => Member_Desc_For_All_Qualifier_P_As_Bool'Access,
Member_Index_For_Limited_Node_P_As_Bool => Member_Desc_For_Limited_Node_P_As_Bool'Access
   );

   -----------------------------
   -- Struct type descriptors --
   -----------------------------

   
      
      Node_Name_For_Gpr_Node : aliased constant Text_Type :=
        "Gpr_Node";
         Node_Repr_Name_For_Gpr_Node : aliased constant Text_Type :=
           "GprNode";
      Node_Desc_For_Gpr_Node : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 27,
         Member_Count      => 11,
         Base_Type         => No_Type_Index,
         Is_Abstract       => True,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Gpr_Node'Access,
         Repr_Name         => Node_Repr_Name_For_Gpr_Node'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 => Type_Index_For_All_Qualifier,
2 => Type_Index_For_Attribute_Decl,
3 => Type_Index_For_Attribute_Reference,
4 => Type_Index_For_Base_List,
5 => Type_Index_For_Builtin_Function_Call,
6 => Type_Index_For_Case_Construction,
7 => Type_Index_For_Case_Item,
8 => Type_Index_For_Compilation_Unit,
9 => Type_Index_For_Empty_Decl,
10 => Type_Index_For_Expr,
11 => Type_Index_For_Limited,
12 => Type_Index_For_Others_Designator,
13 => Type_Index_For_Package_Decl,
14 => Type_Index_For_Package_Extension,
15 => Type_Index_For_Package_Renaming,
16 => Type_Index_For_Package_Spec,
17 => Type_Index_For_Project,
18 => Type_Index_For_Project_Declaration,
19 => Type_Index_For_Project_Extension,
20 => Type_Index_For_Project_Qualifier,
21 => Type_Index_For_String_Literal_At,
22 => Type_Index_For_Terms,
23 => Type_Index_For_Type_Reference,
24 => Type_Index_For_Typed_String_Decl,
25 => Type_Index_For_Variable_Decl,
26 => Type_Index_For_Variable_Reference,
27 => Type_Index_For_With_Decl
         ),
         Members           => (
              1 => Member_Index_For_Parent,
2 => Member_Index_For_Parents,
3 => Member_Index_For_Children,
4 => Member_Index_For_Token_Start,
5 => Member_Index_For_Token_End,
6 => Member_Index_For_Child_Index,
7 => Member_Index_For_Previous_Sibling,
8 => Member_Index_For_Next_Sibling,
9 => Member_Index_For_Unit,
10 => Member_Index_For_Is_Ghost,
11 => Member_Index_For_Full_Sloc_Image
         ));
      
      Node_Name_For_All_Qualifier : aliased constant Text_Type :=
        "All_Qualifier";
         Node_Repr_Name_For_All_Qualifier : aliased constant Text_Type :=
           "AllQualifier";
      Node_Desc_For_All_Qualifier : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => True,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_All_Qualifier'Access,
         Repr_Name         => Node_Repr_Name_For_All_Qualifier'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 => Type_Index_For_All_Qualifier_Absent,
2 => Type_Index_For_All_Qualifier_Present
         ),
         Members           => (
              1 => Member_Index_For_All_Qualifier_P_As_Bool
         ));
      
      Node_Name_For_All_Qualifier_Absent : aliased constant Text_Type :=
        "All_Qualifier_Absent";
         Node_Repr_Name_For_All_Qualifier_Absent : aliased constant Text_Type :=
           "AllQualifierAbsent";
      Node_Desc_For_All_Qualifier_Absent : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_All_Qualifier,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_All_Qualifier_Absent'Access,
         Repr_Name         => Node_Repr_Name_For_All_Qualifier_Absent'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_All_Qualifier_Present : aliased constant Text_Type :=
        "All_Qualifier_Present";
         Node_Repr_Name_For_All_Qualifier_Present : aliased constant Text_Type :=
           "AllQualifierPresent";
      Node_Desc_For_All_Qualifier_Present : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_All_Qualifier,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_All_Qualifier_Present'Access,
         Repr_Name         => Node_Repr_Name_For_All_Qualifier_Present'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Attribute_Decl : aliased constant Text_Type :=
        "Attribute_Decl";
         Node_Repr_Name_For_Attribute_Decl : aliased constant Text_Type :=
           "AttributeDecl";
      Node_Desc_For_Attribute_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Attribute_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Attribute_Decl'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Attribute_Decl_F_Attr_Name,
2 => Member_Index_For_Attribute_Decl_F_Attr_Index,
3 => Member_Index_For_Attribute_Decl_F_Expr
         ));
      
      Node_Name_For_Attribute_Reference : aliased constant Text_Type :=
        "Attribute_Reference";
         Node_Repr_Name_For_Attribute_Reference : aliased constant Text_Type :=
           "AttributeReference";
      Node_Desc_For_Attribute_Reference : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Attribute_Reference'Access,
         Repr_Name         => Node_Repr_Name_For_Attribute_Reference'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Attribute_Reference_F_Attribute_Name,
2 => Member_Index_For_Attribute_Reference_F_Attribute_Index
         ));
      
      Node_Name_For_Base_List : aliased constant Text_Type :=
        "Base_List";
         Node_Repr_Name_For_Base_List : aliased constant Text_Type :=
           "BaseList";
      Node_Desc_For_Base_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 6,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => True,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Base_List'Access,
         Repr_Name         => Node_Repr_Name_For_Base_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 => Type_Index_For_Case_Item_List,
2 => Type_Index_For_Gpr_Node_List,
3 => Type_Index_For_Identifier_List,
4 => Type_Index_For_String_Literal_List,
5 => Type_Index_For_Term_List_List,
6 => Type_Index_For_With_Decl_List
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Case_Item_List : aliased constant Text_Type :=
        "Case_Item_List";
         Node_Repr_Name_For_Case_Item_List : aliased constant Text_Type :=
           "CaseItemList";
      Node_Desc_For_Case_Item_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Base_List,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => True,
         Name              => Node_Name_For_Case_Item_List'Access,
         Repr_Name         => Node_Repr_Name_For_Case_Item_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Gpr_Node_List : aliased constant Text_Type :=
        "Gpr_Node_List";
         Node_Repr_Name_For_Gpr_Node_List : aliased constant Text_Type :=
           "GprNodeList";
      Node_Desc_For_Gpr_Node_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Base_List,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => True,
         Name              => Node_Name_For_Gpr_Node_List'Access,
         Repr_Name         => Node_Repr_Name_For_Gpr_Node_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 => Type_Index_For_Choices,
2 => Type_Index_For_Term_List
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Choices : aliased constant Text_Type :=
        "Choices";
         Node_Repr_Name_For_Choices : aliased constant Text_Type :=
           "Choices";
      Node_Desc_For_Choices : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Gpr_Node_List,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => True,
         Name              => Node_Name_For_Choices'Access,
         Repr_Name         => Node_Repr_Name_For_Choices'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Term_List : aliased constant Text_Type :=
        "Term_List";
         Node_Repr_Name_For_Term_List : aliased constant Text_Type :=
           "TermList";
      Node_Desc_For_Term_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Gpr_Node_List,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => True,
         Name              => Node_Name_For_Term_List'Access,
         Repr_Name         => Node_Repr_Name_For_Term_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Identifier_List : aliased constant Text_Type :=
        "Identifier_List";
         Node_Repr_Name_For_Identifier_List : aliased constant Text_Type :=
           "IdentifierList";
      Node_Desc_For_Identifier_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Base_List,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => True,
         Name              => Node_Name_For_Identifier_List'Access,
         Repr_Name         => Node_Repr_Name_For_Identifier_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_String_Literal_List : aliased constant Text_Type :=
        "String_Literal_List";
         Node_Repr_Name_For_String_Literal_List : aliased constant Text_Type :=
           "StringLiteralList";
      Node_Desc_For_String_Literal_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Base_List,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => True,
         Name              => Node_Name_For_String_Literal_List'Access,
         Repr_Name         => Node_Repr_Name_For_String_Literal_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Term_List_List : aliased constant Text_Type :=
        "Term_List_List";
         Node_Repr_Name_For_Term_List_List : aliased constant Text_Type :=
           "TermListList";
      Node_Desc_For_Term_List_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Base_List,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => True,
         Name              => Node_Name_For_Term_List_List'Access,
         Repr_Name         => Node_Repr_Name_For_Term_List_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_With_Decl_List : aliased constant Text_Type :=
        "With_Decl_List";
         Node_Repr_Name_For_With_Decl_List : aliased constant Text_Type :=
           "WithDeclList";
      Node_Desc_For_With_Decl_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Base_List,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => True,
         Name              => Node_Name_For_With_Decl_List'Access,
         Repr_Name         => Node_Repr_Name_For_With_Decl_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Builtin_Function_Call : aliased constant Text_Type :=
        "Builtin_Function_Call";
         Node_Repr_Name_For_Builtin_Function_Call : aliased constant Text_Type :=
           "BuiltinFunctionCall";
      Node_Desc_For_Builtin_Function_Call : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Builtin_Function_Call'Access,
         Repr_Name         => Node_Repr_Name_For_Builtin_Function_Call'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Builtin_Function_Call_F_Function_Name,
2 => Member_Index_For_Builtin_Function_Call_F_Parameters
         ));
      
      Node_Name_For_Case_Construction : aliased constant Text_Type :=
        "Case_Construction";
         Node_Repr_Name_For_Case_Construction : aliased constant Text_Type :=
           "CaseConstruction";
      Node_Desc_For_Case_Construction : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Case_Construction'Access,
         Repr_Name         => Node_Repr_Name_For_Case_Construction'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Case_Construction_F_Var_Ref,
2 => Member_Index_For_Case_Construction_F_Items
         ));
      
      Node_Name_For_Case_Item : aliased constant Text_Type :=
        "Case_Item";
         Node_Repr_Name_For_Case_Item : aliased constant Text_Type :=
           "CaseItem";
      Node_Desc_For_Case_Item : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Case_Item'Access,
         Repr_Name         => Node_Repr_Name_For_Case_Item'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Case_Item_F_Choice,
2 => Member_Index_For_Case_Item_F_Decls
         ));
      
      Node_Name_For_Compilation_Unit : aliased constant Text_Type :=
        "Compilation_Unit";
         Node_Repr_Name_For_Compilation_Unit : aliased constant Text_Type :=
           "CompilationUnit";
      Node_Desc_For_Compilation_Unit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Compilation_Unit'Access,
         Repr_Name         => Node_Repr_Name_For_Compilation_Unit'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Compilation_Unit_F_Project
         ));
      
      Node_Name_For_Empty_Decl : aliased constant Text_Type :=
        "Empty_Decl";
         Node_Repr_Name_For_Empty_Decl : aliased constant Text_Type :=
           "EmptyDecl";
      Node_Desc_For_Empty_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Empty_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Empty_Decl'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Expr : aliased constant Text_Type :=
        "Expr";
         Node_Repr_Name_For_Expr : aliased constant Text_Type :=
           "Expr";
      Node_Desc_For_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => True,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Expr'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 => Type_Index_For_Prefix,
2 => Type_Index_For_Single_Tok_Node
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Prefix : aliased constant Text_Type :=
        "Prefix";
         Node_Repr_Name_For_Prefix : aliased constant Text_Type :=
           "Prefix";
      Node_Desc_For_Prefix : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Prefix'Access,
         Repr_Name         => Node_Repr_Name_For_Prefix'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Prefix_F_Prefix,
2 => Member_Index_For_Prefix_F_Suffix
         ));
      
      Node_Name_For_Single_Tok_Node : aliased constant Text_Type :=
        "Single_Tok_Node";
         Node_Repr_Name_For_Single_Tok_Node : aliased constant Text_Type :=
           "SingleTokNode";
      Node_Desc_For_Single_Tok_Node : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 3,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => True,
         Is_Token_Node     => True,
         Is_List_Node      => False,
         Name              => Node_Name_For_Single_Tok_Node'Access,
         Repr_Name         => Node_Repr_Name_For_Single_Tok_Node'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 => Type_Index_For_Identifier,
2 => Type_Index_For_Num_Literal,
3 => Type_Index_For_String_Literal
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Identifier : aliased constant Text_Type :=
        "Identifier";
         Node_Repr_Name_For_Identifier : aliased constant Text_Type :=
           "Id";
      Node_Desc_For_Identifier : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Single_Tok_Node,
         Is_Abstract       => False,
         Is_Token_Node     => True,
         Is_List_Node      => False,
         Name              => Node_Name_For_Identifier'Access,
         Repr_Name         => Node_Repr_Name_For_Identifier'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Num_Literal : aliased constant Text_Type :=
        "Num_Literal";
         Node_Repr_Name_For_Num_Literal : aliased constant Text_Type :=
           "Num";
      Node_Desc_For_Num_Literal : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Single_Tok_Node,
         Is_Abstract       => False,
         Is_Token_Node     => True,
         Is_List_Node      => False,
         Name              => Node_Name_For_Num_Literal'Access,
         Repr_Name         => Node_Repr_Name_For_Num_Literal'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_String_Literal : aliased constant Text_Type :=
        "String_Literal";
         Node_Repr_Name_For_String_Literal : aliased constant Text_Type :=
           "Str";
      Node_Desc_For_String_Literal : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Single_Tok_Node,
         Is_Abstract       => False,
         Is_Token_Node     => True,
         Is_List_Node      => False,
         Name              => Node_Name_For_String_Literal'Access,
         Repr_Name         => Node_Repr_Name_For_String_Literal'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Limited_Node : aliased constant Text_Type :=
        "Limited_Node";
         Node_Repr_Name_For_Limited_Node : aliased constant Text_Type :=
           "LimitedNode";
      Node_Desc_For_Limited_Node : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => True,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Limited_Node'Access,
         Repr_Name         => Node_Repr_Name_For_Limited_Node'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 => Type_Index_For_Limited_Absent,
2 => Type_Index_For_Limited_Present
         ),
         Members           => (
              1 => Member_Index_For_Limited_Node_P_As_Bool
         ));
      
      Node_Name_For_Limited_Absent : aliased constant Text_Type :=
        "Limited_Absent";
         Node_Repr_Name_For_Limited_Absent : aliased constant Text_Type :=
           "LimitedAbsent";
      Node_Desc_For_Limited_Absent : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Limited,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Limited_Absent'Access,
         Repr_Name         => Node_Repr_Name_For_Limited_Absent'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Limited_Present : aliased constant Text_Type :=
        "Limited_Present";
         Node_Repr_Name_For_Limited_Present : aliased constant Text_Type :=
           "LimitedPresent";
      Node_Desc_For_Limited_Present : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Limited,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Limited_Present'Access,
         Repr_Name         => Node_Repr_Name_For_Limited_Present'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Others_Designator : aliased constant Text_Type :=
        "Others_Designator";
         Node_Repr_Name_For_Others_Designator : aliased constant Text_Type :=
           "OthersDesignator";
      Node_Desc_For_Others_Designator : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Others_Designator'Access,
         Repr_Name         => Node_Repr_Name_For_Others_Designator'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Package_Decl : aliased constant Text_Type :=
        "Package_Decl";
         Node_Repr_Name_For_Package_Decl : aliased constant Text_Type :=
           "PackageDecl";
      Node_Desc_For_Package_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Package_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Package_Decl'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Package_Decl_F_Pkg_Name,
2 => Member_Index_For_Package_Decl_F_Pkg_Spec
         ));
      
      Node_Name_For_Package_Extension : aliased constant Text_Type :=
        "Package_Extension";
         Node_Repr_Name_For_Package_Extension : aliased constant Text_Type :=
           "PackageExtension";
      Node_Desc_For_Package_Extension : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Package_Extension'Access,
         Repr_Name         => Node_Repr_Name_For_Package_Extension'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Package_Extension_F_Extended_Name
         ));
      
      Node_Name_For_Package_Renaming : aliased constant Text_Type :=
        "Package_Renaming";
         Node_Repr_Name_For_Package_Renaming : aliased constant Text_Type :=
           "PackageRenaming";
      Node_Desc_For_Package_Renaming : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Package_Renaming'Access,
         Repr_Name         => Node_Repr_Name_For_Package_Renaming'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Package_Renaming_F_Renamed_Name
         ));
      
      Node_Name_For_Package_Spec : aliased constant Text_Type :=
        "Package_Spec";
         Node_Repr_Name_For_Package_Spec : aliased constant Text_Type :=
           "PackageSpec";
      Node_Desc_For_Package_Spec : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Package_Spec'Access,
         Repr_Name         => Node_Repr_Name_For_Package_Spec'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Package_Spec_F_Extension,
2 => Member_Index_For_Package_Spec_F_Decls,
3 => Member_Index_For_Package_Spec_F_End_Name
         ));
      
      Node_Name_For_Project : aliased constant Text_Type :=
        "Project";
         Node_Repr_Name_For_Project : aliased constant Text_Type :=
           "Project";
      Node_Desc_For_Project : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Project'Access,
         Repr_Name         => Node_Repr_Name_For_Project'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Project_F_Context_Clauses,
2 => Member_Index_For_Project_F_Project_Decl
         ));
      
      Node_Name_For_Project_Declaration : aliased constant Text_Type :=
        "Project_Declaration";
         Node_Repr_Name_For_Project_Declaration : aliased constant Text_Type :=
           "ProjectDeclaration";
      Node_Desc_For_Project_Declaration : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 5,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Project_Declaration'Access,
         Repr_Name         => Node_Repr_Name_For_Project_Declaration'Access,
         Inherited_Members => 16,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Project_Declaration_F_Qualifier,
2 => Member_Index_For_Project_Declaration_F_Project_Name,
3 => Member_Index_For_Project_Declaration_F_Extension,
4 => Member_Index_For_Project_Declaration_F_Decls,
5 => Member_Index_For_Project_Declaration_F_End_Name
         ));
      
      Node_Name_For_Project_Extension : aliased constant Text_Type :=
        "Project_Extension";
         Node_Repr_Name_For_Project_Extension : aliased constant Text_Type :=
           "ProjectExtension";
      Node_Desc_For_Project_Extension : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Project_Extension'Access,
         Repr_Name         => Node_Repr_Name_For_Project_Extension'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Project_Extension_F_Is_All,
2 => Member_Index_For_Project_Extension_F_Path_Name
         ));
      
      Node_Name_For_Project_Qualifier : aliased constant Text_Type :=
        "Project_Qualifier";
         Node_Repr_Name_For_Project_Qualifier : aliased constant Text_Type :=
           "ProjectQualifier";
      Node_Desc_For_Project_Qualifier : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 6,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => True,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Project_Qualifier'Access,
         Repr_Name         => Node_Repr_Name_For_Project_Qualifier'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 => Type_Index_For_Project_Qualifier_Abstract,
2 => Type_Index_For_Project_Qualifier_Aggregate,
3 => Type_Index_For_Project_Qualifier_Aggregate_Library,
4 => Type_Index_For_Project_Qualifier_Configuration,
5 => Type_Index_For_Project_Qualifier_Library,
6 => Type_Index_For_Project_Qualifier_Standard
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Project_Qualifier_Abstract : aliased constant Text_Type :=
        "Project_Qualifier_Abstract";
         Node_Repr_Name_For_Project_Qualifier_Abstract : aliased constant Text_Type :=
           "ProjectQualifierAbstract";
      Node_Desc_For_Project_Qualifier_Abstract : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Project_Qualifier,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Project_Qualifier_Abstract'Access,
         Repr_Name         => Node_Repr_Name_For_Project_Qualifier_Abstract'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Project_Qualifier_Aggregate : aliased constant Text_Type :=
        "Project_Qualifier_Aggregate";
         Node_Repr_Name_For_Project_Qualifier_Aggregate : aliased constant Text_Type :=
           "ProjectQualifierAggregate";
      Node_Desc_For_Project_Qualifier_Aggregate : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Project_Qualifier,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Project_Qualifier_Aggregate'Access,
         Repr_Name         => Node_Repr_Name_For_Project_Qualifier_Aggregate'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Project_Qualifier_Aggregate_Library : aliased constant Text_Type :=
        "Project_Qualifier_Aggregate_Library";
         Node_Repr_Name_For_Project_Qualifier_Aggregate_Library : aliased constant Text_Type :=
           "ProjectQualifierAggregateLibrary";
      Node_Desc_For_Project_Qualifier_Aggregate_Library : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Project_Qualifier,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Project_Qualifier_Aggregate_Library'Access,
         Repr_Name         => Node_Repr_Name_For_Project_Qualifier_Aggregate_Library'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Project_Qualifier_Configuration : aliased constant Text_Type :=
        "Project_Qualifier_Configuration";
         Node_Repr_Name_For_Project_Qualifier_Configuration : aliased constant Text_Type :=
           "ProjectQualifierConfiguration";
      Node_Desc_For_Project_Qualifier_Configuration : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Project_Qualifier,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Project_Qualifier_Configuration'Access,
         Repr_Name         => Node_Repr_Name_For_Project_Qualifier_Configuration'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Project_Qualifier_Library : aliased constant Text_Type :=
        "Project_Qualifier_Library";
         Node_Repr_Name_For_Project_Qualifier_Library : aliased constant Text_Type :=
           "ProjectQualifierLibrary";
      Node_Desc_For_Project_Qualifier_Library : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Project_Qualifier,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Project_Qualifier_Library'Access,
         Repr_Name         => Node_Repr_Name_For_Project_Qualifier_Library'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Project_Qualifier_Standard : aliased constant Text_Type :=
        "Project_Qualifier_Standard";
         Node_Repr_Name_For_Project_Qualifier_Standard : aliased constant Text_Type :=
           "ProjectQualifierStandard";
      Node_Desc_For_Project_Qualifier_Standard : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Project_Qualifier,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Project_Qualifier_Standard'Access,
         Repr_Name         => Node_Repr_Name_For_Project_Qualifier_Standard'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_String_Literal_At : aliased constant Text_Type :=
        "String_Literal_At";
         Node_Repr_Name_For_String_Literal_At : aliased constant Text_Type :=
           "StringLiteralAt";
      Node_Desc_For_String_Literal_At : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_String_Literal_At'Access,
         Repr_Name         => Node_Repr_Name_For_String_Literal_At'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_String_Literal_At_F_Str_Lit,
2 => Member_Index_For_String_Literal_At_F_At_Lit
         ));
      
      Node_Name_For_Terms : aliased constant Text_Type :=
        "Terms";
         Node_Repr_Name_For_Terms : aliased constant Text_Type :=
           "Terms";
      Node_Desc_For_Terms : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Terms'Access,
         Repr_Name         => Node_Repr_Name_For_Terms'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Terms_F_Terms
         ));
      
      Node_Name_For_Type_Reference : aliased constant Text_Type :=
        "Type_Reference";
         Node_Repr_Name_For_Type_Reference : aliased constant Text_Type :=
           "TypeReference";
      Node_Desc_For_Type_Reference : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Type_Reference'Access,
         Repr_Name         => Node_Repr_Name_For_Type_Reference'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Type_Reference_F_Var_Type_Name
         ));
      
      Node_Name_For_Typed_String_Decl : aliased constant Text_Type :=
        "Typed_String_Decl";
         Node_Repr_Name_For_Typed_String_Decl : aliased constant Text_Type :=
           "TypedStringDecl";
      Node_Desc_For_Typed_String_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Typed_String_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Typed_String_Decl'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Typed_String_Decl_F_Type_Id,
2 => Member_Index_For_Typed_String_Decl_F_String_Literals
         ));
      
      Node_Name_For_Variable_Decl : aliased constant Text_Type :=
        "Variable_Decl";
         Node_Repr_Name_For_Variable_Decl : aliased constant Text_Type :=
           "VariableDecl";
      Node_Desc_For_Variable_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Variable_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Variable_Decl'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Variable_Decl_F_Var_Name,
2 => Member_Index_For_Variable_Decl_F_Var_Type,
3 => Member_Index_For_Variable_Decl_F_Expr
         ));
      
      Node_Name_For_Variable_Reference : aliased constant Text_Type :=
        "Variable_Reference";
         Node_Repr_Name_For_Variable_Reference : aliased constant Text_Type :=
           "VariableReference";
      Node_Desc_For_Variable_Reference : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_Variable_Reference'Access,
         Repr_Name         => Node_Repr_Name_For_Variable_Reference'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Variable_Reference_F_Variable_Name,
2 => Member_Index_For_Variable_Reference_F_Attribute_Ref
         ));
      
      Node_Name_For_With_Decl : aliased constant Text_Type :=
        "With_Decl";
         Node_Repr_Name_For_With_Decl : aliased constant Text_Type :=
           "WithDecl";
      Node_Desc_For_With_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Gpr_Node,
         Is_Abstract       => False,
         Is_Token_Node     => False,
         Is_List_Node      => False,
         Name              => Node_Name_For_With_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_With_Decl'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_With_Decl_F_Is_Limited,
2 => Member_Index_For_With_Decl_F_Path_Names
         ));

   Struct_Types : aliased constant Struct_Type_Descriptor_Array := (
      Type_Index_For_Gpr_Node => Node_Desc_For_Gpr_Node'Access,
Type_Index_For_All_Qualifier => Node_Desc_For_All_Qualifier'Access,
Type_Index_For_All_Qualifier_Absent => Node_Desc_For_All_Qualifier_Absent'Access,
Type_Index_For_All_Qualifier_Present => Node_Desc_For_All_Qualifier_Present'Access,
Type_Index_For_Attribute_Decl => Node_Desc_For_Attribute_Decl'Access,
Type_Index_For_Attribute_Reference => Node_Desc_For_Attribute_Reference'Access,
Type_Index_For_Base_List => Node_Desc_For_Base_List'Access,
Type_Index_For_Case_Item_List => Node_Desc_For_Case_Item_List'Access,
Type_Index_For_Gpr_Node_List => Node_Desc_For_Gpr_Node_List'Access,
Type_Index_For_Choices => Node_Desc_For_Choices'Access,
Type_Index_For_Term_List => Node_Desc_For_Term_List'Access,
Type_Index_For_Identifier_List => Node_Desc_For_Identifier_List'Access,
Type_Index_For_String_Literal_List => Node_Desc_For_String_Literal_List'Access,
Type_Index_For_Term_List_List => Node_Desc_For_Term_List_List'Access,
Type_Index_For_With_Decl_List => Node_Desc_For_With_Decl_List'Access,
Type_Index_For_Builtin_Function_Call => Node_Desc_For_Builtin_Function_Call'Access,
Type_Index_For_Case_Construction => Node_Desc_For_Case_Construction'Access,
Type_Index_For_Case_Item => Node_Desc_For_Case_Item'Access,
Type_Index_For_Compilation_Unit => Node_Desc_For_Compilation_Unit'Access,
Type_Index_For_Empty_Decl => Node_Desc_For_Empty_Decl'Access,
Type_Index_For_Expr => Node_Desc_For_Expr'Access,
Type_Index_For_Prefix => Node_Desc_For_Prefix'Access,
Type_Index_For_Single_Tok_Node => Node_Desc_For_Single_Tok_Node'Access,
Type_Index_For_Identifier => Node_Desc_For_Identifier'Access,
Type_Index_For_Num_Literal => Node_Desc_For_Num_Literal'Access,
Type_Index_For_String_Literal => Node_Desc_For_String_Literal'Access,
Type_Index_For_Limited => Node_Desc_For_Limited_Node'Access,
Type_Index_For_Limited_Absent => Node_Desc_For_Limited_Absent'Access,
Type_Index_For_Limited_Present => Node_Desc_For_Limited_Present'Access,
Type_Index_For_Others_Designator => Node_Desc_For_Others_Designator'Access,
Type_Index_For_Package_Decl => Node_Desc_For_Package_Decl'Access,
Type_Index_For_Package_Extension => Node_Desc_For_Package_Extension'Access,
Type_Index_For_Package_Renaming => Node_Desc_For_Package_Renaming'Access,
Type_Index_For_Package_Spec => Node_Desc_For_Package_Spec'Access,
Type_Index_For_Project => Node_Desc_For_Project'Access,
Type_Index_For_Project_Declaration => Node_Desc_For_Project_Declaration'Access,
Type_Index_For_Project_Extension => Node_Desc_For_Project_Extension'Access,
Type_Index_For_Project_Qualifier => Node_Desc_For_Project_Qualifier'Access,
Type_Index_For_Project_Qualifier_Abstract => Node_Desc_For_Project_Qualifier_Abstract'Access,
Type_Index_For_Project_Qualifier_Aggregate => Node_Desc_For_Project_Qualifier_Aggregate'Access,
Type_Index_For_Project_Qualifier_Aggregate_Library => Node_Desc_For_Project_Qualifier_Aggregate_Library'Access,
Type_Index_For_Project_Qualifier_Configuration => Node_Desc_For_Project_Qualifier_Configuration'Access,
Type_Index_For_Project_Qualifier_Library => Node_Desc_For_Project_Qualifier_Library'Access,
Type_Index_For_Project_Qualifier_Standard => Node_Desc_For_Project_Qualifier_Standard'Access,
Type_Index_For_String_Literal_At => Node_Desc_For_String_Literal_At'Access,
Type_Index_For_Terms => Node_Desc_For_Terms'Access,
Type_Index_For_Type_Reference => Node_Desc_For_Type_Reference'Access,
Type_Index_For_Typed_String_Decl => Node_Desc_For_Typed_String_Decl'Access,
Type_Index_For_Variable_Decl => Node_Desc_For_Variable_Decl'Access,
Type_Index_For_Variable_Reference => Node_Desc_For_Variable_Reference'Access,
Type_Index_For_With_Decl => Node_Desc_For_With_Decl'Access
   );

   First_Node     : constant Type_Index := Type_Index_For_Gpr_Node;
   First_Property : constant Struct_Member_Index :=
     Member_Index_For_Parent;

   function Eval_Node_Member
     (Node      : Internal_Acc_Node;
      Member    : Struct_Member_Index;
      Arguments : Internal_Value_Array) return Internal_Value_Access;
   --  Implementation for the Eval_Node_Member operation in the language
   --  descriptor.

   Builtin_Types : aliased constant Builtin_Types_Record :=
     (Analysis_Unit         => Type_Index_For_Analysis_Unit,
      Big_Int               => Type_Index_For_Big_Int,
      Bool                  => Type_Index_For_Bool,
      Char                  => Type_Index_For_Character,
      Int                   => Type_Index_For_Int,
      Source_Location_Range => Type_Index_For_Source_Location_Range,
      String                => Type_Index_For_String,
      Token                 => Type_Index_For_Token,
      Symbol                => Type_Index_For_Symbol);

   Node_Kinds : constant array (Gpr_Node_Kind_Type) of Type_Index :=
     (Gpr_All_Qualifier_Absent => Type_Index_For_All_Qualifier_Absent, Gpr_All_Qualifier_Present => Type_Index_For_All_Qualifier_Present, Gpr_Attribute_Decl => Type_Index_For_Attribute_Decl, Gpr_Attribute_Reference => Type_Index_For_Attribute_Reference, Gpr_Case_Item_List => Type_Index_For_Case_Item_List, Gpr_Gpr_Node_List => Type_Index_For_Gpr_Node_List, Gpr_Choices => Type_Index_For_Choices, Gpr_Term_List => Type_Index_For_Term_List, Gpr_Identifier_List => Type_Index_For_Identifier_List, Gpr_String_Literal_List => Type_Index_For_String_Literal_List, Gpr_Term_List_List => Type_Index_For_Term_List_List, Gpr_With_Decl_List => Type_Index_For_With_Decl_List, Gpr_Builtin_Function_Call => Type_Index_For_Builtin_Function_Call, Gpr_Case_Construction => Type_Index_For_Case_Construction, Gpr_Case_Item => Type_Index_For_Case_Item, Gpr_Compilation_Unit => Type_Index_For_Compilation_Unit, Gpr_Empty_Decl => Type_Index_For_Empty_Decl, Gpr_Prefix => Type_Index_For_Prefix, Gpr_Identifier => Type_Index_For_Identifier, Gpr_Num_Literal => Type_Index_For_Num_Literal, Gpr_String_Literal => Type_Index_For_String_Literal, Gpr_Limited_Absent => Type_Index_For_Limited_Absent, Gpr_Limited_Present => Type_Index_For_Limited_Present, Gpr_Others_Designator => Type_Index_For_Others_Designator, Gpr_Package_Decl => Type_Index_For_Package_Decl, Gpr_Package_Extension => Type_Index_For_Package_Extension, Gpr_Package_Renaming => Type_Index_For_Package_Renaming, Gpr_Package_Spec => Type_Index_For_Package_Spec, Gpr_Project => Type_Index_For_Project, Gpr_Project_Declaration => Type_Index_For_Project_Declaration, Gpr_Project_Extension => Type_Index_For_Project_Extension, Gpr_Project_Qualifier_Abstract => Type_Index_For_Project_Qualifier_Abstract, Gpr_Project_Qualifier_Aggregate => Type_Index_For_Project_Qualifier_Aggregate, Gpr_Project_Qualifier_Aggregate_Library => Type_Index_For_Project_Qualifier_Aggregate_Library, Gpr_Project_Qualifier_Configuration => Type_Index_For_Project_Qualifier_Configuration, Gpr_Project_Qualifier_Library => Type_Index_For_Project_Qualifier_Library, Gpr_Project_Qualifier_Standard => Type_Index_For_Project_Qualifier_Standard, Gpr_String_Literal_At => Type_Index_For_String_Literal_At, Gpr_Terms => Type_Index_For_Terms, Gpr_Type_Reference => Type_Index_For_Type_Reference, Gpr_Typed_String_Decl => Type_Index_For_Typed_String_Decl, Gpr_Variable_Decl => Type_Index_For_Variable_Decl, Gpr_Variable_Reference => Type_Index_For_Variable_Reference, Gpr_With_Decl => Type_Index_For_With_Decl);
   --  Associate a type index to each concrete node

   -----------------------------------------------------
   --  Getter/setter helpers for introspection values --
   -----------------------------------------------------

   --  These helpers factorize common code needed in array/struct generic
   --  access/construction operations.

   procedure Set_Unit
     (Intr_Value   : Internal_Acc_Analysis_Unit;
      Actual_Value : Analysis_Unit);

   function Get_Unit
     (Intr_Value : Internal_Rec_Analysis_Unit)
      return Analysis_Unit;

   procedure Set_Big_Int
     (Intr_Value   : Internal_Acc_Big_Int;
      Actual_Value : Big_Integer);

   procedure Get_Big_Int
     (Intr_Value   : Internal_Rec_Big_Int;
      Actual_Value : out Big_Integer);

   procedure Set_Node
     (Intr_Value   : Internal_Acc_Node;
      Actual_Value : Gpr_Node'Class);

   function Get_Node
     (Intr_Value : Internal_Rec_Node)
      return Gpr_Node;

end Gpr_Parser.Generic_Introspection;
