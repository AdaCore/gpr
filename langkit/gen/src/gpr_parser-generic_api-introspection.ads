
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


--  This package provides contants to refer to Gpr_Parser types and struct
--  members in the generic introspection API
--  (``Gpr_Parser_Support.Generic_API.Introspection``).

with Gpr_Parser_Support.Generic_API.Introspection;

package Gpr_Parser.Generic_API.Introspection is

   package G renames Gpr_Parser_Support.Generic_API.Introspection;

   ---------------------
   -- Type references --
   ---------------------

   package Type_Refs is
         Analysis_Unit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 1);
         Big_Integer : constant G.Type_Ref :=
           G.From_Index (Self_Id, 2);
         Boolean : constant G.Type_Ref :=
           G.From_Index (Self_Id, 3);
         Character_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 4);
         Integer : constant G.Type_Ref :=
           G.From_Index (Self_Id, 5);
         Source_Location_Range : constant G.Type_Ref :=
           G.From_Index (Self_Id, 6);
         Text_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 7);
         Token_Reference : constant G.Type_Ref :=
           G.From_Index (Self_Id, 8);
         Unbounded_Text_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 9);
         Analysis_Unit_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 10);
         Lookup_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 11);
         Designated_Env_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 12);
         Grammar_Rule : constant G.Type_Ref :=
           G.From_Index (Self_Id, 13);
         Gpr_Node_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 14);
         Gpr_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 15);
         All_Qualifier : constant G.Type_Ref :=
           G.From_Index (Self_Id, 16);
         All_Qualifier_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 17);
         All_Qualifier_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 18);
         Attribute_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 19);
         Attribute_Reference : constant G.Type_Ref :=
           G.From_Index (Self_Id, 20);
         Base_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 21);
         Case_Item_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 22);
         Gpr_Node_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 23);
         Choices : constant G.Type_Ref :=
           G.From_Index (Self_Id, 24);
         Term_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 25);
         Identifier_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 26);
         String_Literal_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 27);
         Term_List_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 28);
         With_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 29);
         Builtin_Function_Call : constant G.Type_Ref :=
           G.From_Index (Self_Id, 30);
         Case_Construction : constant G.Type_Ref :=
           G.From_Index (Self_Id, 31);
         Case_Item : constant G.Type_Ref :=
           G.From_Index (Self_Id, 32);
         Compilation_Unit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 33);
         Empty_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 34);
         Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 35);
         Prefix : constant G.Type_Ref :=
           G.From_Index (Self_Id, 36);
         Single_Tok_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 37);
         Identifier : constant G.Type_Ref :=
           G.From_Index (Self_Id, 38);
         Num_Literal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 39);
         String_Literal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 40);
         Limited_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 41);
         Limited_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 42);
         Limited_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 43);
         Others_Designator : constant G.Type_Ref :=
           G.From_Index (Self_Id, 44);
         Package_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 45);
         Package_Extension : constant G.Type_Ref :=
           G.From_Index (Self_Id, 46);
         Package_Renaming : constant G.Type_Ref :=
           G.From_Index (Self_Id, 47);
         Package_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 48);
         Project : constant G.Type_Ref :=
           G.From_Index (Self_Id, 49);
         Project_Declaration : constant G.Type_Ref :=
           G.From_Index (Self_Id, 50);
         Project_Extension : constant G.Type_Ref :=
           G.From_Index (Self_Id, 51);
         Project_Qualifier : constant G.Type_Ref :=
           G.From_Index (Self_Id, 52);
         Project_Qualifier_Abstract : constant G.Type_Ref :=
           G.From_Index (Self_Id, 53);
         Project_Qualifier_Aggregate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 54);
         Project_Qualifier_Aggregate_Library : constant G.Type_Ref :=
           G.From_Index (Self_Id, 55);
         Project_Qualifier_Configuration : constant G.Type_Ref :=
           G.From_Index (Self_Id, 56);
         Project_Qualifier_Library : constant G.Type_Ref :=
           G.From_Index (Self_Id, 57);
         Project_Qualifier_Standard : constant G.Type_Ref :=
           G.From_Index (Self_Id, 58);
         String_Literal_At : constant G.Type_Ref :=
           G.From_Index (Self_Id, 59);
         Terms : constant G.Type_Ref :=
           G.From_Index (Self_Id, 60);
         Type_Reference : constant G.Type_Ref :=
           G.From_Index (Self_Id, 61);
         Typed_String_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 62);
         Variable_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 63);
         Variable_Reference : constant G.Type_Ref :=
           G.From_Index (Self_Id, 64);
         With_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 65);
   end Type_Refs;

   Kind_To_Type : constant array (Gpr_Node_Kind_Type) of G.Type_Ref := (
      Gpr_All_Qualifier_Absent => Type_Refs.All_Qualifier_Absent,
Gpr_All_Qualifier_Present => Type_Refs.All_Qualifier_Present,
Gpr_Attribute_Decl => Type_Refs.Attribute_Decl,
Gpr_Attribute_Reference => Type_Refs.Attribute_Reference,
Gpr_Case_Item_List => Type_Refs.Case_Item_List,
Gpr_Gpr_Node_List => Type_Refs.Gpr_Node_List,
Gpr_Choices => Type_Refs.Choices,
Gpr_Term_List => Type_Refs.Term_List,
Gpr_Identifier_List => Type_Refs.Identifier_List,
Gpr_String_Literal_List => Type_Refs.String_Literal_List,
Gpr_Term_List_List => Type_Refs.Term_List_List,
Gpr_With_Decl_List => Type_Refs.With_Decl_List,
Gpr_Builtin_Function_Call => Type_Refs.Builtin_Function_Call,
Gpr_Case_Construction => Type_Refs.Case_Construction,
Gpr_Case_Item => Type_Refs.Case_Item,
Gpr_Compilation_Unit => Type_Refs.Compilation_Unit,
Gpr_Empty_Decl => Type_Refs.Empty_Decl,
Gpr_Prefix => Type_Refs.Prefix,
Gpr_Identifier => Type_Refs.Identifier,
Gpr_Num_Literal => Type_Refs.Num_Literal,
Gpr_String_Literal => Type_Refs.String_Literal,
Gpr_Limited_Absent => Type_Refs.Limited_Absent,
Gpr_Limited_Present => Type_Refs.Limited_Present,
Gpr_Others_Designator => Type_Refs.Others_Designator,
Gpr_Package_Decl => Type_Refs.Package_Decl,
Gpr_Package_Extension => Type_Refs.Package_Extension,
Gpr_Package_Renaming => Type_Refs.Package_Renaming,
Gpr_Package_Spec => Type_Refs.Package_Spec,
Gpr_Project => Type_Refs.Project,
Gpr_Project_Declaration => Type_Refs.Project_Declaration,
Gpr_Project_Extension => Type_Refs.Project_Extension,
Gpr_Project_Qualifier_Abstract => Type_Refs.Project_Qualifier_Abstract,
Gpr_Project_Qualifier_Aggregate => Type_Refs.Project_Qualifier_Aggregate,
Gpr_Project_Qualifier_Aggregate_Library => Type_Refs.Project_Qualifier_Aggregate_Library,
Gpr_Project_Qualifier_Configuration => Type_Refs.Project_Qualifier_Configuration,
Gpr_Project_Qualifier_Library => Type_Refs.Project_Qualifier_Library,
Gpr_Project_Qualifier_Standard => Type_Refs.Project_Qualifier_Standard,
Gpr_String_Literal_At => Type_Refs.String_Literal_At,
Gpr_Terms => Type_Refs.Terms,
Gpr_Type_Reference => Type_Refs.Type_Reference,
Gpr_Typed_String_Decl => Type_Refs.Typed_String_Decl,
Gpr_Variable_Decl => Type_Refs.Variable_Decl,
Gpr_Variable_Reference => Type_Refs.Variable_Reference,
Gpr_With_Decl => Type_Refs.With_Decl
   );

   -----------------------
   -- Member references --
   -----------------------

   package Member_Refs is
         Attribute_Decl_F_Attr_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 1);
         Attribute_Decl_F_Attr_Index : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 2);
         Attribute_Decl_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 3);
         Attribute_Reference_F_Attribute_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 4);
         Attribute_Reference_F_Attribute_Index : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 5);
         Builtin_Function_Call_F_Function_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 6);
         Builtin_Function_Call_F_Parameters : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 7);
         Case_Construction_F_Var_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 8);
         Case_Construction_F_Items : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 9);
         Case_Item_F_Choice : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 10);
         Case_Item_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 11);
         Compilation_Unit_F_Project : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 12);
         Prefix_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 13);
         Prefix_F_Suffix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 14);
         Package_Decl_F_Pkg_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 15);
         Package_Decl_F_Pkg_Spec : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 16);
         Package_Extension_F_Extended_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 17);
         Package_Renaming_F_Renamed_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 18);
         Package_Spec_F_Extension : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 19);
         Package_Spec_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 20);
         Package_Spec_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 21);
         Project_F_Context_Clauses : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 22);
         Project_F_Project_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 23);
         Project_Declaration_F_Qualifier : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 24);
         Project_Declaration_F_Project_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 25);
         Project_Declaration_F_Extension : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 26);
         Project_Declaration_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 27);
         Project_Declaration_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 28);
         Project_Extension_F_Is_All : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 29);
         Project_Extension_F_Path_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 30);
         String_Literal_At_F_Str_Lit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 31);
         String_Literal_At_F_At_Lit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 32);
         Terms_F_Terms : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 33);
         Type_Reference_F_Var_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 34);
         Typed_String_Decl_F_Type_Id : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 35);
         Typed_String_Decl_F_String_Literals : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 36);
         Variable_Decl_F_Var_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 37);
         Variable_Decl_F_Var_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 38);
         Variable_Decl_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 39);
         Variable_Reference_F_Variable_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 40);
         Variable_Reference_F_Attribute_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 41);
         With_Decl_F_Is_Limited : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 42);
         With_Decl_F_Path_Names : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 43);
         Parent : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 44);
         Parents : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 45);
         Children : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 46);
         Token_Start : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 47);
         Token_End : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 48);
         Child_Index : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 49);
         Previous_Sibling : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 50);
         Next_Sibling : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 51);
         Unit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 52);
         Is_Ghost : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 53);
         Full_Sloc_Image : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 54);
         All_Qualifier_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 55);
         Limited_Node_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 56);
   end Member_Refs;

end Gpr_Parser.Generic_API.Introspection;
