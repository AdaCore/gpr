
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
         Ada_Prelude_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 16);
         Ada_Access_Subp : constant G.Type_Ref :=
           G.From_Index (Self_Id, 17);
         Ada_Context_Clause : constant G.Type_Ref :=
           G.From_Index (Self_Id, 18);
         Ada_Pragma : constant G.Type_Ref :=
           G.From_Index (Self_Id, 19);
         Ada_Use : constant G.Type_Ref :=
           G.From_Index (Self_Id, 20);
         Ada_With : constant G.Type_Ref :=
           G.From_Index (Self_Id, 21);
         Ada_Entity_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 22);
         Ada_Entity_Kind_Function : constant G.Type_Ref :=
           G.From_Index (Self_Id, 23);
         Ada_Entity_Kind_Package : constant G.Type_Ref :=
           G.From_Index (Self_Id, 24);
         Ada_Entity_Kind_Procedure : constant G.Type_Ref :=
           G.From_Index (Self_Id, 25);
         Ada_Generic : constant G.Type_Ref :=
           G.From_Index (Self_Id, 26);
         Ada_Library_Item : constant G.Type_Ref :=
           G.From_Index (Self_Id, 27);
         Ada_Main : constant G.Type_Ref :=
           G.From_Index (Self_Id, 28);
         Ada_Pkg : constant G.Type_Ref :=
           G.From_Index (Self_Id, 29);
         Ada_Pkg_Body : constant G.Type_Ref :=
           G.From_Index (Self_Id, 30);
         Ada_Subp : constant G.Type_Ref :=
           G.From_Index (Self_Id, 31);
         Ada_Prelude : constant G.Type_Ref :=
           G.From_Index (Self_Id, 32);
         Ada_Separate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 33);
         Ada_Skip : constant G.Type_Ref :=
           G.From_Index (Self_Id, 34);
         Ada_With_Formal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 35);
         All_Qualifier : constant G.Type_Ref :=
           G.From_Index (Self_Id, 36);
         All_Qualifier_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 37);
         All_Qualifier_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 38);
         Attribute_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 39);
         Attribute_Reference : constant G.Type_Ref :=
           G.From_Index (Self_Id, 40);
         Base_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 41);
         Ada_Context_Clause_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 42);
         Ada_Prelude_Node_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 43);
         Ada_Skip_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 44);
         Case_Item_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 45);
         Expr_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 46);
         Gpr_Node_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 47);
         Choices : constant G.Type_Ref :=
           G.From_Index (Self_Id, 48);
         Term_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 49);
         Identifier_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 50);
         String_Literal_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 51);
         Term_List_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 52);
         With_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 53);
         Builtin_Function_Call : constant G.Type_Ref :=
           G.From_Index (Self_Id, 54);
         Case_Construction : constant G.Type_Ref :=
           G.From_Index (Self_Id, 55);
         Case_Item : constant G.Type_Ref :=
           G.From_Index (Self_Id, 56);
         Compilation_Unit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 57);
         Empty_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 58);
         Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 59);
         Prefix : constant G.Type_Ref :=
           G.From_Index (Self_Id, 60);
         Single_Tok_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 61);
         Identifier : constant G.Type_Ref :=
           G.From_Index (Self_Id, 62);
         Num_Literal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 63);
         String_Literal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 64);
         Limited_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 65);
         Limited_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 66);
         Limited_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 67);
         Others_Designator : constant G.Type_Ref :=
           G.From_Index (Self_Id, 68);
         Package_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 69);
         Package_Extension : constant G.Type_Ref :=
           G.From_Index (Self_Id, 70);
         Package_Renaming : constant G.Type_Ref :=
           G.From_Index (Self_Id, 71);
         Package_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 72);
         Private_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 73);
         Private_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 74);
         Private_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 75);
         Project : constant G.Type_Ref :=
           G.From_Index (Self_Id, 76);
         Project_Declaration : constant G.Type_Ref :=
           G.From_Index (Self_Id, 77);
         Project_Extension : constant G.Type_Ref :=
           G.From_Index (Self_Id, 78);
         Project_Qualifier : constant G.Type_Ref :=
           G.From_Index (Self_Id, 79);
         Project_Qualifier_Abstract : constant G.Type_Ref :=
           G.From_Index (Self_Id, 80);
         Project_Qualifier_Aggregate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 81);
         Project_Qualifier_Aggregate_Library : constant G.Type_Ref :=
           G.From_Index (Self_Id, 82);
         Project_Qualifier_Configuration : constant G.Type_Ref :=
           G.From_Index (Self_Id, 83);
         Project_Qualifier_Library : constant G.Type_Ref :=
           G.From_Index (Self_Id, 84);
         Project_Qualifier_Standard : constant G.Type_Ref :=
           G.From_Index (Self_Id, 85);
         Project_Reference : constant G.Type_Ref :=
           G.From_Index (Self_Id, 86);
         String_Literal_At : constant G.Type_Ref :=
           G.From_Index (Self_Id, 87);
         Terms : constant G.Type_Ref :=
           G.From_Index (Self_Id, 88);
         Type_Reference : constant G.Type_Ref :=
           G.From_Index (Self_Id, 89);
         Typed_String_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 90);
         Variable_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 91);
         Variable_Reference : constant G.Type_Ref :=
           G.From_Index (Self_Id, 92);
         With_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 93);
   end Type_Refs;

   -----------------------
   -- Member references --
   -----------------------

   package Member_Refs is
         Ada_Access_Subp_F_Subp_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 1);
         Ada_Access_Subp_F_Skips : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 2);
         Ada_Pragma_F_Skips : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 3);
         Ada_Use_F_Skips : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 4);
         Ada_With_F_Has_Limited : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 5);
         Ada_With_F_Has_Private : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 6);
         Ada_With_F_Packages : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 7);
         Ada_Generic_F_Skips : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 8);
         Ada_Library_Item_F_Generic_Stub : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 9);
         Ada_Library_Item_F_Separate : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 10);
         Ada_Library_Item_F_Main : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 11);
         Ada_Main_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 12);
         Ada_Pkg_F_Has_Private : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 13);
         Ada_Subp_F_Subp_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 14);
         Ada_Prelude_F_Context_Clauses : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 15);
         Ada_Prelude_F_Library_Item : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 16);
         Ada_Separate_F_Parent_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 17);
         Ada_With_Formal_F_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 18);
         Ada_With_Formal_F_Skips : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 19);
         Attribute_Decl_F_Attr_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 20);
         Attribute_Decl_F_Attr_Index : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 21);
         Attribute_Decl_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 22);
         Attribute_Reference_F_Attribute_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 23);
         Attribute_Reference_F_Attribute_Index : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 24);
         Builtin_Function_Call_F_Function_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 25);
         Builtin_Function_Call_F_Parameters : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 26);
         Case_Construction_F_Var_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 27);
         Case_Construction_F_Items : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 28);
         Case_Item_F_Choice : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 29);
         Case_Item_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 30);
         Compilation_Unit_F_Project : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 31);
         Prefix_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 32);
         Prefix_F_Suffix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 33);
         Package_Decl_F_Pkg_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 34);
         Package_Decl_F_Pkg_Spec : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 35);
         Package_Extension_F_Extended_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 36);
         Package_Renaming_F_Renamed_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 37);
         Package_Spec_F_Extension : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 38);
         Package_Spec_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 39);
         Package_Spec_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 40);
         Project_F_Context_Clauses : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 41);
         Project_F_Project_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 42);
         Project_Declaration_F_Qualifier : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 43);
         Project_Declaration_F_Project_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 44);
         Project_Declaration_F_Extension : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 45);
         Project_Declaration_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 46);
         Project_Declaration_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 47);
         Project_Extension_F_Is_All : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 48);
         Project_Extension_F_Path_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 49);
         Project_Reference_F_Attr_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 50);
         String_Literal_At_F_Str_Lit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 51);
         String_Literal_At_F_At_Lit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 52);
         Terms_F_Terms : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 53);
         Type_Reference_F_Var_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 54);
         Typed_String_Decl_F_Type_Id : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 55);
         Typed_String_Decl_F_String_Literals : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 56);
         Variable_Decl_F_Var_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 57);
         Variable_Decl_F_Var_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 58);
         Variable_Decl_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 59);
         Variable_Reference_F_Variable_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 60);
         Variable_Reference_F_Attribute_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 61);
         With_Decl_F_Is_Limited : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 62);
         With_Decl_F_Path_Names : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 63);
         Parent : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 64);
         Parents : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 65);
         Children : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 66);
         Token_Start : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 67);
         Token_End : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 68);
         Child_Index : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 69);
         Previous_Sibling : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 70);
         Next_Sibling : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 71);
         Unit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 72);
         Is_Ghost : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 73);
         Full_Sloc_Image : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 74);
         All_Qualifier_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 75);
         Limited_Node_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 76);
         Private_Node_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 77);
   end Member_Refs;

end Gpr_Parser.Generic_API.Introspection;
