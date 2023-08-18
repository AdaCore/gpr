
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


pragma Warnings (Off, "referenced");
with Gpr_Parser_Support.Internal.Analysis; use Gpr_Parser_Support.Internal.Analysis;
with Gpr_Parser_Support.Internal.Conversions;
use Gpr_Parser_Support.Internal.Conversions;

with Gpr_Parser.Implementation;
with Gpr_Parser.Generic_API;       use Gpr_Parser.Generic_API;
with Gpr_Parser.Generic_Impl;      use Gpr_Parser.Generic_Impl;
with Gpr_Parser.Public_Converters; use Gpr_Parser.Public_Converters;
with Gpr_Parser.Private_Converters;
use Gpr_Parser.Private_Converters;
pragma Warnings (On, "referenced");

package body Gpr_Parser.Generic_Introspection is

   


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Analysis_Unit_Kind) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Analysis_Unit_Kind) return Type_Index is
      begin
         return Type_Index_For_Analysis_Unit_Kind;
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : Internal_Rec_Analysis_Unit_Kind) return String is
      begin
         return "Analysis_Unit_Kind(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : Internal_Rec_Analysis_Unit_Kind) return Enum_Value_Index
      is
      begin
         return Analysis_Unit_Kind'Pos (Value.Value) + 1;
      end Value_Index;


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Lookup_Kind) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Lookup_Kind) return Type_Index is
      begin
         return Type_Index_For_Lookup_Kind;
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : Internal_Rec_Lookup_Kind) return String is
      begin
         return "Lookup_Kind(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : Internal_Rec_Lookup_Kind) return Enum_Value_Index
      is
      begin
         return Lookup_Kind'Pos (Value.Value) + 1;
      end Value_Index;


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Designated_Env_Kind) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Designated_Env_Kind) return Type_Index is
      begin
         return Type_Index_For_Designated_Env_Kind;
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : Internal_Rec_Designated_Env_Kind) return String is
      begin
         return "Designated_Env_Kind(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : Internal_Rec_Designated_Env_Kind) return Enum_Value_Index
      is
      begin
         return Designated_Env_Kind'Pos (Value.Value) + 1;
      end Value_Index;


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Grammar_Rule) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Grammar_Rule) return Type_Index is
      begin
         return Type_Index_For_Grammar_Rule;
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : Internal_Rec_Grammar_Rule) return String is
      begin
         return "Grammar_Rule(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : Internal_Rec_Grammar_Rule) return Enum_Value_Index
      is
      begin
         return Grammar_Rule'Pos (Value.Value) + 1;
      end Value_Index;


   -----------------
   -- Create_Enum --
   -----------------

   function Create_Enum
     (Enum_Type   : Type_Index;
      Value_Index : Enum_Value_Index) return Internal_Value_Access
   is
   begin
      case Enum_Type is
            when Type_Index_For_Analysis_Unit_Kind =>
               declare
                  Result : constant Internal_Acc_Analysis_Unit_Kind :=
                    new Internal_Rec_Analysis_Unit_Kind;
               begin
                  Result.Value := Analysis_Unit_Kind'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Lookup_Kind =>
               declare
                  Result : constant Internal_Acc_Lookup_Kind :=
                    new Internal_Rec_Lookup_Kind;
               begin
                  Result.Value := Lookup_Kind'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Designated_Env_Kind =>
               declare
                  Result : constant Internal_Acc_Designated_Env_Kind :=
                    new Internal_Rec_Designated_Env_Kind;
               begin
                  Result.Value := Designated_Env_Kind'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Grammar_Rule =>
               declare
                  Result : constant Internal_Acc_Grammar_Rule :=
                    new Internal_Rec_Grammar_Rule;
               begin
                  Result.Value := Grammar_Rule'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;

         when others =>
            --  Validation in public wrappers is supposed to prevent calling
            --  this function on non-enum types.
            raise Program_Error;
      end case;
   end Create_Enum;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Gpr_Node_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Gpr_Node_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Gpr_Node_Array) return Type_Index is
      begin
         return Type_Index_For_Gpr_Node_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Gpr_Node_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Gpr_Node_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Gpr_Node renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Gpr_Node_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Gpr_Node_Array := new Internal_Rec_Gpr_Node_Array do
            Result.Value := new Gpr_Node_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Gpr_Node renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value);
               end;
            end loop;
         end return;
      end Create_Array;


   ------------------
   -- Create_Array --
   ------------------

   function Create_Array
     (Array_Type : Type_Index;
      Values     : Internal_Value_Array) return Internal_Value_Access is
   begin
      case Array_Type is
            when Type_Index_For_Gpr_Node_Array =>
               declare
                  Result : constant Internal_Acc_Gpr_Node_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;

         when others =>
            --  Validation in public wrappers is supposed to prevent calling
            --  this function on non-array types.
            raise Program_Error;
      end case;
   end Create_Array;



   -------------------
   -- Create_Struct --
   -------------------

   function Create_Struct
     (Struct_Type : Type_Index;
      Values      : Internal_Value_Array) return Internal_Value_Access is
   begin
         pragma Unreferenced (Values);

      case Struct_Type is

         when others =>
            --  Validation in public wrappers is supposed to prevent calling
            --  this function on non-array types.
            return (raise Program_Error);
      end case;
   end Create_Struct;

   ----------------------
   -- Eval_Node_Member --
   ----------------------

   function Eval_Node_Member
     (Node      : Internal_Acc_Node;
      Member    : Struct_Member_Index;
      Arguments : Internal_Value_Array) return Internal_Value_Access
   is
      Int_Entity : constant Implementation.Internal_Entity :=
        +Gpr_Parser_Support.Internal.Conversions.Unwrap_Node (Node.Value);
      N          : constant Gpr_Node :=
        Public_Converters.Wrap_Node.all (Int_Entity.Node, Int_Entity.Info);
      Kind       : constant Gpr_Node_Kind_Type := N.Kind;
      Result     : Internal_Value_Access;
   begin
      

      case Member is
when Member_Index_For_Parent =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.Parent);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Parents =>
declare
Arg_With_Self : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Gpr_Node_Array :=  new Internal_Rec_Gpr_Node_Array;
begin
R.Value := new Gpr_Node_Array'(N.Parents (Arg_With_Self));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Children =>
declare
R : Internal_Acc_Gpr_Node_Array :=  new Internal_Rec_Gpr_Node_Array;
begin
R.Value := new Gpr_Node_Array'(N.Children);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Token_Start =>
declare
R : Internal_Acc_Token :=  new Internal_Rec_Token;
begin
R.Value := To_Generic (N.Token_Start);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Token_End =>
declare
R : Internal_Acc_Token :=  new Internal_Rec_Token;
begin
R.Value := To_Generic (N.Token_End);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Child_Index =>
declare
R : Internal_Acc_Int :=  new Internal_Rec_Int;
begin
R.Value := N.Child_Index;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Previous_Sibling =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.Previous_Sibling);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Next_Sibling =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.Next_Sibling);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Unit =>
declare
R : Internal_Acc_Analysis_Unit :=  new Internal_Rec_Analysis_Unit;
begin
Set_Unit (R, N.Unit);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Is_Ghost =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N.Is_Ghost;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Full_Sloc_Image =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N.Full_Sloc_Image);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Gpr_Gpr_Node (Kind) is
when Gpr_Ada_Access_Subp_Range =>
declare
N_Bare_Ada_Access_Subp : constant Analysis.Ada_Access_Subp := N.As_Ada_Access_Subp;
begin
case Member is
when Member_Index_For_Ada_Access_Subp_F_Subp_Kind =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_Access_Subp.F_Subp_Kind);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Access_Subp_F_Skips =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_Access_Subp.F_Skips);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Ada_Pragma_Range =>
declare
N_Bare_Ada_Pragma : constant Analysis.Ada_Pragma := N.As_Ada_Pragma;
begin
case Member is
when Member_Index_For_Ada_Pragma_F_Skips =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_Pragma.F_Skips);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Ada_Use_Range =>
declare
N_Bare_Ada_Use : constant Analysis.Ada_Use := N.As_Ada_Use;
begin
case Member is
when Member_Index_For_Ada_Use_F_Skips =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_Use.F_Skips);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Ada_With_Range =>
declare
N_Bare_Ada_With : constant Analysis.Ada_With := N.As_Ada_With;
begin
case Member is
when Member_Index_For_Ada_With_F_Has_Limited =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_With.F_Has_Limited);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_With_F_Has_Private =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_With.F_Has_Private);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_With_F_Packages =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_With.F_Packages);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Ada_Generic_Range =>
declare
N_Bare_Ada_Generic : constant Analysis.Ada_Generic := N.As_Ada_Generic;
begin
case Member is
when Member_Index_For_Ada_Generic_F_Skips =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_Generic.F_Skips);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Ada_Library_Item_Range =>
declare
N_Bare_Ada_Library_Item : constant Analysis.Ada_Library_Item := N.As_Ada_Library_Item;
begin
case Member is
when Member_Index_For_Ada_Library_Item_F_Generic_Stub =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_Library_Item.F_Generic_Stub);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Library_Item_F_Separate =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_Library_Item.F_Separate);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Library_Item_F_Main =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_Library_Item.F_Main);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Ada_Main =>
declare
N_Bare_Ada_Main : constant Analysis.Ada_Main := N.As_Ada_Main;
begin
case Member is
when Member_Index_For_Ada_Main_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_Main.F_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Gpr_Ada_Main (Kind) is
when Gpr_Ada_Pkg_Range =>
declare
N_Bare_Ada_Pkg : constant Analysis.Ada_Pkg := N_Bare_Ada_Main.As_Ada_Pkg;
begin
case Member is
when Member_Index_For_Ada_Pkg_F_Has_Private =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_Pkg.F_Has_Private);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Ada_Subp_Range =>
declare
N_Bare_Ada_Subp : constant Analysis.Ada_Subp := N_Bare_Ada_Main.As_Ada_Subp;
begin
case Member is
when Member_Index_For_Ada_Subp_F_Subp_Kind =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_Subp.F_Subp_Kind);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Gpr_Ada_Prelude_Range =>
declare
N_Bare_Ada_Prelude : constant Analysis.Ada_Prelude := N.As_Ada_Prelude;
begin
case Member is
when Member_Index_For_Ada_Prelude_F_Context_Clauses =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_Prelude.F_Context_Clauses);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Prelude_F_Library_Item =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_Prelude.F_Library_Item);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Ada_Separate_Range =>
declare
N_Bare_Ada_Separate : constant Analysis.Ada_Separate := N.As_Ada_Separate;
begin
case Member is
when Member_Index_For_Ada_Separate_F_Parent_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_Separate.F_Parent_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Ada_With_Formal_Range =>
declare
N_Bare_Ada_With_Formal : constant Analysis.Ada_With_Formal := N.As_Ada_With_Formal;
begin
case Member is
when Member_Index_For_Ada_With_Formal_F_Kind =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_With_Formal.F_Kind);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_With_Formal_F_Skips =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ada_With_Formal.F_Skips);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_All_Qualifier =>
declare
N_Bare_All_Qualifier : constant Analysis.All_Qualifier := N.As_All_Qualifier;
begin
case Member is
when Member_Index_For_All_Qualifier_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_All_Qualifier.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Attribute_Decl_Range =>
declare
N_Bare_Attribute_Decl : constant Analysis.Attribute_Decl := N.As_Attribute_Decl;
begin
case Member is
when Member_Index_For_Attribute_Decl_F_Attr_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Attribute_Decl.F_Attr_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Attribute_Decl_F_Attr_Index =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Attribute_Decl.F_Attr_Index);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Attribute_Decl_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Attribute_Decl.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Attribute_Reference_Range =>
declare
N_Bare_Attribute_Reference : constant Analysis.Attribute_Reference := N.As_Attribute_Reference;
begin
case Member is
when Member_Index_For_Attribute_Reference_F_Attribute_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Attribute_Reference.F_Attribute_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Attribute_Reference_F_Attribute_Index =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Attribute_Reference.F_Attribute_Index);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Builtin_Function_Call_Range =>
declare
N_Bare_Builtin_Function_Call : constant Analysis.Builtin_Function_Call := N.As_Builtin_Function_Call;
begin
case Member is
when Member_Index_For_Builtin_Function_Call_F_Function_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Builtin_Function_Call.F_Function_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Builtin_Function_Call_F_Parameters =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Builtin_Function_Call.F_Parameters);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Case_Construction_Range =>
declare
N_Bare_Case_Construction : constant Analysis.Case_Construction := N.As_Case_Construction;
begin
case Member is
when Member_Index_For_Case_Construction_F_Var_Ref =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Case_Construction.F_Var_Ref);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Case_Construction_F_Items =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Case_Construction.F_Items);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Case_Item_Range =>
declare
N_Bare_Case_Item : constant Analysis.Case_Item := N.As_Case_Item;
begin
case Member is
when Member_Index_For_Case_Item_F_Choice =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Case_Item.F_Choice);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Case_Item_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Case_Item.F_Decls);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Compilation_Unit_Range =>
declare
N_Bare_Compilation_Unit : constant Analysis.Compilation_Unit := N.As_Compilation_Unit;
begin
case Member is
when Member_Index_For_Compilation_Unit_F_Project =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Compilation_Unit.F_Project);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Prefix_Range =>
declare
N_Bare_Prefix : constant Analysis.Prefix := N.As_Prefix;
begin
case Member is
when Member_Index_For_Prefix_F_Prefix =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Prefix.F_Prefix);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Prefix_F_Suffix =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Prefix.F_Suffix);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Limited_Node =>
declare
N_Bare_Limited_Node : constant Analysis.Limited_Node := N.As_Limited_Node;
begin
case Member is
when Member_Index_For_Limited_Node_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Limited_Node.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Package_Decl_Range =>
declare
N_Bare_Package_Decl : constant Analysis.Package_Decl := N.As_Package_Decl;
begin
case Member is
when Member_Index_For_Package_Decl_F_Pkg_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Decl.F_Pkg_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Package_Decl_F_Pkg_Spec =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Decl.F_Pkg_Spec);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Package_Extension_Range =>
declare
N_Bare_Package_Extension : constant Analysis.Package_Extension := N.As_Package_Extension;
begin
case Member is
when Member_Index_For_Package_Extension_F_Extended_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Extension.F_Extended_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Package_Renaming_Range =>
declare
N_Bare_Package_Renaming : constant Analysis.Package_Renaming := N.As_Package_Renaming;
begin
case Member is
when Member_Index_For_Package_Renaming_F_Renamed_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Renaming.F_Renamed_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Package_Spec_Range =>
declare
N_Bare_Package_Spec : constant Analysis.Package_Spec := N.As_Package_Spec;
begin
case Member is
when Member_Index_For_Package_Spec_F_Extension =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Spec.F_Extension);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Package_Spec_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Spec.F_Decls);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Package_Spec_F_End_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Spec.F_End_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Private_Node =>
declare
N_Bare_Private_Node : constant Analysis.Private_Node := N.As_Private_Node;
begin
case Member is
when Member_Index_For_Private_Node_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Private_Node.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Project_Range =>
declare
N_Bare_Project : constant Analysis.Project := N.As_Project;
begin
case Member is
when Member_Index_For_Project_F_Context_Clauses =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Project.F_Context_Clauses);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Project_F_Project_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Project.F_Project_Decl);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Project_Declaration_Range =>
declare
N_Bare_Project_Declaration : constant Analysis.Project_Declaration := N.As_Project_Declaration;
begin
case Member is
when Member_Index_For_Project_Declaration_F_Qualifier =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Project_Declaration.F_Qualifier);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Project_Declaration_F_Project_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Project_Declaration.F_Project_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Project_Declaration_F_Extension =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Project_Declaration.F_Extension);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Project_Declaration_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Project_Declaration.F_Decls);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Project_Declaration_F_End_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Project_Declaration.F_End_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Project_Extension_Range =>
declare
N_Bare_Project_Extension : constant Analysis.Project_Extension := N.As_Project_Extension;
begin
case Member is
when Member_Index_For_Project_Extension_F_Is_All =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Project_Extension.F_Is_All);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Project_Extension_F_Path_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Project_Extension.F_Path_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_String_Literal_At_Range =>
declare
N_Bare_String_Literal_At : constant Analysis.String_Literal_At := N.As_String_Literal_At;
begin
case Member is
when Member_Index_For_String_Literal_At_F_Str_Lit =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_String_Literal_At.F_Str_Lit);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_String_Literal_At_F_At_Lit =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_String_Literal_At.F_At_Lit);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Terms_Range =>
declare
N_Bare_Terms : constant Analysis.Terms := N.As_Terms;
begin
case Member is
when Member_Index_For_Terms_F_Terms =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Terms.F_Terms);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Type_Reference_Range =>
declare
N_Bare_Type_Reference : constant Analysis.Type_Reference := N.As_Type_Reference;
begin
case Member is
when Member_Index_For_Type_Reference_F_Var_Type_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Reference.F_Var_Type_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Typed_String_Decl_Range =>
declare
N_Bare_Typed_String_Decl : constant Analysis.Typed_String_Decl := N.As_Typed_String_Decl;
begin
case Member is
when Member_Index_For_Typed_String_Decl_F_Type_Id =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Typed_String_Decl.F_Type_Id);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Typed_String_Decl_F_String_Literals =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Typed_String_Decl.F_String_Literals);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Variable_Decl_Range =>
declare
N_Bare_Variable_Decl : constant Analysis.Variable_Decl := N.As_Variable_Decl;
begin
case Member is
when Member_Index_For_Variable_Decl_F_Var_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Variable_Decl.F_Var_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Variable_Decl_F_Var_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Variable_Decl.F_Var_Type);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Variable_Decl_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Variable_Decl.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_Variable_Reference_Range =>
declare
N_Bare_Variable_Reference : constant Analysis.Variable_Reference := N.As_Variable_Reference;
begin
case Member is
when Member_Index_For_Variable_Reference_F_Variable_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Variable_Reference.F_Variable_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Variable_Reference_F_Attribute_Ref =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Variable_Reference.F_Attribute_Ref);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Gpr_With_Decl_Range =>
declare
N_Bare_With_Decl : constant Analysis.With_Decl := N.As_With_Decl;
begin
case Member is
when Member_Index_For_With_Decl_F_Is_Limited =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_With_Decl.F_Is_Limited);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_With_Decl_F_Path_Names =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_With_Decl.F_Path_Names);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
      pragma Assert (Result /= null);
      return Result;
   end Eval_Node_Member;

   --------------
   -- Set_Unit --
   --------------

   procedure Set_Unit
     (Intr_Value   : Internal_Acc_Analysis_Unit;
      Actual_Value : Analysis_Unit)
   is
      U : constant Internal_Unit :=
        +Public_Converters.Unwrap_Unit (Actual_Value);
   begin
      Intr_Value.Value :=
        Gpr_Parser_Support.Internal.Conversions.Wrap_Unit (Self_Id, U);
   end Set_Unit;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Intr_Value : Internal_Rec_Analysis_Unit)
      return Analysis_Unit
   is
      U : constant Implementation.Internal_Unit :=
        +Gpr_Parser_Support.Internal.Conversions.Unwrap_Unit (Intr_Value.Value);
   begin
      return Public_Converters.Wrap_Unit (U);
   end Get_Unit;

   -----------------
   -- Set_Big_Int --
   -----------------

   procedure Set_Big_Int
     (Intr_Value   : Internal_Acc_Big_Int;
      Actual_Value : Big_Integer) is
   begin
      Intr_Value.Value.Set (Actual_Value);
   end Set_Big_Int;

   -----------------
   -- Get_Big_Int --
   -----------------

   procedure Get_Big_Int
     (Intr_Value   : Internal_Rec_Big_Int;
      Actual_Value : out Big_Integer)
   is
   begin
      Actual_Value.Set (Intr_Value.Value);
   end Get_Big_Int;

   --------------
   -- Set_Node --
   --------------

   procedure Set_Node
     (Intr_Value   : Internal_Acc_Node;
      Actual_Value : Gpr_Node'Class)
   is
      E : constant Internal_Entity := +Unwrap_Entity (Actual_Value);
   begin
      Intr_Value.Value :=
        Gpr_Parser_Support.Internal.Conversions.Wrap_Node (Self_Id, E);
   end Set_Node;

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (Intr_Value : Internal_Rec_Node)
      return Gpr_Node
   is
      E : constant Implementation.Internal_Entity :=
        +Gpr_Parser_Support.Internal.Conversions.Unwrap_Node (Intr_Value.Value);
   begin
      return Public_Converters.Wrap_Node (E.Node, E.Info);
   end Get_Node;

end Gpr_Parser.Generic_Introspection;
