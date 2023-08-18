
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with Gpr_Parser_Support.Diagnostics; use Gpr_Parser_Support.Diagnostics;
with Gpr_Parser_Support.Packrat;
with Gpr_Parser_Support.Slocs;       use Gpr_Parser_Support.Slocs;

pragma Warnings (Off, "referenced");
with Gpr_Parser_Support.Symbols; use Gpr_Parser_Support.Symbols;
pragma Warnings (On, "referenced");

with Gpr_Parser_Support.Text;        use Gpr_Parser_Support.Text;

with Gpr_Parser.Common;         use Gpr_Parser.Common;
with Gpr_Parser.Implementation; use Gpr_Parser.Implementation;
use Gpr_Parser.Implementation.Precomputed_Symbols;

pragma Warnings (Off, "referenced");
with Gpr_Parser.Private_Converters; use Gpr_Parser.Private_Converters;
pragma Warnings (On, "referenced");



package body Gpr_Parser.Parsers is
   pragma Warnings (Off, "use clause");
   use all type Gpr_Parser_Support.Symbols.Symbol_Type;
   pragma Warnings (On, "use clause");

   --  Prepare packrat instantiations: one per enum type and onefor each kind
   --  of node (including lists). Likewise for bump ptr. allocators, except
   --  we need them only for non-abstract AST nodes.

   pragma Warnings (Off, "is not referenced");
      package Bare_Gpr_Node_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Gpr_Node, Token_Index);

      package Bare_Ada_Prelude_Node_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Prelude_Node, Token_Index);

      package Bare_Ada_Access_Subp_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Access_Subp, Token_Index);

         
         subtype Subtype_For_Ada_Access_Subp is
            Root_Node_Record (Gpr_Ada_Access_Subp);
         type Access_To_Subtype_For_Ada_Access_Subp is access all Subtype_For_Ada_Access_Subp;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Access_Subp);
         package Bare_Ada_Access_Subp_Alloc is new Alloc
           (Subtype_For_Ada_Access_Subp, Access_To_Subtype_For_Ada_Access_Subp);

         function Allocate_Ada_Access_Subp
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Access_Subp;

         function Allocate_Ada_Access_Subp
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Access_Subp
         is
            Result      : constant Access_To_Subtype_For_Ada_Access_Subp := Bare_Ada_Access_Subp_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Access_Subp;
            return Bare_Ada_Access_Subp (Result);
         end Allocate_Ada_Access_Subp;

      package Bare_Ada_Context_Clause_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Context_Clause, Token_Index);

      package Bare_Ada_Pragma_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Pragma, Token_Index);

         
         subtype Subtype_For_Ada_Pragma is
            Root_Node_Record (Gpr_Ada_Pragma);
         type Access_To_Subtype_For_Ada_Pragma is access all Subtype_For_Ada_Pragma;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Pragma);
         package Bare_Ada_Pragma_Alloc is new Alloc
           (Subtype_For_Ada_Pragma, Access_To_Subtype_For_Ada_Pragma);

         function Allocate_Ada_Pragma
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Pragma;

         function Allocate_Ada_Pragma
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Pragma
         is
            Result      : constant Access_To_Subtype_For_Ada_Pragma := Bare_Ada_Pragma_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Pragma;
            return Bare_Ada_Pragma (Result);
         end Allocate_Ada_Pragma;

      package Bare_Ada_Use_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Use, Token_Index);

         
         subtype Subtype_For_Ada_Use is
            Root_Node_Record (Gpr_Ada_Use);
         type Access_To_Subtype_For_Ada_Use is access all Subtype_For_Ada_Use;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Use);
         package Bare_Ada_Use_Alloc is new Alloc
           (Subtype_For_Ada_Use, Access_To_Subtype_For_Ada_Use);

         function Allocate_Ada_Use
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Use;

         function Allocate_Ada_Use
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Use
         is
            Result      : constant Access_To_Subtype_For_Ada_Use := Bare_Ada_Use_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Use;
            return Bare_Ada_Use (Result);
         end Allocate_Ada_Use;

      package Bare_Ada_With_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_With, Token_Index);

         
         subtype Subtype_For_Ada_With is
            Root_Node_Record (Gpr_Ada_With);
         type Access_To_Subtype_For_Ada_With is access all Subtype_For_Ada_With;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_With);
         package Bare_Ada_With_Alloc is new Alloc
           (Subtype_For_Ada_With, Access_To_Subtype_For_Ada_With);

         function Allocate_Ada_With
           (Pool : Bump_Ptr_Pool) return Bare_Ada_With;

         function Allocate_Ada_With
           (Pool : Bump_Ptr_Pool) return Bare_Ada_With
         is
            Result      : constant Access_To_Subtype_For_Ada_With := Bare_Ada_With_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_With;
            return Bare_Ada_With (Result);
         end Allocate_Ada_With;

      package Bare_Ada_Entity_Kind_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Entity_Kind, Token_Index);

      package Bare_Ada_Entity_Kind_Function_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Entity_Kind_Function, Token_Index);

         
         subtype Subtype_For_Ada_Entity_Kind_Function is
            Root_Node_Record (Gpr_Ada_Entity_Kind_Function);
         type Access_To_Subtype_For_Ada_Entity_Kind_Function is access all Subtype_For_Ada_Entity_Kind_Function;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Entity_Kind_Function);
         package Bare_Ada_Entity_Kind_Function_Alloc is new Alloc
           (Subtype_For_Ada_Entity_Kind_Function, Access_To_Subtype_For_Ada_Entity_Kind_Function);

         function Allocate_Ada_Entity_Kind_Function
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Entity_Kind_Function;

         function Allocate_Ada_Entity_Kind_Function
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Entity_Kind_Function
         is
            Result      : constant Access_To_Subtype_For_Ada_Entity_Kind_Function := Bare_Ada_Entity_Kind_Function_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Entity_Kind_Function;
            return Bare_Ada_Entity_Kind_Function (Result);
         end Allocate_Ada_Entity_Kind_Function;

      package Bare_Ada_Entity_Kind_Package_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Entity_Kind_Package, Token_Index);

         
         subtype Subtype_For_Ada_Entity_Kind_Package is
            Root_Node_Record (Gpr_Ada_Entity_Kind_Package);
         type Access_To_Subtype_For_Ada_Entity_Kind_Package is access all Subtype_For_Ada_Entity_Kind_Package;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Entity_Kind_Package);
         package Bare_Ada_Entity_Kind_Package_Alloc is new Alloc
           (Subtype_For_Ada_Entity_Kind_Package, Access_To_Subtype_For_Ada_Entity_Kind_Package);

         function Allocate_Ada_Entity_Kind_Package
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Entity_Kind_Package;

         function Allocate_Ada_Entity_Kind_Package
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Entity_Kind_Package
         is
            Result      : constant Access_To_Subtype_For_Ada_Entity_Kind_Package := Bare_Ada_Entity_Kind_Package_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Entity_Kind_Package;
            return Bare_Ada_Entity_Kind_Package (Result);
         end Allocate_Ada_Entity_Kind_Package;

      package Bare_Ada_Entity_Kind_Procedure_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Entity_Kind_Procedure, Token_Index);

         
         subtype Subtype_For_Ada_Entity_Kind_Procedure is
            Root_Node_Record (Gpr_Ada_Entity_Kind_Procedure);
         type Access_To_Subtype_For_Ada_Entity_Kind_Procedure is access all Subtype_For_Ada_Entity_Kind_Procedure;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Entity_Kind_Procedure);
         package Bare_Ada_Entity_Kind_Procedure_Alloc is new Alloc
           (Subtype_For_Ada_Entity_Kind_Procedure, Access_To_Subtype_For_Ada_Entity_Kind_Procedure);

         function Allocate_Ada_Entity_Kind_Procedure
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Entity_Kind_Procedure;

         function Allocate_Ada_Entity_Kind_Procedure
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Entity_Kind_Procedure
         is
            Result      : constant Access_To_Subtype_For_Ada_Entity_Kind_Procedure := Bare_Ada_Entity_Kind_Procedure_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Entity_Kind_Procedure;
            return Bare_Ada_Entity_Kind_Procedure (Result);
         end Allocate_Ada_Entity_Kind_Procedure;

      package Bare_Ada_Generic_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Generic, Token_Index);

         
         subtype Subtype_For_Ada_Generic is
            Root_Node_Record (Gpr_Ada_Generic);
         type Access_To_Subtype_For_Ada_Generic is access all Subtype_For_Ada_Generic;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Generic);
         package Bare_Ada_Generic_Alloc is new Alloc
           (Subtype_For_Ada_Generic, Access_To_Subtype_For_Ada_Generic);

         function Allocate_Ada_Generic
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Generic;

         function Allocate_Ada_Generic
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Generic
         is
            Result      : constant Access_To_Subtype_For_Ada_Generic := Bare_Ada_Generic_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Generic;
            return Bare_Ada_Generic (Result);
         end Allocate_Ada_Generic;

      package Bare_Ada_Library_Item_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Library_Item, Token_Index);

         
         subtype Subtype_For_Ada_Library_Item is
            Root_Node_Record (Gpr_Ada_Library_Item);
         type Access_To_Subtype_For_Ada_Library_Item is access all Subtype_For_Ada_Library_Item;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Library_Item);
         package Bare_Ada_Library_Item_Alloc is new Alloc
           (Subtype_For_Ada_Library_Item, Access_To_Subtype_For_Ada_Library_Item);

         function Allocate_Ada_Library_Item
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Library_Item;

         function Allocate_Ada_Library_Item
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Library_Item
         is
            Result      : constant Access_To_Subtype_For_Ada_Library_Item := Bare_Ada_Library_Item_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Library_Item;
            return Bare_Ada_Library_Item (Result);
         end Allocate_Ada_Library_Item;

      package Bare_Ada_Main_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Main, Token_Index);

      package Bare_Ada_Pkg_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Pkg, Token_Index);

         
         subtype Subtype_For_Ada_Pkg is
            Root_Node_Record (Gpr_Ada_Pkg);
         type Access_To_Subtype_For_Ada_Pkg is access all Subtype_For_Ada_Pkg;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Pkg);
         package Bare_Ada_Pkg_Alloc is new Alloc
           (Subtype_For_Ada_Pkg, Access_To_Subtype_For_Ada_Pkg);

         function Allocate_Ada_Pkg
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Pkg;

         function Allocate_Ada_Pkg
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Pkg
         is
            Result      : constant Access_To_Subtype_For_Ada_Pkg := Bare_Ada_Pkg_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Pkg;
            return Bare_Ada_Pkg (Result);
         end Allocate_Ada_Pkg;

      package Bare_Ada_Pkg_Body_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Pkg_Body, Token_Index);

         
         subtype Subtype_For_Ada_Pkg_Body is
            Root_Node_Record (Gpr_Ada_Pkg_Body);
         type Access_To_Subtype_For_Ada_Pkg_Body is access all Subtype_For_Ada_Pkg_Body;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Pkg_Body);
         package Bare_Ada_Pkg_Body_Alloc is new Alloc
           (Subtype_For_Ada_Pkg_Body, Access_To_Subtype_For_Ada_Pkg_Body);

         function Allocate_Ada_Pkg_Body
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Pkg_Body;

         function Allocate_Ada_Pkg_Body
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Pkg_Body
         is
            Result      : constant Access_To_Subtype_For_Ada_Pkg_Body := Bare_Ada_Pkg_Body_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Pkg_Body;
            return Bare_Ada_Pkg_Body (Result);
         end Allocate_Ada_Pkg_Body;

      package Bare_Ada_Subp_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Subp, Token_Index);

         
         subtype Subtype_For_Ada_Subp is
            Root_Node_Record (Gpr_Ada_Subp);
         type Access_To_Subtype_For_Ada_Subp is access all Subtype_For_Ada_Subp;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Subp);
         package Bare_Ada_Subp_Alloc is new Alloc
           (Subtype_For_Ada_Subp, Access_To_Subtype_For_Ada_Subp);

         function Allocate_Ada_Subp
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Subp;

         function Allocate_Ada_Subp
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Subp
         is
            Result      : constant Access_To_Subtype_For_Ada_Subp := Bare_Ada_Subp_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Subp;
            return Bare_Ada_Subp (Result);
         end Allocate_Ada_Subp;

      package Bare_Ada_Prelude_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Prelude, Token_Index);

         
         subtype Subtype_For_Ada_Prelude is
            Root_Node_Record (Gpr_Ada_Prelude);
         type Access_To_Subtype_For_Ada_Prelude is access all Subtype_For_Ada_Prelude;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Prelude);
         package Bare_Ada_Prelude_Alloc is new Alloc
           (Subtype_For_Ada_Prelude, Access_To_Subtype_For_Ada_Prelude);

         function Allocate_Ada_Prelude
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Prelude;

         function Allocate_Ada_Prelude
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Prelude
         is
            Result      : constant Access_To_Subtype_For_Ada_Prelude := Bare_Ada_Prelude_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Prelude;
            return Bare_Ada_Prelude (Result);
         end Allocate_Ada_Prelude;

      package Bare_Ada_Separate_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Separate, Token_Index);

         
         subtype Subtype_For_Ada_Separate is
            Root_Node_Record (Gpr_Ada_Separate);
         type Access_To_Subtype_For_Ada_Separate is access all Subtype_For_Ada_Separate;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Separate);
         package Bare_Ada_Separate_Alloc is new Alloc
           (Subtype_For_Ada_Separate, Access_To_Subtype_For_Ada_Separate);

         function Allocate_Ada_Separate
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Separate;

         function Allocate_Ada_Separate
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Separate
         is
            Result      : constant Access_To_Subtype_For_Ada_Separate := Bare_Ada_Separate_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Separate;
            return Bare_Ada_Separate (Result);
         end Allocate_Ada_Separate;

      package Bare_Ada_Skip_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Skip, Token_Index);

         
         subtype Subtype_For_Ada_Skip is
            Root_Node_Record (Gpr_Ada_Skip);
         type Access_To_Subtype_For_Ada_Skip is access all Subtype_For_Ada_Skip;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Skip);
         package Bare_Ada_Skip_Alloc is new Alloc
           (Subtype_For_Ada_Skip, Access_To_Subtype_For_Ada_Skip);

         function Allocate_Ada_Skip
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Skip;

         function Allocate_Ada_Skip
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Skip
         is
            Result      : constant Access_To_Subtype_For_Ada_Skip := Bare_Ada_Skip_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Skip;
            return Bare_Ada_Skip (Result);
         end Allocate_Ada_Skip;

      package Bare_Ada_With_Formal_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_With_Formal, Token_Index);

         
         subtype Subtype_For_Ada_With_Formal is
            Root_Node_Record (Gpr_Ada_With_Formal);
         type Access_To_Subtype_For_Ada_With_Formal is access all Subtype_For_Ada_With_Formal;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_With_Formal);
         package Bare_Ada_With_Formal_Alloc is new Alloc
           (Subtype_For_Ada_With_Formal, Access_To_Subtype_For_Ada_With_Formal);

         function Allocate_Ada_With_Formal
           (Pool : Bump_Ptr_Pool) return Bare_Ada_With_Formal;

         function Allocate_Ada_With_Formal
           (Pool : Bump_Ptr_Pool) return Bare_Ada_With_Formal
         is
            Result      : constant Access_To_Subtype_For_Ada_With_Formal := Bare_Ada_With_Formal_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_With_Formal;
            return Bare_Ada_With_Formal (Result);
         end Allocate_Ada_With_Formal;

      package Bare_All_Qualifier_Memos is new Gpr_Parser_Support.Packrat
        (Bare_All_Qualifier, Token_Index);

      package Bare_All_Qualifier_Absent_Memos is new Gpr_Parser_Support.Packrat
        (Bare_All_Qualifier_Absent, Token_Index);

         
         subtype Subtype_For_All_Qualifier_Absent is
            Root_Node_Record (Gpr_All_Qualifier_Absent);
         type Access_To_Subtype_For_All_Qualifier_Absent is access all Subtype_For_All_Qualifier_Absent;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_All_Qualifier_Absent);
         package Bare_All_Qualifier_Absent_Alloc is new Alloc
           (Subtype_For_All_Qualifier_Absent, Access_To_Subtype_For_All_Qualifier_Absent);

         function Allocate_All_Qualifier_Absent
           (Pool : Bump_Ptr_Pool) return Bare_All_Qualifier_Absent;

         function Allocate_All_Qualifier_Absent
           (Pool : Bump_Ptr_Pool) return Bare_All_Qualifier_Absent
         is
            Result      : constant Access_To_Subtype_For_All_Qualifier_Absent := Bare_All_Qualifier_Absent_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_All_Qualifier_Absent;
            return Bare_All_Qualifier_Absent (Result);
         end Allocate_All_Qualifier_Absent;

      package Bare_All_Qualifier_Present_Memos is new Gpr_Parser_Support.Packrat
        (Bare_All_Qualifier_Present, Token_Index);

         
         subtype Subtype_For_All_Qualifier_Present is
            Root_Node_Record (Gpr_All_Qualifier_Present);
         type Access_To_Subtype_For_All_Qualifier_Present is access all Subtype_For_All_Qualifier_Present;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_All_Qualifier_Present);
         package Bare_All_Qualifier_Present_Alloc is new Alloc
           (Subtype_For_All_Qualifier_Present, Access_To_Subtype_For_All_Qualifier_Present);

         function Allocate_All_Qualifier_Present
           (Pool : Bump_Ptr_Pool) return Bare_All_Qualifier_Present;

         function Allocate_All_Qualifier_Present
           (Pool : Bump_Ptr_Pool) return Bare_All_Qualifier_Present
         is
            Result      : constant Access_To_Subtype_For_All_Qualifier_Present := Bare_All_Qualifier_Present_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_All_Qualifier_Present;
            return Bare_All_Qualifier_Present (Result);
         end Allocate_All_Qualifier_Present;

      package Bare_Attribute_Decl_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Attribute_Decl, Token_Index);

         
         subtype Subtype_For_Attribute_Decl is
            Root_Node_Record (Gpr_Attribute_Decl);
         type Access_To_Subtype_For_Attribute_Decl is access all Subtype_For_Attribute_Decl;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Attribute_Decl);
         package Bare_Attribute_Decl_Alloc is new Alloc
           (Subtype_For_Attribute_Decl, Access_To_Subtype_For_Attribute_Decl);

         function Allocate_Attribute_Decl
           (Pool : Bump_Ptr_Pool) return Bare_Attribute_Decl;

         function Allocate_Attribute_Decl
           (Pool : Bump_Ptr_Pool) return Bare_Attribute_Decl
         is
            Result      : constant Access_To_Subtype_For_Attribute_Decl := Bare_Attribute_Decl_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Attribute_Decl;
            return Bare_Attribute_Decl (Result);
         end Allocate_Attribute_Decl;

      package Bare_Attribute_Reference_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Attribute_Reference, Token_Index);

         
         subtype Subtype_For_Attribute_Reference is
            Root_Node_Record (Gpr_Attribute_Reference);
         type Access_To_Subtype_For_Attribute_Reference is access all Subtype_For_Attribute_Reference;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Attribute_Reference);
         package Bare_Attribute_Reference_Alloc is new Alloc
           (Subtype_For_Attribute_Reference, Access_To_Subtype_For_Attribute_Reference);

         function Allocate_Attribute_Reference
           (Pool : Bump_Ptr_Pool) return Bare_Attribute_Reference;

         function Allocate_Attribute_Reference
           (Pool : Bump_Ptr_Pool) return Bare_Attribute_Reference
         is
            Result      : constant Access_To_Subtype_For_Attribute_Reference := Bare_Attribute_Reference_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Attribute_Reference;
            return Bare_Attribute_Reference (Result);
         end Allocate_Attribute_Reference;

      package Bare_Base_List_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Base_List, Token_Index);

      package Bare_Ada_Context_Clause_List_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Context_Clause_List, Token_Index);

         
         subtype Subtype_For_Ada_Context_Clause_List is
            Root_Node_Record (Gpr_Ada_Context_Clause_List);
         type Access_To_Subtype_For_Ada_Context_Clause_List is access all Subtype_For_Ada_Context_Clause_List;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Context_Clause_List);
         package Bare_Ada_Context_Clause_List_Alloc is new Alloc
           (Subtype_For_Ada_Context_Clause_List, Access_To_Subtype_For_Ada_Context_Clause_List);

         function Allocate_Ada_Context_Clause_List
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Context_Clause_List;

         function Allocate_Ada_Context_Clause_List
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Context_Clause_List
         is
            Result      : constant Access_To_Subtype_For_Ada_Context_Clause_List := Bare_Ada_Context_Clause_List_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Context_Clause_List;
            return Bare_Ada_Context_Clause_List (Result);
         end Allocate_Ada_Context_Clause_List;

      package Bare_Ada_Prelude_Node_List_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Prelude_Node_List, Token_Index);

         
         subtype Subtype_For_Ada_Prelude_Node_List is
            Root_Node_Record (Gpr_Ada_Prelude_Node_List);
         type Access_To_Subtype_For_Ada_Prelude_Node_List is access all Subtype_For_Ada_Prelude_Node_List;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Prelude_Node_List);
         package Bare_Ada_Prelude_Node_List_Alloc is new Alloc
           (Subtype_For_Ada_Prelude_Node_List, Access_To_Subtype_For_Ada_Prelude_Node_List);

         function Allocate_Ada_Prelude_Node_List
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Prelude_Node_List;

         function Allocate_Ada_Prelude_Node_List
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Prelude_Node_List
         is
            Result      : constant Access_To_Subtype_For_Ada_Prelude_Node_List := Bare_Ada_Prelude_Node_List_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Prelude_Node_List;
            return Bare_Ada_Prelude_Node_List (Result);
         end Allocate_Ada_Prelude_Node_List;

      package Bare_Ada_Skip_List_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Ada_Skip_List, Token_Index);

         
         subtype Subtype_For_Ada_Skip_List is
            Root_Node_Record (Gpr_Ada_Skip_List);
         type Access_To_Subtype_For_Ada_Skip_List is access all Subtype_For_Ada_Skip_List;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Ada_Skip_List);
         package Bare_Ada_Skip_List_Alloc is new Alloc
           (Subtype_For_Ada_Skip_List, Access_To_Subtype_For_Ada_Skip_List);

         function Allocate_Ada_Skip_List
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Skip_List;

         function Allocate_Ada_Skip_List
           (Pool : Bump_Ptr_Pool) return Bare_Ada_Skip_List
         is
            Result      : constant Access_To_Subtype_For_Ada_Skip_List := Bare_Ada_Skip_List_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Ada_Skip_List;
            return Bare_Ada_Skip_List (Result);
         end Allocate_Ada_Skip_List;

      package Bare_Case_Item_List_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Case_Item_List, Token_Index);

         
         subtype Subtype_For_Case_Item_List is
            Root_Node_Record (Gpr_Case_Item_List);
         type Access_To_Subtype_For_Case_Item_List is access all Subtype_For_Case_Item_List;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Case_Item_List);
         package Bare_Case_Item_List_Alloc is new Alloc
           (Subtype_For_Case_Item_List, Access_To_Subtype_For_Case_Item_List);

         function Allocate_Case_Item_List
           (Pool : Bump_Ptr_Pool) return Bare_Case_Item_List;

         function Allocate_Case_Item_List
           (Pool : Bump_Ptr_Pool) return Bare_Case_Item_List
         is
            Result      : constant Access_To_Subtype_For_Case_Item_List := Bare_Case_Item_List_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Case_Item_List;
            return Bare_Case_Item_List (Result);
         end Allocate_Case_Item_List;

      package Bare_Expr_List_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Expr_List, Token_Index);

         
         subtype Subtype_For_Expr_List is
            Root_Node_Record (Gpr_Expr_List);
         type Access_To_Subtype_For_Expr_List is access all Subtype_For_Expr_List;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Expr_List);
         package Bare_Expr_List_Alloc is new Alloc
           (Subtype_For_Expr_List, Access_To_Subtype_For_Expr_List);

         function Allocate_Expr_List
           (Pool : Bump_Ptr_Pool) return Bare_Expr_List;

         function Allocate_Expr_List
           (Pool : Bump_Ptr_Pool) return Bare_Expr_List
         is
            Result      : constant Access_To_Subtype_For_Expr_List := Bare_Expr_List_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Expr_List;
            return Bare_Expr_List (Result);
         end Allocate_Expr_List;

      package Bare_Gpr_Node_List_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Gpr_Node_List, Token_Index);

         
         subtype Subtype_For_Gpr_Node_List is
            Root_Node_Record (Gpr_Gpr_Node_List);
         type Access_To_Subtype_For_Gpr_Node_List is access all Subtype_For_Gpr_Node_List;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Gpr_Node_List);
         package Bare_Gpr_Node_List_Alloc is new Alloc
           (Subtype_For_Gpr_Node_List, Access_To_Subtype_For_Gpr_Node_List);

         function Allocate_Gpr_Node_List
           (Pool : Bump_Ptr_Pool) return Bare_Gpr_Node_List;

         function Allocate_Gpr_Node_List
           (Pool : Bump_Ptr_Pool) return Bare_Gpr_Node_List
         is
            Result      : constant Access_To_Subtype_For_Gpr_Node_List := Bare_Gpr_Node_List_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Gpr_Node_List;
            return Bare_Gpr_Node_List (Result);
         end Allocate_Gpr_Node_List;

      package Bare_Choices_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Choices, Token_Index);

         
         subtype Subtype_For_Choices is
            Root_Node_Record (Gpr_Choices);
         type Access_To_Subtype_For_Choices is access all Subtype_For_Choices;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Choices);
         package Bare_Choices_Alloc is new Alloc
           (Subtype_For_Choices, Access_To_Subtype_For_Choices);

         function Allocate_Choices
           (Pool : Bump_Ptr_Pool) return Bare_Choices;

         function Allocate_Choices
           (Pool : Bump_Ptr_Pool) return Bare_Choices
         is
            Result      : constant Access_To_Subtype_For_Choices := Bare_Choices_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Choices;
            return Bare_Choices (Result);
         end Allocate_Choices;

      package Bare_Term_List_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Term_List, Token_Index);

         
         subtype Subtype_For_Term_List is
            Root_Node_Record (Gpr_Term_List);
         type Access_To_Subtype_For_Term_List is access all Subtype_For_Term_List;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Term_List);
         package Bare_Term_List_Alloc is new Alloc
           (Subtype_For_Term_List, Access_To_Subtype_For_Term_List);

         function Allocate_Term_List
           (Pool : Bump_Ptr_Pool) return Bare_Term_List;

         function Allocate_Term_List
           (Pool : Bump_Ptr_Pool) return Bare_Term_List
         is
            Result      : constant Access_To_Subtype_For_Term_List := Bare_Term_List_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Term_List;
            return Bare_Term_List (Result);
         end Allocate_Term_List;

      package Bare_Identifier_List_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Identifier_List, Token_Index);

         
         subtype Subtype_For_Identifier_List is
            Root_Node_Record (Gpr_Identifier_List);
         type Access_To_Subtype_For_Identifier_List is access all Subtype_For_Identifier_List;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Identifier_List);
         package Bare_Identifier_List_Alloc is new Alloc
           (Subtype_For_Identifier_List, Access_To_Subtype_For_Identifier_List);

         function Allocate_Identifier_List
           (Pool : Bump_Ptr_Pool) return Bare_Identifier_List;

         function Allocate_Identifier_List
           (Pool : Bump_Ptr_Pool) return Bare_Identifier_List
         is
            Result      : constant Access_To_Subtype_For_Identifier_List := Bare_Identifier_List_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Identifier_List;
            return Bare_Identifier_List (Result);
         end Allocate_Identifier_List;

      package Bare_String_Literal_List_Memos is new Gpr_Parser_Support.Packrat
        (Bare_String_Literal_List, Token_Index);

         
         subtype Subtype_For_String_Literal_List is
            Root_Node_Record (Gpr_String_Literal_List);
         type Access_To_Subtype_For_String_Literal_List is access all Subtype_For_String_Literal_List;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_String_Literal_List);
         package Bare_String_Literal_List_Alloc is new Alloc
           (Subtype_For_String_Literal_List, Access_To_Subtype_For_String_Literal_List);

         function Allocate_String_Literal_List
           (Pool : Bump_Ptr_Pool) return Bare_String_Literal_List;

         function Allocate_String_Literal_List
           (Pool : Bump_Ptr_Pool) return Bare_String_Literal_List
         is
            Result      : constant Access_To_Subtype_For_String_Literal_List := Bare_String_Literal_List_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_String_Literal_List;
            return Bare_String_Literal_List (Result);
         end Allocate_String_Literal_List;

      package Bare_Term_List_List_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Term_List_List, Token_Index);

         
         subtype Subtype_For_Term_List_List is
            Root_Node_Record (Gpr_Term_List_List);
         type Access_To_Subtype_For_Term_List_List is access all Subtype_For_Term_List_List;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Term_List_List);
         package Bare_Term_List_List_Alloc is new Alloc
           (Subtype_For_Term_List_List, Access_To_Subtype_For_Term_List_List);

         function Allocate_Term_List_List
           (Pool : Bump_Ptr_Pool) return Bare_Term_List_List;

         function Allocate_Term_List_List
           (Pool : Bump_Ptr_Pool) return Bare_Term_List_List
         is
            Result      : constant Access_To_Subtype_For_Term_List_List := Bare_Term_List_List_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Term_List_List;
            return Bare_Term_List_List (Result);
         end Allocate_Term_List_List;

      package Bare_With_Decl_List_Memos is new Gpr_Parser_Support.Packrat
        (Bare_With_Decl_List, Token_Index);

         
         subtype Subtype_For_With_Decl_List is
            Root_Node_Record (Gpr_With_Decl_List);
         type Access_To_Subtype_For_With_Decl_List is access all Subtype_For_With_Decl_List;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_With_Decl_List);
         package Bare_With_Decl_List_Alloc is new Alloc
           (Subtype_For_With_Decl_List, Access_To_Subtype_For_With_Decl_List);

         function Allocate_With_Decl_List
           (Pool : Bump_Ptr_Pool) return Bare_With_Decl_List;

         function Allocate_With_Decl_List
           (Pool : Bump_Ptr_Pool) return Bare_With_Decl_List
         is
            Result      : constant Access_To_Subtype_For_With_Decl_List := Bare_With_Decl_List_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_With_Decl_List;
            return Bare_With_Decl_List (Result);
         end Allocate_With_Decl_List;

      package Bare_Builtin_Function_Call_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Builtin_Function_Call, Token_Index);

         
         subtype Subtype_For_Builtin_Function_Call is
            Root_Node_Record (Gpr_Builtin_Function_Call);
         type Access_To_Subtype_For_Builtin_Function_Call is access all Subtype_For_Builtin_Function_Call;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Builtin_Function_Call);
         package Bare_Builtin_Function_Call_Alloc is new Alloc
           (Subtype_For_Builtin_Function_Call, Access_To_Subtype_For_Builtin_Function_Call);

         function Allocate_Builtin_Function_Call
           (Pool : Bump_Ptr_Pool) return Bare_Builtin_Function_Call;

         function Allocate_Builtin_Function_Call
           (Pool : Bump_Ptr_Pool) return Bare_Builtin_Function_Call
         is
            Result      : constant Access_To_Subtype_For_Builtin_Function_Call := Bare_Builtin_Function_Call_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Builtin_Function_Call;
            return Bare_Builtin_Function_Call (Result);
         end Allocate_Builtin_Function_Call;

      package Bare_Case_Construction_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Case_Construction, Token_Index);

         
         subtype Subtype_For_Case_Construction is
            Root_Node_Record (Gpr_Case_Construction);
         type Access_To_Subtype_For_Case_Construction is access all Subtype_For_Case_Construction;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Case_Construction);
         package Bare_Case_Construction_Alloc is new Alloc
           (Subtype_For_Case_Construction, Access_To_Subtype_For_Case_Construction);

         function Allocate_Case_Construction
           (Pool : Bump_Ptr_Pool) return Bare_Case_Construction;

         function Allocate_Case_Construction
           (Pool : Bump_Ptr_Pool) return Bare_Case_Construction
         is
            Result      : constant Access_To_Subtype_For_Case_Construction := Bare_Case_Construction_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Case_Construction;
            return Bare_Case_Construction (Result);
         end Allocate_Case_Construction;

      package Bare_Case_Item_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Case_Item, Token_Index);

         
         subtype Subtype_For_Case_Item is
            Root_Node_Record (Gpr_Case_Item);
         type Access_To_Subtype_For_Case_Item is access all Subtype_For_Case_Item;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Case_Item);
         package Bare_Case_Item_Alloc is new Alloc
           (Subtype_For_Case_Item, Access_To_Subtype_For_Case_Item);

         function Allocate_Case_Item
           (Pool : Bump_Ptr_Pool) return Bare_Case_Item;

         function Allocate_Case_Item
           (Pool : Bump_Ptr_Pool) return Bare_Case_Item
         is
            Result      : constant Access_To_Subtype_For_Case_Item := Bare_Case_Item_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Case_Item;
            return Bare_Case_Item (Result);
         end Allocate_Case_Item;

      package Bare_Compilation_Unit_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Compilation_Unit, Token_Index);

         
         subtype Subtype_For_Compilation_Unit is
            Root_Node_Record (Gpr_Compilation_Unit);
         type Access_To_Subtype_For_Compilation_Unit is access all Subtype_For_Compilation_Unit;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Compilation_Unit);
         package Bare_Compilation_Unit_Alloc is new Alloc
           (Subtype_For_Compilation_Unit, Access_To_Subtype_For_Compilation_Unit);

         function Allocate_Compilation_Unit
           (Pool : Bump_Ptr_Pool) return Bare_Compilation_Unit;

         function Allocate_Compilation_Unit
           (Pool : Bump_Ptr_Pool) return Bare_Compilation_Unit
         is
            Result      : constant Access_To_Subtype_For_Compilation_Unit := Bare_Compilation_Unit_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Compilation_Unit;
            return Bare_Compilation_Unit (Result);
         end Allocate_Compilation_Unit;

      package Bare_Empty_Decl_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Empty_Decl, Token_Index);

         
         subtype Subtype_For_Empty_Decl is
            Root_Node_Record (Gpr_Empty_Decl);
         type Access_To_Subtype_For_Empty_Decl is access all Subtype_For_Empty_Decl;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Empty_Decl);
         package Bare_Empty_Decl_Alloc is new Alloc
           (Subtype_For_Empty_Decl, Access_To_Subtype_For_Empty_Decl);

         function Allocate_Empty_Decl
           (Pool : Bump_Ptr_Pool) return Bare_Empty_Decl;

         function Allocate_Empty_Decl
           (Pool : Bump_Ptr_Pool) return Bare_Empty_Decl
         is
            Result      : constant Access_To_Subtype_For_Empty_Decl := Bare_Empty_Decl_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Empty_Decl;
            return Bare_Empty_Decl (Result);
         end Allocate_Empty_Decl;

      package Bare_Expr_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Expr, Token_Index);

      package Bare_Prefix_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Prefix, Token_Index);

         
         subtype Subtype_For_Prefix is
            Root_Node_Record (Gpr_Prefix);
         type Access_To_Subtype_For_Prefix is access all Subtype_For_Prefix;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Prefix);
         package Bare_Prefix_Alloc is new Alloc
           (Subtype_For_Prefix, Access_To_Subtype_For_Prefix);

         function Allocate_Prefix
           (Pool : Bump_Ptr_Pool) return Bare_Prefix;

         function Allocate_Prefix
           (Pool : Bump_Ptr_Pool) return Bare_Prefix
         is
            Result      : constant Access_To_Subtype_For_Prefix := Bare_Prefix_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Prefix;
            return Bare_Prefix (Result);
         end Allocate_Prefix;

      package Bare_Single_Tok_Node_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Single_Tok_Node, Token_Index);

      package Bare_Identifier_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Identifier, Token_Index);

         
         subtype Subtype_For_Identifier is
            Root_Node_Record (Gpr_Identifier);
         type Access_To_Subtype_For_Identifier is access all Subtype_For_Identifier;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Identifier);
         package Bare_Identifier_Alloc is new Alloc
           (Subtype_For_Identifier, Access_To_Subtype_For_Identifier);

         function Allocate_Identifier
           (Pool : Bump_Ptr_Pool) return Bare_Identifier;

         function Allocate_Identifier
           (Pool : Bump_Ptr_Pool) return Bare_Identifier
         is
            Result      : constant Access_To_Subtype_For_Identifier := Bare_Identifier_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Identifier;
            return Bare_Identifier (Result);
         end Allocate_Identifier;

      package Bare_Num_Literal_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Num_Literal, Token_Index);

         
         subtype Subtype_For_Num_Literal is
            Root_Node_Record (Gpr_Num_Literal);
         type Access_To_Subtype_For_Num_Literal is access all Subtype_For_Num_Literal;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Num_Literal);
         package Bare_Num_Literal_Alloc is new Alloc
           (Subtype_For_Num_Literal, Access_To_Subtype_For_Num_Literal);

         function Allocate_Num_Literal
           (Pool : Bump_Ptr_Pool) return Bare_Num_Literal;

         function Allocate_Num_Literal
           (Pool : Bump_Ptr_Pool) return Bare_Num_Literal
         is
            Result      : constant Access_To_Subtype_For_Num_Literal := Bare_Num_Literal_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Num_Literal;
            return Bare_Num_Literal (Result);
         end Allocate_Num_Literal;

      package Bare_String_Literal_Memos is new Gpr_Parser_Support.Packrat
        (Bare_String_Literal, Token_Index);

         
         subtype Subtype_For_String_Literal is
            Root_Node_Record (Gpr_String_Literal);
         type Access_To_Subtype_For_String_Literal is access all Subtype_For_String_Literal;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_String_Literal);
         package Bare_String_Literal_Alloc is new Alloc
           (Subtype_For_String_Literal, Access_To_Subtype_For_String_Literal);

         function Allocate_String_Literal
           (Pool : Bump_Ptr_Pool) return Bare_String_Literal;

         function Allocate_String_Literal
           (Pool : Bump_Ptr_Pool) return Bare_String_Literal
         is
            Result      : constant Access_To_Subtype_For_String_Literal := Bare_String_Literal_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_String_Literal;
            return Bare_String_Literal (Result);
         end Allocate_String_Literal;

      package Bare_Limited_Node_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Limited_Node, Token_Index);

      package Bare_Limited_Absent_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Limited_Absent, Token_Index);

         
         subtype Subtype_For_Limited_Absent is
            Root_Node_Record (Gpr_Limited_Absent);
         type Access_To_Subtype_For_Limited_Absent is access all Subtype_For_Limited_Absent;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Limited_Absent);
         package Bare_Limited_Absent_Alloc is new Alloc
           (Subtype_For_Limited_Absent, Access_To_Subtype_For_Limited_Absent);

         function Allocate_Limited_Absent
           (Pool : Bump_Ptr_Pool) return Bare_Limited_Absent;

         function Allocate_Limited_Absent
           (Pool : Bump_Ptr_Pool) return Bare_Limited_Absent
         is
            Result      : constant Access_To_Subtype_For_Limited_Absent := Bare_Limited_Absent_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Limited_Absent;
            return Bare_Limited_Absent (Result);
         end Allocate_Limited_Absent;

      package Bare_Limited_Present_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Limited_Present, Token_Index);

         
         subtype Subtype_For_Limited_Present is
            Root_Node_Record (Gpr_Limited_Present);
         type Access_To_Subtype_For_Limited_Present is access all Subtype_For_Limited_Present;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Limited_Present);
         package Bare_Limited_Present_Alloc is new Alloc
           (Subtype_For_Limited_Present, Access_To_Subtype_For_Limited_Present);

         function Allocate_Limited_Present
           (Pool : Bump_Ptr_Pool) return Bare_Limited_Present;

         function Allocate_Limited_Present
           (Pool : Bump_Ptr_Pool) return Bare_Limited_Present
         is
            Result      : constant Access_To_Subtype_For_Limited_Present := Bare_Limited_Present_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Limited_Present;
            return Bare_Limited_Present (Result);
         end Allocate_Limited_Present;

      package Bare_Others_Designator_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Others_Designator, Token_Index);

         
         subtype Subtype_For_Others_Designator is
            Root_Node_Record (Gpr_Others_Designator);
         type Access_To_Subtype_For_Others_Designator is access all Subtype_For_Others_Designator;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Others_Designator);
         package Bare_Others_Designator_Alloc is new Alloc
           (Subtype_For_Others_Designator, Access_To_Subtype_For_Others_Designator);

         function Allocate_Others_Designator
           (Pool : Bump_Ptr_Pool) return Bare_Others_Designator;

         function Allocate_Others_Designator
           (Pool : Bump_Ptr_Pool) return Bare_Others_Designator
         is
            Result      : constant Access_To_Subtype_For_Others_Designator := Bare_Others_Designator_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Others_Designator;
            return Bare_Others_Designator (Result);
         end Allocate_Others_Designator;

      package Bare_Package_Decl_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Package_Decl, Token_Index);

         
         subtype Subtype_For_Package_Decl is
            Root_Node_Record (Gpr_Package_Decl);
         type Access_To_Subtype_For_Package_Decl is access all Subtype_For_Package_Decl;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Package_Decl);
         package Bare_Package_Decl_Alloc is new Alloc
           (Subtype_For_Package_Decl, Access_To_Subtype_For_Package_Decl);

         function Allocate_Package_Decl
           (Pool : Bump_Ptr_Pool) return Bare_Package_Decl;

         function Allocate_Package_Decl
           (Pool : Bump_Ptr_Pool) return Bare_Package_Decl
         is
            Result      : constant Access_To_Subtype_For_Package_Decl := Bare_Package_Decl_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Package_Decl;
            return Bare_Package_Decl (Result);
         end Allocate_Package_Decl;

      package Bare_Package_Extension_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Package_Extension, Token_Index);

         
         subtype Subtype_For_Package_Extension is
            Root_Node_Record (Gpr_Package_Extension);
         type Access_To_Subtype_For_Package_Extension is access all Subtype_For_Package_Extension;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Package_Extension);
         package Bare_Package_Extension_Alloc is new Alloc
           (Subtype_For_Package_Extension, Access_To_Subtype_For_Package_Extension);

         function Allocate_Package_Extension
           (Pool : Bump_Ptr_Pool) return Bare_Package_Extension;

         function Allocate_Package_Extension
           (Pool : Bump_Ptr_Pool) return Bare_Package_Extension
         is
            Result      : constant Access_To_Subtype_For_Package_Extension := Bare_Package_Extension_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Package_Extension;
            return Bare_Package_Extension (Result);
         end Allocate_Package_Extension;

      package Bare_Package_Renaming_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Package_Renaming, Token_Index);

         
         subtype Subtype_For_Package_Renaming is
            Root_Node_Record (Gpr_Package_Renaming);
         type Access_To_Subtype_For_Package_Renaming is access all Subtype_For_Package_Renaming;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Package_Renaming);
         package Bare_Package_Renaming_Alloc is new Alloc
           (Subtype_For_Package_Renaming, Access_To_Subtype_For_Package_Renaming);

         function Allocate_Package_Renaming
           (Pool : Bump_Ptr_Pool) return Bare_Package_Renaming;

         function Allocate_Package_Renaming
           (Pool : Bump_Ptr_Pool) return Bare_Package_Renaming
         is
            Result      : constant Access_To_Subtype_For_Package_Renaming := Bare_Package_Renaming_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Package_Renaming;
            return Bare_Package_Renaming (Result);
         end Allocate_Package_Renaming;

      package Bare_Package_Spec_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Package_Spec, Token_Index);

         
         subtype Subtype_For_Package_Spec is
            Root_Node_Record (Gpr_Package_Spec);
         type Access_To_Subtype_For_Package_Spec is access all Subtype_For_Package_Spec;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Package_Spec);
         package Bare_Package_Spec_Alloc is new Alloc
           (Subtype_For_Package_Spec, Access_To_Subtype_For_Package_Spec);

         function Allocate_Package_Spec
           (Pool : Bump_Ptr_Pool) return Bare_Package_Spec;

         function Allocate_Package_Spec
           (Pool : Bump_Ptr_Pool) return Bare_Package_Spec
         is
            Result      : constant Access_To_Subtype_For_Package_Spec := Bare_Package_Spec_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Package_Spec;
            return Bare_Package_Spec (Result);
         end Allocate_Package_Spec;

      package Bare_Private_Node_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Private_Node, Token_Index);

      package Bare_Private_Absent_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Private_Absent, Token_Index);

         
         subtype Subtype_For_Private_Absent is
            Root_Node_Record (Gpr_Private_Absent);
         type Access_To_Subtype_For_Private_Absent is access all Subtype_For_Private_Absent;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Private_Absent);
         package Bare_Private_Absent_Alloc is new Alloc
           (Subtype_For_Private_Absent, Access_To_Subtype_For_Private_Absent);

         function Allocate_Private_Absent
           (Pool : Bump_Ptr_Pool) return Bare_Private_Absent;

         function Allocate_Private_Absent
           (Pool : Bump_Ptr_Pool) return Bare_Private_Absent
         is
            Result      : constant Access_To_Subtype_For_Private_Absent := Bare_Private_Absent_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Private_Absent;
            return Bare_Private_Absent (Result);
         end Allocate_Private_Absent;

      package Bare_Private_Present_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Private_Present, Token_Index);

         
         subtype Subtype_For_Private_Present is
            Root_Node_Record (Gpr_Private_Present);
         type Access_To_Subtype_For_Private_Present is access all Subtype_For_Private_Present;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Private_Present);
         package Bare_Private_Present_Alloc is new Alloc
           (Subtype_For_Private_Present, Access_To_Subtype_For_Private_Present);

         function Allocate_Private_Present
           (Pool : Bump_Ptr_Pool) return Bare_Private_Present;

         function Allocate_Private_Present
           (Pool : Bump_Ptr_Pool) return Bare_Private_Present
         is
            Result      : constant Access_To_Subtype_For_Private_Present := Bare_Private_Present_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Private_Present;
            return Bare_Private_Present (Result);
         end Allocate_Private_Present;

      package Bare_Project_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Project, Token_Index);

         
         subtype Subtype_For_Project is
            Root_Node_Record (Gpr_Project);
         type Access_To_Subtype_For_Project is access all Subtype_For_Project;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Project);
         package Bare_Project_Alloc is new Alloc
           (Subtype_For_Project, Access_To_Subtype_For_Project);

         function Allocate_Project
           (Pool : Bump_Ptr_Pool) return Bare_Project;

         function Allocate_Project
           (Pool : Bump_Ptr_Pool) return Bare_Project
         is
            Result      : constant Access_To_Subtype_For_Project := Bare_Project_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Project;
            return Bare_Project (Result);
         end Allocate_Project;

      package Bare_Project_Declaration_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Project_Declaration, Token_Index);

         
         subtype Subtype_For_Project_Declaration is
            Root_Node_Record (Gpr_Project_Declaration);
         type Access_To_Subtype_For_Project_Declaration is access all Subtype_For_Project_Declaration;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Project_Declaration);
         package Bare_Project_Declaration_Alloc is new Alloc
           (Subtype_For_Project_Declaration, Access_To_Subtype_For_Project_Declaration);

         function Allocate_Project_Declaration
           (Pool : Bump_Ptr_Pool) return Bare_Project_Declaration;

         function Allocate_Project_Declaration
           (Pool : Bump_Ptr_Pool) return Bare_Project_Declaration
         is
            Result      : constant Access_To_Subtype_For_Project_Declaration := Bare_Project_Declaration_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Project_Declaration;
            return Bare_Project_Declaration (Result);
         end Allocate_Project_Declaration;

      package Bare_Project_Extension_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Project_Extension, Token_Index);

         
         subtype Subtype_For_Project_Extension is
            Root_Node_Record (Gpr_Project_Extension);
         type Access_To_Subtype_For_Project_Extension is access all Subtype_For_Project_Extension;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Project_Extension);
         package Bare_Project_Extension_Alloc is new Alloc
           (Subtype_For_Project_Extension, Access_To_Subtype_For_Project_Extension);

         function Allocate_Project_Extension
           (Pool : Bump_Ptr_Pool) return Bare_Project_Extension;

         function Allocate_Project_Extension
           (Pool : Bump_Ptr_Pool) return Bare_Project_Extension
         is
            Result      : constant Access_To_Subtype_For_Project_Extension := Bare_Project_Extension_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Project_Extension;
            return Bare_Project_Extension (Result);
         end Allocate_Project_Extension;

      package Bare_Project_Qualifier_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Project_Qualifier, Token_Index);

      package Bare_Project_Qualifier_Abstract_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Project_Qualifier_Abstract, Token_Index);

         
         subtype Subtype_For_Project_Qualifier_Abstract is
            Root_Node_Record (Gpr_Project_Qualifier_Abstract);
         type Access_To_Subtype_For_Project_Qualifier_Abstract is access all Subtype_For_Project_Qualifier_Abstract;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Project_Qualifier_Abstract);
         package Bare_Project_Qualifier_Abstract_Alloc is new Alloc
           (Subtype_For_Project_Qualifier_Abstract, Access_To_Subtype_For_Project_Qualifier_Abstract);

         function Allocate_Project_Qualifier_Abstract
           (Pool : Bump_Ptr_Pool) return Bare_Project_Qualifier_Abstract;

         function Allocate_Project_Qualifier_Abstract
           (Pool : Bump_Ptr_Pool) return Bare_Project_Qualifier_Abstract
         is
            Result      : constant Access_To_Subtype_For_Project_Qualifier_Abstract := Bare_Project_Qualifier_Abstract_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Project_Qualifier_Abstract;
            return Bare_Project_Qualifier_Abstract (Result);
         end Allocate_Project_Qualifier_Abstract;

      package Bare_Project_Qualifier_Aggregate_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Project_Qualifier_Aggregate, Token_Index);

         
         subtype Subtype_For_Project_Qualifier_Aggregate is
            Root_Node_Record (Gpr_Project_Qualifier_Aggregate);
         type Access_To_Subtype_For_Project_Qualifier_Aggregate is access all Subtype_For_Project_Qualifier_Aggregate;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Project_Qualifier_Aggregate);
         package Bare_Project_Qualifier_Aggregate_Alloc is new Alloc
           (Subtype_For_Project_Qualifier_Aggregate, Access_To_Subtype_For_Project_Qualifier_Aggregate);

         function Allocate_Project_Qualifier_Aggregate
           (Pool : Bump_Ptr_Pool) return Bare_Project_Qualifier_Aggregate;

         function Allocate_Project_Qualifier_Aggregate
           (Pool : Bump_Ptr_Pool) return Bare_Project_Qualifier_Aggregate
         is
            Result      : constant Access_To_Subtype_For_Project_Qualifier_Aggregate := Bare_Project_Qualifier_Aggregate_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Project_Qualifier_Aggregate;
            return Bare_Project_Qualifier_Aggregate (Result);
         end Allocate_Project_Qualifier_Aggregate;

      package Bare_Project_Qualifier_Aggregate_Library_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Project_Qualifier_Aggregate_Library, Token_Index);

         
         subtype Subtype_For_Project_Qualifier_Aggregate_Library is
            Root_Node_Record (Gpr_Project_Qualifier_Aggregate_Library);
         type Access_To_Subtype_For_Project_Qualifier_Aggregate_Library is access all Subtype_For_Project_Qualifier_Aggregate_Library;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Project_Qualifier_Aggregate_Library);
         package Bare_Project_Qualifier_Aggregate_Library_Alloc is new Alloc
           (Subtype_For_Project_Qualifier_Aggregate_Library, Access_To_Subtype_For_Project_Qualifier_Aggregate_Library);

         function Allocate_Project_Qualifier_Aggregate_Library
           (Pool : Bump_Ptr_Pool) return Bare_Project_Qualifier_Aggregate_Library;

         function Allocate_Project_Qualifier_Aggregate_Library
           (Pool : Bump_Ptr_Pool) return Bare_Project_Qualifier_Aggregate_Library
         is
            Result      : constant Access_To_Subtype_For_Project_Qualifier_Aggregate_Library := Bare_Project_Qualifier_Aggregate_Library_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Project_Qualifier_Aggregate_Library;
            return Bare_Project_Qualifier_Aggregate_Library (Result);
         end Allocate_Project_Qualifier_Aggregate_Library;

      package Bare_Project_Qualifier_Configuration_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Project_Qualifier_Configuration, Token_Index);

         
         subtype Subtype_For_Project_Qualifier_Configuration is
            Root_Node_Record (Gpr_Project_Qualifier_Configuration);
         type Access_To_Subtype_For_Project_Qualifier_Configuration is access all Subtype_For_Project_Qualifier_Configuration;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Project_Qualifier_Configuration);
         package Bare_Project_Qualifier_Configuration_Alloc is new Alloc
           (Subtype_For_Project_Qualifier_Configuration, Access_To_Subtype_For_Project_Qualifier_Configuration);

         function Allocate_Project_Qualifier_Configuration
           (Pool : Bump_Ptr_Pool) return Bare_Project_Qualifier_Configuration;

         function Allocate_Project_Qualifier_Configuration
           (Pool : Bump_Ptr_Pool) return Bare_Project_Qualifier_Configuration
         is
            Result      : constant Access_To_Subtype_For_Project_Qualifier_Configuration := Bare_Project_Qualifier_Configuration_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Project_Qualifier_Configuration;
            return Bare_Project_Qualifier_Configuration (Result);
         end Allocate_Project_Qualifier_Configuration;

      package Bare_Project_Qualifier_Library_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Project_Qualifier_Library, Token_Index);

         
         subtype Subtype_For_Project_Qualifier_Library is
            Root_Node_Record (Gpr_Project_Qualifier_Library);
         type Access_To_Subtype_For_Project_Qualifier_Library is access all Subtype_For_Project_Qualifier_Library;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Project_Qualifier_Library);
         package Bare_Project_Qualifier_Library_Alloc is new Alloc
           (Subtype_For_Project_Qualifier_Library, Access_To_Subtype_For_Project_Qualifier_Library);

         function Allocate_Project_Qualifier_Library
           (Pool : Bump_Ptr_Pool) return Bare_Project_Qualifier_Library;

         function Allocate_Project_Qualifier_Library
           (Pool : Bump_Ptr_Pool) return Bare_Project_Qualifier_Library
         is
            Result      : constant Access_To_Subtype_For_Project_Qualifier_Library := Bare_Project_Qualifier_Library_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Project_Qualifier_Library;
            return Bare_Project_Qualifier_Library (Result);
         end Allocate_Project_Qualifier_Library;

      package Bare_Project_Qualifier_Standard_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Project_Qualifier_Standard, Token_Index);

         
         subtype Subtype_For_Project_Qualifier_Standard is
            Root_Node_Record (Gpr_Project_Qualifier_Standard);
         type Access_To_Subtype_For_Project_Qualifier_Standard is access all Subtype_For_Project_Qualifier_Standard;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Project_Qualifier_Standard);
         package Bare_Project_Qualifier_Standard_Alloc is new Alloc
           (Subtype_For_Project_Qualifier_Standard, Access_To_Subtype_For_Project_Qualifier_Standard);

         function Allocate_Project_Qualifier_Standard
           (Pool : Bump_Ptr_Pool) return Bare_Project_Qualifier_Standard;

         function Allocate_Project_Qualifier_Standard
           (Pool : Bump_Ptr_Pool) return Bare_Project_Qualifier_Standard
         is
            Result      : constant Access_To_Subtype_For_Project_Qualifier_Standard := Bare_Project_Qualifier_Standard_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Project_Qualifier_Standard;
            return Bare_Project_Qualifier_Standard (Result);
         end Allocate_Project_Qualifier_Standard;

      package Bare_String_Literal_At_Memos is new Gpr_Parser_Support.Packrat
        (Bare_String_Literal_At, Token_Index);

         
         subtype Subtype_For_String_Literal_At is
            Root_Node_Record (Gpr_String_Literal_At);
         type Access_To_Subtype_For_String_Literal_At is access all Subtype_For_String_Literal_At;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_String_Literal_At);
         package Bare_String_Literal_At_Alloc is new Alloc
           (Subtype_For_String_Literal_At, Access_To_Subtype_For_String_Literal_At);

         function Allocate_String_Literal_At
           (Pool : Bump_Ptr_Pool) return Bare_String_Literal_At;

         function Allocate_String_Literal_At
           (Pool : Bump_Ptr_Pool) return Bare_String_Literal_At
         is
            Result      : constant Access_To_Subtype_For_String_Literal_At := Bare_String_Literal_At_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_String_Literal_At;
            return Bare_String_Literal_At (Result);
         end Allocate_String_Literal_At;

      package Bare_Terms_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Terms, Token_Index);

         
         subtype Subtype_For_Terms is
            Root_Node_Record (Gpr_Terms);
         type Access_To_Subtype_For_Terms is access all Subtype_For_Terms;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Terms);
         package Bare_Terms_Alloc is new Alloc
           (Subtype_For_Terms, Access_To_Subtype_For_Terms);

         function Allocate_Terms
           (Pool : Bump_Ptr_Pool) return Bare_Terms;

         function Allocate_Terms
           (Pool : Bump_Ptr_Pool) return Bare_Terms
         is
            Result      : constant Access_To_Subtype_For_Terms := Bare_Terms_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Terms;
            return Bare_Terms (Result);
         end Allocate_Terms;

      package Bare_Type_Reference_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Type_Reference, Token_Index);

         
         subtype Subtype_For_Type_Reference is
            Root_Node_Record (Gpr_Type_Reference);
         type Access_To_Subtype_For_Type_Reference is access all Subtype_For_Type_Reference;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Type_Reference);
         package Bare_Type_Reference_Alloc is new Alloc
           (Subtype_For_Type_Reference, Access_To_Subtype_For_Type_Reference);

         function Allocate_Type_Reference
           (Pool : Bump_Ptr_Pool) return Bare_Type_Reference;

         function Allocate_Type_Reference
           (Pool : Bump_Ptr_Pool) return Bare_Type_Reference
         is
            Result      : constant Access_To_Subtype_For_Type_Reference := Bare_Type_Reference_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Type_Reference;
            return Bare_Type_Reference (Result);
         end Allocate_Type_Reference;

      package Bare_Typed_String_Decl_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Typed_String_Decl, Token_Index);

         
         subtype Subtype_For_Typed_String_Decl is
            Root_Node_Record (Gpr_Typed_String_Decl);
         type Access_To_Subtype_For_Typed_String_Decl is access all Subtype_For_Typed_String_Decl;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Typed_String_Decl);
         package Bare_Typed_String_Decl_Alloc is new Alloc
           (Subtype_For_Typed_String_Decl, Access_To_Subtype_For_Typed_String_Decl);

         function Allocate_Typed_String_Decl
           (Pool : Bump_Ptr_Pool) return Bare_Typed_String_Decl;

         function Allocate_Typed_String_Decl
           (Pool : Bump_Ptr_Pool) return Bare_Typed_String_Decl
         is
            Result      : constant Access_To_Subtype_For_Typed_String_Decl := Bare_Typed_String_Decl_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Typed_String_Decl;
            return Bare_Typed_String_Decl (Result);
         end Allocate_Typed_String_Decl;

      package Bare_Variable_Decl_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Variable_Decl, Token_Index);

         
         subtype Subtype_For_Variable_Decl is
            Root_Node_Record (Gpr_Variable_Decl);
         type Access_To_Subtype_For_Variable_Decl is access all Subtype_For_Variable_Decl;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Variable_Decl);
         package Bare_Variable_Decl_Alloc is new Alloc
           (Subtype_For_Variable_Decl, Access_To_Subtype_For_Variable_Decl);

         function Allocate_Variable_Decl
           (Pool : Bump_Ptr_Pool) return Bare_Variable_Decl;

         function Allocate_Variable_Decl
           (Pool : Bump_Ptr_Pool) return Bare_Variable_Decl
         is
            Result      : constant Access_To_Subtype_For_Variable_Decl := Bare_Variable_Decl_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Variable_Decl;
            return Bare_Variable_Decl (Result);
         end Allocate_Variable_Decl;

      package Bare_Variable_Reference_Memos is new Gpr_Parser_Support.Packrat
        (Bare_Variable_Reference, Token_Index);

         
         subtype Subtype_For_Variable_Reference is
            Root_Node_Record (Gpr_Variable_Reference);
         type Access_To_Subtype_For_Variable_Reference is access all Subtype_For_Variable_Reference;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_Variable_Reference);
         package Bare_Variable_Reference_Alloc is new Alloc
           (Subtype_For_Variable_Reference, Access_To_Subtype_For_Variable_Reference);

         function Allocate_Variable_Reference
           (Pool : Bump_Ptr_Pool) return Bare_Variable_Reference;

         function Allocate_Variable_Reference
           (Pool : Bump_Ptr_Pool) return Bare_Variable_Reference
         is
            Result      : constant Access_To_Subtype_For_Variable_Reference := Bare_Variable_Reference_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_Variable_Reference;
            return Bare_Variable_Reference (Result);
         end Allocate_Variable_Reference;

      package Bare_With_Decl_Memos is new Gpr_Parser_Support.Packrat
        (Bare_With_Decl, Token_Index);

         
         subtype Subtype_For_With_Decl is
            Root_Node_Record (Gpr_With_Decl);
         type Access_To_Subtype_For_With_Decl is access all Subtype_For_With_Decl;
         pragma No_Strict_Aliasing (Access_To_Subtype_For_With_Decl);
         package Bare_With_Decl_Alloc is new Alloc
           (Subtype_For_With_Decl, Access_To_Subtype_For_With_Decl);

         function Allocate_With_Decl
           (Pool : Bump_Ptr_Pool) return Bare_With_Decl;

         function Allocate_With_Decl
           (Pool : Bump_Ptr_Pool) return Bare_With_Decl
         is
            Result      : constant Access_To_Subtype_For_With_Decl := Bare_With_Decl_Alloc.Alloc (Pool);
            Result_Kind : Gpr_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Gpr_With_Decl;
            return Bare_With_Decl (Result);
         end Allocate_With_Decl;

   pragma Warnings (On, "is not referenced");

   type Dontskip_Parser_Function is access function
     (Parser : in out Parser_Type;
      Pos    : Token_Index) return Bare_Gpr_Node;

   package Dont_Skip_Fn_Vectors
   is new Ada.Containers.Vectors (Natural, Dontskip_Parser_Function);

   type Free_Parse_List_Record;
   type Free_Parse_List is access all Free_Parse_List_Record;
   --  Cache of temporary lists of AST nodes used in List parsers

   type Free_Parse_List_Record is record
      Nodes : Bare_Gpr_Node_Vectors.Vector;
      Next  : Free_Parse_List;
   end record;

   type Parser_Private_Part_Type is record
      Parse_Lists : Free_Parse_List;

      
      Ada_Context_Item_Or_Parse0_Memo : Bare_Ada_Context_Clause_Memos.Memo_Type;
      
      Ada_Context_List_Parse0_Memo : Bare_Ada_Context_Clause_List_Memos.Memo_Type;
      
      Ada_Context_Skip_List_Parse0_Memo : Bare_Ada_Skip_List_Memos.Memo_Type;
      
      Ada_Library_Item_Transform_Parse8_Memo : Bare_Ada_Library_Item_Memos.Memo_Type;
      
      Ada_Pkg_Kind_Transform_Parse0_Memo : Bare_Ada_Entity_Kind_Package_Memos.Memo_Type;
      
      Ada_Pragma_Transform_Parse0_Memo : Bare_Ada_Pragma_Memos.Memo_Type;
      
      Ada_Prelude_Transform_Parse0_Memo : Bare_Ada_Prelude_Memos.Memo_Type;
      
      Ada_Subp_Kind_Or_Parse0_Memo : Bare_Ada_Entity_Kind_Memos.Memo_Type;
      
      Ada_Use_Clause_Transform_Parse0_Memo : Bare_Ada_Use_Memos.Memo_Type;
      
      Ada_With_Clause_Transform_Parse0_Memo : Bare_Ada_With_Memos.Memo_Type;
      
      Associative_Array_Index_Or_Parse0_Memo : Bare_Gpr_Node_Memos.Memo_Type;
      
      Attribute_Decl_Transform_Parse0_Memo : Bare_Attribute_Decl_Memos.Memo_Type;
      
      Attribute_Reference_Transform_Parse0_Memo : Bare_Attribute_Reference_Memos.Memo_Type;
      
      Builtin_Function_Call_Transform_Parse0_Memo : Bare_Builtin_Function_Call_Memos.Memo_Type;
      
      Case_Construction_Transform_Parse0_Memo : Bare_Case_Construction_Memos.Memo_Type;
      
      Case_Item_Transform_Parse0_Memo : Bare_Case_Item_Memos.Memo_Type;
      
      Choice_Or_Parse0_Memo : Bare_Gpr_Node_Memos.Memo_Type;
      
      Compilation_Unit_Transform_Parse0_Memo : Bare_Compilation_Unit_Memos.Memo_Type;
      
      Context_Clauses_List_Parse0_Memo : Bare_With_Decl_List_Memos.Memo_Type;
      
      Declarative_Item_Or_Parse0_Memo : Bare_Gpr_Node_Memos.Memo_Type;
      
      Declarative_Items_List_Parse0_Memo : Bare_Gpr_Node_List_Memos.Memo_Type;
      
      Discrete_Choice_List_List_Parse0_Memo : Bare_Choices_Memos.Memo_Type;
      
      Dontskip_Ada_Library_Item0_Extract_Parse0_Memo : Bare_Gpr_Node_Memos.Memo_Type;
      
      Dontskip_Ada_Library_Item1_Extract_Parse0_Memo : Bare_Gpr_Node_Memos.Memo_Type;
      
      Dontskip_Ada_Library_Item2_Extract_Parse0_Memo : Bare_Gpr_Node_Memos.Memo_Type;
      
      Dontskip_Ada_Pragma0_Extract_Parse0_Memo : Bare_Gpr_Node_Memos.Memo_Type;
      
      Dontskip_Ada_Use_Clause0_Extract_Parse0_Memo : Bare_Gpr_Node_Memos.Memo_Type;
      
      Empty_Declaration_Transform_Parse0_Memo : Bare_Empty_Decl_Memos.Memo_Type;
      
      Expression_List_Parse0_Memo : Bare_Term_List_Memos.Memo_Type;
      
      Expression_List_Transform_Parse0_Memo : Bare_Terms_Memos.Memo_Type;
      
      Identifier_Transform_Parse0_Memo : Bare_Identifier_Memos.Memo_Type;
      
      Num_Literal_Transform_Parse0_Memo : Bare_Num_Literal_Memos.Memo_Type;
      
      Others_Designator_Transform_Parse0_Memo : Bare_Others_Designator_Memos.Memo_Type;
      
      Package_Decl_Transform_Parse0_Memo : Bare_Package_Decl_Memos.Memo_Type;
      
      Package_Extension_Transform_Parse0_Memo : Bare_Package_Extension_Memos.Memo_Type;
      
      Package_Renaming_Transform_Parse0_Memo : Bare_Package_Renaming_Memos.Memo_Type;
      
      Package_Spec_Transform_Parse0_Memo : Bare_Package_Spec_Memos.Memo_Type;
      
      Project_Declaration_Transform_Parse0_Memo : Bare_Project_Declaration_Memos.Memo_Type;
      
      Project_Extension_Transform_Parse0_Memo : Bare_Project_Extension_Memos.Memo_Type;
      
      Project_Qualifier_Or_Parse0_Memo : Bare_Project_Qualifier_Memos.Memo_Type;
      
      Project_Transform_Parse0_Memo : Bare_Project_Memos.Memo_Type;
      
      Simple_Declarative_Item_Or_Parse0_Memo : Bare_Gpr_Node_Memos.Memo_Type;
      
      Simple_Declarative_Items_List_Parse0_Memo : Bare_Gpr_Node_List_Memos.Memo_Type;
      
      Static_Name_Or_Parse0_Memo : Bare_Expr_Memos.Memo_Type;
      
      String_Literal_At_Transform_Parse0_Memo : Bare_String_Literal_At_Memos.Memo_Type;
      
      String_Literal_Transform_Parse0_Memo : Bare_String_Literal_Memos.Memo_Type;
      
      Term_Or_Parse0_Memo : Bare_Gpr_Node_Memos.Memo_Type;
      
      Type_Reference_Transform_Parse0_Memo : Bare_Type_Reference_Memos.Memo_Type;
      
      Typed_String_Decl_Transform_Parse0_Memo : Bare_Typed_String_Decl_Memos.Memo_Type;
      
      Variable_Decl_Transform_Parse0_Memo : Bare_Variable_Decl_Memos.Memo_Type;
      
      Variable_Reference_Transform_Parse0_Memo : Bare_Variable_Reference_Memos.Memo_Type;
      
      With_Decl_Transform_Parse0_Memo : Bare_With_Decl_Memos.Memo_Type;

      Dont_Skip : Dont_Skip_Fn_Vectors.Vector;
   end record;

   
function Ada_Context_List_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Context_Clause_List;

   
function Ada_Context_Item_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Context_Clause;

   
function Ada_Context_Skip_List_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Skip_List;

   
function Ada_Library_Item_Transform_Parse8
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Library_Item;

   
function Ada_Pkg_Kind_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Entity_Kind_Package;

   
function Ada_Pragma_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Pragma;

   
function Ada_Prelude_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Prelude;

   
function Ada_Subp_Kind_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Entity_Kind;

   
function Ada_Use_Clause_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Use;

   
function Ada_With_Clause_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_With;

   
function Associative_Array_Index_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node;

   
function Attribute_Decl_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Attribute_Decl;

   
function Attribute_Reference_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Attribute_Reference;

   
function Builtin_Function_Call_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Builtin_Function_Call;

   
function Case_Construction_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Case_Construction;

   
function Case_Item_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Case_Item;

   
function Choice_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node;

   
function Compilation_Unit_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Compilation_Unit;

   
function Context_Clauses_List_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_With_Decl_List;

   
function Declarative_Item_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node;

   
function Declarative_Items_List_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node_List;

   
function Discrete_Choice_List_List_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Choices;

   
function Dontskip_Ada_Library_Item0_Extract_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node;

   
function Dontskip_Ada_Library_Item1_Extract_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node;

   
function Dontskip_Ada_Library_Item2_Extract_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node;

   
function Dontskip_Ada_Pragma0_Extract_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node;

   
function Dontskip_Ada_Use_Clause0_Extract_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node;

   
function Empty_Declaration_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Empty_Decl;

   
function Expression_List_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Term_List;

   
function Expression_List_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Terms;

   
function Identifier_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Identifier;

   
function Num_Literal_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Num_Literal;

   
function Others_Designator_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Others_Designator;

   
function Package_Decl_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Package_Decl;

   
function Package_Extension_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Package_Extension;

   
function Package_Renaming_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Package_Renaming;

   
function Package_Spec_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Package_Spec;

   
function Project_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Project;

   
function Project_Declaration_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Project_Declaration;

   
function Project_Extension_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Project_Extension;

   
function Project_Qualifier_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Project_Qualifier;

   
function Simple_Declarative_Item_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node;

   
function Simple_Declarative_Items_List_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node_List;

   
function Static_Name_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function String_Literal_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_String_Literal;

   
function String_Literal_At_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_String_Literal_At;

   
function Term_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node;

   
function Type_Reference_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Type_Reference;

   
function Typed_String_Decl_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Typed_String_Decl;

   
function Variable_Decl_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Variable_Decl;

   
function Variable_Reference_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Variable_Reference;

   
function With_Decl_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_With_Decl;


   procedure Process_Parsing_Error
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True);
   --  Helper for the user parsing function, to be called after a low-level
   --  parsing function. Check_Complete has the same semantics as in Parse. If
   --  the parsing failed (Parser.Current_Pos = No_Token_Index), append
   --  corresponding diagnostics to Parser.Diagnostics, do nothing instead.

   procedure Add_Last_Fail_Diagnostic (Parser : in out Parser_Type);
   --  Add a diagnostic for the last fail position of the parser

   pragma Warnings (Off, "is not referenced");

   function Get_Parse_List (Parser : Parser_Type) return Free_Parse_List;
   --  Get a free parse list, or allocate one if there is no free parse list in
   --  Parser. When done with the result, the caller must invoke
   --  Release_Parse_List.

   procedure Release_Parse_List
     (Parser : Parser_Type; List : in out Free_Parse_List);
   --  Release a parse list, putting it in Parsers' free list. Set List to
   --  null.

   procedure Initialize_List
     (Self   : Bare_Base_List;
      Parser : Parser_Type;
      Count  : Natural);
   --  Helper for parsers, to initialize the list of children in a freshly
   --  allocated list node.

   pragma Warnings (On, "is not referenced");

   ---------------------
   -- Initialize_List --
   ---------------------

   procedure Initialize_List
     (Self   : Bare_Base_List;
      Parser : Parser_Type;
      Count  : Natural) is
   begin
      Self.Count := Count;
      Self.Nodes := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, 0);
   end Initialize_List;

   -----------------
   -- Init_Parser --
   -----------------

   procedure Init_Parser
     (Input       : Internal_Lexer_Input;
      With_Trivia : Boolean;
      Unit        : access Implementation.Analysis_Unit_Type;
      TDH         : Token_Data_Handler_Access;
      Parser      : in out Parser_Type)
   is
      --  Never try to use file readers for internal units: these are generally
      --  not actual source files, and file readers, which are external users
      --  of the generated library, have no reason to be aware of them.

      FR : constant Internal_File_Reader_Access :=
        (if Unit.Is_Internal
         then null
         else Unit.Context.File_Reader);
   begin
      Reset (Parser);
      Extract_Tokens (Input, With_Trivia, FR, TDH.all, Parser.Diagnostics);
      Parser.Unit := Unit;
      Parser.TDH := TDH;
   end Init_Parser;

   ------------------------------
   -- Add_Last_Fail_Diagnostic --
   ------------------------------

   procedure Add_Last_Fail_Diagnostic (Parser : in out Parser_Type)
   is
      Last_Token : Stored_Token_Data renames
         Get_Token (Parser.TDH.all, Parser.Last_Fail.Pos);
      D : constant Diagnostic :=
        (if Parser.Last_Fail.Kind = Token_Fail then
          Create (Sloc_Range (Parser.TDH.all, Last_Token), To_Text
            ("Expected "
             & Token_Error_Image (Parser.Last_Fail.Expected_Token_Id)
             & ", got "
             & Token_Error_Image (Parser.Last_Fail.Found_Token_Id)))
         else
           Create (Sloc_Range (Parser.TDH.all, Last_Token),
                   To_Text (Parser.Last_Fail.Custom_Message.all)));
   begin
      Parser.Diagnostics.Append (D);
   end Add_Last_Fail_Diagnostic;

   ---------------------------
   -- Process_Parsing_Error --
   ---------------------------

   procedure Process_Parsing_Error
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True) is
   begin

      if Parser.Current_Pos = No_Token_Index then
         Add_Last_Fail_Diagnostic (Parser);
      elsif Check_Complete
        and then Parser.Current_Pos /= Last_Token (Parser.TDH.all)
      then
         --  If the fail pos is the current position of the parser or after,
         --  it means that the longest parse is the correct result, and that we
         --  have some garbage afterwards.
         if Parser.Current_Pos >= Parser.Last_Fail.Pos then
            declare
               First_Garbage_Token : Stored_Token_Data renames
                  Get_Token (Parser.TDH.all, Parser.Current_Pos);
            begin
               Append
                 (Parser.Diagnostics,
                  Sloc_Range (Parser.TDH.all, First_Garbage_Token),
                  To_Text
                    ("End of input expected, got """
                     & Token_Kind_Name
                         (To_Token_Kind (First_Garbage_Token.Kind))
                     & """"));
            end;

         --  Else, the last fail pos is further down the line, and we want to
         --  have the diagnostic of what exactly failed.
         else
            Add_Last_Fail_Diagnostic (Parser);
         end if;
      end if;

   end Process_Parsing_Error;

   -----------
   -- Parse --
   -----------

   function Parse
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True;
      Rule           : Grammar_Rule) return Parsed_Node
   is
      Result : Bare_Gpr_Node;
   begin
      case Rule is
         when Project_Qualifier_Rule =>
            Result := Project_Qualifier_Or_Parse0
              (Parser, First_Token_Index);
         when Project_Extension_Rule =>
            Result := Project_Extension_Transform_Parse0
              (Parser, First_Token_Index);
         when Project_Declaration_Rule =>
            Result := Project_Declaration_Transform_Parse0
              (Parser, First_Token_Index);
         when Project_Rule =>
            Result := Project_Transform_Parse0
              (Parser, First_Token_Index);
         when Declarative_Items_Rule =>
            Result := Declarative_Items_List_Parse0
              (Parser, First_Token_Index);
         when Declarative_Item_Rule =>
            Result := Declarative_Item_Or_Parse0
              (Parser, First_Token_Index);
         when Simple_Declarative_Items_Rule =>
            Result := Simple_Declarative_Items_List_Parse0
              (Parser, First_Token_Index);
         when Simple_Declarative_Item_Rule =>
            Result := Simple_Declarative_Item_Or_Parse0
              (Parser, First_Token_Index);
         when Variable_Decl_Rule =>
            Result := Variable_Decl_Transform_Parse0
              (Parser, First_Token_Index);
         when Attribute_Decl_Rule =>
            Result := Attribute_Decl_Transform_Parse0
              (Parser, First_Token_Index);
         when Associative_Array_Index_Rule =>
            Result := Associative_Array_Index_Or_Parse0
              (Parser, First_Token_Index);
         when Package_Decl_Rule =>
            Result := Package_Decl_Transform_Parse0
              (Parser, First_Token_Index);
         when Package_Renaming_Rule =>
            Result := Package_Renaming_Transform_Parse0
              (Parser, First_Token_Index);
         when Package_Extension_Rule =>
            Result := Package_Extension_Transform_Parse0
              (Parser, First_Token_Index);
         when Package_Spec_Rule =>
            Result := Package_Spec_Transform_Parse0
              (Parser, First_Token_Index);
         when Empty_Declaration_Rule =>
            Result := Empty_Declaration_Transform_Parse0
              (Parser, First_Token_Index);
         when Case_Construction_Rule =>
            Result := Case_Construction_Transform_Parse0
              (Parser, First_Token_Index);
         when Case_Item_Rule =>
            Result := Case_Item_Transform_Parse0
              (Parser, First_Token_Index);
         when Others_Designator_Rule =>
            Result := Others_Designator_Transform_Parse0
              (Parser, First_Token_Index);
         when Choice_Rule =>
            Result := Choice_Or_Parse0
              (Parser, First_Token_Index);
         when Discrete_Choice_List_Rule =>
            Result := Discrete_Choice_List_List_Parse0
              (Parser, First_Token_Index);
         when With_Decl_Rule =>
            Result := With_Decl_Transform_Parse0
              (Parser, First_Token_Index);
         when Context_Clauses_Rule =>
            Result := Context_Clauses_List_Parse0
              (Parser, First_Token_Index);
         when Ada_With_Clause_Rule =>
            Result := Ada_With_Clause_Transform_Parse0
              (Parser, First_Token_Index);
         when Ada_Context_Rule =>
            Result := Ada_Context_List_Parse0
              (Parser, First_Token_Index);
         when Ada_Context_Item_Rule =>
            Result := Ada_Context_Item_Or_Parse0
              (Parser, First_Token_Index);
         when Ada_Context_Skip_Rule =>
            Result := Ada_Context_Skip_List_Parse0
              (Parser, First_Token_Index);
         when Ada_Use_Clause_Rule =>
            Result := Ada_Use_Clause_Transform_Parse0
              (Parser, First_Token_Index);
         when Ada_Pragma_Rule =>
            Result := Ada_Pragma_Transform_Parse0
              (Parser, First_Token_Index);
         when Ada_Subp_Kind_Rule =>
            Result := Ada_Subp_Kind_Or_Parse0
              (Parser, First_Token_Index);
         when Ada_Pkg_Kind_Rule =>
            Result := Ada_Pkg_Kind_Transform_Parse0
              (Parser, First_Token_Index);
         when Ada_Library_Item_Rule =>
            Result := Ada_Library_Item_Transform_Parse8
              (Parser, First_Token_Index);
         when Ada_Prelude_Rule =>
            Result := Ada_Prelude_Transform_Parse0
              (Parser, First_Token_Index);
         when Typed_String_Decl_Rule =>
            Result := Typed_String_Decl_Transform_Parse0
              (Parser, First_Token_Index);
         when Identifier_Rule =>
            Result := Identifier_Transform_Parse0
              (Parser, First_Token_Index);
         when String_Literal_Rule =>
            Result := String_Literal_Transform_Parse0
              (Parser, First_Token_Index);
         when Num_Literal_Rule =>
            Result := Num_Literal_Transform_Parse0
              (Parser, First_Token_Index);
         when Static_Name_Rule =>
            Result := Static_Name_Or_Parse0
              (Parser, First_Token_Index);
         when Attribute_Reference_Rule =>
            Result := Attribute_Reference_Transform_Parse0
              (Parser, First_Token_Index);
         when Variable_Reference_Rule =>
            Result := Variable_Reference_Transform_Parse0
              (Parser, First_Token_Index);
         when Type_Reference_Rule =>
            Result := Type_Reference_Transform_Parse0
              (Parser, First_Token_Index);
         when Builtin_Function_Call_Rule =>
            Result := Builtin_Function_Call_Transform_Parse0
              (Parser, First_Token_Index);
         when Expression_Rule =>
            Result := Expression_List_Parse0
              (Parser, First_Token_Index);
         when Expression_List_Rule =>
            Result := Expression_List_Transform_Parse0
              (Parser, First_Token_Index);
         when String_Literal_At_Rule =>
            Result := String_Literal_At_Transform_Parse0
              (Parser, First_Token_Index);
         when Term_Rule =>
            Result := Term_Or_Parse0
              (Parser, First_Token_Index);
         when Compilation_Unit_Rule =>
            Result := Compilation_Unit_Transform_Parse0
              (Parser, First_Token_Index);
      end case;
      Process_Parsing_Error (Parser, Check_Complete);
      Set_Parents (Result, null);
      return Parsed_Node (Result);
   exception
      when Exc : Property_Error =>
         Append
           (Parser.Diagnostics,
            No_Source_Location_Range,
            To_Text ("Error during parsing: "
                     & Ada.Exceptions.Exception_Message (Exc)));
         return Parsed_Node (No_Bare_Gpr_Node);
   end Parse;

   


function Ada_Context_List_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Context_Clause_List
is
   use Bare_Ada_Context_Clause_List_Memos;

      Lst_Cpos0 :
            Token_Index
               := No_Token_Index;
      Tmp_List0 :
            Free_Parse_List;
      Defer_Pos0 :
            Token_Index
               := No_Token_Index;
      Defer_Res0 :
            Bare_Ada_Context_Clause
               := No_Bare_Gpr_Node;
      List_Pos0 :
            Token_Index
               := No_Token_Index;
      List_Res0 :
            Bare_Ada_Context_Clause_List
               := No_Bare_Gpr_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Ada_Context_List_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res0 := M.Instance;
      return List_Res0;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return List_Res0;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start list_code

    List_Pos0 := Pos;



Lst_Cpos0 := Pos;
Tmp_List0 := Get_Parse_List (Parser);

loop
   
Defer_Res0 :=
   Ada_Context_Item_Or_Parse0 (Parser, Lst_Cpos0);
Defer_Pos0 := Parser.Current_Pos;


   exit when Defer_Pos0 = No_Token_Index;

   List_Pos0 := Defer_Pos0;
   Lst_Cpos0 := List_Pos0;

   Tmp_List0.Nodes.Append (Defer_Res0);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List0.Nodes.Length;
begin
   List_Res0 :=
      Allocate_Ada_Context_Clause_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Pos;
      Token_End := (if Lst_Cpos0 = Pos
                    then Pos
                    else Lst_Cpos0 - 1);

   else
      Token_Start := Token_Index'Max (Pos, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res0,
      Kind              => Gpr_Ada_Context_Clause_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res0,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List0.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res0.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List0);

--  End list_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Ada_Context_List_Parse0_Memo,
      List_Pos0 /= No_Token_Index,
      List_Res0,
      Pos,
      List_Pos0);


   Parser.Current_Pos := List_Pos0;

   return List_Res0;
end Ada_Context_List_Parse0;

   


function Ada_Context_Item_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Context_Clause
is
   use Bare_Ada_Context_Clause_Memos;

      Defer_Pos1 :
            Token_Index
               := No_Token_Index;
      Defer_Res1 :
            Bare_Ada_With
               := No_Bare_Gpr_Node;
      Defer_Pos2 :
            Token_Index
               := No_Token_Index;
      Defer_Res2 :
            Bare_Ada_Use
               := No_Bare_Gpr_Node;
      Defer_Pos3 :
            Token_Index
               := No_Token_Index;
      Defer_Res3 :
            Bare_Ada_Pragma
               := No_Bare_Gpr_Node;
      Or_Pos0 :
            Token_Index
               := No_Token_Index;
      Or_Res0 :
            Bare_Ada_Context_Clause
               := No_Bare_Gpr_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Ada_Context_Item_Or_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res0 := M.Instance;
      return Or_Res0;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Or_Res0;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos0 := No_Token_Index;
Or_Res0 := No_Bare_Gpr_Node;
    
Defer_Res1 :=
   Ada_With_Clause_Transform_Parse0 (Parser, Pos);
Defer_Pos1 := Parser.Current_Pos;

    if Defer_Pos1 /= No_Token_Index then
        Or_Pos0 := Defer_Pos1;
        Or_Res0 := Defer_Res1;
        goto Exit_Or0;
    end if;
    
Defer_Res2 :=
   Ada_Use_Clause_Transform_Parse0 (Parser, Pos);
Defer_Pos2 := Parser.Current_Pos;

    if Defer_Pos2 /= No_Token_Index then
        Or_Pos0 := Defer_Pos2;
        Or_Res0 := Defer_Res2;
        goto Exit_Or0;
    end if;
    
Defer_Res3 :=
   Ada_Pragma_Transform_Parse0 (Parser, Pos);
Defer_Pos3 := Parser.Current_Pos;

    if Defer_Pos3 /= No_Token_Index then
        Or_Pos0 := Defer_Pos3;
        Or_Res0 := Defer_Res3;
        goto Exit_Or0;
    end if;
<<Exit_Or0>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Ada_Context_Item_Or_Parse0_Memo,
      Or_Pos0 /= No_Token_Index,
      Or_Res0,
      Pos,
      Or_Pos0);


   Parser.Current_Pos := Or_Pos0;

   return Or_Res0;
end Ada_Context_Item_Or_Parse0;

   


function Ada_Context_Skip_List_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Skip_List
is
   use Bare_Ada_Skip_List_Memos;

      Lst_Cpos1 :
            Token_Index
               := No_Token_Index;
      Tmp_List1 :
            Free_Parse_List;
      Row_Pos0 :
            Token_Index
               := No_Token_Index;
      Transform_Res0 :
            Bare_Ada_Skip
               := No_Bare_Gpr_Node;
      Transform_Diags0 :
            Ada.Containers.Count_Type;
      Skip_Pos0 :
            Token_Index
               := No_Token_Index;
      Skip_Dummy0 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;
      Or_Pos1 :
            Token_Index
               := No_Token_Index;
      Or_Res1 :
            Bare_Ada_Skip
               := No_Bare_Gpr_Node;
      List_Pos1 :
            Token_Index
               := No_Token_Index;
      List_Res1 :
            Bare_Ada_Skip_List
               := No_Bare_Gpr_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Ada_Context_Skip_List_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res1 := M.Instance;
      return List_Res1;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return List_Res1;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start list_code

    List_Pos1 := No_Token_Index;



Lst_Cpos1 := Pos;
Tmp_List1 := Get_Parse_List (Parser);

loop
   
--  Start or_code

Or_Pos1 := No_Token_Index;
Or_Res1 := No_Bare_Gpr_Node;
    
if Get_Token (Parser.TDH.all, Lst_Cpos1).Kind
   = From_Token_Kind (Gpr_Termination)
then
   Skip_Pos0 := No_Token_Index;
   goto Exit_Or2;
end if;

for Fn of Parser.Private_Part.Dont_Skip loop
   Skip_Dummy0 := Fn (Parser, Lst_Cpos1);

   if Parser.Current_Pos /= No_Token_Index  then
      Skip_Pos0 := No_Token_Index;
      goto Exit_Or2;
   end if;
end loop;

Skip_Pos0 := Lst_Cpos1 + 1;

--  Start transform_code

Transform_Diags0 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos0 := Lst_Cpos1;


pragma Warnings (Off, "referenced");
<<Exit_Row0_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos0 /= No_Token_Index then

   Transform_Res0 := Allocate_Ada_Skip (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res0,
      Kind => Gpr_Ada_Skip,
      Unit => Parser.Unit,

      Token_Start_Index => Lst_Cpos1,
      Token_End_Index   => (if Row_Pos0 = Lst_Cpos1
                            then No_Token_Index
                            else Row_Pos0 - 1));




elsif Row_Pos0 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags0);
end if;

--  End transform_code

Transform_Res0.Token_End_Index := Lst_Cpos1;

Append (Parser.Diagnostics,
        Sloc_Range (Parser.TDH.all,
                    Get_Token (Parser.TDH.all, Lst_Cpos1)),
        To_Text ("Skipped token ")
        & Common.Text
            (Wrap_Token_Reference
               (Parser.Unit.Context,
                Parser.TDH,
                (Lst_Cpos1, No_Token_Index))));

<<Exit_Or2>>

    if Skip_Pos0 /= No_Token_Index then
        Or_Pos1 := Skip_Pos0;
        Or_Res1 := Transform_Res0;
        goto Exit_Or1;
    end if;
<<Exit_Or1>>

--  End or_code


   exit when Or_Pos1 = No_Token_Index;

   List_Pos1 := Or_Pos1;
   Lst_Cpos1 := List_Pos1;

   Tmp_List1.Nodes.Append (Or_Res1);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List1.Nodes.Length;
begin
   List_Res1 :=
      Allocate_Ada_Skip_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Pos;
      Token_End := (if Lst_Cpos1 = Pos
                    then Pos
                    else Lst_Cpos1 - 1);

   else
      Token_Start := Token_Index'Max (Pos, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res1,
      Kind              => Gpr_Ada_Skip_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res1,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List1.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res1.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List1);

--  End list_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Ada_Context_Skip_List_Parse0_Memo,
      List_Pos1 /= No_Token_Index,
      List_Res1,
      Pos,
      List_Pos1);


   Parser.Current_Pos := List_Pos1;

   return List_Res1;
end Ada_Context_Skip_List_Parse0;

   


function Ada_Library_Item_Transform_Parse8
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Library_Item
is
   use Bare_Ada_Library_Item_Memos;

      Row_Pos1 :
            Token_Index
               := No_Token_Index;
      Row_Pos2 :
            Token_Index
               := No_Token_Index;
      Token_Pos0 :
            Token_Index
               := No_Token_Index;
      Token_Res0 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos2 :
            Token_Index
               := No_Token_Index;
      Tmp_List2 :
            Free_Parse_List;
      Row_Pos3 :
            Token_Index
               := No_Token_Index;
      Token_Pos1 :
            Token_Index
               := No_Token_Index;
      Token_Res1 :
            Token_Index
               := No_Token_Index;
      Defer_Pos4 :
            Token_Index
               := No_Token_Index;
      Defer_Res4 :
            Bare_Ada_Entity_Kind
               := No_Bare_Gpr_Node;
      Defer_Pos5 :
            Token_Index
               := No_Token_Index;
      Defer_Res5 :
            Bare_Ada_Entity_Kind_Package
               := No_Bare_Gpr_Node;
      Or_Pos2 :
            Token_Index
               := No_Token_Index;
      Or_Res2 :
            Bare_Ada_Entity_Kind
               := No_Bare_Gpr_Node;
      Defer_Pos6 :
            Token_Index
               := No_Token_Index;
      Defer_Res6 :
            Bare_Ada_Skip_List
               := No_Bare_Gpr_Node;
      Transform_Res1 :
            Bare_Ada_With_Formal
               := No_Bare_Gpr_Node;
      Transform_Diags1 :
            Ada.Containers.Count_Type;
      Row_Pos4 :
            Token_Index
               := No_Token_Index;
      Token_Pos2 :
            Token_Index
               := No_Token_Index;
      Token_Res2 :
            Token_Index
               := No_Token_Index;
      Defer_Pos7 :
            Token_Index
               := No_Token_Index;
      Defer_Res7 :
            Bare_Ada_Entity_Kind
               := No_Bare_Gpr_Node;
      Defer_Pos8 :
            Token_Index
               := No_Token_Index;
      Defer_Res8 :
            Bare_Ada_Skip_List
               := No_Bare_Gpr_Node;
      Transform_Res2 :
            Bare_Ada_Access_Subp
               := No_Bare_Gpr_Node;
      Transform_Diags2 :
            Ada.Containers.Count_Type;
      Row_Pos5 :
            Token_Index
               := No_Token_Index;
      Transform_Res3 :
            Bare_Ada_Skip
               := No_Bare_Gpr_Node;
      Transform_Diags3 :
            Ada.Containers.Count_Type;
      Skip_Pos1 :
            Token_Index
               := No_Token_Index;
      Skip_Dummy1 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;
      Or_Pos3 :
            Token_Index
               := No_Token_Index;
      Or_Res3 :
            Bare_Ada_Prelude_Node
               := No_Bare_Gpr_Node;
      List_Pos2 :
            Token_Index
               := No_Token_Index;
      List_Res2 :
            Bare_Ada_Prelude_Node_List
               := No_Bare_Gpr_Node;
      Transform_Res4 :
            Bare_Ada_Generic
               := No_Bare_Gpr_Node;
      Transform_Diags4 :
            Ada.Containers.Count_Type;
      Row_Pos6 :
            Token_Index
               := No_Token_Index;
      Token_Pos3 :
            Token_Index
               := No_Token_Index;
      Token_Res3 :
            Token_Index
               := No_Token_Index;
      Token_Pos4 :
            Token_Index
               := No_Token_Index;
      Token_Res4 :
            Token_Index
               := No_Token_Index;
      Defer_Pos9 :
            Token_Index
               := No_Token_Index;
      Defer_Res9 :
            Bare_Expr
               := No_Bare_Gpr_Node;
      Token_Pos5 :
            Token_Index
               := No_Token_Index;
      Token_Res5 :
            Token_Index
               := No_Token_Index;
      Transform_Res5 :
            Bare_Ada_Separate
               := No_Bare_Gpr_Node;
      Transform_Diags5 :
            Ada.Containers.Count_Type;
      Row_Pos7 :
            Token_Index
               := No_Token_Index;
      Defer_Pos10 :
            Token_Index
               := No_Token_Index;
      Defer_Res10 :
            Bare_Ada_Entity_Kind
               := No_Bare_Gpr_Node;
      Defer_Pos11 :
            Token_Index
               := No_Token_Index;
      Defer_Res11 :
            Bare_Expr
               := No_Bare_Gpr_Node;
      Transform_Res6 :
            Bare_Ada_Subp
               := No_Bare_Gpr_Node;
      Transform_Diags6 :
            Ada.Containers.Count_Type;
      Row_Pos8 :
            Token_Index
               := No_Token_Index;
      Token_Pos6 :
            Token_Index
               := No_Token_Index;
      Token_Res6 :
            Token_Index
               := No_Token_Index;
      Token_Pos7 :
            Token_Index
               := No_Token_Index;
      Token_Res7 :
            Token_Index
               := No_Token_Index;
      Defer_Pos12 :
            Token_Index
               := No_Token_Index;
      Defer_Res12 :
            Bare_Expr
               := No_Bare_Gpr_Node;
      Transform_Res7 :
            Bare_Ada_Pkg_Body
               := No_Bare_Gpr_Node;
      Transform_Diags7 :
            Ada.Containers.Count_Type;
      Row_Pos9 :
            Token_Index
               := No_Token_Index;
      Token_Pos8 :
            Token_Index
               := No_Token_Index;
      Token_Res8 :
            Token_Index
               := No_Token_Index;
      Opt_Res0 :
            Bare_Private_Node
               := No_Bare_Gpr_Node;
      Token_Pos9 :
            Token_Index
               := No_Token_Index;
      Token_Res9 :
            Token_Index
               := No_Token_Index;
      Defer_Pos13 :
            Token_Index
               := No_Token_Index;
      Defer_Res13 :
            Bare_Expr
               := No_Bare_Gpr_Node;
      Transform_Res8 :
            Bare_Ada_Pkg
               := No_Bare_Gpr_Node;
      Transform_Diags8 :
            Ada.Containers.Count_Type;
      Or_Pos4 :
            Token_Index
               := No_Token_Index;
      Or_Res4 :
            Bare_Ada_Main
               := No_Bare_Gpr_Node;
      Transform_Res9 :
            Bare_Ada_Library_Item
               := No_Bare_Gpr_Node;
      Transform_Diags9 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Ada_Library_Item_Transform_Parse8_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res9 := M.Instance;
      return Transform_Res9;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res9;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags9 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos1 := Pos;



--  Start opt_code












--  Start transform_code

Transform_Diags4 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos2 := Row_Pos1;



--  Start tok_code

Token_Res0 := Row_Pos2;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res0));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
      or else T.Symbol /= Precomputed_Symbol
        (Precomputed_Symbol_Table (Parser.TDH.Symbols),
         Precomputed_Sym_Generic)
   then
       Token_Pos0 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos2 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos2,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos0 := Row_Pos2 + 1;
   end if;
end;

--  End tok_code




if Token_Pos0 /= No_Token_Index then

   Row_Pos2 := Token_Pos0;

else
   Row_Pos2 := No_Token_Index;
   goto Exit_Row2_0;

end if;


        Parser.Private_Part.Dont_Skip.Append
          (Dontskip_Ada_Library_Item0_Extract_Parse0'Access);
        
--  Start list_code

    List_Pos2 := Row_Pos2;



Lst_Cpos2 := Row_Pos2;
Tmp_List2 := Get_Parse_List (Parser);

loop
   
--  Start or_code

Or_Pos3 := No_Token_Index;
Or_Res3 := No_Bare_Gpr_Node;
    
--  Start transform_code

Transform_Diags1 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos3 := Lst_Cpos2;



--  Start tok_code

Token_Res1 := Row_Pos3;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res1));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_With)
   then
       Token_Pos1 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos3 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos3,
             Expected_Token_Id => Gpr_With,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos1 := Row_Pos3 + 1;
   end if;
end;

--  End tok_code




if Token_Pos1 /= No_Token_Index then

   Row_Pos3 := Token_Pos1;

else
   Row_Pos3 := No_Token_Index;
   goto Exit_Row3_0;

end if;


--  Start or_code

Or_Pos2 := No_Token_Index;
Or_Res2 := No_Bare_Gpr_Node;
    
Defer_Res4 :=
   Ada_Subp_Kind_Or_Parse0 (Parser, Row_Pos3);
Defer_Pos4 := Parser.Current_Pos;

    if Defer_Pos4 /= No_Token_Index then
        Or_Pos2 := Defer_Pos4;
        Or_Res2 := Defer_Res4;
        goto Exit_Or4;
    end if;
    
Defer_Res5 :=
   Ada_Pkg_Kind_Transform_Parse0 (Parser, Row_Pos3);
Defer_Pos5 := Parser.Current_Pos;

    if Defer_Pos5 /= No_Token_Index then
        Or_Pos2 := Defer_Pos5;
        Or_Res2 := Defer_Res5;
        goto Exit_Or4;
    end if;
<<Exit_Or4>>

--  End or_code




if Or_Pos2 /= No_Token_Index then

   Row_Pos3 := Or_Pos2;

else
   Row_Pos3 := No_Token_Index;
   goto Exit_Row3_0;

end if;


        Parser.Private_Part.Dont_Skip.Append
          (Dontskip_Ada_Library_Item1_Extract_Parse0'Access);
        
Defer_Res6 :=
   Ada_Context_Skip_List_Parse0 (Parser, Row_Pos3);
Defer_Pos6 := Parser.Current_Pos;

        Parser.Private_Part.Dont_Skip.Delete_Last;
        



if Defer_Pos6 /= No_Token_Index then

   Row_Pos3 := Defer_Pos6;

else
   Row_Pos3 := No_Token_Index;
   goto Exit_Row3_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row3_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos3 /= No_Token_Index then

   Transform_Res1 := Allocate_Ada_With_Formal (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res1,
      Kind => Gpr_Ada_With_Formal,
      Unit => Parser.Unit,

      Token_Start_Index => Lst_Cpos2,
      Token_End_Index   => (if Row_Pos3 = Lst_Cpos2
                            then No_Token_Index
                            else Row_Pos3 - 1));

      Initialize_Fields_For_Ada_With_Formal
        (Self => Transform_Res1, Ada_With_Formal_F_Kind => Or_Res2, Ada_With_Formal_F_Skips => Defer_Res6);

         if Or_Res2 /= null and then Is_Incomplete (Or_Res2) then
            Transform_Res1.Last_Attempted_Child := 0;
         elsif Or_Res2 /= null and then not Is_Ghost (Or_Res2) then
            Transform_Res1.Last_Attempted_Child := -1;
         end if;
         if Defer_Res6 /= null and then Is_Incomplete (Defer_Res6) then
            Transform_Res1.Last_Attempted_Child := 0;
         elsif Defer_Res6 /= null and then not Is_Ghost (Defer_Res6) then
            Transform_Res1.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos3 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags1);
end if;

--  End transform_code

    if Row_Pos3 /= No_Token_Index then
        Or_Pos3 := Row_Pos3;
        Or_Res3 := Transform_Res1;
        goto Exit_Or3;
    end if;
    
--  Start transform_code

Transform_Diags2 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos4 := Lst_Cpos2;



--  Start tok_code

Token_Res2 := Row_Pos4;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res2));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
      or else T.Symbol /= Precomputed_Symbol
        (Precomputed_Symbol_Table (Parser.TDH.Symbols),
         Precomputed_Sym_Access)
   then
       Token_Pos2 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos4 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos4,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos2 := Row_Pos4 + 1;
   end if;
end;

--  End tok_code




if Token_Pos2 /= No_Token_Index then

   Row_Pos4 := Token_Pos2;

else
   Row_Pos4 := No_Token_Index;
   goto Exit_Row4_0;

end if;


Defer_Res7 :=
   Ada_Subp_Kind_Or_Parse0 (Parser, Row_Pos4);
Defer_Pos7 := Parser.Current_Pos;




if Defer_Pos7 /= No_Token_Index then

   Row_Pos4 := Defer_Pos7;

else
   Row_Pos4 := No_Token_Index;
   goto Exit_Row4_0;

end if;


        Parser.Private_Part.Dont_Skip.Append
          (Dontskip_Ada_Library_Item2_Extract_Parse0'Access);
        
Defer_Res8 :=
   Ada_Context_Skip_List_Parse0 (Parser, Row_Pos4);
Defer_Pos8 := Parser.Current_Pos;

        Parser.Private_Part.Dont_Skip.Delete_Last;
        



if Defer_Pos8 /= No_Token_Index then

   Row_Pos4 := Defer_Pos8;

else
   Row_Pos4 := No_Token_Index;
   goto Exit_Row4_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row4_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos4 /= No_Token_Index then

   Transform_Res2 := Allocate_Ada_Access_Subp (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res2,
      Kind => Gpr_Ada_Access_Subp,
      Unit => Parser.Unit,

      Token_Start_Index => Lst_Cpos2,
      Token_End_Index   => (if Row_Pos4 = Lst_Cpos2
                            then No_Token_Index
                            else Row_Pos4 - 1));

      Initialize_Fields_For_Ada_Access_Subp
        (Self => Transform_Res2, Ada_Access_Subp_F_Subp_Kind => Defer_Res7, Ada_Access_Subp_F_Skips => Defer_Res8);

         if Defer_Res7 /= null and then Is_Incomplete (Defer_Res7) then
            Transform_Res2.Last_Attempted_Child := 0;
         elsif Defer_Res7 /= null and then not Is_Ghost (Defer_Res7) then
            Transform_Res2.Last_Attempted_Child := -1;
         end if;
         if Defer_Res8 /= null and then Is_Incomplete (Defer_Res8) then
            Transform_Res2.Last_Attempted_Child := 0;
         elsif Defer_Res8 /= null and then not Is_Ghost (Defer_Res8) then
            Transform_Res2.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos4 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags2);
end if;

--  End transform_code

    if Row_Pos4 /= No_Token_Index then
        Or_Pos3 := Row_Pos4;
        Or_Res3 := Transform_Res2;
        goto Exit_Or3;
    end if;
    
if Get_Token (Parser.TDH.all, Lst_Cpos2).Kind
   = From_Token_Kind (Gpr_Termination)
then
   Skip_Pos1 := No_Token_Index;
   goto Exit_Or5;
end if;

for Fn of Parser.Private_Part.Dont_Skip loop
   Skip_Dummy1 := Fn (Parser, Lst_Cpos2);

   if Parser.Current_Pos /= No_Token_Index  then
      Skip_Pos1 := No_Token_Index;
      goto Exit_Or5;
   end if;
end loop;

Skip_Pos1 := Lst_Cpos2 + 1;

--  Start transform_code

Transform_Diags3 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos5 := Lst_Cpos2;


pragma Warnings (Off, "referenced");
<<Exit_Row5_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos5 /= No_Token_Index then

   Transform_Res3 := Allocate_Ada_Skip (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res3,
      Kind => Gpr_Ada_Skip,
      Unit => Parser.Unit,

      Token_Start_Index => Lst_Cpos2,
      Token_End_Index   => (if Row_Pos5 = Lst_Cpos2
                            then No_Token_Index
                            else Row_Pos5 - 1));




elsif Row_Pos5 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags3);
end if;

--  End transform_code

Transform_Res3.Token_End_Index := Lst_Cpos2;

Append (Parser.Diagnostics,
        Sloc_Range (Parser.TDH.all,
                    Get_Token (Parser.TDH.all, Lst_Cpos2)),
        To_Text ("Skipped token ")
        & Common.Text
            (Wrap_Token_Reference
               (Parser.Unit.Context,
                Parser.TDH,
                (Lst_Cpos2, No_Token_Index))));

<<Exit_Or5>>

    if Skip_Pos1 /= No_Token_Index then
        Or_Pos3 := Skip_Pos1;
        Or_Res3 := Transform_Res3;
        goto Exit_Or3;
    end if;
<<Exit_Or3>>

--  End or_code


   exit when Or_Pos3 = No_Token_Index;

   List_Pos2 := Or_Pos3;
   Lst_Cpos2 := List_Pos2;

   Tmp_List2.Nodes.Append (Or_Res3);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List2.Nodes.Length;
begin
   List_Res2 :=
      Allocate_Ada_Prelude_Node_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos2;
      Token_End := (if Lst_Cpos2 = Row_Pos2
                    then Row_Pos2
                    else Lst_Cpos2 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos2, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res2,
      Kind              => Gpr_Ada_Prelude_Node_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res2,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List2.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res2.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List2);

--  End list_code

        Parser.Private_Part.Dont_Skip.Delete_Last;
        



if List_Pos2 /= No_Token_Index then

   Row_Pos2 := List_Pos2;

else
   Row_Pos2 := No_Token_Index;
   goto Exit_Row2_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row2_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos2 /= No_Token_Index then

   Transform_Res4 := Allocate_Ada_Generic (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res4,
      Kind => Gpr_Ada_Generic,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos1,
      Token_End_Index   => (if Row_Pos2 = Row_Pos1
                            then No_Token_Index
                            else Row_Pos2 - 1));

      Initialize_Fields_For_Ada_Generic
        (Self => Transform_Res4, Ada_Generic_F_Skips => List_Res2);

         if List_Res2 /= null and then Is_Incomplete (List_Res2) then
            Transform_Res4.Last_Attempted_Child := 0;
         elsif List_Res2 /= null and then not Is_Ghost (List_Res2) then
            Transform_Res4.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos2 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags4);
end if;

--  End transform_code


if Row_Pos2 = No_Token_Index then

         
   Transform_Res4 := No_Bare_Gpr_Node;



       
   Row_Pos2 := Row_Pos1;



end if;

--  End opt_code




if Row_Pos2 /= No_Token_Index then

   Row_Pos1 := Row_Pos2;

else
   Row_Pos1 := No_Token_Index;
   goto Exit_Row1_0;

end if;


--  Start opt_code












--  Start transform_code

Transform_Diags5 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos6 := Row_Pos1;



--  Start tok_code

Token_Res3 := Row_Pos6;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res3));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
      or else T.Symbol /= Precomputed_Symbol
        (Precomputed_Symbol_Table (Parser.TDH.Symbols),
         Precomputed_Sym_Separate)
   then
       Token_Pos3 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos6 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos6,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos3 := Row_Pos6 + 1;
   end if;
end;

--  End tok_code




if Token_Pos3 /= No_Token_Index then

   Row_Pos6 := Token_Pos3;

else
   Row_Pos6 := No_Token_Index;
   goto Exit_Row6_0;

end if;


--  Start tok_code

Token_Res4 := Row_Pos6;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res4));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Par_Open)
   then
       Token_Pos4 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos6 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos6,
             Expected_Token_Id => Gpr_Par_Open,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos4 := Row_Pos6 + 1;
   end if;
end;

--  End tok_code




if Token_Pos4 /= No_Token_Index then

   Row_Pos6 := Token_Pos4;

else
   Row_Pos6 := No_Token_Index;
   goto Exit_Row6_0;

end if;


Defer_Res9 :=
   Static_Name_Or_Parse0 (Parser, Row_Pos6);
Defer_Pos9 := Parser.Current_Pos;




if Defer_Pos9 /= No_Token_Index then

   Row_Pos6 := Defer_Pos9;

else
   Row_Pos6 := No_Token_Index;
   goto Exit_Row6_0;

end if;


--  Start tok_code

Token_Res5 := Row_Pos6;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res5));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Par_Close)
   then
       Token_Pos5 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos6 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos6,
             Expected_Token_Id => Gpr_Par_Close,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos5 := Row_Pos6 + 1;
   end if;
end;

--  End tok_code




if Token_Pos5 /= No_Token_Index then

   Row_Pos6 := Token_Pos5;

else
   Row_Pos6 := No_Token_Index;
   goto Exit_Row6_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row6_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos6 /= No_Token_Index then

   Transform_Res5 := Allocate_Ada_Separate (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res5,
      Kind => Gpr_Ada_Separate,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos1,
      Token_End_Index   => (if Row_Pos6 = Row_Pos1
                            then No_Token_Index
                            else Row_Pos6 - 1));

      Initialize_Fields_For_Ada_Separate
        (Self => Transform_Res5, Ada_Separate_F_Parent_Name => Defer_Res9);

         if Defer_Res9 /= null and then Is_Incomplete (Defer_Res9) then
            Transform_Res5.Last_Attempted_Child := 0;
         elsif Defer_Res9 /= null and then not Is_Ghost (Defer_Res9) then
            Transform_Res5.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos6 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags5);
end if;

--  End transform_code


if Row_Pos6 = No_Token_Index then

         
   Transform_Res5 := No_Bare_Gpr_Node;



       
   Row_Pos6 := Row_Pos1;



end if;

--  End opt_code




if Row_Pos6 /= No_Token_Index then

   Row_Pos1 := Row_Pos6;

else
   Row_Pos1 := No_Token_Index;
   goto Exit_Row1_0;

end if;


--  Start or_code

Or_Pos4 := No_Token_Index;
Or_Res4 := No_Bare_Gpr_Node;
    
--  Start transform_code

Transform_Diags6 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos7 := Row_Pos1;



Defer_Res10 :=
   Ada_Subp_Kind_Or_Parse0 (Parser, Row_Pos7);
Defer_Pos10 := Parser.Current_Pos;




if Defer_Pos10 /= No_Token_Index then

   Row_Pos7 := Defer_Pos10;

else
   Row_Pos7 := No_Token_Index;
   goto Exit_Row7_0;

end if;


Defer_Res11 :=
   Static_Name_Or_Parse0 (Parser, Row_Pos7);
Defer_Pos11 := Parser.Current_Pos;




if Defer_Pos11 /= No_Token_Index then

   Row_Pos7 := Defer_Pos11;

else
   Row_Pos7 := No_Token_Index;
   goto Exit_Row7_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row7_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos7 /= No_Token_Index then

   Transform_Res6 := Allocate_Ada_Subp (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res6,
      Kind => Gpr_Ada_Subp,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos1,
      Token_End_Index   => (if Row_Pos7 = Row_Pos1
                            then No_Token_Index
                            else Row_Pos7 - 1));

      Initialize_Fields_For_Ada_Subp
        (Self => Transform_Res6, Ada_Subp_F_Subp_Kind => Defer_Res10, Ada_Subp_F_Name => Defer_Res11);

         if Defer_Res10 /= null and then Is_Incomplete (Defer_Res10) then
            Transform_Res6.Last_Attempted_Child := 0;
         elsif Defer_Res10 /= null and then not Is_Ghost (Defer_Res10) then
            Transform_Res6.Last_Attempted_Child := -1;
         end if;
         if Defer_Res11 /= null and then Is_Incomplete (Defer_Res11) then
            Transform_Res6.Last_Attempted_Child := 0;
         elsif Defer_Res11 /= null and then not Is_Ghost (Defer_Res11) then
            Transform_Res6.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos7 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags6);
end if;

--  End transform_code

    if Row_Pos7 /= No_Token_Index then
        Or_Pos4 := Row_Pos7;
        Or_Res4 := Transform_Res6;
        goto Exit_Or6;
    end if;
    
--  Start transform_code

Transform_Diags7 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos8 := Row_Pos1;



--  Start tok_code

Token_Res6 := Row_Pos8;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res6));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Package)
   then
       Token_Pos6 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos8 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos8,
             Expected_Token_Id => Gpr_Package,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos6 := Row_Pos8 + 1;
   end if;
end;

--  End tok_code




if Token_Pos6 /= No_Token_Index then

   Row_Pos8 := Token_Pos6;

else
   Row_Pos8 := No_Token_Index;
   goto Exit_Row8_0;

end if;


--  Start tok_code

Token_Res7 := Row_Pos8;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res7));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
      or else T.Symbol /= Precomputed_Symbol
        (Precomputed_Symbol_Table (Parser.TDH.Symbols),
         Precomputed_Sym_Body)
   then
       Token_Pos7 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos8 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos8,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos7 := Row_Pos8 + 1;
   end if;
end;

--  End tok_code




if Token_Pos7 /= No_Token_Index then

   Row_Pos8 := Token_Pos7;

else
   Row_Pos8 := No_Token_Index;
   goto Exit_Row8_0;

end if;


Defer_Res12 :=
   Static_Name_Or_Parse0 (Parser, Row_Pos8);
Defer_Pos12 := Parser.Current_Pos;




if Defer_Pos12 /= No_Token_Index then

   Row_Pos8 := Defer_Pos12;

else
   Row_Pos8 := No_Token_Index;
   goto Exit_Row8_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row8_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos8 /= No_Token_Index then

   Transform_Res7 := Allocate_Ada_Pkg_Body (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res7,
      Kind => Gpr_Ada_Pkg_Body,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos1,
      Token_End_Index   => (if Row_Pos8 = Row_Pos1
                            then No_Token_Index
                            else Row_Pos8 - 1));

      Initialize_Fields_For_Ada_Pkg_Body
        (Self => Transform_Res7, Ada_Pkg_Body_F_Name => Defer_Res12);

         if Defer_Res12 /= null and then Is_Incomplete (Defer_Res12) then
            Transform_Res7.Last_Attempted_Child := 0;
         elsif Defer_Res12 /= null and then not Is_Ghost (Defer_Res12) then
            Transform_Res7.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos8 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags7);
end if;

--  End transform_code

    if Row_Pos8 /= No_Token_Index then
        Or_Pos4 := Row_Pos8;
        Or_Res4 := Transform_Res7;
        goto Exit_Or6;
    end if;
    
--  Start transform_code

Transform_Diags8 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos9 := Row_Pos1;



--  Start opt_code












--  Start tok_code

Token_Res8 := Row_Pos9;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res8));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Private)
   then
       Token_Pos8 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos9 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos9,
             Expected_Token_Id => Gpr_Private,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos8 := Row_Pos9 + 1;
   end if;
end;

--  End tok_code


if Token_Pos8 = No_Token_Index then

         Opt_Res0 := Allocate_Private_Absent (Parser.Mem_Pool);
         Initialize
           (Self              => Opt_Res0,
            Kind              => Gpr_Private_Absent,
            Unit              => Parser.Unit,
            Token_Start_Index => Row_Pos9,
            Token_End_Index   => No_Token_Index);


       
   Token_Pos8 := Row_Pos9;


else

      Opt_Res0 := Allocate_Private_Present (Parser.Mem_Pool);
      Initialize
        (Self              => Opt_Res0,
         Kind              => Gpr_Private_Present,
         Unit              => Parser.Unit,
         Token_Start_Index => Row_Pos9,
         Token_End_Index   => Token_Pos8 - 1);

end if;

--  End opt_code




if Token_Pos8 /= No_Token_Index then

   Row_Pos9 := Token_Pos8;

else
   Row_Pos9 := No_Token_Index;
   goto Exit_Row9_0;

end if;


--  Start tok_code

Token_Res9 := Row_Pos9;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res9));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Package)
   then
       Token_Pos9 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos9 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos9,
             Expected_Token_Id => Gpr_Package,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos9 := Row_Pos9 + 1;
   end if;
end;

--  End tok_code




if Token_Pos9 /= No_Token_Index then

   Row_Pos9 := Token_Pos9;

else
   Row_Pos9 := No_Token_Index;
   goto Exit_Row9_0;

end if;


Defer_Res13 :=
   Static_Name_Or_Parse0 (Parser, Row_Pos9);
Defer_Pos13 := Parser.Current_Pos;




if Defer_Pos13 /= No_Token_Index then

   Row_Pos9 := Defer_Pos13;

else
   Row_Pos9 := No_Token_Index;
   goto Exit_Row9_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row9_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos9 /= No_Token_Index then

   Transform_Res8 := Allocate_Ada_Pkg (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res8,
      Kind => Gpr_Ada_Pkg,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos1,
      Token_End_Index   => (if Row_Pos9 = Row_Pos1
                            then No_Token_Index
                            else Row_Pos9 - 1));

      Initialize_Fields_For_Ada_Pkg
        (Self => Transform_Res8, Ada_Pkg_F_Has_Private => Opt_Res0, Ada_Pkg_F_Name => Defer_Res13);

         if Opt_Res0 /= null and then Is_Incomplete (Opt_Res0) then
            Transform_Res8.Last_Attempted_Child := 0;
         elsif Opt_Res0 /= null and then not Is_Ghost (Opt_Res0) then
            Transform_Res8.Last_Attempted_Child := -1;
         end if;
         if Defer_Res13 /= null and then Is_Incomplete (Defer_Res13) then
            Transform_Res8.Last_Attempted_Child := 0;
         elsif Defer_Res13 /= null and then not Is_Ghost (Defer_Res13) then
            Transform_Res8.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos9 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags8);
end if;

--  End transform_code

    if Row_Pos9 /= No_Token_Index then
        Or_Pos4 := Row_Pos9;
        Or_Res4 := Transform_Res8;
        goto Exit_Or6;
    end if;
<<Exit_Or6>>

--  End or_code




if Or_Pos4 /= No_Token_Index then

   Row_Pos1 := Or_Pos4;

else
   Row_Pos1 := No_Token_Index;
   goto Exit_Row1_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row1_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos1 /= No_Token_Index then

   Transform_Res9 := Allocate_Ada_Library_Item (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res9,
      Kind => Gpr_Ada_Library_Item,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos1 = Pos
                            then No_Token_Index
                            else Row_Pos1 - 1));

      Initialize_Fields_For_Ada_Library_Item
        (Self => Transform_Res9, Ada_Library_Item_F_Generic_Stub => Transform_Res4, Ada_Library_Item_F_Separate => Transform_Res5, Ada_Library_Item_F_Main => Or_Res4);

         if Transform_Res4 /= null and then Is_Incomplete (Transform_Res4) then
            Transform_Res9.Last_Attempted_Child := 0;
         elsif Transform_Res4 /= null and then not Is_Ghost (Transform_Res4) then
            Transform_Res9.Last_Attempted_Child := -1;
         end if;
         if Transform_Res5 /= null and then Is_Incomplete (Transform_Res5) then
            Transform_Res9.Last_Attempted_Child := 0;
         elsif Transform_Res5 /= null and then not Is_Ghost (Transform_Res5) then
            Transform_Res9.Last_Attempted_Child := -1;
         end if;
         if Or_Res4 /= null and then Is_Incomplete (Or_Res4) then
            Transform_Res9.Last_Attempted_Child := 0;
         elsif Or_Res4 /= null and then not Is_Ghost (Or_Res4) then
            Transform_Res9.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos1 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags9);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Ada_Library_Item_Transform_Parse8_Memo,
      Row_Pos1 /= No_Token_Index,
      Transform_Res9,
      Pos,
      Row_Pos1);


   Parser.Current_Pos := Row_Pos1;

   return Transform_Res9;
end Ada_Library_Item_Transform_Parse8;

   


function Ada_Pkg_Kind_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Entity_Kind_Package
is
   use Bare_Ada_Entity_Kind_Package_Memos;

      Row_Pos10 :
            Token_Index
               := No_Token_Index;
      Token_Pos10 :
            Token_Index
               := No_Token_Index;
      Token_Res10 :
            Token_Index
               := No_Token_Index;
      Transform_Res10 :
            Bare_Ada_Entity_Kind_Package
               := No_Bare_Gpr_Node;
      Transform_Diags10 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Ada_Pkg_Kind_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res10 := M.Instance;
      return Transform_Res10;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res10;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags10 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos10 := Pos;



--  Start tok_code

Token_Res10 := Row_Pos10;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res10));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Package)
   then
       Token_Pos10 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos10 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos10,
             Expected_Token_Id => Gpr_Package,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos10 := Row_Pos10 + 1;
   end if;
end;

--  End tok_code




if Token_Pos10 /= No_Token_Index then

   Row_Pos10 := Token_Pos10;

else
   Row_Pos10 := No_Token_Index;
   goto Exit_Row10_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row10_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos10 /= No_Token_Index then

   Transform_Res10 := Allocate_Ada_Entity_Kind_Package (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res10,
      Kind => Gpr_Ada_Entity_Kind_Package,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos10 = Pos
                            then No_Token_Index
                            else Row_Pos10 - 1));




elsif Row_Pos10 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags10);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Ada_Pkg_Kind_Transform_Parse0_Memo,
      Row_Pos10 /= No_Token_Index,
      Transform_Res10,
      Pos,
      Row_Pos10);


   Parser.Current_Pos := Row_Pos10;

   return Transform_Res10;
end Ada_Pkg_Kind_Transform_Parse0;

   


function Ada_Pragma_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Pragma
is
   use Bare_Ada_Pragma_Memos;

      Row_Pos11 :
            Token_Index
               := No_Token_Index;
      Token_Pos11 :
            Token_Index
               := No_Token_Index;
      Token_Res11 :
            Token_Index
               := No_Token_Index;
      Defer_Pos14 :
            Token_Index
               := No_Token_Index;
      Defer_Res14 :
            Bare_Ada_Skip_List
               := No_Bare_Gpr_Node;
      Token_Pos12 :
            Token_Index
               := No_Token_Index;
      Token_Res12 :
            Token_Index
               := No_Token_Index;
      Transform_Res11 :
            Bare_Ada_Pragma
               := No_Bare_Gpr_Node;
      Transform_Diags11 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Ada_Pragma_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res11 := M.Instance;
      return Transform_Res11;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res11;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags11 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos11 := Pos;



--  Start tok_code

Token_Res11 := Row_Pos11;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res11));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Pragma)
   then
       Token_Pos11 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos11 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos11,
             Expected_Token_Id => Gpr_Pragma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos11 := Row_Pos11 + 1;
   end if;
end;

--  End tok_code




if Token_Pos11 /= No_Token_Index then

   Row_Pos11 := Token_Pos11;

else
   Row_Pos11 := No_Token_Index;
   goto Exit_Row11_0;

end if;


        Parser.Private_Part.Dont_Skip.Append
          (Dontskip_Ada_Pragma0_Extract_Parse0'Access);
        
Defer_Res14 :=
   Ada_Context_Skip_List_Parse0 (Parser, Row_Pos11);
Defer_Pos14 := Parser.Current_Pos;

        Parser.Private_Part.Dont_Skip.Delete_Last;
        



if Defer_Pos14 /= No_Token_Index then

   Row_Pos11 := Defer_Pos14;

else
   Row_Pos11 := No_Token_Index;
   goto Exit_Row11_0;

end if;


--  Start tok_code

Token_Res12 := Row_Pos11;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res12));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Semicolon)
   then
       Token_Pos12 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos11 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos11,
             Expected_Token_Id => Gpr_Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos12 := Row_Pos11 + 1;
   end if;
end;

--  End tok_code




if Token_Pos12 /= No_Token_Index then

   Row_Pos11 := Token_Pos12;

else
   Row_Pos11 := No_Token_Index;
   goto Exit_Row11_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row11_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos11 /= No_Token_Index then

   Transform_Res11 := Allocate_Ada_Pragma (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res11,
      Kind => Gpr_Ada_Pragma,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos11 = Pos
                            then No_Token_Index
                            else Row_Pos11 - 1));

      Initialize_Fields_For_Ada_Pragma
        (Self => Transform_Res11, Ada_Pragma_F_Skips => Defer_Res14);

         if Defer_Res14 /= null and then Is_Incomplete (Defer_Res14) then
            Transform_Res11.Last_Attempted_Child := 0;
         elsif Defer_Res14 /= null and then not Is_Ghost (Defer_Res14) then
            Transform_Res11.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos11 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags11);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Ada_Pragma_Transform_Parse0_Memo,
      Row_Pos11 /= No_Token_Index,
      Transform_Res11,
      Pos,
      Row_Pos11);


   Parser.Current_Pos := Row_Pos11;

   return Transform_Res11;
end Ada_Pragma_Transform_Parse0;

   


function Ada_Prelude_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Prelude
is
   use Bare_Ada_Prelude_Memos;

      Row_Pos12 :
            Token_Index
               := No_Token_Index;
      Defer_Pos15 :
            Token_Index
               := No_Token_Index;
      Defer_Res15 :
            Bare_Ada_Context_Clause_List
               := No_Bare_Gpr_Node;
      Defer_Pos16 :
            Token_Index
               := No_Token_Index;
      Defer_Res16 :
            Bare_Ada_Library_Item
               := No_Bare_Gpr_Node;
      Transform_Res12 :
            Bare_Ada_Prelude
               := No_Bare_Gpr_Node;
      Transform_Diags12 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Ada_Prelude_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res12 := M.Instance;
      return Transform_Res12;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res12;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags12 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos12 := Pos;



Defer_Res15 :=
   Ada_Context_List_Parse0 (Parser, Row_Pos12);
Defer_Pos15 := Parser.Current_Pos;




if Defer_Pos15 /= No_Token_Index then

   Row_Pos12 := Defer_Pos15;

else
   Row_Pos12 := No_Token_Index;
   goto Exit_Row12_0;

end if;


Defer_Res16 :=
   Ada_Library_Item_Transform_Parse8 (Parser, Row_Pos12);
Defer_Pos16 := Parser.Current_Pos;




if Defer_Pos16 /= No_Token_Index then

   Row_Pos12 := Defer_Pos16;

else
   Row_Pos12 := No_Token_Index;
   goto Exit_Row12_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row12_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos12 /= No_Token_Index then

   Transform_Res12 := Allocate_Ada_Prelude (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res12,
      Kind => Gpr_Ada_Prelude,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos12 = Pos
                            then No_Token_Index
                            else Row_Pos12 - 1));

      Initialize_Fields_For_Ada_Prelude
        (Self => Transform_Res12, Ada_Prelude_F_Context_Clauses => Defer_Res15, Ada_Prelude_F_Library_Item => Defer_Res16);

         if Defer_Res15 /= null and then Is_Incomplete (Defer_Res15) then
            Transform_Res12.Last_Attempted_Child := 0;
         elsif Defer_Res15 /= null and then not Is_Ghost (Defer_Res15) then
            Transform_Res12.Last_Attempted_Child := -1;
         end if;
         if Defer_Res16 /= null and then Is_Incomplete (Defer_Res16) then
            Transform_Res12.Last_Attempted_Child := 0;
         elsif Defer_Res16 /= null and then not Is_Ghost (Defer_Res16) then
            Transform_Res12.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos12 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags12);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Ada_Prelude_Transform_Parse0_Memo,
      Row_Pos12 /= No_Token_Index,
      Transform_Res12,
      Pos,
      Row_Pos12);


   Parser.Current_Pos := Row_Pos12;

   return Transform_Res12;
end Ada_Prelude_Transform_Parse0;

   


function Ada_Subp_Kind_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Entity_Kind
is
   use Bare_Ada_Entity_Kind_Memos;

      Row_Pos13 :
            Token_Index
               := No_Token_Index;
      Token_Pos13 :
            Token_Index
               := No_Token_Index;
      Token_Res13 :
            Token_Index
               := No_Token_Index;
      Transform_Res13 :
            Bare_Ada_Entity_Kind_Procedure
               := No_Bare_Gpr_Node;
      Transform_Diags13 :
            Ada.Containers.Count_Type;
      Row_Pos14 :
            Token_Index
               := No_Token_Index;
      Token_Pos14 :
            Token_Index
               := No_Token_Index;
      Token_Res14 :
            Token_Index
               := No_Token_Index;
      Transform_Res14 :
            Bare_Ada_Entity_Kind_Function
               := No_Bare_Gpr_Node;
      Transform_Diags14 :
            Ada.Containers.Count_Type;
      Or_Pos5 :
            Token_Index
               := No_Token_Index;
      Or_Res5 :
            Bare_Ada_Entity_Kind
               := No_Bare_Gpr_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Ada_Subp_Kind_Or_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res5 := M.Instance;
      return Or_Res5;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Or_Res5;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos5 := No_Token_Index;
Or_Res5 := No_Bare_Gpr_Node;
    
--  Start transform_code

Transform_Diags13 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos13 := Pos;



--  Start tok_code

Token_Res13 := Row_Pos13;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res13));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
      or else T.Symbol /= Precomputed_Symbol
        (Precomputed_Symbol_Table (Parser.TDH.Symbols),
         Precomputed_Sym_Procedure)
   then
       Token_Pos13 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos13 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos13,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos13 := Row_Pos13 + 1;
   end if;
end;

--  End tok_code




if Token_Pos13 /= No_Token_Index then

   Row_Pos13 := Token_Pos13;

else
   Row_Pos13 := No_Token_Index;
   goto Exit_Row13_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row13_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos13 /= No_Token_Index then

   Transform_Res13 := Allocate_Ada_Entity_Kind_Procedure (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res13,
      Kind => Gpr_Ada_Entity_Kind_Procedure,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos13 = Pos
                            then No_Token_Index
                            else Row_Pos13 - 1));




elsif Row_Pos13 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags13);
end if;

--  End transform_code

    if Row_Pos13 /= No_Token_Index then
        Or_Pos5 := Row_Pos13;
        Or_Res5 := Transform_Res13;
        goto Exit_Or7;
    end if;
    
--  Start transform_code

Transform_Diags14 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos14 := Pos;



--  Start tok_code

Token_Res14 := Row_Pos14;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res14));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
      or else T.Symbol /= Precomputed_Symbol
        (Precomputed_Symbol_Table (Parser.TDH.Symbols),
         Precomputed_Sym_Function)
   then
       Token_Pos14 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos14 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos14,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos14 := Row_Pos14 + 1;
   end if;
end;

--  End tok_code




if Token_Pos14 /= No_Token_Index then

   Row_Pos14 := Token_Pos14;

else
   Row_Pos14 := No_Token_Index;
   goto Exit_Row14_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row14_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos14 /= No_Token_Index then

   Transform_Res14 := Allocate_Ada_Entity_Kind_Function (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res14,
      Kind => Gpr_Ada_Entity_Kind_Function,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos14 = Pos
                            then No_Token_Index
                            else Row_Pos14 - 1));




elsif Row_Pos14 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags14);
end if;

--  End transform_code

    if Row_Pos14 /= No_Token_Index then
        Or_Pos5 := Row_Pos14;
        Or_Res5 := Transform_Res14;
        goto Exit_Or7;
    end if;
<<Exit_Or7>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Ada_Subp_Kind_Or_Parse0_Memo,
      Or_Pos5 /= No_Token_Index,
      Or_Res5,
      Pos,
      Or_Pos5);


   Parser.Current_Pos := Or_Pos5;

   return Or_Res5;
end Ada_Subp_Kind_Or_Parse0;

   


function Ada_Use_Clause_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_Use
is
   use Bare_Ada_Use_Memos;

      Row_Pos15 :
            Token_Index
               := No_Token_Index;
      Token_Pos15 :
            Token_Index
               := No_Token_Index;
      Token_Res15 :
            Token_Index
               := No_Token_Index;
      Defer_Pos17 :
            Token_Index
               := No_Token_Index;
      Defer_Res17 :
            Bare_Ada_Skip_List
               := No_Bare_Gpr_Node;
      Token_Pos16 :
            Token_Index
               := No_Token_Index;
      Token_Res16 :
            Token_Index
               := No_Token_Index;
      Transform_Res15 :
            Bare_Ada_Use
               := No_Bare_Gpr_Node;
      Transform_Diags15 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Ada_Use_Clause_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res15 := M.Instance;
      return Transform_Res15;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res15;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags15 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos15 := Pos;



--  Start tok_code

Token_Res15 := Row_Pos15;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res15));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Use)
   then
       Token_Pos15 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos15 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos15,
             Expected_Token_Id => Gpr_Use,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos15 := Row_Pos15 + 1;
   end if;
end;

--  End tok_code




if Token_Pos15 /= No_Token_Index then

   Row_Pos15 := Token_Pos15;

else
   Row_Pos15 := No_Token_Index;
   goto Exit_Row15_0;

end if;


        Parser.Private_Part.Dont_Skip.Append
          (Dontskip_Ada_Use_Clause0_Extract_Parse0'Access);
        
Defer_Res17 :=
   Ada_Context_Skip_List_Parse0 (Parser, Row_Pos15);
Defer_Pos17 := Parser.Current_Pos;

        Parser.Private_Part.Dont_Skip.Delete_Last;
        



if Defer_Pos17 /= No_Token_Index then

   Row_Pos15 := Defer_Pos17;

else
   Row_Pos15 := No_Token_Index;
   goto Exit_Row15_0;

end if;


--  Start tok_code

Token_Res16 := Row_Pos15;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res16));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Semicolon)
   then
       Token_Pos16 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos15 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos15,
             Expected_Token_Id => Gpr_Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos16 := Row_Pos15 + 1;
   end if;
end;

--  End tok_code




if Token_Pos16 /= No_Token_Index then

   Row_Pos15 := Token_Pos16;

else
   Row_Pos15 := No_Token_Index;
   goto Exit_Row15_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row15_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos15 /= No_Token_Index then

   Transform_Res15 := Allocate_Ada_Use (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res15,
      Kind => Gpr_Ada_Use,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos15 = Pos
                            then No_Token_Index
                            else Row_Pos15 - 1));

      Initialize_Fields_For_Ada_Use
        (Self => Transform_Res15, Ada_Use_F_Skips => Defer_Res17);

         if Defer_Res17 /= null and then Is_Incomplete (Defer_Res17) then
            Transform_Res15.Last_Attempted_Child := 0;
         elsif Defer_Res17 /= null and then not Is_Ghost (Defer_Res17) then
            Transform_Res15.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos15 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags15);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Ada_Use_Clause_Transform_Parse0_Memo,
      Row_Pos15 /= No_Token_Index,
      Transform_Res15,
      Pos,
      Row_Pos15);


   Parser.Current_Pos := Row_Pos15;

   return Transform_Res15;
end Ada_Use_Clause_Transform_Parse0;

   


function Ada_With_Clause_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Ada_With
is
   use Bare_Ada_With_Memos;

      Row_Pos16 :
            Token_Index
               := No_Token_Index;
      Token_Pos17 :
            Token_Index
               := No_Token_Index;
      Token_Res17 :
            Token_Index
               := No_Token_Index;
      Opt_Res1 :
            Bare_Limited_Node
               := No_Bare_Gpr_Node;
      Token_Pos18 :
            Token_Index
               := No_Token_Index;
      Token_Res18 :
            Token_Index
               := No_Token_Index;
      Opt_Res2 :
            Bare_Private_Node
               := No_Bare_Gpr_Node;
      Token_Pos19 :
            Token_Index
               := No_Token_Index;
      Token_Res19 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos3 :
            Token_Index
               := No_Token_Index;
      Tmp_List3 :
            Free_Parse_List;
      Defer_Pos18 :
            Token_Index
               := No_Token_Index;
      Defer_Res18 :
            Bare_Expr
               := No_Bare_Gpr_Node;
      Token_Pos20 :
            Token_Index
               := No_Token_Index;
      Token_Res20 :
            Token_Index
               := No_Token_Index;
      List_Pos3 :
            Token_Index
               := No_Token_Index;
      List_Res3 :
            Bare_Expr_List
               := No_Bare_Gpr_Node;
      Token_Pos21 :
            Token_Index
               := No_Token_Index;
      Token_Res21 :
            Token_Index
               := No_Token_Index;
      Transform_Res16 :
            Bare_Ada_With
               := No_Bare_Gpr_Node;
      Transform_Diags16 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Ada_With_Clause_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res16 := M.Instance;
      return Transform_Res16;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res16;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags16 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos16 := Pos;



--  Start opt_code












--  Start tok_code

Token_Res17 := Row_Pos16;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res17));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Limited)
   then
       Token_Pos17 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos16 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos16,
             Expected_Token_Id => Gpr_Limited,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos17 := Row_Pos16 + 1;
   end if;
end;

--  End tok_code


if Token_Pos17 = No_Token_Index then

         Opt_Res1 := Allocate_Limited_Absent (Parser.Mem_Pool);
         Initialize
           (Self              => Opt_Res1,
            Kind              => Gpr_Limited_Absent,
            Unit              => Parser.Unit,
            Token_Start_Index => Row_Pos16,
            Token_End_Index   => No_Token_Index);


       
   Token_Pos17 := Row_Pos16;


else

      Opt_Res1 := Allocate_Limited_Present (Parser.Mem_Pool);
      Initialize
        (Self              => Opt_Res1,
         Kind              => Gpr_Limited_Present,
         Unit              => Parser.Unit,
         Token_Start_Index => Row_Pos16,
         Token_End_Index   => Token_Pos17 - 1);

end if;

--  End opt_code




if Token_Pos17 /= No_Token_Index then

   Row_Pos16 := Token_Pos17;

else
   Row_Pos16 := No_Token_Index;
   goto Exit_Row16_0;

end if;


--  Start opt_code












--  Start tok_code

Token_Res18 := Row_Pos16;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res18));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Private)
   then
       Token_Pos18 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos16 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos16,
             Expected_Token_Id => Gpr_Private,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos18 := Row_Pos16 + 1;
   end if;
end;

--  End tok_code


if Token_Pos18 = No_Token_Index then

         Opt_Res2 := Allocate_Private_Absent (Parser.Mem_Pool);
         Initialize
           (Self              => Opt_Res2,
            Kind              => Gpr_Private_Absent,
            Unit              => Parser.Unit,
            Token_Start_Index => Row_Pos16,
            Token_End_Index   => No_Token_Index);


       
   Token_Pos18 := Row_Pos16;


else

      Opt_Res2 := Allocate_Private_Present (Parser.Mem_Pool);
      Initialize
        (Self              => Opt_Res2,
         Kind              => Gpr_Private_Present,
         Unit              => Parser.Unit,
         Token_Start_Index => Row_Pos16,
         Token_End_Index   => Token_Pos18 - 1);

end if;

--  End opt_code




if Token_Pos18 /= No_Token_Index then

   Row_Pos16 := Token_Pos18;

else
   Row_Pos16 := No_Token_Index;
   goto Exit_Row16_0;

end if;


--  Start tok_code

Token_Res19 := Row_Pos16;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res19));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_With)
   then
       Token_Pos19 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos16 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos16,
             Expected_Token_Id => Gpr_With,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos19 := Row_Pos16 + 1;
   end if;
end;

--  End tok_code




if Token_Pos19 /= No_Token_Index then

   Row_Pos16 := Token_Pos19;

else
   Row_Pos16 := No_Token_Index;
   goto Exit_Row16_0;

end if;


--  Start list_code

    List_Pos3 := No_Token_Index;



Lst_Cpos3 := Row_Pos16;
Tmp_List3 := Get_Parse_List (Parser);

loop
   
Defer_Res18 :=
   Static_Name_Or_Parse0 (Parser, Lst_Cpos3);
Defer_Pos18 := Parser.Current_Pos;


   exit when Defer_Pos18 = No_Token_Index;

   List_Pos3 := Defer_Pos18;
   Lst_Cpos3 := List_Pos3;

   Tmp_List3.Nodes.Append (Defer_Res18);

      
--  Start tok_code

Token_Res20 := Lst_Cpos3;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res20));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Comma)
   then
       Token_Pos20 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos3 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos3,
             Expected_Token_Id => Gpr_Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos20 := Lst_Cpos3 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos20 /= No_Token_Index then
          Lst_Cpos3 := Token_Pos20;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List3.Nodes.Length;
begin
   List_Res3 :=
      Allocate_Expr_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos16;
      Token_End := (if Lst_Cpos3 = Row_Pos16
                    then Row_Pos16
                    else Lst_Cpos3 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos16, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res3,
      Kind              => Gpr_Expr_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res3,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List3.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res3.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List3);

--  End list_code




if List_Pos3 /= No_Token_Index then

   Row_Pos16 := List_Pos3;

else
   Row_Pos16 := No_Token_Index;
   goto Exit_Row16_0;

end if;


--  Start tok_code

Token_Res21 := Row_Pos16;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res21));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Semicolon)
   then
       Token_Pos21 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos16 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos16,
             Expected_Token_Id => Gpr_Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos21 := Row_Pos16 + 1;
   end if;
end;

--  End tok_code




if Token_Pos21 /= No_Token_Index then

   Row_Pos16 := Token_Pos21;

else
   Row_Pos16 := No_Token_Index;
   goto Exit_Row16_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row16_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos16 /= No_Token_Index then

   Transform_Res16 := Allocate_Ada_With (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res16,
      Kind => Gpr_Ada_With,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos16 = Pos
                            then No_Token_Index
                            else Row_Pos16 - 1));

      Initialize_Fields_For_Ada_With
        (Self => Transform_Res16, Ada_With_F_Has_Limited => Opt_Res1, Ada_With_F_Has_Private => Opt_Res2, Ada_With_F_Packages => List_Res3);

         if Opt_Res1 /= null and then Is_Incomplete (Opt_Res1) then
            Transform_Res16.Last_Attempted_Child := 0;
         elsif Opt_Res1 /= null and then not Is_Ghost (Opt_Res1) then
            Transform_Res16.Last_Attempted_Child := -1;
         end if;
         if Opt_Res2 /= null and then Is_Incomplete (Opt_Res2) then
            Transform_Res16.Last_Attempted_Child := 0;
         elsif Opt_Res2 /= null and then not Is_Ghost (Opt_Res2) then
            Transform_Res16.Last_Attempted_Child := -1;
         end if;
         if List_Res3 /= null and then Is_Incomplete (List_Res3) then
            Transform_Res16.Last_Attempted_Child := 0;
         elsif List_Res3 /= null and then not Is_Ghost (List_Res3) then
            Transform_Res16.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos16 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags16);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Ada_With_Clause_Transform_Parse0_Memo,
      Row_Pos16 /= No_Token_Index,
      Transform_Res16,
      Pos,
      Row_Pos16);


   Parser.Current_Pos := Row_Pos16;

   return Transform_Res16;
end Ada_With_Clause_Transform_Parse0;

   


function Associative_Array_Index_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node
is
   use Bare_Gpr_Node_Memos;

      Defer_Pos19 :
            Token_Index
               := No_Token_Index;
      Defer_Res19 :
            Bare_Others_Designator
               := No_Bare_Gpr_Node;
      Defer_Pos20 :
            Token_Index
               := No_Token_Index;
      Defer_Res20 :
            Bare_String_Literal_At
               := No_Bare_Gpr_Node;
      Or_Pos6 :
            Token_Index
               := No_Token_Index;
      Or_Res6 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Associative_Array_Index_Or_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res6 := M.Instance;
      return Or_Res6;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Or_Res6;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos6 := No_Token_Index;
Or_Res6 := No_Bare_Gpr_Node;
    
Defer_Res19 :=
   Others_Designator_Transform_Parse0 (Parser, Pos);
Defer_Pos19 := Parser.Current_Pos;

    if Defer_Pos19 /= No_Token_Index then
        Or_Pos6 := Defer_Pos19;
        Or_Res6 := Defer_Res19;
        goto Exit_Or8;
    end if;
    
Defer_Res20 :=
   String_Literal_At_Transform_Parse0 (Parser, Pos);
Defer_Pos20 := Parser.Current_Pos;

    if Defer_Pos20 /= No_Token_Index then
        Or_Pos6 := Defer_Pos20;
        Or_Res6 := Defer_Res20;
        goto Exit_Or8;
    end if;
<<Exit_Or8>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Associative_Array_Index_Or_Parse0_Memo,
      Or_Pos6 /= No_Token_Index,
      Or_Res6,
      Pos,
      Or_Pos6);


   Parser.Current_Pos := Or_Pos6;

   return Or_Res6;
end Associative_Array_Index_Or_Parse0;

   


function Attribute_Decl_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Attribute_Decl
is
   use Bare_Attribute_Decl_Memos;

      Row_Pos17 :
            Token_Index
               := No_Token_Index;
      Token_Pos22 :
            Token_Index
               := No_Token_Index;
      Token_Res22 :
            Token_Index
               := No_Token_Index;
      Defer_Pos21 :
            Token_Index
               := No_Token_Index;
      Defer_Res21 :
            Bare_Identifier
               := No_Bare_Gpr_Node;
      Row_Pos18 :
            Token_Index
               := No_Token_Index;
      Token_Pos23 :
            Token_Index
               := No_Token_Index;
      Token_Res23 :
            Token_Index
               := No_Token_Index;
      Defer_Pos22 :
            Token_Index
               := No_Token_Index;
      Defer_Res22 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;
      Token_Pos24 :
            Token_Index
               := No_Token_Index;
      Token_Res24 :
            Token_Index
               := No_Token_Index;
      Token_Pos25 :
            Token_Index
               := No_Token_Index;
      Token_Res25 :
            Token_Index
               := No_Token_Index;
      Defer_Pos23 :
            Token_Index
               := No_Token_Index;
      Defer_Res23 :
            Bare_Term_List
               := No_Bare_Gpr_Node;
      Token_Pos26 :
            Token_Index
               := No_Token_Index;
      Token_Res26 :
            Token_Index
               := No_Token_Index;
      Transform_Res17 :
            Bare_Attribute_Decl
               := No_Bare_Gpr_Node;
      Transform_Diags17 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Attribute_Decl_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res17 := M.Instance;
      return Transform_Res17;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res17;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags17 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos17 := Pos;



--  Start tok_code

Token_Res22 := Row_Pos17;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res22));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_For)
   then
       Token_Pos22 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos17 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos17,
             Expected_Token_Id => Gpr_For,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos22 := Row_Pos17 + 1;
   end if;
end;

--  End tok_code




if Token_Pos22 /= No_Token_Index then

   Row_Pos17 := Token_Pos22;

else
   Row_Pos17 := No_Token_Index;
   goto Exit_Row17_0;

end if;


Defer_Res21 :=
   Identifier_Transform_Parse0 (Parser, Row_Pos17);
Defer_Pos21 := Parser.Current_Pos;




if Defer_Pos21 /= No_Token_Index then

   Row_Pos17 := Defer_Pos21;

else
   Row_Pos17 := No_Token_Index;
   goto Exit_Row17_0;

end if;


--  Start opt_code












--  Start row_code

Row_Pos18 := Row_Pos17;



--  Start tok_code

Token_Res23 := Row_Pos18;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res23));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Par_Open)
   then
       Token_Pos23 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos18 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos18,
             Expected_Token_Id => Gpr_Par_Open,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos23 := Row_Pos18 + 1;
   end if;
end;

--  End tok_code




if Token_Pos23 /= No_Token_Index then

   Row_Pos18 := Token_Pos23;

else
   Row_Pos18 := No_Token_Index;
   goto Exit_Row18_0;

end if;


Defer_Res22 :=
   Associative_Array_Index_Or_Parse0 (Parser, Row_Pos18);
Defer_Pos22 := Parser.Current_Pos;




if Defer_Pos22 /= No_Token_Index then

   Row_Pos18 := Defer_Pos22;

else
   Row_Pos18 := No_Token_Index;
   goto Exit_Row18_0;

end if;


--  Start tok_code

Token_Res24 := Row_Pos18;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res24));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Par_Close)
   then
       Token_Pos24 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos18 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos18,
             Expected_Token_Id => Gpr_Par_Close,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos24 := Row_Pos18 + 1;
   end if;
end;

--  End tok_code




if Token_Pos24 /= No_Token_Index then

   Row_Pos18 := Token_Pos24;

else
   Row_Pos18 := No_Token_Index;
   goto Exit_Row18_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row18_0>>
pragma Warnings (On, "referenced");

--  End row_code


if Row_Pos18 = No_Token_Index then

         
   Defer_Res22 := No_Bare_Gpr_Node;



       
   Row_Pos18 := Row_Pos17;



end if;

--  End opt_code




if Row_Pos18 /= No_Token_Index then

   Row_Pos17 := Row_Pos18;

else
   Row_Pos17 := No_Token_Index;
   goto Exit_Row17_0;

end if;


--  Start tok_code

Token_Res25 := Row_Pos17;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res25));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Use)
   then
       Token_Pos25 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos17 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos17,
             Expected_Token_Id => Gpr_Use,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos25 := Row_Pos17 + 1;
   end if;
end;

--  End tok_code




if Token_Pos25 /= No_Token_Index then

   Row_Pos17 := Token_Pos25;

else
   Row_Pos17 := No_Token_Index;
   goto Exit_Row17_0;

end if;


Defer_Res23 :=
   Expression_List_Parse0 (Parser, Row_Pos17);
Defer_Pos23 := Parser.Current_Pos;




if Defer_Pos23 /= No_Token_Index then

   Row_Pos17 := Defer_Pos23;

else
   Row_Pos17 := No_Token_Index;
   goto Exit_Row17_0;

end if;


--  Start tok_code

Token_Res26 := Row_Pos17;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res26));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Semicolon)
   then
       Token_Pos26 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos17 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos17,
             Expected_Token_Id => Gpr_Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos26 := Row_Pos17 + 1;
   end if;
end;

--  End tok_code




if Token_Pos26 /= No_Token_Index then

   Row_Pos17 := Token_Pos26;

else
   Row_Pos17 := No_Token_Index;
   goto Exit_Row17_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row17_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos17 /= No_Token_Index then

   Transform_Res17 := Allocate_Attribute_Decl (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res17,
      Kind => Gpr_Attribute_Decl,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos17 = Pos
                            then No_Token_Index
                            else Row_Pos17 - 1));

      Initialize_Fields_For_Attribute_Decl
        (Self => Transform_Res17, Attribute_Decl_F_Attr_Name => Defer_Res21, Attribute_Decl_F_Attr_Index => Defer_Res22, Attribute_Decl_F_Expr => Defer_Res23);

         if Defer_Res21 /= null and then Is_Incomplete (Defer_Res21) then
            Transform_Res17.Last_Attempted_Child := 0;
         elsif Defer_Res21 /= null and then not Is_Ghost (Defer_Res21) then
            Transform_Res17.Last_Attempted_Child := -1;
         end if;
         if Defer_Res22 /= null and then Is_Incomplete (Defer_Res22) then
            Transform_Res17.Last_Attempted_Child := 0;
         elsif Defer_Res22 /= null and then not Is_Ghost (Defer_Res22) then
            Transform_Res17.Last_Attempted_Child := -1;
         end if;
         if Defer_Res23 /= null and then Is_Incomplete (Defer_Res23) then
            Transform_Res17.Last_Attempted_Child := 0;
         elsif Defer_Res23 /= null and then not Is_Ghost (Defer_Res23) then
            Transform_Res17.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos17 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags17);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Attribute_Decl_Transform_Parse0_Memo,
      Row_Pos17 /= No_Token_Index,
      Transform_Res17,
      Pos,
      Row_Pos17);


   Parser.Current_Pos := Row_Pos17;

   return Transform_Res17;
end Attribute_Decl_Transform_Parse0;

   


function Attribute_Reference_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Attribute_Reference
is
   use Bare_Attribute_Reference_Memos;

      Row_Pos19 :
            Token_Index
               := No_Token_Index;
      Defer_Pos24 :
            Token_Index
               := No_Token_Index;
      Defer_Res24 :
            Bare_Identifier
               := No_Bare_Gpr_Node;
      Row_Pos20 :
            Token_Index
               := No_Token_Index;
      Token_Pos27 :
            Token_Index
               := No_Token_Index;
      Token_Res27 :
            Token_Index
               := No_Token_Index;
      Defer_Pos25 :
            Token_Index
               := No_Token_Index;
      Defer_Res25 :
            Bare_Others_Designator
               := No_Bare_Gpr_Node;
      Defer_Pos26 :
            Token_Index
               := No_Token_Index;
      Defer_Res26 :
            Bare_String_Literal
               := No_Bare_Gpr_Node;
      Or_Pos7 :
            Token_Index
               := No_Token_Index;
      Or_Res7 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;
      Token_Pos28 :
            Token_Index
               := No_Token_Index;
      Token_Res28 :
            Token_Index
               := No_Token_Index;
      Transform_Res18 :
            Bare_Attribute_Reference
               := No_Bare_Gpr_Node;
      Transform_Diags18 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Attribute_Reference_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res18 := M.Instance;
      return Transform_Res18;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res18;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags18 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos19 := Pos;



Defer_Res24 :=
   Identifier_Transform_Parse0 (Parser, Row_Pos19);
Defer_Pos24 := Parser.Current_Pos;




if Defer_Pos24 /= No_Token_Index then

   Row_Pos19 := Defer_Pos24;

else
   Row_Pos19 := No_Token_Index;
   goto Exit_Row19_0;

end if;


--  Start opt_code












--  Start row_code

Row_Pos20 := Row_Pos19;



--  Start tok_code

Token_Res27 := Row_Pos20;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res27));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Par_Open)
   then
       Token_Pos27 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos20 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos20,
             Expected_Token_Id => Gpr_Par_Open,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos27 := Row_Pos20 + 1;
   end if;
end;

--  End tok_code




if Token_Pos27 /= No_Token_Index then

   Row_Pos20 := Token_Pos27;

else
   Row_Pos20 := No_Token_Index;
   goto Exit_Row20_0;

end if;


--  Start or_code

Or_Pos7 := No_Token_Index;
Or_Res7 := No_Bare_Gpr_Node;
    
Defer_Res25 :=
   Others_Designator_Transform_Parse0 (Parser, Row_Pos20);
Defer_Pos25 := Parser.Current_Pos;

    if Defer_Pos25 /= No_Token_Index then
        Or_Pos7 := Defer_Pos25;
        Or_Res7 := Defer_Res25;
        goto Exit_Or9;
    end if;
    
Defer_Res26 :=
   String_Literal_Transform_Parse0 (Parser, Row_Pos20);
Defer_Pos26 := Parser.Current_Pos;

    if Defer_Pos26 /= No_Token_Index then
        Or_Pos7 := Defer_Pos26;
        Or_Res7 := Defer_Res26;
        goto Exit_Or9;
    end if;
<<Exit_Or9>>

--  End or_code




if Or_Pos7 /= No_Token_Index then

   Row_Pos20 := Or_Pos7;

else
   Row_Pos20 := No_Token_Index;
   goto Exit_Row20_0;

end if;


--  Start tok_code

Token_Res28 := Row_Pos20;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res28));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Par_Close)
   then
       Token_Pos28 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos20 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos20,
             Expected_Token_Id => Gpr_Par_Close,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos28 := Row_Pos20 + 1;
   end if;
end;

--  End tok_code




if Token_Pos28 /= No_Token_Index then

   Row_Pos20 := Token_Pos28;

else
   Row_Pos20 := No_Token_Index;
   goto Exit_Row20_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row20_0>>
pragma Warnings (On, "referenced");

--  End row_code


if Row_Pos20 = No_Token_Index then

         
   Or_Res7 := No_Bare_Gpr_Node;



       
   Row_Pos20 := Row_Pos19;



end if;

--  End opt_code




if Row_Pos20 /= No_Token_Index then

   Row_Pos19 := Row_Pos20;

else
   Row_Pos19 := No_Token_Index;
   goto Exit_Row19_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row19_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos19 /= No_Token_Index then

   Transform_Res18 := Allocate_Attribute_Reference (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res18,
      Kind => Gpr_Attribute_Reference,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos19 = Pos
                            then No_Token_Index
                            else Row_Pos19 - 1));

      Initialize_Fields_For_Attribute_Reference
        (Self => Transform_Res18, Attribute_Reference_F_Attribute_Name => Defer_Res24, Attribute_Reference_F_Attribute_Index => Or_Res7);

         if Defer_Res24 /= null and then Is_Incomplete (Defer_Res24) then
            Transform_Res18.Last_Attempted_Child := 0;
         elsif Defer_Res24 /= null and then not Is_Ghost (Defer_Res24) then
            Transform_Res18.Last_Attempted_Child := -1;
         end if;
         if Or_Res7 /= null and then Is_Incomplete (Or_Res7) then
            Transform_Res18.Last_Attempted_Child := 0;
         elsif Or_Res7 /= null and then not Is_Ghost (Or_Res7) then
            Transform_Res18.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos19 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags18);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Attribute_Reference_Transform_Parse0_Memo,
      Row_Pos19 /= No_Token_Index,
      Transform_Res18,
      Pos,
      Row_Pos19);


   Parser.Current_Pos := Row_Pos19;

   return Transform_Res18;
end Attribute_Reference_Transform_Parse0;

   


function Builtin_Function_Call_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Builtin_Function_Call
is
   use Bare_Builtin_Function_Call_Memos;

      Row_Pos21 :
            Token_Index
               := No_Token_Index;
      Defer_Pos27 :
            Token_Index
               := No_Token_Index;
      Defer_Res27 :
            Bare_Identifier
               := No_Bare_Gpr_Node;
      Defer_Pos28 :
            Token_Index
               := No_Token_Index;
      Defer_Res28 :
            Bare_Terms
               := No_Bare_Gpr_Node;
      Transform_Res19 :
            Bare_Builtin_Function_Call
               := No_Bare_Gpr_Node;
      Transform_Diags19 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Builtin_Function_Call_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res19 := M.Instance;
      return Transform_Res19;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res19;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags19 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos21 := Pos;



Defer_Res27 :=
   Identifier_Transform_Parse0 (Parser, Row_Pos21);
Defer_Pos27 := Parser.Current_Pos;




if Defer_Pos27 /= No_Token_Index then

   Row_Pos21 := Defer_Pos27;

else
   Row_Pos21 := No_Token_Index;
   goto Exit_Row21_0;

end if;


Defer_Res28 :=
   Expression_List_Transform_Parse0 (Parser, Row_Pos21);
Defer_Pos28 := Parser.Current_Pos;




if Defer_Pos28 /= No_Token_Index then

   Row_Pos21 := Defer_Pos28;

else
   Row_Pos21 := No_Token_Index;
   goto Exit_Row21_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row21_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos21 /= No_Token_Index then

   Transform_Res19 := Allocate_Builtin_Function_Call (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res19,
      Kind => Gpr_Builtin_Function_Call,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos21 = Pos
                            then No_Token_Index
                            else Row_Pos21 - 1));

      Initialize_Fields_For_Builtin_Function_Call
        (Self => Transform_Res19, Builtin_Function_Call_F_Function_Name => Defer_Res27, Builtin_Function_Call_F_Parameters => Defer_Res28);

         if Defer_Res27 /= null and then Is_Incomplete (Defer_Res27) then
            Transform_Res19.Last_Attempted_Child := 0;
         elsif Defer_Res27 /= null and then not Is_Ghost (Defer_Res27) then
            Transform_Res19.Last_Attempted_Child := -1;
         end if;
         if Defer_Res28 /= null and then Is_Incomplete (Defer_Res28) then
            Transform_Res19.Last_Attempted_Child := 0;
         elsif Defer_Res28 /= null and then not Is_Ghost (Defer_Res28) then
            Transform_Res19.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos21 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags19);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Builtin_Function_Call_Transform_Parse0_Memo,
      Row_Pos21 /= No_Token_Index,
      Transform_Res19,
      Pos,
      Row_Pos21);


   Parser.Current_Pos := Row_Pos21;

   return Transform_Res19;
end Builtin_Function_Call_Transform_Parse0;

   


function Case_Construction_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Case_Construction
is
   use Bare_Case_Construction_Memos;

      Row_Pos22 :
            Token_Index
               := No_Token_Index;
      Token_Pos29 :
            Token_Index
               := No_Token_Index;
      Token_Res29 :
            Token_Index
               := No_Token_Index;
      Defer_Pos29 :
            Token_Index
               := No_Token_Index;
      Defer_Res29 :
            Bare_Variable_Reference
               := No_Bare_Gpr_Node;
      Token_Pos30 :
            Token_Index
               := No_Token_Index;
      Token_Res30 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos4 :
            Token_Index
               := No_Token_Index;
      Tmp_List4 :
            Free_Parse_List;
      Defer_Pos30 :
            Token_Index
               := No_Token_Index;
      Defer_Res30 :
            Bare_Case_Item
               := No_Bare_Gpr_Node;
      List_Pos4 :
            Token_Index
               := No_Token_Index;
      List_Res4 :
            Bare_Case_Item_List
               := No_Bare_Gpr_Node;
      Token_Pos31 :
            Token_Index
               := No_Token_Index;
      Token_Res31 :
            Token_Index
               := No_Token_Index;
      Token_Pos32 :
            Token_Index
               := No_Token_Index;
      Token_Res32 :
            Token_Index
               := No_Token_Index;
      Token_Pos33 :
            Token_Index
               := No_Token_Index;
      Token_Res33 :
            Token_Index
               := No_Token_Index;
      Transform_Res20 :
            Bare_Case_Construction
               := No_Bare_Gpr_Node;
      Transform_Diags20 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Case_Construction_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res20 := M.Instance;
      return Transform_Res20;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res20;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags20 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos22 := Pos;



--  Start tok_code

Token_Res29 := Row_Pos22;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res29));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Case)
   then
       Token_Pos29 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos22 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos22,
             Expected_Token_Id => Gpr_Case,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos29 := Row_Pos22 + 1;
   end if;
end;

--  End tok_code




if Token_Pos29 /= No_Token_Index then

   Row_Pos22 := Token_Pos29;

else
   Row_Pos22 := No_Token_Index;
   goto Exit_Row22_0;

end if;


Defer_Res29 :=
   Variable_Reference_Transform_Parse0 (Parser, Row_Pos22);
Defer_Pos29 := Parser.Current_Pos;




if Defer_Pos29 /= No_Token_Index then

   Row_Pos22 := Defer_Pos29;

else
   Row_Pos22 := No_Token_Index;
   goto Exit_Row22_0;

end if;


--  Start tok_code

Token_Res30 := Row_Pos22;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res30));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Is)
   then
       Token_Pos30 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos22 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos22,
             Expected_Token_Id => Gpr_Is,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos30 := Row_Pos22 + 1;
   end if;
end;

--  End tok_code




if Token_Pos30 /= No_Token_Index then

   Row_Pos22 := Token_Pos30;

else
   Row_Pos22 := No_Token_Index;
   goto Exit_Row22_0;

end if;


--  Start list_code

    List_Pos4 := Row_Pos22;



Lst_Cpos4 := Row_Pos22;
Tmp_List4 := Get_Parse_List (Parser);

loop
   
Defer_Res30 :=
   Case_Item_Transform_Parse0 (Parser, Lst_Cpos4);
Defer_Pos30 := Parser.Current_Pos;


   exit when Defer_Pos30 = No_Token_Index;

   List_Pos4 := Defer_Pos30;
   Lst_Cpos4 := List_Pos4;

   Tmp_List4.Nodes.Append (Defer_Res30);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List4.Nodes.Length;
begin
   List_Res4 :=
      Allocate_Case_Item_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos22;
      Token_End := (if Lst_Cpos4 = Row_Pos22
                    then Row_Pos22
                    else Lst_Cpos4 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos22, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res4,
      Kind              => Gpr_Case_Item_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res4,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List4.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res4.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List4);

--  End list_code




if List_Pos4 /= No_Token_Index then

   Row_Pos22 := List_Pos4;

else
   Row_Pos22 := No_Token_Index;
   goto Exit_Row22_0;

end if;


--  Start tok_code

Token_Res31 := Row_Pos22;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res31));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_End)
   then
       Token_Pos31 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos22 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos22,
             Expected_Token_Id => Gpr_End,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos31 := Row_Pos22 + 1;
   end if;
end;

--  End tok_code




if Token_Pos31 /= No_Token_Index then

   Row_Pos22 := Token_Pos31;

else
   Row_Pos22 := No_Token_Index;
   goto Exit_Row22_0;

end if;


--  Start tok_code

Token_Res32 := Row_Pos22;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res32));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Case)
   then
       Token_Pos32 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos22 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos22,
             Expected_Token_Id => Gpr_Case,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos32 := Row_Pos22 + 1;
   end if;
end;

--  End tok_code




if Token_Pos32 /= No_Token_Index then

   Row_Pos22 := Token_Pos32;

else
   Row_Pos22 := No_Token_Index;
   goto Exit_Row22_0;

end if;


--  Start tok_code

Token_Res33 := Row_Pos22;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res33));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Semicolon)
   then
       Token_Pos33 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos22 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos22,
             Expected_Token_Id => Gpr_Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos33 := Row_Pos22 + 1;
   end if;
end;

--  End tok_code




if Token_Pos33 /= No_Token_Index then

   Row_Pos22 := Token_Pos33;

else
   Row_Pos22 := No_Token_Index;
   goto Exit_Row22_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row22_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos22 /= No_Token_Index then

   Transform_Res20 := Allocate_Case_Construction (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res20,
      Kind => Gpr_Case_Construction,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos22 = Pos
                            then No_Token_Index
                            else Row_Pos22 - 1));

      Initialize_Fields_For_Case_Construction
        (Self => Transform_Res20, Case_Construction_F_Var_Ref => Defer_Res29, Case_Construction_F_Items => List_Res4);

         if Defer_Res29 /= null and then Is_Incomplete (Defer_Res29) then
            Transform_Res20.Last_Attempted_Child := 0;
         elsif Defer_Res29 /= null and then not Is_Ghost (Defer_Res29) then
            Transform_Res20.Last_Attempted_Child := -1;
         end if;
         if List_Res4 /= null and then Is_Incomplete (List_Res4) then
            Transform_Res20.Last_Attempted_Child := 0;
         elsif List_Res4 /= null and then not Is_Ghost (List_Res4) then
            Transform_Res20.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos22 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags20);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Case_Construction_Transform_Parse0_Memo,
      Row_Pos22 /= No_Token_Index,
      Transform_Res20,
      Pos,
      Row_Pos22);


   Parser.Current_Pos := Row_Pos22;

   return Transform_Res20;
end Case_Construction_Transform_Parse0;

   


function Case_Item_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Case_Item
is
   use Bare_Case_Item_Memos;

      Row_Pos23 :
            Token_Index
               := No_Token_Index;
      Token_Pos34 :
            Token_Index
               := No_Token_Index;
      Token_Res34 :
            Token_Index
               := No_Token_Index;
      Defer_Pos31 :
            Token_Index
               := No_Token_Index;
      Defer_Res31 :
            Bare_Choices
               := No_Bare_Gpr_Node;
      Token_Pos35 :
            Token_Index
               := No_Token_Index;
      Token_Res35 :
            Token_Index
               := No_Token_Index;
      Defer_Pos32 :
            Token_Index
               := No_Token_Index;
      Defer_Res32 :
            Bare_Gpr_Node_List
               := No_Bare_Gpr_Node;
      Transform_Res21 :
            Bare_Case_Item
               := No_Bare_Gpr_Node;
      Transform_Diags21 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Case_Item_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res21 := M.Instance;
      return Transform_Res21;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res21;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags21 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos23 := Pos;



--  Start tok_code

Token_Res34 := Row_Pos23;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res34));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_When)
   then
       Token_Pos34 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos23 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos23,
             Expected_Token_Id => Gpr_When,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos34 := Row_Pos23 + 1;
   end if;
end;

--  End tok_code




if Token_Pos34 /= No_Token_Index then

   Row_Pos23 := Token_Pos34;

else
   Row_Pos23 := No_Token_Index;
   goto Exit_Row23_0;

end if;


Defer_Res31 :=
   Discrete_Choice_List_List_Parse0 (Parser, Row_Pos23);
Defer_Pos31 := Parser.Current_Pos;




if Defer_Pos31 /= No_Token_Index then

   Row_Pos23 := Defer_Pos31;

else
   Row_Pos23 := No_Token_Index;
   goto Exit_Row23_0;

end if;


--  Start tok_code

Token_Res35 := Row_Pos23;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res35));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Arrow)
   then
       Token_Pos35 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos23 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos23,
             Expected_Token_Id => Gpr_Arrow,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos35 := Row_Pos23 + 1;
   end if;
end;

--  End tok_code




if Token_Pos35 /= No_Token_Index then

   Row_Pos23 := Token_Pos35;

else
   Row_Pos23 := No_Token_Index;
   goto Exit_Row23_0;

end if;


Defer_Res32 :=
   Simple_Declarative_Items_List_Parse0 (Parser, Row_Pos23);
Defer_Pos32 := Parser.Current_Pos;




if Defer_Pos32 /= No_Token_Index then

   Row_Pos23 := Defer_Pos32;

else
   Row_Pos23 := No_Token_Index;
   goto Exit_Row23_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row23_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos23 /= No_Token_Index then

   Transform_Res21 := Allocate_Case_Item (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res21,
      Kind => Gpr_Case_Item,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos23 = Pos
                            then No_Token_Index
                            else Row_Pos23 - 1));

      Initialize_Fields_For_Case_Item
        (Self => Transform_Res21, Case_Item_F_Choice => Defer_Res31, Case_Item_F_Decls => Defer_Res32);

         if Defer_Res31 /= null and then Is_Incomplete (Defer_Res31) then
            Transform_Res21.Last_Attempted_Child := 0;
         elsif Defer_Res31 /= null and then not Is_Ghost (Defer_Res31) then
            Transform_Res21.Last_Attempted_Child := -1;
         end if;
         if Defer_Res32 /= null and then Is_Incomplete (Defer_Res32) then
            Transform_Res21.Last_Attempted_Child := 0;
         elsif Defer_Res32 /= null and then not Is_Ghost (Defer_Res32) then
            Transform_Res21.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos23 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags21);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Case_Item_Transform_Parse0_Memo,
      Row_Pos23 /= No_Token_Index,
      Transform_Res21,
      Pos,
      Row_Pos23);


   Parser.Current_Pos := Row_Pos23;

   return Transform_Res21;
end Case_Item_Transform_Parse0;

   


function Choice_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node
is
   use Bare_Gpr_Node_Memos;

      Defer_Pos33 :
            Token_Index
               := No_Token_Index;
      Defer_Res33 :
            Bare_String_Literal
               := No_Bare_Gpr_Node;
      Defer_Pos34 :
            Token_Index
               := No_Token_Index;
      Defer_Res34 :
            Bare_Others_Designator
               := No_Bare_Gpr_Node;
      Or_Pos8 :
            Token_Index
               := No_Token_Index;
      Or_Res8 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Choice_Or_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res8 := M.Instance;
      return Or_Res8;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Or_Res8;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos8 := No_Token_Index;
Or_Res8 := No_Bare_Gpr_Node;
    
Defer_Res33 :=
   String_Literal_Transform_Parse0 (Parser, Pos);
Defer_Pos33 := Parser.Current_Pos;

    if Defer_Pos33 /= No_Token_Index then
        Or_Pos8 := Defer_Pos33;
        Or_Res8 := Defer_Res33;
        goto Exit_Or10;
    end if;
    
Defer_Res34 :=
   Others_Designator_Transform_Parse0 (Parser, Pos);
Defer_Pos34 := Parser.Current_Pos;

    if Defer_Pos34 /= No_Token_Index then
        Or_Pos8 := Defer_Pos34;
        Or_Res8 := Defer_Res34;
        goto Exit_Or10;
    end if;
<<Exit_Or10>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Choice_Or_Parse0_Memo,
      Or_Pos8 /= No_Token_Index,
      Or_Res8,
      Pos,
      Or_Pos8);


   Parser.Current_Pos := Or_Pos8;

   return Or_Res8;
end Choice_Or_Parse0;

   


function Compilation_Unit_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Compilation_Unit
is
   use Bare_Compilation_Unit_Memos;

      Row_Pos24 :
            Token_Index
               := No_Token_Index;
      Defer_Pos35 :
            Token_Index
               := No_Token_Index;
      Defer_Res35 :
            Bare_Project
               := No_Bare_Gpr_Node;
      Transform_Res22 :
            Bare_Compilation_Unit
               := No_Bare_Gpr_Node;
      Transform_Diags22 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Compilation_Unit_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res22 := M.Instance;
      return Transform_Res22;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res22;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags22 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos24 := Pos;



Defer_Res35 :=
   Project_Transform_Parse0 (Parser, Row_Pos24);
Defer_Pos35 := Parser.Current_Pos;




if Defer_Pos35 /= No_Token_Index then

   Row_Pos24 := Defer_Pos35;

else
   Row_Pos24 := No_Token_Index;
   goto Exit_Row24_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row24_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos24 /= No_Token_Index then

   Transform_Res22 := Allocate_Compilation_Unit (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res22,
      Kind => Gpr_Compilation_Unit,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos24 = Pos
                            then No_Token_Index
                            else Row_Pos24 - 1));

      Initialize_Fields_For_Compilation_Unit
        (Self => Transform_Res22, Compilation_Unit_F_Project => Defer_Res35);

         if Defer_Res35 /= null and then Is_Incomplete (Defer_Res35) then
            Transform_Res22.Last_Attempted_Child := 0;
         elsif Defer_Res35 /= null and then not Is_Ghost (Defer_Res35) then
            Transform_Res22.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos24 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags22);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Compilation_Unit_Transform_Parse0_Memo,
      Row_Pos24 /= No_Token_Index,
      Transform_Res22,
      Pos,
      Row_Pos24);


   Parser.Current_Pos := Row_Pos24;

   return Transform_Res22;
end Compilation_Unit_Transform_Parse0;

   


function Context_Clauses_List_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_With_Decl_List
is
   use Bare_With_Decl_List_Memos;

      Lst_Cpos5 :
            Token_Index
               := No_Token_Index;
      Tmp_List5 :
            Free_Parse_List;
      Defer_Pos36 :
            Token_Index
               := No_Token_Index;
      Defer_Res36 :
            Bare_With_Decl
               := No_Bare_Gpr_Node;
      List_Pos5 :
            Token_Index
               := No_Token_Index;
      List_Res5 :
            Bare_With_Decl_List
               := No_Bare_Gpr_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Context_Clauses_List_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res5 := M.Instance;
      return List_Res5;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return List_Res5;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start list_code

    List_Pos5 := Pos;



Lst_Cpos5 := Pos;
Tmp_List5 := Get_Parse_List (Parser);

loop
   
Defer_Res36 :=
   With_Decl_Transform_Parse0 (Parser, Lst_Cpos5);
Defer_Pos36 := Parser.Current_Pos;


   exit when Defer_Pos36 = No_Token_Index;

   List_Pos5 := Defer_Pos36;
   Lst_Cpos5 := List_Pos5;

   Tmp_List5.Nodes.Append (Defer_Res36);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List5.Nodes.Length;
begin
   List_Res5 :=
      Allocate_With_Decl_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Pos;
      Token_End := (if Lst_Cpos5 = Pos
                    then Pos
                    else Lst_Cpos5 - 1);

   else
      Token_Start := Token_Index'Max (Pos, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res5,
      Kind              => Gpr_With_Decl_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res5,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List5.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res5.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List5);

--  End list_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Context_Clauses_List_Parse0_Memo,
      List_Pos5 /= No_Token_Index,
      List_Res5,
      Pos,
      List_Pos5);


   Parser.Current_Pos := List_Pos5;

   return List_Res5;
end Context_Clauses_List_Parse0;

   


function Declarative_Item_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node
is
   use Bare_Gpr_Node_Memos;

      Defer_Pos37 :
            Token_Index
               := No_Token_Index;
      Defer_Res37 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;
      Defer_Pos38 :
            Token_Index
               := No_Token_Index;
      Defer_Res38 :
            Bare_Typed_String_Decl
               := No_Bare_Gpr_Node;
      Defer_Pos39 :
            Token_Index
               := No_Token_Index;
      Defer_Res39 :
            Bare_Package_Decl
               := No_Bare_Gpr_Node;
      Or_Pos9 :
            Token_Index
               := No_Token_Index;
      Or_Res9 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Declarative_Item_Or_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res9 := M.Instance;
      return Or_Res9;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Or_Res9;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos9 := No_Token_Index;
Or_Res9 := No_Bare_Gpr_Node;
    
Defer_Res37 :=
   Simple_Declarative_Item_Or_Parse0 (Parser, Pos);
Defer_Pos37 := Parser.Current_Pos;

    if Defer_Pos37 /= No_Token_Index then
        Or_Pos9 := Defer_Pos37;
        Or_Res9 := Defer_Res37;
        goto Exit_Or11;
    end if;
    
Defer_Res38 :=
   Typed_String_Decl_Transform_Parse0 (Parser, Pos);
Defer_Pos38 := Parser.Current_Pos;

    if Defer_Pos38 /= No_Token_Index then
        Or_Pos9 := Defer_Pos38;
        Or_Res9 := Defer_Res38;
        goto Exit_Or11;
    end if;
    
Defer_Res39 :=
   Package_Decl_Transform_Parse0 (Parser, Pos);
Defer_Pos39 := Parser.Current_Pos;

    if Defer_Pos39 /= No_Token_Index then
        Or_Pos9 := Defer_Pos39;
        Or_Res9 := Defer_Res39;
        goto Exit_Or11;
    end if;
<<Exit_Or11>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Declarative_Item_Or_Parse0_Memo,
      Or_Pos9 /= No_Token_Index,
      Or_Res9,
      Pos,
      Or_Pos9);


   Parser.Current_Pos := Or_Pos9;

   return Or_Res9;
end Declarative_Item_Or_Parse0;

   


function Declarative_Items_List_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node_List
is
   use Bare_Gpr_Node_List_Memos;

      Lst_Cpos6 :
            Token_Index
               := No_Token_Index;
      Tmp_List6 :
            Free_Parse_List;
      Defer_Pos40 :
            Token_Index
               := No_Token_Index;
      Defer_Res40 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;
      List_Pos6 :
            Token_Index
               := No_Token_Index;
      List_Res6 :
            Bare_Gpr_Node_List
               := No_Bare_Gpr_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Declarative_Items_List_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res6 := M.Instance;
      return List_Res6;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return List_Res6;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start list_code

    List_Pos6 := Pos;



Lst_Cpos6 := Pos;
Tmp_List6 := Get_Parse_List (Parser);

loop
   
Defer_Res40 :=
   Declarative_Item_Or_Parse0 (Parser, Lst_Cpos6);
Defer_Pos40 := Parser.Current_Pos;


   exit when Defer_Pos40 = No_Token_Index;

   List_Pos6 := Defer_Pos40;
   Lst_Cpos6 := List_Pos6;

   Tmp_List6.Nodes.Append (Defer_Res40);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List6.Nodes.Length;
begin
   List_Res6 :=
      Allocate_Gpr_Node_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Pos;
      Token_End := (if Lst_Cpos6 = Pos
                    then Pos
                    else Lst_Cpos6 - 1);

   else
      Token_Start := Token_Index'Max (Pos, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res6,
      Kind              => Gpr_Gpr_Node_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res6,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List6.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res6.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List6);

--  End list_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Declarative_Items_List_Parse0_Memo,
      List_Pos6 /= No_Token_Index,
      List_Res6,
      Pos,
      List_Pos6);


   Parser.Current_Pos := List_Pos6;

   return List_Res6;
end Declarative_Items_List_Parse0;

   


function Discrete_Choice_List_List_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Choices
is
   use Bare_Choices_Memos;

      Lst_Cpos7 :
            Token_Index
               := No_Token_Index;
      Tmp_List7 :
            Free_Parse_List;
      Defer_Pos41 :
            Token_Index
               := No_Token_Index;
      Defer_Res41 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;
      Token_Pos36 :
            Token_Index
               := No_Token_Index;
      Token_Res36 :
            Token_Index
               := No_Token_Index;
      List_Pos7 :
            Token_Index
               := No_Token_Index;
      List_Res7 :
            Bare_Choices
               := No_Bare_Gpr_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Discrete_Choice_List_List_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res7 := M.Instance;
      return List_Res7;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return List_Res7;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start list_code

    List_Pos7 := No_Token_Index;



Lst_Cpos7 := Pos;
Tmp_List7 := Get_Parse_List (Parser);

loop
   
Defer_Res41 :=
   Choice_Or_Parse0 (Parser, Lst_Cpos7);
Defer_Pos41 := Parser.Current_Pos;


   exit when Defer_Pos41 = No_Token_Index;

   List_Pos7 := Defer_Pos41;
   Lst_Cpos7 := List_Pos7;

   Tmp_List7.Nodes.Append (Defer_Res41);

      
--  Start tok_code

Token_Res36 := Lst_Cpos7;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res36));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Pipe)
   then
       Token_Pos36 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos7 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos7,
             Expected_Token_Id => Gpr_Pipe,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos36 := Lst_Cpos7 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos36 /= No_Token_Index then
          Lst_Cpos7 := Token_Pos36;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List7.Nodes.Length;
begin
   List_Res7 :=
      Allocate_Choices (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Pos;
      Token_End := (if Lst_Cpos7 = Pos
                    then Pos
                    else Lst_Cpos7 - 1);

   else
      Token_Start := Token_Index'Max (Pos, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res7,
      Kind              => Gpr_Choices,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res7,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List7.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res7.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List7);

--  End list_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Discrete_Choice_List_List_Parse0_Memo,
      List_Pos7 /= No_Token_Index,
      List_Res7,
      Pos,
      List_Pos7);


   Parser.Current_Pos := List_Pos7;

   return List_Res7;
end Discrete_Choice_List_List_Parse0;

   


function Dontskip_Ada_Library_Item0_Extract_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node
is
   use Bare_Gpr_Node_Memos;

      Row_Pos25 :
            Token_Index
               := No_Token_Index;
      Null_Res0 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;
      Token_Pos37 :
            Token_Index
               := No_Token_Index;
      Token_Res37 :
            Token_Index
               := No_Token_Index;
      Token_Pos38 :
            Token_Index
               := No_Token_Index;
      Token_Res38 :
            Token_Index
               := No_Token_Index;
      Token_Pos39 :
            Token_Index
               := No_Token_Index;
      Token_Res39 :
            Token_Index
               := No_Token_Index;
      Or_Pos10 :
            Token_Index
               := No_Token_Index;
      Or_Res10 :
            Token_Index
               := No_Token_Index;


   M : Memo_Entry := Get (Parser.Private_Part.Dontskip_Ada_Library_Item0_Extract_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Null_Res0 := M.Instance;
      return Null_Res0;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Null_Res0;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start row_code

Row_Pos25 := Pos;



   Null_Res0 := No_Bare_Gpr_Node;




if Row_Pos25 /= No_Token_Index then

   Row_Pos25 := Row_Pos25;

else
   Row_Pos25 := No_Token_Index;
   goto Exit_Row25_0;

end if;


--  Start or_code

Or_Pos10 := No_Token_Index;
Or_Res10 := No_Token_Index;
    
--  Start tok_code

Token_Res37 := Row_Pos25;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res37));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
      or else T.Symbol /= Precomputed_Symbol
        (Precomputed_Symbol_Table (Parser.TDH.Symbols),
         Precomputed_Sym_Procedure)
   then
       Token_Pos37 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos25 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos25,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos37 := Row_Pos25 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos37 /= No_Token_Index then
        Or_Pos10 := Token_Pos37;
        Or_Res10 := Token_Res37;
        goto Exit_Or12;
    end if;
    
--  Start tok_code

Token_Res38 := Row_Pos25;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res38));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
      or else T.Symbol /= Precomputed_Symbol
        (Precomputed_Symbol_Table (Parser.TDH.Symbols),
         Precomputed_Sym_Function)
   then
       Token_Pos38 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos25 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos25,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos38 := Row_Pos25 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos38 /= No_Token_Index then
        Or_Pos10 := Token_Pos38;
        Or_Res10 := Token_Res38;
        goto Exit_Or12;
    end if;
    
--  Start tok_code

Token_Res39 := Row_Pos25;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res39));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Package)
   then
       Token_Pos39 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos25 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos25,
             Expected_Token_Id => Gpr_Package,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos39 := Row_Pos25 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos39 /= No_Token_Index then
        Or_Pos10 := Token_Pos39;
        Or_Res10 := Token_Res39;
        goto Exit_Or12;
    end if;
<<Exit_Or12>>

--  End or_code




if Or_Pos10 /= No_Token_Index then

   Row_Pos25 := Or_Pos10;

else
   Row_Pos25 := No_Token_Index;
   goto Exit_Row25_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row25_0>>
pragma Warnings (On, "referenced");

--  End row_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Dontskip_Ada_Library_Item0_Extract_Parse0_Memo,
      Row_Pos25 /= No_Token_Index,
      Null_Res0,
      Pos,
      Row_Pos25);


   Parser.Current_Pos := Row_Pos25;

   return Null_Res0;
end Dontskip_Ada_Library_Item0_Extract_Parse0;

   


function Dontskip_Ada_Library_Item1_Extract_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node
is
   use Bare_Gpr_Node_Memos;

      Row_Pos26 :
            Token_Index
               := No_Token_Index;
      Null_Res1 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;
      Token_Pos40 :
            Token_Index
               := No_Token_Index;
      Token_Res40 :
            Token_Index
               := No_Token_Index;


   M : Memo_Entry := Get (Parser.Private_Part.Dontskip_Ada_Library_Item1_Extract_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Null_Res1 := M.Instance;
      return Null_Res1;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Null_Res1;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start row_code

Row_Pos26 := Pos;



   Null_Res1 := No_Bare_Gpr_Node;




if Row_Pos26 /= No_Token_Index then

   Row_Pos26 := Row_Pos26;

else
   Row_Pos26 := No_Token_Index;
   goto Exit_Row26_0;

end if;


--  Start tok_code

Token_Res40 := Row_Pos26;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res40));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Semicolon)
   then
       Token_Pos40 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos26 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos26,
             Expected_Token_Id => Gpr_Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos40 := Row_Pos26 + 1;
   end if;
end;

--  End tok_code




if Token_Pos40 /= No_Token_Index then

   Row_Pos26 := Token_Pos40;

else
   Row_Pos26 := No_Token_Index;
   goto Exit_Row26_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row26_0>>
pragma Warnings (On, "referenced");

--  End row_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Dontskip_Ada_Library_Item1_Extract_Parse0_Memo,
      Row_Pos26 /= No_Token_Index,
      Null_Res1,
      Pos,
      Row_Pos26);


   Parser.Current_Pos := Row_Pos26;

   return Null_Res1;
end Dontskip_Ada_Library_Item1_Extract_Parse0;

   


function Dontskip_Ada_Library_Item2_Extract_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node
is
   use Bare_Gpr_Node_Memos;

      Row_Pos27 :
            Token_Index
               := No_Token_Index;
      Null_Res2 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;
      Token_Pos41 :
            Token_Index
               := No_Token_Index;
      Token_Res41 :
            Token_Index
               := No_Token_Index;


   M : Memo_Entry := Get (Parser.Private_Part.Dontskip_Ada_Library_Item2_Extract_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Null_Res2 := M.Instance;
      return Null_Res2;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Null_Res2;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start row_code

Row_Pos27 := Pos;



   Null_Res2 := No_Bare_Gpr_Node;




if Row_Pos27 /= No_Token_Index then

   Row_Pos27 := Row_Pos27;

else
   Row_Pos27 := No_Token_Index;
   goto Exit_Row27_0;

end if;


--  Start tok_code

Token_Res41 := Row_Pos27;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res41));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Semicolon)
   then
       Token_Pos41 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos27 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos27,
             Expected_Token_Id => Gpr_Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos41 := Row_Pos27 + 1;
   end if;
end;

--  End tok_code




if Token_Pos41 /= No_Token_Index then

   Row_Pos27 := Token_Pos41;

else
   Row_Pos27 := No_Token_Index;
   goto Exit_Row27_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row27_0>>
pragma Warnings (On, "referenced");

--  End row_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Dontskip_Ada_Library_Item2_Extract_Parse0_Memo,
      Row_Pos27 /= No_Token_Index,
      Null_Res2,
      Pos,
      Row_Pos27);


   Parser.Current_Pos := Row_Pos27;

   return Null_Res2;
end Dontskip_Ada_Library_Item2_Extract_Parse0;

   


function Dontskip_Ada_Pragma0_Extract_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node
is
   use Bare_Gpr_Node_Memos;

      Row_Pos28 :
            Token_Index
               := No_Token_Index;
      Null_Res3 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;
      Token_Pos42 :
            Token_Index
               := No_Token_Index;
      Token_Res42 :
            Token_Index
               := No_Token_Index;


   M : Memo_Entry := Get (Parser.Private_Part.Dontskip_Ada_Pragma0_Extract_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Null_Res3 := M.Instance;
      return Null_Res3;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Null_Res3;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start row_code

Row_Pos28 := Pos;



   Null_Res3 := No_Bare_Gpr_Node;




if Row_Pos28 /= No_Token_Index then

   Row_Pos28 := Row_Pos28;

else
   Row_Pos28 := No_Token_Index;
   goto Exit_Row28_0;

end if;


--  Start tok_code

Token_Res42 := Row_Pos28;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res42));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Semicolon)
   then
       Token_Pos42 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos28 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos28,
             Expected_Token_Id => Gpr_Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos42 := Row_Pos28 + 1;
   end if;
end;

--  End tok_code




if Token_Pos42 /= No_Token_Index then

   Row_Pos28 := Token_Pos42;

else
   Row_Pos28 := No_Token_Index;
   goto Exit_Row28_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row28_0>>
pragma Warnings (On, "referenced");

--  End row_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Dontskip_Ada_Pragma0_Extract_Parse0_Memo,
      Row_Pos28 /= No_Token_Index,
      Null_Res3,
      Pos,
      Row_Pos28);


   Parser.Current_Pos := Row_Pos28;

   return Null_Res3;
end Dontskip_Ada_Pragma0_Extract_Parse0;

   


function Dontskip_Ada_Use_Clause0_Extract_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node
is
   use Bare_Gpr_Node_Memos;

      Row_Pos29 :
            Token_Index
               := No_Token_Index;
      Null_Res4 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;
      Token_Pos43 :
            Token_Index
               := No_Token_Index;
      Token_Res43 :
            Token_Index
               := No_Token_Index;


   M : Memo_Entry := Get (Parser.Private_Part.Dontskip_Ada_Use_Clause0_Extract_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Null_Res4 := M.Instance;
      return Null_Res4;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Null_Res4;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start row_code

Row_Pos29 := Pos;



   Null_Res4 := No_Bare_Gpr_Node;




if Row_Pos29 /= No_Token_Index then

   Row_Pos29 := Row_Pos29;

else
   Row_Pos29 := No_Token_Index;
   goto Exit_Row29_0;

end if;


--  Start tok_code

Token_Res43 := Row_Pos29;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res43));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Semicolon)
   then
       Token_Pos43 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos29 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos29,
             Expected_Token_Id => Gpr_Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos43 := Row_Pos29 + 1;
   end if;
end;

--  End tok_code




if Token_Pos43 /= No_Token_Index then

   Row_Pos29 := Token_Pos43;

else
   Row_Pos29 := No_Token_Index;
   goto Exit_Row29_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row29_0>>
pragma Warnings (On, "referenced");

--  End row_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Dontskip_Ada_Use_Clause0_Extract_Parse0_Memo,
      Row_Pos29 /= No_Token_Index,
      Null_Res4,
      Pos,
      Row_Pos29);


   Parser.Current_Pos := Row_Pos29;

   return Null_Res4;
end Dontskip_Ada_Use_Clause0_Extract_Parse0;

   


function Empty_Declaration_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Empty_Decl
is
   use Bare_Empty_Decl_Memos;

      Row_Pos30 :
            Token_Index
               := No_Token_Index;
      Token_Pos44 :
            Token_Index
               := No_Token_Index;
      Token_Res44 :
            Token_Index
               := No_Token_Index;
      Token_Pos45 :
            Token_Index
               := No_Token_Index;
      Token_Res45 :
            Token_Index
               := No_Token_Index;
      Transform_Res23 :
            Bare_Empty_Decl
               := No_Bare_Gpr_Node;
      Transform_Diags23 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Empty_Declaration_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res23 := M.Instance;
      return Transform_Res23;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res23;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags23 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos30 := Pos;



--  Start tok_code

Token_Res44 := Row_Pos30;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res44));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Null)
   then
       Token_Pos44 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos30 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos30,
             Expected_Token_Id => Gpr_Null,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos44 := Row_Pos30 + 1;
   end if;
end;

--  End tok_code




if Token_Pos44 /= No_Token_Index then

   Row_Pos30 := Token_Pos44;

else
   Row_Pos30 := No_Token_Index;
   goto Exit_Row30_0;

end if;


--  Start tok_code

Token_Res45 := Row_Pos30;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res45));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Semicolon)
   then
       Token_Pos45 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos30 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos30,
             Expected_Token_Id => Gpr_Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos45 := Row_Pos30 + 1;
   end if;
end;

--  End tok_code




if Token_Pos45 /= No_Token_Index then

   Row_Pos30 := Token_Pos45;

else
   Row_Pos30 := No_Token_Index;
   goto Exit_Row30_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row30_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos30 /= No_Token_Index then

   Transform_Res23 := Allocate_Empty_Decl (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res23,
      Kind => Gpr_Empty_Decl,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos30 = Pos
                            then No_Token_Index
                            else Row_Pos30 - 1));




elsif Row_Pos30 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags23);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Empty_Declaration_Transform_Parse0_Memo,
      Row_Pos30 /= No_Token_Index,
      Transform_Res23,
      Pos,
      Row_Pos30);


   Parser.Current_Pos := Row_Pos30;

   return Transform_Res23;
end Empty_Declaration_Transform_Parse0;

   


function Expression_List_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Term_List
is
   use Bare_Term_List_Memos;

      Lst_Cpos8 :
            Token_Index
               := No_Token_Index;
      Tmp_List8 :
            Free_Parse_List;
      Defer_Pos42 :
            Token_Index
               := No_Token_Index;
      Defer_Res42 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;
      Token_Pos46 :
            Token_Index
               := No_Token_Index;
      Token_Res46 :
            Token_Index
               := No_Token_Index;
      List_Pos8 :
            Token_Index
               := No_Token_Index;
      List_Res8 :
            Bare_Term_List
               := No_Bare_Gpr_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Expression_List_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res8 := M.Instance;
      return List_Res8;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return List_Res8;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start list_code

    List_Pos8 := No_Token_Index;



Lst_Cpos8 := Pos;
Tmp_List8 := Get_Parse_List (Parser);

loop
   
Defer_Res42 :=
   Term_Or_Parse0 (Parser, Lst_Cpos8);
Defer_Pos42 := Parser.Current_Pos;


   exit when Defer_Pos42 = No_Token_Index;

   List_Pos8 := Defer_Pos42;
   Lst_Cpos8 := List_Pos8;

   Tmp_List8.Nodes.Append (Defer_Res42);

      
--  Start tok_code

Token_Res46 := Lst_Cpos8;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res46));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Amp)
   then
       Token_Pos46 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos8 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos8,
             Expected_Token_Id => Gpr_Amp,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos46 := Lst_Cpos8 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos46 /= No_Token_Index then
          Lst_Cpos8 := Token_Pos46;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List8.Nodes.Length;
begin
   List_Res8 :=
      Allocate_Term_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Pos;
      Token_End := (if Lst_Cpos8 = Pos
                    then Pos
                    else Lst_Cpos8 - 1);

   else
      Token_Start := Token_Index'Max (Pos, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res8,
      Kind              => Gpr_Term_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res8,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List8.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res8.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List8);

--  End list_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Expression_List_Parse0_Memo,
      List_Pos8 /= No_Token_Index,
      List_Res8,
      Pos,
      List_Pos8);


   Parser.Current_Pos := List_Pos8;

   return List_Res8;
end Expression_List_Parse0;

   


function Expression_List_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Terms
is
   use Bare_Terms_Memos;

      Row_Pos31 :
            Token_Index
               := No_Token_Index;
      Token_Pos47 :
            Token_Index
               := No_Token_Index;
      Token_Res47 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos9 :
            Token_Index
               := No_Token_Index;
      Tmp_List9 :
            Free_Parse_List;
      Defer_Pos43 :
            Token_Index
               := No_Token_Index;
      Defer_Res43 :
            Bare_Term_List
               := No_Bare_Gpr_Node;
      Token_Pos48 :
            Token_Index
               := No_Token_Index;
      Token_Res48 :
            Token_Index
               := No_Token_Index;
      List_Pos9 :
            Token_Index
               := No_Token_Index;
      List_Res9 :
            Bare_Term_List_List
               := No_Bare_Gpr_Node;
      Token_Pos49 :
            Token_Index
               := No_Token_Index;
      Token_Res49 :
            Token_Index
               := No_Token_Index;
      Transform_Res24 :
            Bare_Terms
               := No_Bare_Gpr_Node;
      Transform_Diags24 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Expression_List_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res24 := M.Instance;
      return Transform_Res24;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res24;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags24 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos31 := Pos;



--  Start tok_code

Token_Res47 := Row_Pos31;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res47));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Par_Open)
   then
       Token_Pos47 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos31 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos31,
             Expected_Token_Id => Gpr_Par_Open,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos47 := Row_Pos31 + 1;
   end if;
end;

--  End tok_code




if Token_Pos47 /= No_Token_Index then

   Row_Pos31 := Token_Pos47;

else
   Row_Pos31 := No_Token_Index;
   goto Exit_Row31_0;

end if;


--  Start list_code

    List_Pos9 := Row_Pos31;



Lst_Cpos9 := Row_Pos31;
Tmp_List9 := Get_Parse_List (Parser);

loop
   
Defer_Res43 :=
   Expression_List_Parse0 (Parser, Lst_Cpos9);
Defer_Pos43 := Parser.Current_Pos;


   exit when Defer_Pos43 = No_Token_Index;

   List_Pos9 := Defer_Pos43;
   Lst_Cpos9 := List_Pos9;

   Tmp_List9.Nodes.Append (Defer_Res43);

      
--  Start tok_code

Token_Res48 := Lst_Cpos9;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res48));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Comma)
   then
       Token_Pos48 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos9 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos9,
             Expected_Token_Id => Gpr_Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos48 := Lst_Cpos9 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos48 /= No_Token_Index then
          Lst_Cpos9 := Token_Pos48;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List9.Nodes.Length;
begin
   List_Res9 :=
      Allocate_Term_List_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos31;
      Token_End := (if Lst_Cpos9 = Row_Pos31
                    then Row_Pos31
                    else Lst_Cpos9 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos31, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res9,
      Kind              => Gpr_Term_List_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res9,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List9.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res9.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List9);

--  End list_code




if List_Pos9 /= No_Token_Index then

   Row_Pos31 := List_Pos9;

else
   Row_Pos31 := No_Token_Index;
   goto Exit_Row31_0;

end if;


--  Start tok_code

Token_Res49 := Row_Pos31;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res49));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Par_Close)
   then
       Token_Pos49 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos31 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos31,
             Expected_Token_Id => Gpr_Par_Close,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos49 := Row_Pos31 + 1;
   end if;
end;

--  End tok_code




if Token_Pos49 /= No_Token_Index then

   Row_Pos31 := Token_Pos49;

else
   Row_Pos31 := No_Token_Index;
   goto Exit_Row31_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row31_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos31 /= No_Token_Index then

   Transform_Res24 := Allocate_Terms (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res24,
      Kind => Gpr_Terms,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos31 = Pos
                            then No_Token_Index
                            else Row_Pos31 - 1));

      Initialize_Fields_For_Terms
        (Self => Transform_Res24, Terms_F_Terms => List_Res9);

         if List_Res9 /= null and then Is_Incomplete (List_Res9) then
            Transform_Res24.Last_Attempted_Child := 0;
         elsif List_Res9 /= null and then not Is_Ghost (List_Res9) then
            Transform_Res24.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos31 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags24);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Expression_List_Transform_Parse0_Memo,
      Row_Pos31 /= No_Token_Index,
      Transform_Res24,
      Pos,
      Row_Pos31);


   Parser.Current_Pos := Row_Pos31;

   return Transform_Res24;
end Expression_List_Transform_Parse0;

   


function Identifier_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Identifier
is
   use Bare_Identifier_Memos;

      Row_Pos32 :
            Token_Index
               := No_Token_Index;
      Token_Pos50 :
            Token_Index
               := No_Token_Index;
      Token_Res50 :
            Token_Index
               := No_Token_Index;
      Transform_Res25 :
            Bare_Identifier
               := No_Bare_Gpr_Node;
      Transform_Diags25 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Identifier_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res25 := M.Instance;
      return Transform_Res25;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res25;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags25 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos32 := Pos;



--  Start tok_code

Token_Res50 := Row_Pos32;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res50));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
   then
       Token_Pos50 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos32 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos32,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos50 := Row_Pos32 + 1;
   end if;
end;

--  End tok_code




if Token_Pos50 /= No_Token_Index then

   Row_Pos32 := Token_Pos50;

else
   Row_Pos32 := No_Token_Index;
   goto Exit_Row32_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row32_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos32 /= No_Token_Index then

   Transform_Res25 := Allocate_Identifier (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res25,
      Kind => Gpr_Identifier,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos32 = Pos
                            then No_Token_Index
                            else Row_Pos32 - 1));




elsif Row_Pos32 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags25);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Identifier_Transform_Parse0_Memo,
      Row_Pos32 /= No_Token_Index,
      Transform_Res25,
      Pos,
      Row_Pos32);


   Parser.Current_Pos := Row_Pos32;

   return Transform_Res25;
end Identifier_Transform_Parse0;

   


function Num_Literal_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Num_Literal
is
   use Bare_Num_Literal_Memos;

      Row_Pos33 :
            Token_Index
               := No_Token_Index;
      Token_Pos51 :
            Token_Index
               := No_Token_Index;
      Token_Res51 :
            Token_Index
               := No_Token_Index;
      Transform_Res26 :
            Bare_Num_Literal
               := No_Bare_Gpr_Node;
      Transform_Diags26 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Num_Literal_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res26 := M.Instance;
      return Transform_Res26;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res26;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags26 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos33 := Pos;



--  Start tok_code

Token_Res51 := Row_Pos33;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res51));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Number)
   then
       Token_Pos51 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos33 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos33,
             Expected_Token_Id => Gpr_Number,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos51 := Row_Pos33 + 1;
   end if;
end;

--  End tok_code




if Token_Pos51 /= No_Token_Index then

   Row_Pos33 := Token_Pos51;

else
   Row_Pos33 := No_Token_Index;
   goto Exit_Row33_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row33_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos33 /= No_Token_Index then

   Transform_Res26 := Allocate_Num_Literal (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res26,
      Kind => Gpr_Num_Literal,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos33 = Pos
                            then No_Token_Index
                            else Row_Pos33 - 1));




elsif Row_Pos33 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags26);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Num_Literal_Transform_Parse0_Memo,
      Row_Pos33 /= No_Token_Index,
      Transform_Res26,
      Pos,
      Row_Pos33);


   Parser.Current_Pos := Row_Pos33;

   return Transform_Res26;
end Num_Literal_Transform_Parse0;

   


function Others_Designator_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Others_Designator
is
   use Bare_Others_Designator_Memos;

      Row_Pos34 :
            Token_Index
               := No_Token_Index;
      Token_Pos52 :
            Token_Index
               := No_Token_Index;
      Token_Res52 :
            Token_Index
               := No_Token_Index;
      Transform_Res27 :
            Bare_Others_Designator
               := No_Bare_Gpr_Node;
      Transform_Diags27 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Others_Designator_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res27 := M.Instance;
      return Transform_Res27;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res27;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags27 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos34 := Pos;



--  Start tok_code

Token_Res52 := Row_Pos34;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res52));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Others)
   then
       Token_Pos52 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos34 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos34,
             Expected_Token_Id => Gpr_Others,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos52 := Row_Pos34 + 1;
   end if;
end;

--  End tok_code




if Token_Pos52 /= No_Token_Index then

   Row_Pos34 := Token_Pos52;

else
   Row_Pos34 := No_Token_Index;
   goto Exit_Row34_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row34_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos34 /= No_Token_Index then

   Transform_Res27 := Allocate_Others_Designator (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res27,
      Kind => Gpr_Others_Designator,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos34 = Pos
                            then No_Token_Index
                            else Row_Pos34 - 1));




elsif Row_Pos34 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags27);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Others_Designator_Transform_Parse0_Memo,
      Row_Pos34 /= No_Token_Index,
      Transform_Res27,
      Pos,
      Row_Pos34);


   Parser.Current_Pos := Row_Pos34;

   return Transform_Res27;
end Others_Designator_Transform_Parse0;

   


function Package_Decl_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Package_Decl
is
   use Bare_Package_Decl_Memos;

      Row_Pos35 :
            Token_Index
               := No_Token_Index;
      Token_Pos53 :
            Token_Index
               := No_Token_Index;
      Token_Res53 :
            Token_Index
               := No_Token_Index;
      Defer_Pos44 :
            Token_Index
               := No_Token_Index;
      Defer_Res44 :
            Bare_Identifier
               := No_Bare_Gpr_Node;
      Defer_Pos45 :
            Token_Index
               := No_Token_Index;
      Defer_Res45 :
            Bare_Package_Renaming
               := No_Bare_Gpr_Node;
      Defer_Pos46 :
            Token_Index
               := No_Token_Index;
      Defer_Res46 :
            Bare_Package_Spec
               := No_Bare_Gpr_Node;
      Or_Pos11 :
            Token_Index
               := No_Token_Index;
      Or_Res11 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;
      Token_Pos54 :
            Token_Index
               := No_Token_Index;
      Token_Res54 :
            Token_Index
               := No_Token_Index;
      Transform_Res28 :
            Bare_Package_Decl
               := No_Bare_Gpr_Node;
      Transform_Diags28 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Package_Decl_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res28 := M.Instance;
      return Transform_Res28;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res28;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags28 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos35 := Pos;



--  Start tok_code

Token_Res53 := Row_Pos35;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res53));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Package)
   then
       Token_Pos53 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos35 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos35,
             Expected_Token_Id => Gpr_Package,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos53 := Row_Pos35 + 1;
   end if;
end;

--  End tok_code




if Token_Pos53 /= No_Token_Index then

   Row_Pos35 := Token_Pos53;

else
   Row_Pos35 := No_Token_Index;
   goto Exit_Row35_0;

end if;


Defer_Res44 :=
   Identifier_Transform_Parse0 (Parser, Row_Pos35);
Defer_Pos44 := Parser.Current_Pos;




if Defer_Pos44 /= No_Token_Index then

   Row_Pos35 := Defer_Pos44;

else
   Row_Pos35 := No_Token_Index;
   goto Exit_Row35_0;

end if;


--  Start or_code

Or_Pos11 := No_Token_Index;
Or_Res11 := No_Bare_Gpr_Node;
    
Defer_Res45 :=
   Package_Renaming_Transform_Parse0 (Parser, Row_Pos35);
Defer_Pos45 := Parser.Current_Pos;

    if Defer_Pos45 /= No_Token_Index then
        Or_Pos11 := Defer_Pos45;
        Or_Res11 := Defer_Res45;
        goto Exit_Or13;
    end if;
    
Defer_Res46 :=
   Package_Spec_Transform_Parse0 (Parser, Row_Pos35);
Defer_Pos46 := Parser.Current_Pos;

    if Defer_Pos46 /= No_Token_Index then
        Or_Pos11 := Defer_Pos46;
        Or_Res11 := Defer_Res46;
        goto Exit_Or13;
    end if;
<<Exit_Or13>>

--  End or_code




if Or_Pos11 /= No_Token_Index then

   Row_Pos35 := Or_Pos11;

else
   Row_Pos35 := No_Token_Index;
   goto Exit_Row35_0;

end if;


--  Start tok_code

Token_Res54 := Row_Pos35;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res54));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Semicolon)
   then
       Token_Pos54 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos35 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos35,
             Expected_Token_Id => Gpr_Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos54 := Row_Pos35 + 1;
   end if;
end;

--  End tok_code




if Token_Pos54 /= No_Token_Index then

   Row_Pos35 := Token_Pos54;

else
   Row_Pos35 := No_Token_Index;
   goto Exit_Row35_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row35_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos35 /= No_Token_Index then

   Transform_Res28 := Allocate_Package_Decl (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res28,
      Kind => Gpr_Package_Decl,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos35 = Pos
                            then No_Token_Index
                            else Row_Pos35 - 1));

      Initialize_Fields_For_Package_Decl
        (Self => Transform_Res28, Package_Decl_F_Pkg_Name => Defer_Res44, Package_Decl_F_Pkg_Spec => Or_Res11);

         if Defer_Res44 /= null and then Is_Incomplete (Defer_Res44) then
            Transform_Res28.Last_Attempted_Child := 0;
         elsif Defer_Res44 /= null and then not Is_Ghost (Defer_Res44) then
            Transform_Res28.Last_Attempted_Child := -1;
         end if;
         if Or_Res11 /= null and then Is_Incomplete (Or_Res11) then
            Transform_Res28.Last_Attempted_Child := 0;
         elsif Or_Res11 /= null and then not Is_Ghost (Or_Res11) then
            Transform_Res28.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos35 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags28);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Package_Decl_Transform_Parse0_Memo,
      Row_Pos35 /= No_Token_Index,
      Transform_Res28,
      Pos,
      Row_Pos35);


   Parser.Current_Pos := Row_Pos35;

   return Transform_Res28;
end Package_Decl_Transform_Parse0;

   


function Package_Extension_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Package_Extension
is
   use Bare_Package_Extension_Memos;

      Row_Pos36 :
            Token_Index
               := No_Token_Index;
      Token_Pos55 :
            Token_Index
               := No_Token_Index;
      Token_Res55 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos10 :
            Token_Index
               := No_Token_Index;
      Tmp_List10 :
            Free_Parse_List;
      Defer_Pos47 :
            Token_Index
               := No_Token_Index;
      Defer_Res47 :
            Bare_Identifier
               := No_Bare_Gpr_Node;
      Token_Pos56 :
            Token_Index
               := No_Token_Index;
      Token_Res56 :
            Token_Index
               := No_Token_Index;
      List_Pos10 :
            Token_Index
               := No_Token_Index;
      List_Res10 :
            Bare_Identifier_List
               := No_Bare_Gpr_Node;
      Transform_Res29 :
            Bare_Package_Extension
               := No_Bare_Gpr_Node;
      Transform_Diags29 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Package_Extension_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res29 := M.Instance;
      return Transform_Res29;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res29;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags29 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos36 := Pos;



--  Start tok_code

Token_Res55 := Row_Pos36;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res55));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Extends)
   then
       Token_Pos55 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos36 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos36,
             Expected_Token_Id => Gpr_Extends,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos55 := Row_Pos36 + 1;
   end if;
end;

--  End tok_code




if Token_Pos55 /= No_Token_Index then

   Row_Pos36 := Token_Pos55;

else
   Row_Pos36 := No_Token_Index;
   goto Exit_Row36_0;

end if;


--  Start list_code

    List_Pos10 := No_Token_Index;



Lst_Cpos10 := Row_Pos36;
Tmp_List10 := Get_Parse_List (Parser);

loop
   
Defer_Res47 :=
   Identifier_Transform_Parse0 (Parser, Lst_Cpos10);
Defer_Pos47 := Parser.Current_Pos;


   exit when Defer_Pos47 = No_Token_Index;

   List_Pos10 := Defer_Pos47;
   Lst_Cpos10 := List_Pos10;

   Tmp_List10.Nodes.Append (Defer_Res47);

      
--  Start tok_code

Token_Res56 := Lst_Cpos10;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res56));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Dot)
   then
       Token_Pos56 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos10 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos10,
             Expected_Token_Id => Gpr_Dot,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos56 := Lst_Cpos10 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos56 /= No_Token_Index then
          Lst_Cpos10 := Token_Pos56;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List10.Nodes.Length;
begin
   List_Res10 :=
      Allocate_Identifier_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos36;
      Token_End := (if Lst_Cpos10 = Row_Pos36
                    then Row_Pos36
                    else Lst_Cpos10 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos36, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res10,
      Kind              => Gpr_Identifier_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res10,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List10.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res10.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List10);

--  End list_code




if List_Pos10 /= No_Token_Index then

   Row_Pos36 := List_Pos10;

else
   Row_Pos36 := No_Token_Index;
   goto Exit_Row36_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row36_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos36 /= No_Token_Index then

   Transform_Res29 := Allocate_Package_Extension (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res29,
      Kind => Gpr_Package_Extension,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos36 = Pos
                            then No_Token_Index
                            else Row_Pos36 - 1));

      Initialize_Fields_For_Package_Extension
        (Self => Transform_Res29, Package_Extension_F_Extended_Name => List_Res10);

         if List_Res10 /= null and then Is_Incomplete (List_Res10) then
            Transform_Res29.Last_Attempted_Child := 0;
         elsif List_Res10 /= null and then not Is_Ghost (List_Res10) then
            Transform_Res29.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos36 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags29);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Package_Extension_Transform_Parse0_Memo,
      Row_Pos36 /= No_Token_Index,
      Transform_Res29,
      Pos,
      Row_Pos36);


   Parser.Current_Pos := Row_Pos36;

   return Transform_Res29;
end Package_Extension_Transform_Parse0;

   


function Package_Renaming_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Package_Renaming
is
   use Bare_Package_Renaming_Memos;

      Row_Pos37 :
            Token_Index
               := No_Token_Index;
      Token_Pos57 :
            Token_Index
               := No_Token_Index;
      Token_Res57 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos11 :
            Token_Index
               := No_Token_Index;
      Tmp_List11 :
            Free_Parse_List;
      Defer_Pos48 :
            Token_Index
               := No_Token_Index;
      Defer_Res48 :
            Bare_Identifier
               := No_Bare_Gpr_Node;
      Token_Pos58 :
            Token_Index
               := No_Token_Index;
      Token_Res58 :
            Token_Index
               := No_Token_Index;
      List_Pos11 :
            Token_Index
               := No_Token_Index;
      List_Res11 :
            Bare_Identifier_List
               := No_Bare_Gpr_Node;
      Transform_Res30 :
            Bare_Package_Renaming
               := No_Bare_Gpr_Node;
      Transform_Diags30 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Package_Renaming_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res30 := M.Instance;
      return Transform_Res30;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res30;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags30 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos37 := Pos;



--  Start tok_code

Token_Res57 := Row_Pos37;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res57));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Renames)
   then
       Token_Pos57 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos37 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos37,
             Expected_Token_Id => Gpr_Renames,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos57 := Row_Pos37 + 1;
   end if;
end;

--  End tok_code




if Token_Pos57 /= No_Token_Index then

   Row_Pos37 := Token_Pos57;

else
   Row_Pos37 := No_Token_Index;
   goto Exit_Row37_0;

end if;


--  Start list_code

    List_Pos11 := No_Token_Index;



Lst_Cpos11 := Row_Pos37;
Tmp_List11 := Get_Parse_List (Parser);

loop
   
Defer_Res48 :=
   Identifier_Transform_Parse0 (Parser, Lst_Cpos11);
Defer_Pos48 := Parser.Current_Pos;


   exit when Defer_Pos48 = No_Token_Index;

   List_Pos11 := Defer_Pos48;
   Lst_Cpos11 := List_Pos11;

   Tmp_List11.Nodes.Append (Defer_Res48);

      
--  Start tok_code

Token_Res58 := Lst_Cpos11;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res58));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Dot)
   then
       Token_Pos58 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos11 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos11,
             Expected_Token_Id => Gpr_Dot,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos58 := Lst_Cpos11 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos58 /= No_Token_Index then
          Lst_Cpos11 := Token_Pos58;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List11.Nodes.Length;
begin
   List_Res11 :=
      Allocate_Identifier_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos37;
      Token_End := (if Lst_Cpos11 = Row_Pos37
                    then Row_Pos37
                    else Lst_Cpos11 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos37, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res11,
      Kind              => Gpr_Identifier_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res11,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List11.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res11.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List11);

--  End list_code




if List_Pos11 /= No_Token_Index then

   Row_Pos37 := List_Pos11;

else
   Row_Pos37 := No_Token_Index;
   goto Exit_Row37_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row37_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos37 /= No_Token_Index then

   Transform_Res30 := Allocate_Package_Renaming (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res30,
      Kind => Gpr_Package_Renaming,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos37 = Pos
                            then No_Token_Index
                            else Row_Pos37 - 1));

      Initialize_Fields_For_Package_Renaming
        (Self => Transform_Res30, Package_Renaming_F_Renamed_Name => List_Res11);

         if List_Res11 /= null and then Is_Incomplete (List_Res11) then
            Transform_Res30.Last_Attempted_Child := 0;
         elsif List_Res11 /= null and then not Is_Ghost (List_Res11) then
            Transform_Res30.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos37 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags30);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Package_Renaming_Transform_Parse0_Memo,
      Row_Pos37 /= No_Token_Index,
      Transform_Res30,
      Pos,
      Row_Pos37);


   Parser.Current_Pos := Row_Pos37;

   return Transform_Res30;
end Package_Renaming_Transform_Parse0;

   


function Package_Spec_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Package_Spec
is
   use Bare_Package_Spec_Memos;

      Row_Pos38 :
            Token_Index
               := No_Token_Index;
      Defer_Pos49 :
            Token_Index
               := No_Token_Index;
      Defer_Res49 :
            Bare_Package_Extension
               := No_Bare_Gpr_Node;
      Token_Pos59 :
            Token_Index
               := No_Token_Index;
      Token_Res59 :
            Token_Index
               := No_Token_Index;
      Defer_Pos50 :
            Token_Index
               := No_Token_Index;
      Defer_Res50 :
            Bare_Gpr_Node_List
               := No_Bare_Gpr_Node;
      Token_Pos60 :
            Token_Index
               := No_Token_Index;
      Token_Res60 :
            Token_Index
               := No_Token_Index;
      Defer_Pos51 :
            Token_Index
               := No_Token_Index;
      Defer_Res51 :
            Bare_Identifier
               := No_Bare_Gpr_Node;
      Transform_Res31 :
            Bare_Package_Spec
               := No_Bare_Gpr_Node;
      Transform_Diags31 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Package_Spec_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res31 := M.Instance;
      return Transform_Res31;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res31;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags31 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos38 := Pos;



--  Start opt_code












Defer_Res49 :=
   Package_Extension_Transform_Parse0 (Parser, Row_Pos38);
Defer_Pos49 := Parser.Current_Pos;


if Defer_Pos49 = No_Token_Index then

         
   Defer_Res49 := No_Bare_Gpr_Node;



       
   Defer_Pos49 := Row_Pos38;



end if;

--  End opt_code




if Defer_Pos49 /= No_Token_Index then

   Row_Pos38 := Defer_Pos49;

else
   Row_Pos38 := No_Token_Index;
   goto Exit_Row38_0;

end if;


--  Start tok_code

Token_Res59 := Row_Pos38;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res59));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Is)
   then
       Token_Pos59 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos38 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos38,
             Expected_Token_Id => Gpr_Is,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos59 := Row_Pos38 + 1;
   end if;
end;

--  End tok_code




if Token_Pos59 /= No_Token_Index then

   Row_Pos38 := Token_Pos59;

else
   Row_Pos38 := No_Token_Index;
   goto Exit_Row38_0;

end if;


Defer_Res50 :=
   Simple_Declarative_Items_List_Parse0 (Parser, Row_Pos38);
Defer_Pos50 := Parser.Current_Pos;




if Defer_Pos50 /= No_Token_Index then

   Row_Pos38 := Defer_Pos50;

else
   Row_Pos38 := No_Token_Index;
   goto Exit_Row38_0;

end if;


--  Start tok_code

Token_Res60 := Row_Pos38;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res60));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_End)
   then
       Token_Pos60 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos38 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos38,
             Expected_Token_Id => Gpr_End,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos60 := Row_Pos38 + 1;
   end if;
end;

--  End tok_code




if Token_Pos60 /= No_Token_Index then

   Row_Pos38 := Token_Pos60;

else
   Row_Pos38 := No_Token_Index;
   goto Exit_Row38_0;

end if;


Defer_Res51 :=
   Identifier_Transform_Parse0 (Parser, Row_Pos38);
Defer_Pos51 := Parser.Current_Pos;




if Defer_Pos51 /= No_Token_Index then

   Row_Pos38 := Defer_Pos51;

else
   Row_Pos38 := No_Token_Index;
   goto Exit_Row38_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row38_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos38 /= No_Token_Index then

   Transform_Res31 := Allocate_Package_Spec (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res31,
      Kind => Gpr_Package_Spec,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos38 = Pos
                            then No_Token_Index
                            else Row_Pos38 - 1));

      Initialize_Fields_For_Package_Spec
        (Self => Transform_Res31, Package_Spec_F_Extension => Defer_Res49, Package_Spec_F_Decls => Defer_Res50, Package_Spec_F_End_Name => Defer_Res51);

         if Defer_Res49 /= null and then Is_Incomplete (Defer_Res49) then
            Transform_Res31.Last_Attempted_Child := 0;
         elsif Defer_Res49 /= null and then not Is_Ghost (Defer_Res49) then
            Transform_Res31.Last_Attempted_Child := -1;
         end if;
         if Defer_Res50 /= null and then Is_Incomplete (Defer_Res50) then
            Transform_Res31.Last_Attempted_Child := 0;
         elsif Defer_Res50 /= null and then not Is_Ghost (Defer_Res50) then
            Transform_Res31.Last_Attempted_Child := -1;
         end if;
         if Defer_Res51 /= null and then Is_Incomplete (Defer_Res51) then
            Transform_Res31.Last_Attempted_Child := 0;
         elsif Defer_Res51 /= null and then not Is_Ghost (Defer_Res51) then
            Transform_Res31.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos38 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags31);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Package_Spec_Transform_Parse0_Memo,
      Row_Pos38 /= No_Token_Index,
      Transform_Res31,
      Pos,
      Row_Pos38);


   Parser.Current_Pos := Row_Pos38;

   return Transform_Res31;
end Package_Spec_Transform_Parse0;

   


function Project_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Project
is
   use Bare_Project_Memos;

      Row_Pos39 :
            Token_Index
               := No_Token_Index;
      Defer_Pos52 :
            Token_Index
               := No_Token_Index;
      Defer_Res52 :
            Bare_With_Decl_List
               := No_Bare_Gpr_Node;
      Defer_Pos53 :
            Token_Index
               := No_Token_Index;
      Defer_Res53 :
            Bare_Project_Declaration
               := No_Bare_Gpr_Node;
      Transform_Res32 :
            Bare_Project
               := No_Bare_Gpr_Node;
      Transform_Diags32 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Project_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res32 := M.Instance;
      return Transform_Res32;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res32;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags32 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos39 := Pos;



Defer_Res52 :=
   Context_Clauses_List_Parse0 (Parser, Row_Pos39);
Defer_Pos52 := Parser.Current_Pos;




if Defer_Pos52 /= No_Token_Index then

   Row_Pos39 := Defer_Pos52;

else
   Row_Pos39 := No_Token_Index;
   goto Exit_Row39_0;

end if;


Defer_Res53 :=
   Project_Declaration_Transform_Parse0 (Parser, Row_Pos39);
Defer_Pos53 := Parser.Current_Pos;




if Defer_Pos53 /= No_Token_Index then

   Row_Pos39 := Defer_Pos53;

else
   Row_Pos39 := No_Token_Index;
   goto Exit_Row39_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row39_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos39 /= No_Token_Index then

   Transform_Res32 := Allocate_Project (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res32,
      Kind => Gpr_Project,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos39 = Pos
                            then No_Token_Index
                            else Row_Pos39 - 1));

      Initialize_Fields_For_Project
        (Self => Transform_Res32, Project_F_Context_Clauses => Defer_Res52, Project_F_Project_Decl => Defer_Res53);

         if Defer_Res52 /= null and then Is_Incomplete (Defer_Res52) then
            Transform_Res32.Last_Attempted_Child := 0;
         elsif Defer_Res52 /= null and then not Is_Ghost (Defer_Res52) then
            Transform_Res32.Last_Attempted_Child := -1;
         end if;
         if Defer_Res53 /= null and then Is_Incomplete (Defer_Res53) then
            Transform_Res32.Last_Attempted_Child := 0;
         elsif Defer_Res53 /= null and then not Is_Ghost (Defer_Res53) then
            Transform_Res32.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos39 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags32);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Project_Transform_Parse0_Memo,
      Row_Pos39 /= No_Token_Index,
      Transform_Res32,
      Pos,
      Row_Pos39);


   Parser.Current_Pos := Row_Pos39;

   return Transform_Res32;
end Project_Transform_Parse0;

   


function Project_Declaration_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Project_Declaration
is
   use Bare_Project_Declaration_Memos;

      Row_Pos40 :
            Token_Index
               := No_Token_Index;
      Defer_Pos54 :
            Token_Index
               := No_Token_Index;
      Defer_Res54 :
            Bare_Project_Qualifier
               := No_Bare_Gpr_Node;
      Token_Pos61 :
            Token_Index
               := No_Token_Index;
      Token_Res61 :
            Token_Index
               := No_Token_Index;
      Defer_Pos55 :
            Token_Index
               := No_Token_Index;
      Defer_Res55 :
            Bare_Expr
               := No_Bare_Gpr_Node;
      Defer_Pos56 :
            Token_Index
               := No_Token_Index;
      Defer_Res56 :
            Bare_Project_Extension
               := No_Bare_Gpr_Node;
      Token_Pos62 :
            Token_Index
               := No_Token_Index;
      Token_Res62 :
            Token_Index
               := No_Token_Index;
      Defer_Pos57 :
            Token_Index
               := No_Token_Index;
      Defer_Res57 :
            Bare_Gpr_Node_List
               := No_Bare_Gpr_Node;
      Token_Pos63 :
            Token_Index
               := No_Token_Index;
      Token_Res63 :
            Token_Index
               := No_Token_Index;
      Defer_Pos58 :
            Token_Index
               := No_Token_Index;
      Defer_Res58 :
            Bare_Expr
               := No_Bare_Gpr_Node;
      Token_Pos64 :
            Token_Index
               := No_Token_Index;
      Token_Res64 :
            Token_Index
               := No_Token_Index;
      Transform_Res33 :
            Bare_Project_Declaration
               := No_Bare_Gpr_Node;
      Transform_Diags33 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Project_Declaration_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res33 := M.Instance;
      return Transform_Res33;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res33;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags33 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos40 := Pos;



--  Start opt_code












Defer_Res54 :=
   Project_Qualifier_Or_Parse0 (Parser, Row_Pos40);
Defer_Pos54 := Parser.Current_Pos;


if Defer_Pos54 = No_Token_Index then

         
   Defer_Res54 := No_Bare_Gpr_Node;



       
   Defer_Pos54 := Row_Pos40;



end if;

--  End opt_code




if Defer_Pos54 /= No_Token_Index then

   Row_Pos40 := Defer_Pos54;

else
   Row_Pos40 := No_Token_Index;
   goto Exit_Row40_0;

end if;


--  Start tok_code

Token_Res61 := Row_Pos40;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res61));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
      or else T.Symbol /= Precomputed_Symbol
        (Precomputed_Symbol_Table (Parser.TDH.Symbols),
         Precomputed_Sym_Project)
   then
       Token_Pos61 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos40 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos40,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos61 := Row_Pos40 + 1;
   end if;
end;

--  End tok_code




if Token_Pos61 /= No_Token_Index then

   Row_Pos40 := Token_Pos61;

else
   Row_Pos40 := No_Token_Index;
   goto Exit_Row40_0;

end if;


Defer_Res55 :=
   Static_Name_Or_Parse0 (Parser, Row_Pos40);
Defer_Pos55 := Parser.Current_Pos;




if Defer_Pos55 /= No_Token_Index then

   Row_Pos40 := Defer_Pos55;

else
   Row_Pos40 := No_Token_Index;
   goto Exit_Row40_0;

end if;


--  Start opt_code












Defer_Res56 :=
   Project_Extension_Transform_Parse0 (Parser, Row_Pos40);
Defer_Pos56 := Parser.Current_Pos;


if Defer_Pos56 = No_Token_Index then

         
   Defer_Res56 := No_Bare_Gpr_Node;



       
   Defer_Pos56 := Row_Pos40;



end if;

--  End opt_code




if Defer_Pos56 /= No_Token_Index then

   Row_Pos40 := Defer_Pos56;

else
   Row_Pos40 := No_Token_Index;
   goto Exit_Row40_0;

end if;


--  Start tok_code

Token_Res62 := Row_Pos40;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res62));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Is)
   then
       Token_Pos62 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos40 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos40,
             Expected_Token_Id => Gpr_Is,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos62 := Row_Pos40 + 1;
   end if;
end;

--  End tok_code




if Token_Pos62 /= No_Token_Index then

   Row_Pos40 := Token_Pos62;

else
   Row_Pos40 := No_Token_Index;
   goto Exit_Row40_0;

end if;


Defer_Res57 :=
   Declarative_Items_List_Parse0 (Parser, Row_Pos40);
Defer_Pos57 := Parser.Current_Pos;




if Defer_Pos57 /= No_Token_Index then

   Row_Pos40 := Defer_Pos57;

else
   Row_Pos40 := No_Token_Index;
   goto Exit_Row40_0;

end if;


--  Start tok_code

Token_Res63 := Row_Pos40;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res63));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_End)
   then
       Token_Pos63 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos40 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos40,
             Expected_Token_Id => Gpr_End,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos63 := Row_Pos40 + 1;
   end if;
end;

--  End tok_code




if Token_Pos63 /= No_Token_Index then

   Row_Pos40 := Token_Pos63;

else
   Row_Pos40 := No_Token_Index;
   goto Exit_Row40_0;

end if;


Defer_Res58 :=
   Static_Name_Or_Parse0 (Parser, Row_Pos40);
Defer_Pos58 := Parser.Current_Pos;




if Defer_Pos58 /= No_Token_Index then

   Row_Pos40 := Defer_Pos58;

else
   Row_Pos40 := No_Token_Index;
   goto Exit_Row40_0;

end if;


--  Start tok_code

Token_Res64 := Row_Pos40;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res64));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Semicolon)
   then
       Token_Pos64 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos40 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos40,
             Expected_Token_Id => Gpr_Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos64 := Row_Pos40 + 1;
   end if;
end;

--  End tok_code




if Token_Pos64 /= No_Token_Index then

   Row_Pos40 := Token_Pos64;

else
   Row_Pos40 := No_Token_Index;
   goto Exit_Row40_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row40_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos40 /= No_Token_Index then

   Transform_Res33 := Allocate_Project_Declaration (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res33,
      Kind => Gpr_Project_Declaration,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos40 = Pos
                            then No_Token_Index
                            else Row_Pos40 - 1));

      Initialize_Fields_For_Project_Declaration
        (Self => Transform_Res33, Project_Declaration_F_Qualifier => Defer_Res54, Project_Declaration_F_Project_Name => Defer_Res55, Project_Declaration_F_Extension => Defer_Res56, Project_Declaration_F_Decls => Defer_Res57, Project_Declaration_F_End_Name => Defer_Res58);

         if Defer_Res54 /= null and then Is_Incomplete (Defer_Res54) then
            Transform_Res33.Last_Attempted_Child := 0;
         elsif Defer_Res54 /= null and then not Is_Ghost (Defer_Res54) then
            Transform_Res33.Last_Attempted_Child := -1;
         end if;
         if Defer_Res55 /= null and then Is_Incomplete (Defer_Res55) then
            Transform_Res33.Last_Attempted_Child := 0;
         elsif Defer_Res55 /= null and then not Is_Ghost (Defer_Res55) then
            Transform_Res33.Last_Attempted_Child := -1;
         end if;
         if Defer_Res56 /= null and then Is_Incomplete (Defer_Res56) then
            Transform_Res33.Last_Attempted_Child := 0;
         elsif Defer_Res56 /= null and then not Is_Ghost (Defer_Res56) then
            Transform_Res33.Last_Attempted_Child := -1;
         end if;
         if Defer_Res57 /= null and then Is_Incomplete (Defer_Res57) then
            Transform_Res33.Last_Attempted_Child := 0;
         elsif Defer_Res57 /= null and then not Is_Ghost (Defer_Res57) then
            Transform_Res33.Last_Attempted_Child := -1;
         end if;
         if Defer_Res58 /= null and then Is_Incomplete (Defer_Res58) then
            Transform_Res33.Last_Attempted_Child := 0;
         elsif Defer_Res58 /= null and then not Is_Ghost (Defer_Res58) then
            Transform_Res33.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos40 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags33);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Project_Declaration_Transform_Parse0_Memo,
      Row_Pos40 /= No_Token_Index,
      Transform_Res33,
      Pos,
      Row_Pos40);


   Parser.Current_Pos := Row_Pos40;

   return Transform_Res33;
end Project_Declaration_Transform_Parse0;

   


function Project_Extension_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Project_Extension
is
   use Bare_Project_Extension_Memos;

      Row_Pos41 :
            Token_Index
               := No_Token_Index;
      Token_Pos65 :
            Token_Index
               := No_Token_Index;
      Token_Res65 :
            Token_Index
               := No_Token_Index;
      Token_Pos66 :
            Token_Index
               := No_Token_Index;
      Token_Res66 :
            Token_Index
               := No_Token_Index;
      Opt_Res3 :
            Bare_All_Qualifier
               := No_Bare_Gpr_Node;
      Defer_Pos59 :
            Token_Index
               := No_Token_Index;
      Defer_Res59 :
            Bare_String_Literal
               := No_Bare_Gpr_Node;
      Transform_Res34 :
            Bare_Project_Extension
               := No_Bare_Gpr_Node;
      Transform_Diags34 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Project_Extension_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res34 := M.Instance;
      return Transform_Res34;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res34;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags34 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos41 := Pos;



--  Start tok_code

Token_Res65 := Row_Pos41;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res65));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Extends)
   then
       Token_Pos65 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos41 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos41,
             Expected_Token_Id => Gpr_Extends,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos65 := Row_Pos41 + 1;
   end if;
end;

--  End tok_code




if Token_Pos65 /= No_Token_Index then

   Row_Pos41 := Token_Pos65;

else
   Row_Pos41 := No_Token_Index;
   goto Exit_Row41_0;

end if;


--  Start opt_code












--  Start tok_code

Token_Res66 := Row_Pos41;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res66));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_All)
   then
       Token_Pos66 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos41 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos41,
             Expected_Token_Id => Gpr_All,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos66 := Row_Pos41 + 1;
   end if;
end;

--  End tok_code


if Token_Pos66 = No_Token_Index then

         Opt_Res3 := Allocate_All_Qualifier_Absent (Parser.Mem_Pool);
         Initialize
           (Self              => Opt_Res3,
            Kind              => Gpr_All_Qualifier_Absent,
            Unit              => Parser.Unit,
            Token_Start_Index => Row_Pos41,
            Token_End_Index   => No_Token_Index);


       
   Token_Pos66 := Row_Pos41;


else

      Opt_Res3 := Allocate_All_Qualifier_Present (Parser.Mem_Pool);
      Initialize
        (Self              => Opt_Res3,
         Kind              => Gpr_All_Qualifier_Present,
         Unit              => Parser.Unit,
         Token_Start_Index => Row_Pos41,
         Token_End_Index   => Token_Pos66 - 1);

end if;

--  End opt_code




if Token_Pos66 /= No_Token_Index then

   Row_Pos41 := Token_Pos66;

else
   Row_Pos41 := No_Token_Index;
   goto Exit_Row41_0;

end if;


Defer_Res59 :=
   String_Literal_Transform_Parse0 (Parser, Row_Pos41);
Defer_Pos59 := Parser.Current_Pos;




if Defer_Pos59 /= No_Token_Index then

   Row_Pos41 := Defer_Pos59;

else
   Row_Pos41 := No_Token_Index;
   goto Exit_Row41_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row41_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos41 /= No_Token_Index then

   Transform_Res34 := Allocate_Project_Extension (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res34,
      Kind => Gpr_Project_Extension,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos41 = Pos
                            then No_Token_Index
                            else Row_Pos41 - 1));

      Initialize_Fields_For_Project_Extension
        (Self => Transform_Res34, Project_Extension_F_Is_All => Opt_Res3, Project_Extension_F_Path_Name => Defer_Res59);

         if Opt_Res3 /= null and then Is_Incomplete (Opt_Res3) then
            Transform_Res34.Last_Attempted_Child := 0;
         elsif Opt_Res3 /= null and then not Is_Ghost (Opt_Res3) then
            Transform_Res34.Last_Attempted_Child := -1;
         end if;
         if Defer_Res59 /= null and then Is_Incomplete (Defer_Res59) then
            Transform_Res34.Last_Attempted_Child := 0;
         elsif Defer_Res59 /= null and then not Is_Ghost (Defer_Res59) then
            Transform_Res34.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos41 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags34);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Project_Extension_Transform_Parse0_Memo,
      Row_Pos41 /= No_Token_Index,
      Transform_Res34,
      Pos,
      Row_Pos41);


   Parser.Current_Pos := Row_Pos41;

   return Transform_Res34;
end Project_Extension_Transform_Parse0;

   


function Project_Qualifier_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Project_Qualifier
is
   use Bare_Project_Qualifier_Memos;

      Row_Pos42 :
            Token_Index
               := No_Token_Index;
      Token_Pos67 :
            Token_Index
               := No_Token_Index;
      Token_Res67 :
            Token_Index
               := No_Token_Index;
      Transform_Res35 :
            Bare_Project_Qualifier_Abstract
               := No_Bare_Gpr_Node;
      Transform_Diags35 :
            Ada.Containers.Count_Type;
      Row_Pos43 :
            Token_Index
               := No_Token_Index;
      Token_Pos68 :
            Token_Index
               := No_Token_Index;
      Token_Res68 :
            Token_Index
               := No_Token_Index;
      Transform_Res36 :
            Bare_Project_Qualifier_Library
               := No_Bare_Gpr_Node;
      Transform_Diags36 :
            Ada.Containers.Count_Type;
      Row_Pos44 :
            Token_Index
               := No_Token_Index;
      Token_Pos69 :
            Token_Index
               := No_Token_Index;
      Token_Res69 :
            Token_Index
               := No_Token_Index;
      Token_Pos70 :
            Token_Index
               := No_Token_Index;
      Token_Res70 :
            Token_Index
               := No_Token_Index;
      Transform_Res37 :
            Bare_Project_Qualifier_Aggregate_Library
               := No_Bare_Gpr_Node;
      Transform_Diags37 :
            Ada.Containers.Count_Type;
      Row_Pos45 :
            Token_Index
               := No_Token_Index;
      Token_Pos71 :
            Token_Index
               := No_Token_Index;
      Token_Res71 :
            Token_Index
               := No_Token_Index;
      Transform_Res38 :
            Bare_Project_Qualifier_Aggregate
               := No_Bare_Gpr_Node;
      Transform_Diags38 :
            Ada.Containers.Count_Type;
      Row_Pos46 :
            Token_Index
               := No_Token_Index;
      Token_Pos72 :
            Token_Index
               := No_Token_Index;
      Token_Res72 :
            Token_Index
               := No_Token_Index;
      Transform_Res39 :
            Bare_Project_Qualifier_Configuration
               := No_Bare_Gpr_Node;
      Transform_Diags39 :
            Ada.Containers.Count_Type;
      Row_Pos47 :
            Token_Index
               := No_Token_Index;
      Token_Pos73 :
            Token_Index
               := No_Token_Index;
      Token_Res73 :
            Token_Index
               := No_Token_Index;
      Transform_Res40 :
            Bare_Project_Qualifier_Standard
               := No_Bare_Gpr_Node;
      Transform_Diags40 :
            Ada.Containers.Count_Type;
      Or_Pos12 :
            Token_Index
               := No_Token_Index;
      Or_Res12 :
            Bare_Project_Qualifier
               := No_Bare_Gpr_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Project_Qualifier_Or_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res12 := M.Instance;
      return Or_Res12;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Or_Res12;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos12 := No_Token_Index;
Or_Res12 := No_Bare_Gpr_Node;
    
--  Start transform_code

Transform_Diags35 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos42 := Pos;



--  Start tok_code

Token_Res67 := Row_Pos42;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res67));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Abstract)
   then
       Token_Pos67 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos42 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos42,
             Expected_Token_Id => Gpr_Abstract,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos67 := Row_Pos42 + 1;
   end if;
end;

--  End tok_code




if Token_Pos67 /= No_Token_Index then

   Row_Pos42 := Token_Pos67;

else
   Row_Pos42 := No_Token_Index;
   goto Exit_Row42_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row42_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos42 /= No_Token_Index then

   Transform_Res35 := Allocate_Project_Qualifier_Abstract (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res35,
      Kind => Gpr_Project_Qualifier_Abstract,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos42 = Pos
                            then No_Token_Index
                            else Row_Pos42 - 1));




elsif Row_Pos42 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags35);
end if;

--  End transform_code

    if Row_Pos42 /= No_Token_Index then
        Or_Pos12 := Row_Pos42;
        Or_Res12 := Transform_Res35;
        goto Exit_Or14;
    end if;
    
--  Start transform_code

Transform_Diags36 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos43 := Pos;



--  Start tok_code

Token_Res68 := Row_Pos43;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res68));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
      or else T.Symbol /= Precomputed_Symbol
        (Precomputed_Symbol_Table (Parser.TDH.Symbols),
         Precomputed_Sym_Library)
   then
       Token_Pos68 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos43 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos43,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos68 := Row_Pos43 + 1;
   end if;
end;

--  End tok_code




if Token_Pos68 /= No_Token_Index then

   Row_Pos43 := Token_Pos68;

else
   Row_Pos43 := No_Token_Index;
   goto Exit_Row43_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row43_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos43 /= No_Token_Index then

   Transform_Res36 := Allocate_Project_Qualifier_Library (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res36,
      Kind => Gpr_Project_Qualifier_Library,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos43 = Pos
                            then No_Token_Index
                            else Row_Pos43 - 1));




elsif Row_Pos43 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags36);
end if;

--  End transform_code

    if Row_Pos43 /= No_Token_Index then
        Or_Pos12 := Row_Pos43;
        Or_Res12 := Transform_Res36;
        goto Exit_Or14;
    end if;
    
--  Start transform_code

Transform_Diags37 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos44 := Pos;



--  Start tok_code

Token_Res69 := Row_Pos44;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res69));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
      or else T.Symbol /= Precomputed_Symbol
        (Precomputed_Symbol_Table (Parser.TDH.Symbols),
         Precomputed_Sym_Aggregate)
   then
       Token_Pos69 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos44 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos44,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos69 := Row_Pos44 + 1;
   end if;
end;

--  End tok_code




if Token_Pos69 /= No_Token_Index then

   Row_Pos44 := Token_Pos69;

else
   Row_Pos44 := No_Token_Index;
   goto Exit_Row44_0;

end if;


--  Start tok_code

Token_Res70 := Row_Pos44;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res70));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
      or else T.Symbol /= Precomputed_Symbol
        (Precomputed_Symbol_Table (Parser.TDH.Symbols),
         Precomputed_Sym_Library)
   then
       Token_Pos70 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos44 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos44,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos70 := Row_Pos44 + 1;
   end if;
end;

--  End tok_code




if Token_Pos70 /= No_Token_Index then

   Row_Pos44 := Token_Pos70;

else
   Row_Pos44 := No_Token_Index;
   goto Exit_Row44_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row44_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos44 /= No_Token_Index then

   Transform_Res37 := Allocate_Project_Qualifier_Aggregate_Library (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res37,
      Kind => Gpr_Project_Qualifier_Aggregate_Library,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos44 = Pos
                            then No_Token_Index
                            else Row_Pos44 - 1));




elsif Row_Pos44 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags37);
end if;

--  End transform_code

    if Row_Pos44 /= No_Token_Index then
        Or_Pos12 := Row_Pos44;
        Or_Res12 := Transform_Res37;
        goto Exit_Or14;
    end if;
    
--  Start transform_code

Transform_Diags38 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos45 := Pos;



--  Start tok_code

Token_Res71 := Row_Pos45;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res71));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
      or else T.Symbol /= Precomputed_Symbol
        (Precomputed_Symbol_Table (Parser.TDH.Symbols),
         Precomputed_Sym_Aggregate)
   then
       Token_Pos71 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos45 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos45,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos71 := Row_Pos45 + 1;
   end if;
end;

--  End tok_code




if Token_Pos71 /= No_Token_Index then

   Row_Pos45 := Token_Pos71;

else
   Row_Pos45 := No_Token_Index;
   goto Exit_Row45_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row45_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos45 /= No_Token_Index then

   Transform_Res38 := Allocate_Project_Qualifier_Aggregate (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res38,
      Kind => Gpr_Project_Qualifier_Aggregate,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos45 = Pos
                            then No_Token_Index
                            else Row_Pos45 - 1));




elsif Row_Pos45 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags38);
end if;

--  End transform_code

    if Row_Pos45 /= No_Token_Index then
        Or_Pos12 := Row_Pos45;
        Or_Res12 := Transform_Res38;
        goto Exit_Or14;
    end if;
    
--  Start transform_code

Transform_Diags39 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos46 := Pos;



--  Start tok_code

Token_Res72 := Row_Pos46;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res72));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
      or else T.Symbol /= Precomputed_Symbol
        (Precomputed_Symbol_Table (Parser.TDH.Symbols),
         Precomputed_Sym_Configuration)
   then
       Token_Pos72 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos46 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos46,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos72 := Row_Pos46 + 1;
   end if;
end;

--  End tok_code




if Token_Pos72 /= No_Token_Index then

   Row_Pos46 := Token_Pos72;

else
   Row_Pos46 := No_Token_Index;
   goto Exit_Row46_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row46_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos46 /= No_Token_Index then

   Transform_Res39 := Allocate_Project_Qualifier_Configuration (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res39,
      Kind => Gpr_Project_Qualifier_Configuration,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos46 = Pos
                            then No_Token_Index
                            else Row_Pos46 - 1));




elsif Row_Pos46 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags39);
end if;

--  End transform_code

    if Row_Pos46 /= No_Token_Index then
        Or_Pos12 := Row_Pos46;
        Or_Res12 := Transform_Res39;
        goto Exit_Or14;
    end if;
    
--  Start transform_code

Transform_Diags40 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos47 := Pos;



--  Start tok_code

Token_Res73 := Row_Pos47;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res73));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Identifier)
      or else T.Symbol /= Precomputed_Symbol
        (Precomputed_Symbol_Table (Parser.TDH.Symbols),
         Precomputed_Sym_Standard)
   then
       Token_Pos73 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos47 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos47,
             Expected_Token_Id => Gpr_Identifier,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos73 := Row_Pos47 + 1;
   end if;
end;

--  End tok_code




if Token_Pos73 /= No_Token_Index then

   Row_Pos47 := Token_Pos73;

else
   Row_Pos47 := No_Token_Index;
   goto Exit_Row47_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row47_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos47 /= No_Token_Index then

   Transform_Res40 := Allocate_Project_Qualifier_Standard (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res40,
      Kind => Gpr_Project_Qualifier_Standard,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos47 = Pos
                            then No_Token_Index
                            else Row_Pos47 - 1));




elsif Row_Pos47 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags40);
end if;

--  End transform_code

    if Row_Pos47 /= No_Token_Index then
        Or_Pos12 := Row_Pos47;
        Or_Res12 := Transform_Res40;
        goto Exit_Or14;
    end if;
<<Exit_Or14>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Project_Qualifier_Or_Parse0_Memo,
      Or_Pos12 /= No_Token_Index,
      Or_Res12,
      Pos,
      Or_Pos12);


   Parser.Current_Pos := Or_Pos12;

   return Or_Res12;
end Project_Qualifier_Or_Parse0;

   


function Simple_Declarative_Item_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node
is
   use Bare_Gpr_Node_Memos;

      Defer_Pos60 :
            Token_Index
               := No_Token_Index;
      Defer_Res60 :
            Bare_Variable_Decl
               := No_Bare_Gpr_Node;
      Defer_Pos61 :
            Token_Index
               := No_Token_Index;
      Defer_Res61 :
            Bare_Attribute_Decl
               := No_Bare_Gpr_Node;
      Defer_Pos62 :
            Token_Index
               := No_Token_Index;
      Defer_Res62 :
            Bare_Case_Construction
               := No_Bare_Gpr_Node;
      Defer_Pos63 :
            Token_Index
               := No_Token_Index;
      Defer_Res63 :
            Bare_Empty_Decl
               := No_Bare_Gpr_Node;
      Or_Pos13 :
            Token_Index
               := No_Token_Index;
      Or_Res13 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Simple_Declarative_Item_Or_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res13 := M.Instance;
      return Or_Res13;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Or_Res13;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos13 := No_Token_Index;
Or_Res13 := No_Bare_Gpr_Node;
    
Defer_Res60 :=
   Variable_Decl_Transform_Parse0 (Parser, Pos);
Defer_Pos60 := Parser.Current_Pos;

    if Defer_Pos60 /= No_Token_Index then
        Or_Pos13 := Defer_Pos60;
        Or_Res13 := Defer_Res60;
        goto Exit_Or15;
    end if;
    
Defer_Res61 :=
   Attribute_Decl_Transform_Parse0 (Parser, Pos);
Defer_Pos61 := Parser.Current_Pos;

    if Defer_Pos61 /= No_Token_Index then
        Or_Pos13 := Defer_Pos61;
        Or_Res13 := Defer_Res61;
        goto Exit_Or15;
    end if;
    
Defer_Res62 :=
   Case_Construction_Transform_Parse0 (Parser, Pos);
Defer_Pos62 := Parser.Current_Pos;

    if Defer_Pos62 /= No_Token_Index then
        Or_Pos13 := Defer_Pos62;
        Or_Res13 := Defer_Res62;
        goto Exit_Or15;
    end if;
    
Defer_Res63 :=
   Empty_Declaration_Transform_Parse0 (Parser, Pos);
Defer_Pos63 := Parser.Current_Pos;

    if Defer_Pos63 /= No_Token_Index then
        Or_Pos13 := Defer_Pos63;
        Or_Res13 := Defer_Res63;
        goto Exit_Or15;
    end if;
<<Exit_Or15>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Simple_Declarative_Item_Or_Parse0_Memo,
      Or_Pos13 /= No_Token_Index,
      Or_Res13,
      Pos,
      Or_Pos13);


   Parser.Current_Pos := Or_Pos13;

   return Or_Res13;
end Simple_Declarative_Item_Or_Parse0;

   


function Simple_Declarative_Items_List_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node_List
is
   use Bare_Gpr_Node_List_Memos;

      Lst_Cpos12 :
            Token_Index
               := No_Token_Index;
      Tmp_List12 :
            Free_Parse_List;
      Defer_Pos64 :
            Token_Index
               := No_Token_Index;
      Defer_Res64 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;
      List_Pos12 :
            Token_Index
               := No_Token_Index;
      List_Res12 :
            Bare_Gpr_Node_List
               := No_Bare_Gpr_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Simple_Declarative_Items_List_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res12 := M.Instance;
      return List_Res12;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return List_Res12;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start list_code

    List_Pos12 := Pos;



Lst_Cpos12 := Pos;
Tmp_List12 := Get_Parse_List (Parser);

loop
   
Defer_Res64 :=
   Simple_Declarative_Item_Or_Parse0 (Parser, Lst_Cpos12);
Defer_Pos64 := Parser.Current_Pos;


   exit when Defer_Pos64 = No_Token_Index;

   List_Pos12 := Defer_Pos64;
   Lst_Cpos12 := List_Pos12;

   Tmp_List12.Nodes.Append (Defer_Res64);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List12.Nodes.Length;
begin
   List_Res12 :=
      Allocate_Gpr_Node_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Pos;
      Token_End := (if Lst_Cpos12 = Pos
                    then Pos
                    else Lst_Cpos12 - 1);

   else
      Token_Start := Token_Index'Max (Pos, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res12,
      Kind              => Gpr_Gpr_Node_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res12,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List12.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res12.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List12);

--  End list_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Simple_Declarative_Items_List_Parse0_Memo,
      List_Pos12 /= No_Token_Index,
      List_Res12,
      Pos,
      List_Pos12);


   Parser.Current_Pos := List_Pos12;

   return List_Res12;
end Simple_Declarative_Items_List_Parse0;

   


function Static_Name_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

      Row_Pos48 :
            Token_Index
               := No_Token_Index;
      Defer_Pos65 :
            Token_Index
               := No_Token_Index;
      Defer_Res65 :
            Bare_Expr
               := No_Bare_Gpr_Node;
      Token_Pos74 :
            Token_Index
               := No_Token_Index;
      Token_Res74 :
            Token_Index
               := No_Token_Index;
      Defer_Pos66 :
            Token_Index
               := No_Token_Index;
      Defer_Res66 :
            Bare_Identifier
               := No_Bare_Gpr_Node;
      Transform_Res41 :
            Bare_Prefix
               := No_Bare_Gpr_Node;
      Transform_Diags41 :
            Ada.Containers.Count_Type;
      Defer_Pos67 :
            Token_Index
               := No_Token_Index;
      Defer_Res67 :
            Bare_Identifier
               := No_Bare_Gpr_Node;
      Or_Pos14 :
            Token_Index
               := No_Token_Index;
      Or_Res14 :
            Bare_Expr
               := No_Bare_Gpr_Node;

      Mem_Pos : Token_Index := Pos;
      Mem_Res : Bare_Expr := No_Bare_Gpr_Node;

   M : Memo_Entry := Get (Parser.Private_Part.Static_Name_Or_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res14 := M.Instance;
      return Or_Res14;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Or_Res14;
   end if;

       Set (Parser.Private_Part.Static_Name_Or_Parse0_Memo, False, Or_Res14, Pos, Mem_Pos);

       <<Try_Again>>



   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos14 := No_Token_Index;
Or_Res14 := No_Bare_Gpr_Node;
    
--  Start transform_code

Transform_Diags41 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos48 := Pos;



Defer_Res65 :=
   Static_Name_Or_Parse0 (Parser, Row_Pos48);
Defer_Pos65 := Parser.Current_Pos;




if Defer_Pos65 /= No_Token_Index then

   Row_Pos48 := Defer_Pos65;

else
   Row_Pos48 := No_Token_Index;
   goto Exit_Row48_0;

end if;


--  Start tok_code

Token_Res74 := Row_Pos48;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res74));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Dot)
   then
       Token_Pos74 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos48 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos48,
             Expected_Token_Id => Gpr_Dot,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos74 := Row_Pos48 + 1;
   end if;
end;

--  End tok_code




if Token_Pos74 /= No_Token_Index then

   Row_Pos48 := Token_Pos74;

else
   Row_Pos48 := No_Token_Index;
   goto Exit_Row48_0;

end if;


Defer_Res66 :=
   Identifier_Transform_Parse0 (Parser, Row_Pos48);
Defer_Pos66 := Parser.Current_Pos;




if Defer_Pos66 /= No_Token_Index then

   Row_Pos48 := Defer_Pos66;

else
   Row_Pos48 := No_Token_Index;
   goto Exit_Row48_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row48_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos48 /= No_Token_Index then

   Transform_Res41 := Allocate_Prefix (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res41,
      Kind => Gpr_Prefix,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos48 = Pos
                            then No_Token_Index
                            else Row_Pos48 - 1));

      Initialize_Fields_For_Prefix
        (Self => Transform_Res41, Prefix_F_Prefix => Defer_Res65, Prefix_F_Suffix => Defer_Res66);

         if Defer_Res65 /= null and then Is_Incomplete (Defer_Res65) then
            Transform_Res41.Last_Attempted_Child := 0;
         elsif Defer_Res65 /= null and then not Is_Ghost (Defer_Res65) then
            Transform_Res41.Last_Attempted_Child := -1;
         end if;
         if Defer_Res66 /= null and then Is_Incomplete (Defer_Res66) then
            Transform_Res41.Last_Attempted_Child := 0;
         elsif Defer_Res66 /= null and then not Is_Ghost (Defer_Res66) then
            Transform_Res41.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos48 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags41);
end if;

--  End transform_code

    if Row_Pos48 /= No_Token_Index then
        Or_Pos14 := Row_Pos48;
        Or_Res14 := Transform_Res41;
        goto Exit_Or16;
    end if;
    
Defer_Res67 :=
   Identifier_Transform_Parse0 (Parser, Pos);
Defer_Pos67 := Parser.Current_Pos;

    if Defer_Pos67 /= No_Token_Index then
        Or_Pos14 := Defer_Pos67;
        Or_Res14 := Defer_Res67;
        goto Exit_Or16;
    end if;
<<Exit_Or16>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

      if Or_Pos14 > Mem_Pos then
         Mem_Pos := Or_Pos14;
         Mem_Res := Or_Res14;
         Set
           (Parser.Private_Part.Static_Name_Or_Parse0_Memo,
            Or_Pos14 /= No_Token_Index,
            Or_Res14,
            Pos,
            Or_Pos14);
         goto Try_Again;

      elsif Mem_Pos > Pos then
         Or_Res14 := Mem_Res;
         Or_Pos14 := Mem_Pos;
         goto No_Memo;
      end if;

   Set
     (Parser.Private_Part.Static_Name_Or_Parse0_Memo,
      Or_Pos14 /= No_Token_Index,
      Or_Res14,
      Pos,
      Or_Pos14);

       <<No_Memo>>

   Parser.Current_Pos := Or_Pos14;

   return Or_Res14;
end Static_Name_Or_Parse0;

   


function String_Literal_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_String_Literal
is
   use Bare_String_Literal_Memos;

      Row_Pos49 :
            Token_Index
               := No_Token_Index;
      Token_Pos75 :
            Token_Index
               := No_Token_Index;
      Token_Res75 :
            Token_Index
               := No_Token_Index;
      Transform_Res42 :
            Bare_String_Literal
               := No_Bare_Gpr_Node;
      Transform_Diags42 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.String_Literal_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res42 := M.Instance;
      return Transform_Res42;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res42;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags42 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos49 := Pos;



--  Start tok_code

Token_Res75 := Row_Pos49;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res75));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_String)
   then
       Token_Pos75 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos49 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos49,
             Expected_Token_Id => Gpr_String,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos75 := Row_Pos49 + 1;
   end if;
end;

--  End tok_code




if Token_Pos75 /= No_Token_Index then

   Row_Pos49 := Token_Pos75;

else
   Row_Pos49 := No_Token_Index;
   goto Exit_Row49_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row49_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos49 /= No_Token_Index then

   Transform_Res42 := Allocate_String_Literal (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res42,
      Kind => Gpr_String_Literal,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos49 = Pos
                            then No_Token_Index
                            else Row_Pos49 - 1));




elsif Row_Pos49 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags42);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.String_Literal_Transform_Parse0_Memo,
      Row_Pos49 /= No_Token_Index,
      Transform_Res42,
      Pos,
      Row_Pos49);


   Parser.Current_Pos := Row_Pos49;

   return Transform_Res42;
end String_Literal_Transform_Parse0;

   


function String_Literal_At_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_String_Literal_At
is
   use Bare_String_Literal_At_Memos;

      Row_Pos50 :
            Token_Index
               := No_Token_Index;
      Defer_Pos68 :
            Token_Index
               := No_Token_Index;
      Defer_Res68 :
            Bare_String_Literal
               := No_Bare_Gpr_Node;
      Row_Pos51 :
            Token_Index
               := No_Token_Index;
      Token_Pos76 :
            Token_Index
               := No_Token_Index;
      Token_Res76 :
            Token_Index
               := No_Token_Index;
      Defer_Pos69 :
            Token_Index
               := No_Token_Index;
      Defer_Res69 :
            Bare_Num_Literal
               := No_Bare_Gpr_Node;
      Transform_Res43 :
            Bare_String_Literal_At
               := No_Bare_Gpr_Node;
      Transform_Diags43 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.String_Literal_At_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res43 := M.Instance;
      return Transform_Res43;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res43;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags43 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos50 := Pos;



Defer_Res68 :=
   String_Literal_Transform_Parse0 (Parser, Row_Pos50);
Defer_Pos68 := Parser.Current_Pos;




if Defer_Pos68 /= No_Token_Index then

   Row_Pos50 := Defer_Pos68;

else
   Row_Pos50 := No_Token_Index;
   goto Exit_Row50_0;

end if;


--  Start opt_code












--  Start row_code

Row_Pos51 := Row_Pos50;



--  Start tok_code

Token_Res76 := Row_Pos51;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res76));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_At)
   then
       Token_Pos76 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos51 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos51,
             Expected_Token_Id => Gpr_At,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos76 := Row_Pos51 + 1;
   end if;
end;

--  End tok_code




if Token_Pos76 /= No_Token_Index then

   Row_Pos51 := Token_Pos76;

else
   Row_Pos51 := No_Token_Index;
   goto Exit_Row51_0;

end if;


Defer_Res69 :=
   Num_Literal_Transform_Parse0 (Parser, Row_Pos51);
Defer_Pos69 := Parser.Current_Pos;




if Defer_Pos69 /= No_Token_Index then

   Row_Pos51 := Defer_Pos69;

else
   Row_Pos51 := No_Token_Index;
   goto Exit_Row51_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row51_0>>
pragma Warnings (On, "referenced");

--  End row_code


if Row_Pos51 = No_Token_Index then

         
   Defer_Res69 := No_Bare_Gpr_Node;



       
   Row_Pos51 := Row_Pos50;



end if;

--  End opt_code




if Row_Pos51 /= No_Token_Index then

   Row_Pos50 := Row_Pos51;

else
   Row_Pos50 := No_Token_Index;
   goto Exit_Row50_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row50_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos50 /= No_Token_Index then

   Transform_Res43 := Allocate_String_Literal_At (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res43,
      Kind => Gpr_String_Literal_At,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos50 = Pos
                            then No_Token_Index
                            else Row_Pos50 - 1));

      Initialize_Fields_For_String_Literal_At
        (Self => Transform_Res43, String_Literal_At_F_Str_Lit => Defer_Res68, String_Literal_At_F_At_Lit => Defer_Res69);

         if Defer_Res68 /= null and then Is_Incomplete (Defer_Res68) then
            Transform_Res43.Last_Attempted_Child := 0;
         elsif Defer_Res68 /= null and then not Is_Ghost (Defer_Res68) then
            Transform_Res43.Last_Attempted_Child := -1;
         end if;
         if Defer_Res69 /= null and then Is_Incomplete (Defer_Res69) then
            Transform_Res43.Last_Attempted_Child := 0;
         elsif Defer_Res69 /= null and then not Is_Ghost (Defer_Res69) then
            Transform_Res43.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos50 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags43);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.String_Literal_At_Transform_Parse0_Memo,
      Row_Pos50 /= No_Token_Index,
      Transform_Res43,
      Pos,
      Row_Pos50);


   Parser.Current_Pos := Row_Pos50;

   return Transform_Res43;
end String_Literal_At_Transform_Parse0;

   


function Term_Or_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Gpr_Node
is
   use Bare_Gpr_Node_Memos;

      Defer_Pos70 :
            Token_Index
               := No_Token_Index;
      Defer_Res70 :
            Bare_Terms
               := No_Bare_Gpr_Node;
      Defer_Pos71 :
            Token_Index
               := No_Token_Index;
      Defer_Res71 :
            Bare_String_Literal_At
               := No_Bare_Gpr_Node;
      Defer_Pos72 :
            Token_Index
               := No_Token_Index;
      Defer_Res72 :
            Bare_Builtin_Function_Call
               := No_Bare_Gpr_Node;
      Defer_Pos73 :
            Token_Index
               := No_Token_Index;
      Defer_Res73 :
            Bare_Variable_Reference
               := No_Bare_Gpr_Node;
      Or_Pos15 :
            Token_Index
               := No_Token_Index;
      Or_Res15 :
            Bare_Gpr_Node
               := No_Bare_Gpr_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Term_Or_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res15 := M.Instance;
      return Or_Res15;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Or_Res15;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos15 := No_Token_Index;
Or_Res15 := No_Bare_Gpr_Node;
    
Defer_Res70 :=
   Expression_List_Transform_Parse0 (Parser, Pos);
Defer_Pos70 := Parser.Current_Pos;

    if Defer_Pos70 /= No_Token_Index then
        Or_Pos15 := Defer_Pos70;
        Or_Res15 := Defer_Res70;
        goto Exit_Or17;
    end if;
    
Defer_Res71 :=
   String_Literal_At_Transform_Parse0 (Parser, Pos);
Defer_Pos71 := Parser.Current_Pos;

    if Defer_Pos71 /= No_Token_Index then
        Or_Pos15 := Defer_Pos71;
        Or_Res15 := Defer_Res71;
        goto Exit_Or17;
    end if;
    
Defer_Res72 :=
   Builtin_Function_Call_Transform_Parse0 (Parser, Pos);
Defer_Pos72 := Parser.Current_Pos;

    if Defer_Pos72 /= No_Token_Index then
        Or_Pos15 := Defer_Pos72;
        Or_Res15 := Defer_Res72;
        goto Exit_Or17;
    end if;
    
Defer_Res73 :=
   Variable_Reference_Transform_Parse0 (Parser, Pos);
Defer_Pos73 := Parser.Current_Pos;

    if Defer_Pos73 /= No_Token_Index then
        Or_Pos15 := Defer_Pos73;
        Or_Res15 := Defer_Res73;
        goto Exit_Or17;
    end if;
<<Exit_Or17>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Term_Or_Parse0_Memo,
      Or_Pos15 /= No_Token_Index,
      Or_Res15,
      Pos,
      Or_Pos15);


   Parser.Current_Pos := Or_Pos15;

   return Or_Res15;
end Term_Or_Parse0;

   


function Type_Reference_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Type_Reference
is
   use Bare_Type_Reference_Memos;

      Row_Pos52 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos13 :
            Token_Index
               := No_Token_Index;
      Tmp_List13 :
            Free_Parse_List;
      Defer_Pos74 :
            Token_Index
               := No_Token_Index;
      Defer_Res74 :
            Bare_Identifier
               := No_Bare_Gpr_Node;
      Token_Pos77 :
            Token_Index
               := No_Token_Index;
      Token_Res77 :
            Token_Index
               := No_Token_Index;
      List_Pos13 :
            Token_Index
               := No_Token_Index;
      List_Res13 :
            Bare_Identifier_List
               := No_Bare_Gpr_Node;
      Transform_Res44 :
            Bare_Type_Reference
               := No_Bare_Gpr_Node;
      Transform_Diags44 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Type_Reference_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res44 := M.Instance;
      return Transform_Res44;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res44;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags44 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos52 := Pos;



--  Start list_code

    List_Pos13 := No_Token_Index;



Lst_Cpos13 := Row_Pos52;
Tmp_List13 := Get_Parse_List (Parser);

loop
   
Defer_Res74 :=
   Identifier_Transform_Parse0 (Parser, Lst_Cpos13);
Defer_Pos74 := Parser.Current_Pos;


   exit when Defer_Pos74 = No_Token_Index;

   List_Pos13 := Defer_Pos74;
   Lst_Cpos13 := List_Pos13;

   Tmp_List13.Nodes.Append (Defer_Res74);

      
--  Start tok_code

Token_Res77 := Lst_Cpos13;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res77));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Dot)
   then
       Token_Pos77 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos13 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos13,
             Expected_Token_Id => Gpr_Dot,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos77 := Lst_Cpos13 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos77 /= No_Token_Index then
          Lst_Cpos13 := Token_Pos77;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List13.Nodes.Length;
begin
   List_Res13 :=
      Allocate_Identifier_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos52;
      Token_End := (if Lst_Cpos13 = Row_Pos52
                    then Row_Pos52
                    else Lst_Cpos13 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos52, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res13,
      Kind              => Gpr_Identifier_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res13,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List13.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res13.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List13);

--  End list_code




if List_Pos13 /= No_Token_Index then

   Row_Pos52 := List_Pos13;

else
   Row_Pos52 := No_Token_Index;
   goto Exit_Row52_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row52_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos52 /= No_Token_Index then

   Transform_Res44 := Allocate_Type_Reference (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res44,
      Kind => Gpr_Type_Reference,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos52 = Pos
                            then No_Token_Index
                            else Row_Pos52 - 1));

      Initialize_Fields_For_Type_Reference
        (Self => Transform_Res44, Type_Reference_F_Var_Type_Name => List_Res13);

         if List_Res13 /= null and then Is_Incomplete (List_Res13) then
            Transform_Res44.Last_Attempted_Child := 0;
         elsif List_Res13 /= null and then not Is_Ghost (List_Res13) then
            Transform_Res44.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos52 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags44);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Type_Reference_Transform_Parse0_Memo,
      Row_Pos52 /= No_Token_Index,
      Transform_Res44,
      Pos,
      Row_Pos52);


   Parser.Current_Pos := Row_Pos52;

   return Transform_Res44;
end Type_Reference_Transform_Parse0;

   


function Typed_String_Decl_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Typed_String_Decl
is
   use Bare_Typed_String_Decl_Memos;

      Row_Pos53 :
            Token_Index
               := No_Token_Index;
      Token_Pos78 :
            Token_Index
               := No_Token_Index;
      Token_Res78 :
            Token_Index
               := No_Token_Index;
      Defer_Pos75 :
            Token_Index
               := No_Token_Index;
      Defer_Res75 :
            Bare_Identifier
               := No_Bare_Gpr_Node;
      Token_Pos79 :
            Token_Index
               := No_Token_Index;
      Token_Res79 :
            Token_Index
               := No_Token_Index;
      Token_Pos80 :
            Token_Index
               := No_Token_Index;
      Token_Res80 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos14 :
            Token_Index
               := No_Token_Index;
      Tmp_List14 :
            Free_Parse_List;
      Defer_Pos76 :
            Token_Index
               := No_Token_Index;
      Defer_Res76 :
            Bare_String_Literal
               := No_Bare_Gpr_Node;
      Token_Pos81 :
            Token_Index
               := No_Token_Index;
      Token_Res81 :
            Token_Index
               := No_Token_Index;
      List_Pos14 :
            Token_Index
               := No_Token_Index;
      List_Res14 :
            Bare_String_Literal_List
               := No_Bare_Gpr_Node;
      Token_Pos82 :
            Token_Index
               := No_Token_Index;
      Token_Res82 :
            Token_Index
               := No_Token_Index;
      Token_Pos83 :
            Token_Index
               := No_Token_Index;
      Token_Res83 :
            Token_Index
               := No_Token_Index;
      Transform_Res45 :
            Bare_Typed_String_Decl
               := No_Bare_Gpr_Node;
      Transform_Diags45 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Typed_String_Decl_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res45 := M.Instance;
      return Transform_Res45;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res45;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags45 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos53 := Pos;



--  Start tok_code

Token_Res78 := Row_Pos53;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res78));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Type)
   then
       Token_Pos78 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos53 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos53,
             Expected_Token_Id => Gpr_Type,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos78 := Row_Pos53 + 1;
   end if;
end;

--  End tok_code




if Token_Pos78 /= No_Token_Index then

   Row_Pos53 := Token_Pos78;

else
   Row_Pos53 := No_Token_Index;
   goto Exit_Row53_0;

end if;


Defer_Res75 :=
   Identifier_Transform_Parse0 (Parser, Row_Pos53);
Defer_Pos75 := Parser.Current_Pos;




if Defer_Pos75 /= No_Token_Index then

   Row_Pos53 := Defer_Pos75;

else
   Row_Pos53 := No_Token_Index;
   goto Exit_Row53_0;

end if;


--  Start tok_code

Token_Res79 := Row_Pos53;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res79));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Is)
   then
       Token_Pos79 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos53 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos53,
             Expected_Token_Id => Gpr_Is,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos79 := Row_Pos53 + 1;
   end if;
end;

--  End tok_code




if Token_Pos79 /= No_Token_Index then

   Row_Pos53 := Token_Pos79;

else
   Row_Pos53 := No_Token_Index;
   goto Exit_Row53_0;

end if;


--  Start tok_code

Token_Res80 := Row_Pos53;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res80));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Par_Open)
   then
       Token_Pos80 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos53 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos53,
             Expected_Token_Id => Gpr_Par_Open,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos80 := Row_Pos53 + 1;
   end if;
end;

--  End tok_code




if Token_Pos80 /= No_Token_Index then

   Row_Pos53 := Token_Pos80;

else
   Row_Pos53 := No_Token_Index;
   goto Exit_Row53_0;

end if;


--  Start list_code

    List_Pos14 := No_Token_Index;



Lst_Cpos14 := Row_Pos53;
Tmp_List14 := Get_Parse_List (Parser);

loop
   
Defer_Res76 :=
   String_Literal_Transform_Parse0 (Parser, Lst_Cpos14);
Defer_Pos76 := Parser.Current_Pos;


   exit when Defer_Pos76 = No_Token_Index;

   List_Pos14 := Defer_Pos76;
   Lst_Cpos14 := List_Pos14;

   Tmp_List14.Nodes.Append (Defer_Res76);

      
--  Start tok_code

Token_Res81 := Lst_Cpos14;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res81));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Comma)
   then
       Token_Pos81 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos14 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos14,
             Expected_Token_Id => Gpr_Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos81 := Lst_Cpos14 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos81 /= No_Token_Index then
          Lst_Cpos14 := Token_Pos81;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List14.Nodes.Length;
begin
   List_Res14 :=
      Allocate_String_Literal_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos53;
      Token_End := (if Lst_Cpos14 = Row_Pos53
                    then Row_Pos53
                    else Lst_Cpos14 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos53, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res14,
      Kind              => Gpr_String_Literal_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res14,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List14.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res14.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List14);

--  End list_code




if List_Pos14 /= No_Token_Index then

   Row_Pos53 := List_Pos14;

else
   Row_Pos53 := No_Token_Index;
   goto Exit_Row53_0;

end if;


--  Start tok_code

Token_Res82 := Row_Pos53;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res82));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Par_Close)
   then
       Token_Pos82 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos53 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos53,
             Expected_Token_Id => Gpr_Par_Close,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos82 := Row_Pos53 + 1;
   end if;
end;

--  End tok_code




if Token_Pos82 /= No_Token_Index then

   Row_Pos53 := Token_Pos82;

else
   Row_Pos53 := No_Token_Index;
   goto Exit_Row53_0;

end if;


--  Start tok_code

Token_Res83 := Row_Pos53;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res83));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Semicolon)
   then
       Token_Pos83 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos53 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos53,
             Expected_Token_Id => Gpr_Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos83 := Row_Pos53 + 1;
   end if;
end;

--  End tok_code




if Token_Pos83 /= No_Token_Index then

   Row_Pos53 := Token_Pos83;

else
   Row_Pos53 := No_Token_Index;
   goto Exit_Row53_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row53_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos53 /= No_Token_Index then

   Transform_Res45 := Allocate_Typed_String_Decl (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res45,
      Kind => Gpr_Typed_String_Decl,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos53 = Pos
                            then No_Token_Index
                            else Row_Pos53 - 1));

      Initialize_Fields_For_Typed_String_Decl
        (Self => Transform_Res45, Typed_String_Decl_F_Type_Id => Defer_Res75, Typed_String_Decl_F_String_Literals => List_Res14);

         if Defer_Res75 /= null and then Is_Incomplete (Defer_Res75) then
            Transform_Res45.Last_Attempted_Child := 0;
         elsif Defer_Res75 /= null and then not Is_Ghost (Defer_Res75) then
            Transform_Res45.Last_Attempted_Child := -1;
         end if;
         if List_Res14 /= null and then Is_Incomplete (List_Res14) then
            Transform_Res45.Last_Attempted_Child := 0;
         elsif List_Res14 /= null and then not Is_Ghost (List_Res14) then
            Transform_Res45.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos53 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags45);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Typed_String_Decl_Transform_Parse0_Memo,
      Row_Pos53 /= No_Token_Index,
      Transform_Res45,
      Pos,
      Row_Pos53);


   Parser.Current_Pos := Row_Pos53;

   return Transform_Res45;
end Typed_String_Decl_Transform_Parse0;

   


function Variable_Decl_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Variable_Decl
is
   use Bare_Variable_Decl_Memos;

      Row_Pos54 :
            Token_Index
               := No_Token_Index;
      Defer_Pos77 :
            Token_Index
               := No_Token_Index;
      Defer_Res77 :
            Bare_Identifier
               := No_Bare_Gpr_Node;
      Row_Pos55 :
            Token_Index
               := No_Token_Index;
      Token_Pos84 :
            Token_Index
               := No_Token_Index;
      Token_Res84 :
            Token_Index
               := No_Token_Index;
      Defer_Pos78 :
            Token_Index
               := No_Token_Index;
      Defer_Res78 :
            Bare_Type_Reference
               := No_Bare_Gpr_Node;
      Token_Pos85 :
            Token_Index
               := No_Token_Index;
      Token_Res85 :
            Token_Index
               := No_Token_Index;
      Defer_Pos79 :
            Token_Index
               := No_Token_Index;
      Defer_Res79 :
            Bare_Term_List
               := No_Bare_Gpr_Node;
      Token_Pos86 :
            Token_Index
               := No_Token_Index;
      Token_Res86 :
            Token_Index
               := No_Token_Index;
      Transform_Res46 :
            Bare_Variable_Decl
               := No_Bare_Gpr_Node;
      Transform_Diags46 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Variable_Decl_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res46 := M.Instance;
      return Transform_Res46;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res46;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags46 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos54 := Pos;



Defer_Res77 :=
   Identifier_Transform_Parse0 (Parser, Row_Pos54);
Defer_Pos77 := Parser.Current_Pos;




if Defer_Pos77 /= No_Token_Index then

   Row_Pos54 := Defer_Pos77;

else
   Row_Pos54 := No_Token_Index;
   goto Exit_Row54_0;

end if;


--  Start opt_code












--  Start row_code

Row_Pos55 := Row_Pos54;



--  Start tok_code

Token_Res84 := Row_Pos55;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res84));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Colon)
   then
       Token_Pos84 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos55 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos55,
             Expected_Token_Id => Gpr_Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos84 := Row_Pos55 + 1;
   end if;
end;

--  End tok_code




if Token_Pos84 /= No_Token_Index then

   Row_Pos55 := Token_Pos84;

else
   Row_Pos55 := No_Token_Index;
   goto Exit_Row55_0;

end if;


Defer_Res78 :=
   Type_Reference_Transform_Parse0 (Parser, Row_Pos55);
Defer_Pos78 := Parser.Current_Pos;




if Defer_Pos78 /= No_Token_Index then

   Row_Pos55 := Defer_Pos78;

else
   Row_Pos55 := No_Token_Index;
   goto Exit_Row55_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row55_0>>
pragma Warnings (On, "referenced");

--  End row_code


if Row_Pos55 = No_Token_Index then

         
   Defer_Res78 := No_Bare_Gpr_Node;



       
   Row_Pos55 := Row_Pos54;



end if;

--  End opt_code




if Row_Pos55 /= No_Token_Index then

   Row_Pos54 := Row_Pos55;

else
   Row_Pos54 := No_Token_Index;
   goto Exit_Row54_0;

end if;


--  Start tok_code

Token_Res85 := Row_Pos54;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res85));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Assign)
   then
       Token_Pos85 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos54 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos54,
             Expected_Token_Id => Gpr_Assign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos85 := Row_Pos54 + 1;
   end if;
end;

--  End tok_code




if Token_Pos85 /= No_Token_Index then

   Row_Pos54 := Token_Pos85;

else
   Row_Pos54 := No_Token_Index;
   goto Exit_Row54_0;

end if;


Defer_Res79 :=
   Expression_List_Parse0 (Parser, Row_Pos54);
Defer_Pos79 := Parser.Current_Pos;




if Defer_Pos79 /= No_Token_Index then

   Row_Pos54 := Defer_Pos79;

else
   Row_Pos54 := No_Token_Index;
   goto Exit_Row54_0;

end if;


--  Start tok_code

Token_Res86 := Row_Pos54;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res86));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Semicolon)
   then
       Token_Pos86 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos54 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos54,
             Expected_Token_Id => Gpr_Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos86 := Row_Pos54 + 1;
   end if;
end;

--  End tok_code




if Token_Pos86 /= No_Token_Index then

   Row_Pos54 := Token_Pos86;

else
   Row_Pos54 := No_Token_Index;
   goto Exit_Row54_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row54_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos54 /= No_Token_Index then

   Transform_Res46 := Allocate_Variable_Decl (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res46,
      Kind => Gpr_Variable_Decl,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos54 = Pos
                            then No_Token_Index
                            else Row_Pos54 - 1));

      Initialize_Fields_For_Variable_Decl
        (Self => Transform_Res46, Variable_Decl_F_Var_Name => Defer_Res77, Variable_Decl_F_Var_Type => Defer_Res78, Variable_Decl_F_Expr => Defer_Res79);

         if Defer_Res77 /= null and then Is_Incomplete (Defer_Res77) then
            Transform_Res46.Last_Attempted_Child := 0;
         elsif Defer_Res77 /= null and then not Is_Ghost (Defer_Res77) then
            Transform_Res46.Last_Attempted_Child := -1;
         end if;
         if Defer_Res78 /= null and then Is_Incomplete (Defer_Res78) then
            Transform_Res46.Last_Attempted_Child := 0;
         elsif Defer_Res78 /= null and then not Is_Ghost (Defer_Res78) then
            Transform_Res46.Last_Attempted_Child := -1;
         end if;
         if Defer_Res79 /= null and then Is_Incomplete (Defer_Res79) then
            Transform_Res46.Last_Attempted_Child := 0;
         elsif Defer_Res79 /= null and then not Is_Ghost (Defer_Res79) then
            Transform_Res46.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos54 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags46);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Variable_Decl_Transform_Parse0_Memo,
      Row_Pos54 /= No_Token_Index,
      Transform_Res46,
      Pos,
      Row_Pos54);


   Parser.Current_Pos := Row_Pos54;

   return Transform_Res46;
end Variable_Decl_Transform_Parse0;

   


function Variable_Reference_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Variable_Reference
is
   use Bare_Variable_Reference_Memos;

      Row_Pos56 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos15 :
            Token_Index
               := No_Token_Index;
      Tmp_List15 :
            Free_Parse_List;
      Defer_Pos80 :
            Token_Index
               := No_Token_Index;
      Defer_Res80 :
            Bare_Identifier
               := No_Bare_Gpr_Node;
      Token_Pos87 :
            Token_Index
               := No_Token_Index;
      Token_Res87 :
            Token_Index
               := No_Token_Index;
      List_Pos15 :
            Token_Index
               := No_Token_Index;
      List_Res15 :
            Bare_Identifier_List
               := No_Bare_Gpr_Node;
      Row_Pos57 :
            Token_Index
               := No_Token_Index;
      Token_Pos88 :
            Token_Index
               := No_Token_Index;
      Token_Res88 :
            Token_Index
               := No_Token_Index;
      Defer_Pos81 :
            Token_Index
               := No_Token_Index;
      Defer_Res81 :
            Bare_Attribute_Reference
               := No_Bare_Gpr_Node;
      Transform_Res47 :
            Bare_Variable_Reference
               := No_Bare_Gpr_Node;
      Transform_Diags47 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.Variable_Reference_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res47 := M.Instance;
      return Transform_Res47;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res47;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags47 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos56 := Pos;



--  Start list_code

    List_Pos15 := No_Token_Index;



Lst_Cpos15 := Row_Pos56;
Tmp_List15 := Get_Parse_List (Parser);

loop
   
Defer_Res80 :=
   Identifier_Transform_Parse0 (Parser, Lst_Cpos15);
Defer_Pos80 := Parser.Current_Pos;


   exit when Defer_Pos80 = No_Token_Index;

   List_Pos15 := Defer_Pos80;
   Lst_Cpos15 := List_Pos15;

   Tmp_List15.Nodes.Append (Defer_Res80);

      
--  Start tok_code

Token_Res87 := Lst_Cpos15;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res87));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Dot)
   then
       Token_Pos87 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos15 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos15,
             Expected_Token_Id => Gpr_Dot,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos87 := Lst_Cpos15 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos87 /= No_Token_Index then
          Lst_Cpos15 := Token_Pos87;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List15.Nodes.Length;
begin
   List_Res15 :=
      Allocate_Identifier_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos56;
      Token_End := (if Lst_Cpos15 = Row_Pos56
                    then Row_Pos56
                    else Lst_Cpos15 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos56, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res15,
      Kind              => Gpr_Identifier_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res15,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List15.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res15.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List15);

--  End list_code




if List_Pos15 /= No_Token_Index then

   Row_Pos56 := List_Pos15;

else
   Row_Pos56 := No_Token_Index;
   goto Exit_Row56_0;

end if;


--  Start opt_code












--  Start row_code

Row_Pos57 := Row_Pos56;



--  Start tok_code

Token_Res88 := Row_Pos57;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res88));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Tick)
   then
       Token_Pos88 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos57 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos57,
             Expected_Token_Id => Gpr_Tick,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos88 := Row_Pos57 + 1;
   end if;
end;

--  End tok_code




if Token_Pos88 /= No_Token_Index then

   Row_Pos57 := Token_Pos88;

else
   Row_Pos57 := No_Token_Index;
   goto Exit_Row57_0;

end if;


Defer_Res81 :=
   Attribute_Reference_Transform_Parse0 (Parser, Row_Pos57);
Defer_Pos81 := Parser.Current_Pos;




if Defer_Pos81 /= No_Token_Index then

   Row_Pos57 := Defer_Pos81;

else
   Row_Pos57 := No_Token_Index;
   goto Exit_Row57_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row57_0>>
pragma Warnings (On, "referenced");

--  End row_code


if Row_Pos57 = No_Token_Index then

         
   Defer_Res81 := No_Bare_Gpr_Node;



       
   Row_Pos57 := Row_Pos56;



end if;

--  End opt_code




if Row_Pos57 /= No_Token_Index then

   Row_Pos56 := Row_Pos57;

else
   Row_Pos56 := No_Token_Index;
   goto Exit_Row56_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row56_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos56 /= No_Token_Index then

   Transform_Res47 := Allocate_Variable_Reference (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res47,
      Kind => Gpr_Variable_Reference,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos56 = Pos
                            then No_Token_Index
                            else Row_Pos56 - 1));

      Initialize_Fields_For_Variable_Reference
        (Self => Transform_Res47, Variable_Reference_F_Variable_Name => List_Res15, Variable_Reference_F_Attribute_Ref => Defer_Res81);

         if List_Res15 /= null and then Is_Incomplete (List_Res15) then
            Transform_Res47.Last_Attempted_Child := 0;
         elsif List_Res15 /= null and then not Is_Ghost (List_Res15) then
            Transform_Res47.Last_Attempted_Child := -1;
         end if;
         if Defer_Res81 /= null and then Is_Incomplete (Defer_Res81) then
            Transform_Res47.Last_Attempted_Child := 0;
         elsif Defer_Res81 /= null and then not Is_Ghost (Defer_Res81) then
            Transform_Res47.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos56 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags47);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Variable_Reference_Transform_Parse0_Memo,
      Row_Pos56 /= No_Token_Index,
      Transform_Res47,
      Pos,
      Row_Pos56);


   Parser.Current_Pos := Row_Pos56;

   return Transform_Res47;
end Variable_Reference_Transform_Parse0;

   


function With_Decl_Transform_Parse0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_With_Decl
is
   use Bare_With_Decl_Memos;

      Row_Pos58 :
            Token_Index
               := No_Token_Index;
      Token_Pos89 :
            Token_Index
               := No_Token_Index;
      Token_Res89 :
            Token_Index
               := No_Token_Index;
      Opt_Res4 :
            Bare_Limited_Node
               := No_Bare_Gpr_Node;
      Token_Pos90 :
            Token_Index
               := No_Token_Index;
      Token_Res90 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos16 :
            Token_Index
               := No_Token_Index;
      Tmp_List16 :
            Free_Parse_List;
      Defer_Pos82 :
            Token_Index
               := No_Token_Index;
      Defer_Res82 :
            Bare_String_Literal
               := No_Bare_Gpr_Node;
      Token_Pos91 :
            Token_Index
               := No_Token_Index;
      Token_Res91 :
            Token_Index
               := No_Token_Index;
      List_Pos16 :
            Token_Index
               := No_Token_Index;
      List_Res16 :
            Bare_String_Literal_List
               := No_Bare_Gpr_Node;
      Token_Pos92 :
            Token_Index
               := No_Token_Index;
      Token_Res92 :
            Token_Index
               := No_Token_Index;
      Transform_Res48 :
            Bare_With_Decl
               := No_Bare_Gpr_Node;
      Transform_Diags48 :
            Ada.Containers.Count_Type;


   M : Memo_Entry := Get (Parser.Private_Part.With_Decl_Transform_Parse0_Memo, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res48 := M.Instance;
      return Transform_Res48;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return Transform_Res48;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code

Transform_Diags48 := Parser.Diagnostics.Length;


--  Start row_code

Row_Pos58 := Pos;



--  Start opt_code












--  Start tok_code

Token_Res89 := Row_Pos58;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res89));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Limited)
   then
       Token_Pos89 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos58 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos58,
             Expected_Token_Id => Gpr_Limited,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos89 := Row_Pos58 + 1;
   end if;
end;

--  End tok_code


if Token_Pos89 = No_Token_Index then

         Opt_Res4 := Allocate_Limited_Absent (Parser.Mem_Pool);
         Initialize
           (Self              => Opt_Res4,
            Kind              => Gpr_Limited_Absent,
            Unit              => Parser.Unit,
            Token_Start_Index => Row_Pos58,
            Token_End_Index   => No_Token_Index);


       
   Token_Pos89 := Row_Pos58;


else

      Opt_Res4 := Allocate_Limited_Present (Parser.Mem_Pool);
      Initialize
        (Self              => Opt_Res4,
         Kind              => Gpr_Limited_Present,
         Unit              => Parser.Unit,
         Token_Start_Index => Row_Pos58,
         Token_End_Index   => Token_Pos89 - 1);

end if;

--  End opt_code




if Token_Pos89 /= No_Token_Index then

   Row_Pos58 := Token_Pos89;

else
   Row_Pos58 := No_Token_Index;
   goto Exit_Row58_0;

end if;


--  Start tok_code

Token_Res90 := Row_Pos58;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res90));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_With)
   then
       Token_Pos90 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos58 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos58,
             Expected_Token_Id => Gpr_With,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos90 := Row_Pos58 + 1;
   end if;
end;

--  End tok_code




if Token_Pos90 /= No_Token_Index then

   Row_Pos58 := Token_Pos90;

else
   Row_Pos58 := No_Token_Index;
   goto Exit_Row58_0;

end if;


--  Start list_code

    List_Pos16 := No_Token_Index;



Lst_Cpos16 := Row_Pos58;
Tmp_List16 := Get_Parse_List (Parser);

loop
   
Defer_Res82 :=
   String_Literal_Transform_Parse0 (Parser, Lst_Cpos16);
Defer_Pos82 := Parser.Current_Pos;


   exit when Defer_Pos82 = No_Token_Index;

   List_Pos16 := Defer_Pos82;
   Lst_Cpos16 := List_Pos16;

   Tmp_List16.Nodes.Append (Defer_Res82);

      
--  Start tok_code

Token_Res91 := Lst_Cpos16;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res91));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Comma)
   then
       Token_Pos91 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos16 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos16,
             Expected_Token_Id => Gpr_Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos91 := Lst_Cpos16 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos91 /= No_Token_Index then
          Lst_Cpos16 := Token_Pos91;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List16.Nodes.Length;
begin
   List_Res16 :=
      Allocate_String_Literal_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos58;
      Token_End := (if Lst_Cpos16 = Row_Pos58
                    then Row_Pos58
                    else Lst_Cpos16 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos58, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res16,
      Kind              => Gpr_String_Literal_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res16,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Gpr_Node_Vectors.Vector renames
         Tmp_List16.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res16.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List16);

--  End list_code




if List_Pos16 /= No_Token_Index then

   Row_Pos58 := List_Pos16;

else
   Row_Pos58 := No_Token_Index;
   goto Exit_Row58_0;

end if;


--  Start tok_code

Token_Res92 := Row_Pos58;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res92));
begin
   if
      T.Kind /= From_Token_Kind (Gpr_Semicolon)
   then
       Token_Pos92 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos58 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos58,
             Expected_Token_Id => Gpr_Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos92 := Row_Pos58 + 1;
   end if;
end;

--  End tok_code




if Token_Pos92 /= No_Token_Index then

   Row_Pos58 := Token_Pos92;

else
   Row_Pos58 := No_Token_Index;
   goto Exit_Row58_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row58_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos58 /= No_Token_Index then

   Transform_Res48 := Allocate_With_Decl (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res48,
      Kind => Gpr_With_Decl,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos58 = Pos
                            then No_Token_Index
                            else Row_Pos58 - 1));

      Initialize_Fields_For_With_Decl
        (Self => Transform_Res48, With_Decl_F_Is_Limited => Opt_Res4, With_Decl_F_Path_Names => List_Res16);

         if Opt_Res4 /= null and then Is_Incomplete (Opt_Res4) then
            Transform_Res48.Last_Attempted_Child := 0;
         elsif Opt_Res4 /= null and then not Is_Ghost (Opt_Res4) then
            Transform_Res48.Last_Attempted_Child := -1;
         end if;
         if List_Res16 /= null and then Is_Incomplete (List_Res16) then
            Transform_Res48.Last_Attempted_Child := 0;
         elsif List_Res16 /= null and then not Is_Ghost (List_Res16) then
            Transform_Res48.Last_Attempted_Child := -1;
         end if;


elsif Row_Pos58 = No_Token_Index then
   Parser.Diagnostics.Set_Length (Transform_Diags48);
end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.With_Decl_Transform_Parse0_Memo,
      Row_Pos58 /= No_Token_Index,
      Transform_Res48,
      Pos,
      Row_Pos58);


   Parser.Current_Pos := Row_Pos58;

   return Transform_Res48;
end With_Decl_Transform_Parse0;


   -----------
   -- Reset --
   -----------

   procedure Reset (Parser : in out Parser_Type) is
      New_Parser : Parser_Type;
      --  We create this new parser instance to leverage creation of default
      --  values, so as to not repeat them.
   begin
      --  We just keep the private part, to not have to reallocate it
      New_Parser.Private_Part := Parser.Private_Part;

      --  And then reset everything else
      Parser := New_Parser;

      --  Reset the memo tables in the private part
         Bare_Ada_Context_Clause_Memos.Clear
           (Parser.Private_Part.Ada_Context_Item_Or_Parse0_Memo);
         Bare_Ada_Context_Clause_List_Memos.Clear
           (Parser.Private_Part.Ada_Context_List_Parse0_Memo);
         Bare_Ada_Skip_List_Memos.Clear
           (Parser.Private_Part.Ada_Context_Skip_List_Parse0_Memo);
         Bare_Ada_Library_Item_Memos.Clear
           (Parser.Private_Part.Ada_Library_Item_Transform_Parse8_Memo);
         Bare_Ada_Entity_Kind_Package_Memos.Clear
           (Parser.Private_Part.Ada_Pkg_Kind_Transform_Parse0_Memo);
         Bare_Ada_Pragma_Memos.Clear
           (Parser.Private_Part.Ada_Pragma_Transform_Parse0_Memo);
         Bare_Ada_Prelude_Memos.Clear
           (Parser.Private_Part.Ada_Prelude_Transform_Parse0_Memo);
         Bare_Ada_Entity_Kind_Memos.Clear
           (Parser.Private_Part.Ada_Subp_Kind_Or_Parse0_Memo);
         Bare_Ada_Use_Memos.Clear
           (Parser.Private_Part.Ada_Use_Clause_Transform_Parse0_Memo);
         Bare_Ada_With_Memos.Clear
           (Parser.Private_Part.Ada_With_Clause_Transform_Parse0_Memo);
         Bare_Gpr_Node_Memos.Clear
           (Parser.Private_Part.Associative_Array_Index_Or_Parse0_Memo);
         Bare_Attribute_Decl_Memos.Clear
           (Parser.Private_Part.Attribute_Decl_Transform_Parse0_Memo);
         Bare_Attribute_Reference_Memos.Clear
           (Parser.Private_Part.Attribute_Reference_Transform_Parse0_Memo);
         Bare_Builtin_Function_Call_Memos.Clear
           (Parser.Private_Part.Builtin_Function_Call_Transform_Parse0_Memo);
         Bare_Case_Construction_Memos.Clear
           (Parser.Private_Part.Case_Construction_Transform_Parse0_Memo);
         Bare_Case_Item_Memos.Clear
           (Parser.Private_Part.Case_Item_Transform_Parse0_Memo);
         Bare_Gpr_Node_Memos.Clear
           (Parser.Private_Part.Choice_Or_Parse0_Memo);
         Bare_Compilation_Unit_Memos.Clear
           (Parser.Private_Part.Compilation_Unit_Transform_Parse0_Memo);
         Bare_With_Decl_List_Memos.Clear
           (Parser.Private_Part.Context_Clauses_List_Parse0_Memo);
         Bare_Gpr_Node_Memos.Clear
           (Parser.Private_Part.Declarative_Item_Or_Parse0_Memo);
         Bare_Gpr_Node_List_Memos.Clear
           (Parser.Private_Part.Declarative_Items_List_Parse0_Memo);
         Bare_Choices_Memos.Clear
           (Parser.Private_Part.Discrete_Choice_List_List_Parse0_Memo);
         Bare_Gpr_Node_Memos.Clear
           (Parser.Private_Part.Dontskip_Ada_Library_Item0_Extract_Parse0_Memo);
         Bare_Gpr_Node_Memos.Clear
           (Parser.Private_Part.Dontskip_Ada_Library_Item1_Extract_Parse0_Memo);
         Bare_Gpr_Node_Memos.Clear
           (Parser.Private_Part.Dontskip_Ada_Library_Item2_Extract_Parse0_Memo);
         Bare_Gpr_Node_Memos.Clear
           (Parser.Private_Part.Dontskip_Ada_Pragma0_Extract_Parse0_Memo);
         Bare_Gpr_Node_Memos.Clear
           (Parser.Private_Part.Dontskip_Ada_Use_Clause0_Extract_Parse0_Memo);
         Bare_Empty_Decl_Memos.Clear
           (Parser.Private_Part.Empty_Declaration_Transform_Parse0_Memo);
         Bare_Term_List_Memos.Clear
           (Parser.Private_Part.Expression_List_Parse0_Memo);
         Bare_Terms_Memos.Clear
           (Parser.Private_Part.Expression_List_Transform_Parse0_Memo);
         Bare_Identifier_Memos.Clear
           (Parser.Private_Part.Identifier_Transform_Parse0_Memo);
         Bare_Num_Literal_Memos.Clear
           (Parser.Private_Part.Num_Literal_Transform_Parse0_Memo);
         Bare_Others_Designator_Memos.Clear
           (Parser.Private_Part.Others_Designator_Transform_Parse0_Memo);
         Bare_Package_Decl_Memos.Clear
           (Parser.Private_Part.Package_Decl_Transform_Parse0_Memo);
         Bare_Package_Extension_Memos.Clear
           (Parser.Private_Part.Package_Extension_Transform_Parse0_Memo);
         Bare_Package_Renaming_Memos.Clear
           (Parser.Private_Part.Package_Renaming_Transform_Parse0_Memo);
         Bare_Package_Spec_Memos.Clear
           (Parser.Private_Part.Package_Spec_Transform_Parse0_Memo);
         Bare_Project_Declaration_Memos.Clear
           (Parser.Private_Part.Project_Declaration_Transform_Parse0_Memo);
         Bare_Project_Extension_Memos.Clear
           (Parser.Private_Part.Project_Extension_Transform_Parse0_Memo);
         Bare_Project_Qualifier_Memos.Clear
           (Parser.Private_Part.Project_Qualifier_Or_Parse0_Memo);
         Bare_Project_Memos.Clear
           (Parser.Private_Part.Project_Transform_Parse0_Memo);
         Bare_Gpr_Node_Memos.Clear
           (Parser.Private_Part.Simple_Declarative_Item_Or_Parse0_Memo);
         Bare_Gpr_Node_List_Memos.Clear
           (Parser.Private_Part.Simple_Declarative_Items_List_Parse0_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.Static_Name_Or_Parse0_Memo);
         Bare_String_Literal_At_Memos.Clear
           (Parser.Private_Part.String_Literal_At_Transform_Parse0_Memo);
         Bare_String_Literal_Memos.Clear
           (Parser.Private_Part.String_Literal_Transform_Parse0_Memo);
         Bare_Gpr_Node_Memos.Clear
           (Parser.Private_Part.Term_Or_Parse0_Memo);
         Bare_Type_Reference_Memos.Clear
           (Parser.Private_Part.Type_Reference_Transform_Parse0_Memo);
         Bare_Typed_String_Decl_Memos.Clear
           (Parser.Private_Part.Typed_String_Decl_Transform_Parse0_Memo);
         Bare_Variable_Decl_Memos.Clear
           (Parser.Private_Part.Variable_Decl_Transform_Parse0_Memo);
         Bare_Variable_Reference_Memos.Clear
           (Parser.Private_Part.Variable_Reference_Transform_Parse0_Memo);
         Bare_With_Decl_Memos.Clear
           (Parser.Private_Part.With_Decl_Transform_Parse0_Memo);
   end Reset;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Parser : in out Parser_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Parser_Private_Part_Type, Parser_Private_Part);
      procedure Free is new Ada.Unchecked_Deallocation
        (Free_Parse_List_Record, Free_Parse_List);

      Cur : Free_Parse_List renames Parser.Private_Part.Parse_Lists;
   begin
      while Cur /= null loop
         declare
            Next : constant Free_Parse_List := Cur.Next;
         begin
            Cur.Nodes.Destroy;
            Free (Cur);
            Cur := Next;
         end;
      end loop;
      Free (Parser.Private_Part);
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Parser : in out Parser_Type) is
   begin
      Parser.Private_Part := new Parser_Private_Part_Type'(others => <>);
   end Initialize;

   --------------------
   -- Get_Parse_List --
   --------------------

   function Get_Parse_List (Parser : Parser_Type) return Free_Parse_List is
      Lists  : Free_Parse_List renames Parser.Private_Part.Parse_Lists;
      Result : Free_Parse_List;
   begin
      if Lists = null then
         Result := new Free_Parse_List_Record;

      else
         Result := Lists;
         Lists := Lists.Next;
      end if;

      return Result;
   end Get_Parse_List;

   ------------------------
   -- Release_Parse_List --
   ------------------------

   procedure Release_Parse_List
     (Parser : Parser_Type; List : in out Free_Parse_List)
   is
      Lists  : Free_Parse_List renames Parser.Private_Part.Parse_Lists;
   begin
      List.Nodes.Clear;
      List.Next := Lists;
      Lists := List;
      List := null;
   end Release_Parse_List;

end Gpr_Parser.Parsers;
