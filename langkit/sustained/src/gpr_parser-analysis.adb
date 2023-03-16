
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--









with Ada.Containers;            use Ada.Containers;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");

with GNATCOLL.Traces;

pragma Warnings (Off, "referenced");
with Gpr_Parser_Support.Symbols; use Gpr_Parser_Support.Symbols;
pragma Warnings (On, "referenced");

with Gpr_Parser_Support.Types;        use Gpr_Parser_Support.Types;

with Gpr_Parser.Common;
with Gpr_Parser.Private_Converters;
use Gpr_Parser.Private_Converters;
with Gpr_Parser.Public_Converters; use Gpr_Parser.Public_Converters;




package body Gpr_Parser.Analysis is

   use Gpr_Parser.Implementation;
   use AST_Envs;

      


      

      
      function To_Public_Gpr_Node_Array
         (Value : Internal_Entity_Array_Access) return Gpr_Node_Array;


      

      


      

      


      

      


      


      

      

      

      

      



   ----------------
   -- Do_Release --
   ----------------

   procedure Do_Release (Self : in out Event_Handler_Interface'Class) is
   begin
      Self.Release;
   end Do_Release;

   ----------------
   -- Do_Release --
   ----------------

   procedure Do_Release (Provider : in out Unit_Provider_Interface'Class) is
   begin
      Provider.Release;
   end Do_Release;

   ------------------------------------
   -- Create_Unit_Provider_Reference --
   ------------------------------------

   function Create_Unit_Provider_Reference
     (Provider : Unit_Provider_Interface'Class) return Unit_Provider_Reference
   is
   begin
      return Result : Unit_Provider_Reference do
         Result.Set (Provider);
      end return;
   end Create_Unit_Provider_Reference;

   ------------------------------------
   -- Create_Event_Handler_Reference --
   ------------------------------------

   function Create_Event_Handler_Reference
     (Handler : Event_Handler_Interface'Class) return Event_Handler_Reference
   is
   begin
      return Result : Event_Handler_Reference do
         Result.Set (Handler);
      end return;
   end Create_Event_Handler_Reference;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Charset       : String := Default_Charset;
      File_Reader   : File_Reader_Reference := No_File_Reader_Reference;
      Unit_Provider : Unit_Provider_Reference := No_Unit_Provider_Reference;
      Event_Handler : Event_Handler_Reference := No_Event_Handler_Ref;
      With_Trivia   : Boolean := True;
      Tab_Stop      : Positive := 8)
      return Analysis_Context
   is
      use Unit_Provider_References;

      FR     : Internal_File_Reader_Access :=
         Wrap_Public_File_Reader (File_Reader);
      UP     : Internal_Unit_Provider_Access :=
         Wrap_Public_Provider (Unit_Provider);
      EH     : Internal_Event_Handler_Access :=
         Wrap_Public_Event_Handler (Event_Handler);
      Result : Internal_Context := Allocate_Context;
   begin
      Initialize_Context (Result, Charset, FR, UP, EH, With_Trivia, Tab_Stop);

      --  Create_Context created ownership shares for itself, so don't forget
      --  to remove the shares on FR and UP.
      Dec_Ref (FR);
      Dec_Ref (UP);
      Dec_Ref (EH);

      return Context : constant Analysis_Context := Wrap_Context (Result)
      do
         --  Result has one ownership share and the call to Wrap_Context
         --  creates a new one, so don't forget to dec-ref before returning.
         Dec_Ref (Result);
      end return;
   end Create_Context;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit
     (Context       : Analysis_Context'Class;
      Unit_Filename : String) return Boolean is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      return Has_Unit (Unwrap_Context (Context), Unit_Filename);
   end Has_Unit;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      return Wrap_Unit
        (Get_From_File (Unwrap_Context (Context), Filename, Charset,
                        Reparse, Rule));
   end Get_From_File;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      return Wrap_Unit
        (Get_From_Buffer (Unwrap_Context (Context), Filename, Charset,
                          Buffer, Rule));
   end Get_From_Buffer;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : Ada.Strings.Unbounded.Unbounded_String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit
   is
      Bytes       : Big_String_Access;
      Bytes_Count : Natural;
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      Get_String (Buffer, Bytes, Bytes_Count);
      return Wrap_Unit
        (Get_From_Buffer (Unwrap_Context (Context), Filename, Charset,
                          Bytes (1 .. Bytes_Count), Rule));
   end Get_From_Buffer;

   --------------------
   -- Get_With_Error --
   --------------------

   function Get_With_Error
     (Context  : Analysis_Context'Class;
      Filename : String;
      Error    : Text_Type;
      Charset  : String := "";
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit
   is
      Result : Internal_Unit;
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      Result := Implementation.Get_With_Error
        (Unwrap_Context (Context), Filename, Error, Charset, Rule);
      return Wrap_Unit (Result);
   end Get_With_Error;


   -------------------
   -- Unit_Provider --
   -------------------

   function Unit_Provider
     (Context : Analysis_Context'Class) return Unit_Provider_Reference
   is
      Provider : Internal_Unit_Provider_Access;
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      --  By design, Unit_Provider_Wrapper is supposed to be the only
      --  implementation of the Internal_Unit_Provider interface.
      Provider := Unit_Provider (Unwrap_Context (Context));
      if Provider.all not in Unit_Provider_Wrapper'Class then
         raise Program_Error;
      end if;

      return Unit_Provider_Wrapper (Provider.all).Internal;
   end Unit_Provider;

   ----------
   -- Hash --
   ----------

   function Hash (Context : Analysis_Context) return Ada.Containers.Hash_Type
   is
   begin
      return Hash (Unwrap_Context (Context));
   end Hash;

   ---------------------
   -- Has_With_Trivia --
   ---------------------

   function Has_With_Trivia (Context : Analysis_Context'Class) return Boolean
   is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      return Has_With_Trivia (Unwrap_Context (Context));
   end Has_With_Trivia;

   --------------------------------------------
   -- Discard_Errors_In_Populate_Lexical_Env --
   --------------------------------------------

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Analysis_Context'Class; Discard : Boolean) is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      Discard_Errors_In_Populate_Lexical_Env
        (Unwrap_Context (Context), Discard);
   end Discard_Errors_In_Populate_Lexical_Env;

   ----------------------------------
   -- Set_Logic_Resolution_Timeout --
   ----------------------------------

   procedure Set_Logic_Resolution_Timeout
     (Context : Analysis_Context'Class; Timeout : Natural) is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      Set_Logic_Resolution_Timeout (Unwrap_Context (Context), Timeout);
   end Set_Logic_Resolution_Timeout;

   ---------------------------
   -- Set_Lookup_Cache_Mode --
   ---------------------------

   procedure Set_Lookup_Cache_Mode (Mode : Lookup_Cache_Kind) is
   begin
      Lookup_Cache_Mode := Mode;
   end Set_Lookup_Cache_Mode;

   --------------------------
   -- Has_Rewriting_Handle --
   --------------------------

   function Has_Rewriting_Handle
     (Context : Analysis_Context'Class) return Boolean is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      return Has_Rewriting_Handle (Unwrap_Context (Context));
   end Has_Rewriting_Handle;

   ----------------------
   -- Get_Symbol_Table --
   ----------------------

   function Get_Symbol_Table
     (Context : Analysis_Context'Class) return Symbol_Table
   is
   begin
      return Context.Internal.Symbols;
   end Get_Symbol_Table;

   -------------
   -- Context --
   -------------

   function Context (Unit : Analysis_Unit'Class) return Analysis_Context is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Wrap_Context (Context (Unwrap_Unit (Unit)));
   end Context;

   ----------
   -- Hash --
   ----------

   function Hash (Unit : Analysis_Unit) return Ada.Containers.Hash_Type
   is
   begin
      return Hash (Unwrap_Unit (Unit));
   end Hash;

   -------------
   -- Reparse --
   -------------

   procedure Reparse (Unit : Analysis_Unit'Class; Charset : String := "") is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Reparse (Unwrap_Unit (Unit), Charset);
   end Reparse;

   -------------
   -- Reparse --
   -------------

   procedure Reparse
     (Unit : Analysis_Unit'Class; Charset : String := ""; Buffer  : String) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Reparse (Unwrap_Unit (Unit), Charset, Buffer);
   end Reparse;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env (Unit : Analysis_Unit'Class) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Populate_Lexical_Env (Unwrap_Unit (Unit));
   end Populate_Lexical_Env;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Unit : Analysis_Unit'Class) return String is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Get_Filename (Unwrap_Unit (Unit));
   end Get_Filename;

   -----------------
   -- Get_Charset --
   -----------------

   function Get_Charset (Unit : Analysis_Unit'Class) return String is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Get_Charset (Unwrap_Unit (Unit));
   end Get_Charset;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics (Unit : Analysis_Unit'Class) return Boolean is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Has_Diagnostics (Unwrap_Unit (Unit));
   end Has_Diagnostics;

   -----------------
   -- Diagnostics --
   -----------------

   function Diagnostics (Unit : Analysis_Unit'Class) return Diagnostics_Array
   is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Implementation.Diagnostics (Unwrap_Unit (Unit));
   end Diagnostics;

   ---------------------------
   -- Format_GNU_Diagnostic --
   ---------------------------

   function Format_GNU_Diagnostic
     (Unit : Analysis_Unit'Class; D : Diagnostic) return String is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Format_GNU_Diagnostic (Unwrap_Unit (Unit), D);
   end Format_GNU_Diagnostic;

   ----------
   -- Root --
   ----------

   function Root (Unit : Analysis_Unit'Class) return Gpr_Node is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Wrap_Node (Root (Unwrap_Unit (Unit)));
   end Root;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Unit : Analysis_Unit'Class) return Token_Reference is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return First_Token (Unwrap_Unit (Unit));
   end First_Token;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Unit : Analysis_Unit'Class) return Token_Reference is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Last_Token (Unwrap_Unit (Unit));
   end Last_Token;

   -----------------
   -- Token_Count --
   -----------------

   function Token_Count (Unit : Analysis_Unit'Class) return Natural is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Token_Count (Unwrap_Unit (Unit));
   end Token_Count;

   ------------------
   -- Trivia_Count --
   ------------------

   function Trivia_Count (Unit : Analysis_Unit'Class) return Natural is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Trivia_Count (Unwrap_Unit (Unit));
   end Trivia_Count;

   ----------
   -- Unit --
   ----------

   function Unit (Token : Token_Reference) return Analysis_Unit is
   begin
      return Wrap_Unit (Get_Token_Unit (Token));
   end Unit;

   ----------
   -- Text --
   ----------

   function Text (Unit : Analysis_Unit'Class) return Text_Type is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Implementation.Text (Unwrap_Unit (Unit));
   end Text;

   ------------------
   -- Lookup_Token --
   ------------------

   function Lookup_Token
     (Unit : Analysis_Unit'Class; Sloc : Source_Location)
      return Token_Reference is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Lookup_Token (Unwrap_Unit (Unit), Sloc);
   end Lookup_Token;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Unit : Analysis_Unit; Line_Number : Positive) return Text_Type is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Get_Line (Unwrap_Unit (Unit), Line_Number);
   end Get_Line;

   ----------------------
   -- Dump_Lexical_Env --
   ----------------------

   procedure Dump_Lexical_Env (Unit : Analysis_Unit'Class) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Dump_Lexical_Env (Unwrap_Unit (Unit));
   end Dump_Lexical_Env;

   ------------------------
   -- Trigger_Envs_Debug --
   ------------------------

   procedure Trigger_Envs_Debug (Is_Active : Boolean) is
   begin
      Gpr_Parser_Support.Lexical_Envs.Me.Set_Active (Is_Active);
   end Trigger_Envs_Debug;

   -----------
   -- Print --
   -----------

   procedure Print (Unit : Analysis_Unit'Class; Show_Slocs : Boolean := True)
   is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Print (Unwrap_Unit (Unit), Show_Slocs);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia (Unit : Analysis_Unit'Class) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      PP_Trivia (Unwrap_Unit (Unit));
   end PP_Trivia;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Node : Gpr_Node'Class) return Boolean is
     (Node.Internal.Node = null);

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Node : Gpr_Node'Class) return Boolean
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Is_Token_Node (Node.Internal.Node);
   end Is_Token_Node;

   ------------------
   -- Is_Synthetic --
   ------------------

   function Is_Synthetic (Node : Gpr_Node'Class) return Boolean
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Is_Synthetic (Node.Internal.Node);
   end Is_Synthetic;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Gpr_Node'Class) return Boolean is
   begin
      Check_Safety_Net (L);
      Check_Safety_Net (R);
      return Compare_Entity (L.Internal, R.Internal);
   end "=";

   ------------
   -- Equals --
   ------------

   function Equals (L, R : Gpr_Node) return Boolean is
   begin
      Check_Safety_Net (L);
      Check_Safety_Net (R);
      return Compare_Entity (L.Internal, R.Internal);
   end Equals;

   -----------
   -- Image --
   -----------

   function Image (Node : Gpr_Node'Class) return String is
   begin
      Check_Safety_Net (Node);
      return Image (Node.Internal);
   end Image;

   -----------------------
   -- Entity converters --
   -----------------------

      function As_Gpr_Node
        (Node : Gpr_Node'Class) return Gpr_Node
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Gpr_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         

      end;
      function As_Ada_Prelude_Node
        (Node : Gpr_Node'Class) return Ada_Prelude_Node
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Prelude_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Prelude_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaPreludeNode";
         
            end if;
      end;
      function As_Ada_Access_Subp
        (Node : Gpr_Node'Class) return Ada_Access_Subp
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Access_Subp;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Access_Subp_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaAccessSubp";
         
            end if;
      end;
      function As_Ada_Context_Clause
        (Node : Gpr_Node'Class) return Ada_Context_Clause
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Context_Clause;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Context_Clause then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaContextClause";
         
            end if;
      end;
      function As_Base_List
        (Node : Gpr_Node'Class) return Base_List
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Base_List then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseList";
         
            end if;
      end;
      function As_Ada_Context_Clause_List
        (Node : Gpr_Node'Class) return Ada_Context_Clause_List
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Context_Clause_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Context_Clause_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaContextClause.list";
         
            end if;
      end;
      function As_Ada_Entity_Kind
        (Node : Gpr_Node'Class) return Ada_Entity_Kind
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Entity_Kind;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Entity_Kind then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaEntityKind";
         
            end if;
      end;
      function As_Ada_Entity_Kind_Function
        (Node : Gpr_Node'Class) return Ada_Entity_Kind_Function
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Entity_Kind_Function;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Entity_Kind_Function_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaEntityKind.Function";
         
            end if;
      end;
      function As_Ada_Entity_Kind_Package
        (Node : Gpr_Node'Class) return Ada_Entity_Kind_Package
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Entity_Kind_Package;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Entity_Kind_Package_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaEntityKind.Package";
         
            end if;
      end;
      function As_Ada_Entity_Kind_Procedure
        (Node : Gpr_Node'Class) return Ada_Entity_Kind_Procedure
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Entity_Kind_Procedure;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Entity_Kind_Procedure_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaEntityKind.Procedure";
         
            end if;
      end;
      function As_Ada_Generic
        (Node : Gpr_Node'Class) return Ada_Generic
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Generic;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Generic_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaGeneric";
         
            end if;
      end;
      function As_Ada_Library_Item
        (Node : Gpr_Node'Class) return Ada_Library_Item
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Library_Item;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Library_Item_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaLibraryItem";
         
            end if;
      end;
      function As_Ada_Main
        (Node : Gpr_Node'Class) return Ada_Main
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Main;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Main then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaMain";
         
            end if;
      end;
      function As_Ada_Pkg
        (Node : Gpr_Node'Class) return Ada_Pkg
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Pkg;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Pkg_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaPkg";
         
            end if;
      end;
      function As_Ada_Pkg_Body
        (Node : Gpr_Node'Class) return Ada_Pkg_Body
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Pkg_Body;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Pkg_Body_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaPkgBody";
         
            end if;
      end;
      function As_Ada_Pragma
        (Node : Gpr_Node'Class) return Ada_Pragma
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Pragma;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Pragma_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaPragma";
         
            end if;
      end;
      function As_Ada_Prelude
        (Node : Gpr_Node'Class) return Ada_Prelude
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Prelude;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Prelude_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaPrelude";
         
            end if;
      end;
      function As_Ada_Prelude_Node_List
        (Node : Gpr_Node'Class) return Ada_Prelude_Node_List
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Prelude_Node_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Prelude_Node_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaPreludeNode.list";
         
            end if;
      end;
      function As_Ada_Separate
        (Node : Gpr_Node'Class) return Ada_Separate
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Separate;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Separate_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaSeparate";
         
            end if;
      end;
      function As_Ada_Skip
        (Node : Gpr_Node'Class) return Ada_Skip
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Skip;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Skip_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaSkip";
         
            end if;
      end;
      function As_Ada_Skip_List
        (Node : Gpr_Node'Class) return Ada_Skip_List
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Skip_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Skip_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaSkip.list";
         
            end if;
      end;
      function As_Ada_Subp
        (Node : Gpr_Node'Class) return Ada_Subp
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Subp;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Subp_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaSubp";
         
            end if;
      end;
      function As_Ada_Use
        (Node : Gpr_Node'Class) return Ada_Use
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Use;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_Use_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaUse";
         
            end if;
      end;
      function As_Ada_With
        (Node : Gpr_Node'Class) return Ada_With
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_With;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_With_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaWith";
         
            end if;
      end;
      function As_Ada_With_Formal
        (Node : Gpr_Node'Class) return Ada_With_Formal
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_With_Formal;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Ada_With_Formal_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaWithFormal";
         
            end if;
      end;
      function As_All_Qualifier
        (Node : Gpr_Node'Class) return All_Qualifier
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_All_Qualifier;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_All_Qualifier then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AllQualifier";
         
            end if;
      end;
      function As_All_Qualifier_Absent
        (Node : Gpr_Node'Class) return All_Qualifier_Absent
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_All_Qualifier_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_All_Qualifier_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AllQualifier.Absent";
         
            end if;
      end;
      function As_All_Qualifier_Present
        (Node : Gpr_Node'Class) return All_Qualifier_Present
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_All_Qualifier_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_All_Qualifier_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AllQualifier.Present";
         
            end if;
      end;
      function As_Attribute_Decl
        (Node : Gpr_Node'Class) return Attribute_Decl
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Attribute_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Attribute_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AttributeDecl";
         
            end if;
      end;
      function As_Attribute_Reference
        (Node : Gpr_Node'Class) return Attribute_Reference
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Attribute_Reference;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Attribute_Reference_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to AttributeReference";
         
            end if;
      end;
      function As_Builtin_Function_Call
        (Node : Gpr_Node'Class) return Builtin_Function_Call
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Builtin_Function_Call;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Builtin_Function_Call_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to BuiltinFunctionCall";
         
            end if;
      end;
      function As_Case_Construction
        (Node : Gpr_Node'Class) return Case_Construction
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Case_Construction;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Case_Construction_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to CaseConstruction";
         
            end if;
      end;
      function As_Case_Item
        (Node : Gpr_Node'Class) return Case_Item
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Case_Item;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Case_Item_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to CaseItem";
         
            end if;
      end;
      function As_Case_Item_List
        (Node : Gpr_Node'Class) return Case_Item_List
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Case_Item_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Case_Item_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to CaseItem.list";
         
            end if;
      end;
      function As_Gpr_Node_List
        (Node : Gpr_Node'Class) return Gpr_Node_List
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Gpr_Node_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Gpr_Node_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to GprNode.list";
         
            end if;
      end;
      function As_Choices
        (Node : Gpr_Node'Class) return Choices
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Choices;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Choices_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to Choices";
         
            end if;
      end;
      function As_Compilation_Unit
        (Node : Gpr_Node'Class) return Compilation_Unit
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Compilation_Unit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Compilation_Unit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to CompilationUnit";
         
            end if;
      end;
      function As_Empty_Decl
        (Node : Gpr_Node'Class) return Empty_Decl
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Empty_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Empty_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to EmptyDecl";
         
            end if;
      end;
      function As_Expr
        (Node : Gpr_Node'Class) return Expr
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Expr then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to Expr";
         
            end if;
      end;
      function As_Expr_List
        (Node : Gpr_Node'Class) return Expr_List
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Expr_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Expr_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to Expr.list";
         
            end if;
      end;
      function As_Single_Tok_Node
        (Node : Gpr_Node'Class) return Single_Tok_Node
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Single_Tok_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Single_Tok_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to SingleTokNode";
         
            end if;
      end;
      function As_Identifier
        (Node : Gpr_Node'Class) return Identifier
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Identifier;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Identifier_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to Identifier";
         
            end if;
      end;
      function As_Identifier_List
        (Node : Gpr_Node'Class) return Identifier_List
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Identifier_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Identifier_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to Identifier.list";
         
            end if;
      end;
      function As_Limited_Node
        (Node : Gpr_Node'Class) return Limited_Node
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Limited_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Limited_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to Limited";
         
            end if;
      end;
      function As_Limited_Absent
        (Node : Gpr_Node'Class) return Limited_Absent
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Limited_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Limited_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to Limited.Absent";
         
            end if;
      end;
      function As_Limited_Present
        (Node : Gpr_Node'Class) return Limited_Present
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Limited_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Limited_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to Limited.Present";
         
            end if;
      end;
      function As_Num_Literal
        (Node : Gpr_Node'Class) return Num_Literal
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Num_Literal;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Num_Literal_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to NumLiteral";
         
            end if;
      end;
      function As_Others_Designator
        (Node : Gpr_Node'Class) return Others_Designator
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Others_Designator;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Others_Designator_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to OthersDesignator";
         
            end if;
      end;
      function As_Package_Decl
        (Node : Gpr_Node'Class) return Package_Decl
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Package_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Package_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to PackageDecl";
         
            end if;
      end;
      function As_Package_Extension
        (Node : Gpr_Node'Class) return Package_Extension
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Package_Extension;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Package_Extension_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to PackageExtension";
         
            end if;
      end;
      function As_Package_Renaming
        (Node : Gpr_Node'Class) return Package_Renaming
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Package_Renaming;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Package_Renaming_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to PackageRenaming";
         
            end if;
      end;
      function As_Package_Spec
        (Node : Gpr_Node'Class) return Package_Spec
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Package_Spec;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Package_Spec_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to PackageSpec";
         
            end if;
      end;
      function As_Prefix
        (Node : Gpr_Node'Class) return Prefix
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Prefix;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Prefix_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to Prefix";
         
            end if;
      end;
      function As_Private_Node
        (Node : Gpr_Node'Class) return Private_Node
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Private_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Private_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to Private";
         
            end if;
      end;
      function As_Private_Absent
        (Node : Gpr_Node'Class) return Private_Absent
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Private_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Private_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to Private.Absent";
         
            end if;
      end;
      function As_Private_Present
        (Node : Gpr_Node'Class) return Private_Present
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Private_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Private_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to Private.Present";
         
            end if;
      end;
      function As_Project
        (Node : Gpr_Node'Class) return Project
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Project;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Project_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to Project";
         
            end if;
      end;
      function As_Project_Declaration
        (Node : Gpr_Node'Class) return Project_Declaration
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Project_Declaration;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Project_Declaration_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to ProjectDeclaration";
         
            end if;
      end;
      function As_Project_Extension
        (Node : Gpr_Node'Class) return Project_Extension
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Project_Extension;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Project_Extension_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to ProjectExtension";
         
            end if;
      end;
      function As_Project_Qualifier
        (Node : Gpr_Node'Class) return Project_Qualifier
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Project_Qualifier;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Project_Qualifier then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to ProjectQualifier";
         
            end if;
      end;
      function As_Project_Qualifier_Abstract
        (Node : Gpr_Node'Class) return Project_Qualifier_Abstract
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Project_Qualifier_Abstract;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Project_Qualifier_Abstract_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to ProjectQualifier.Abstract";
         
            end if;
      end;
      function As_Project_Qualifier_Aggregate
        (Node : Gpr_Node'Class) return Project_Qualifier_Aggregate
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Project_Qualifier_Aggregate;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Project_Qualifier_Aggregate_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to ProjectQualifier.Aggregate";
         
            end if;
      end;
      function As_Project_Qualifier_Aggregate_Library
        (Node : Gpr_Node'Class) return Project_Qualifier_Aggregate_Library
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Project_Qualifier_Aggregate_Library;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Project_Qualifier_Aggregate_Library_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to ProjectQualifier.AggregateLibrary";
         
            end if;
      end;
      function As_Project_Qualifier_Configuration
        (Node : Gpr_Node'Class) return Project_Qualifier_Configuration
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Project_Qualifier_Configuration;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Project_Qualifier_Configuration_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to ProjectQualifier.Configuration";
         
            end if;
      end;
      function As_Project_Qualifier_Library
        (Node : Gpr_Node'Class) return Project_Qualifier_Library
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Project_Qualifier_Library;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Project_Qualifier_Library_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to ProjectQualifier.Library";
         
            end if;
      end;
      function As_Project_Qualifier_Standard
        (Node : Gpr_Node'Class) return Project_Qualifier_Standard
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Project_Qualifier_Standard;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Project_Qualifier_Standard_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to ProjectQualifier.Standard";
         
            end if;
      end;
      function As_Project_Reference
        (Node : Gpr_Node'Class) return Project_Reference
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Project_Reference;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Project_Reference_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to ProjectReference";
         
            end if;
      end;
      function As_String_Literal
        (Node : Gpr_Node'Class) return String_Literal
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_String_Literal;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_String_Literal_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to StringLiteral";
         
            end if;
      end;
      function As_String_Literal_At
        (Node : Gpr_Node'Class) return String_Literal_At
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_String_Literal_At;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_String_Literal_At_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to StringLiteralAt";
         
            end if;
      end;
      function As_String_Literal_List
        (Node : Gpr_Node'Class) return String_Literal_List
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_String_Literal_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_String_Literal_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to StringLiteral.list";
         
            end if;
      end;
      function As_Term_List
        (Node : Gpr_Node'Class) return Term_List
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Term_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Term_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to TermList";
         
            end if;
      end;
      function As_Term_List_List
        (Node : Gpr_Node'Class) return Term_List_List
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Term_List_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Term_List_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to TermList.list";
         
            end if;
      end;
      function As_Terms
        (Node : Gpr_Node'Class) return Terms
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Terms;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Terms_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to Terms";
         
            end if;
      end;
      function As_Type_Reference
        (Node : Gpr_Node'Class) return Type_Reference
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Type_Reference;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Type_Reference_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to TypeReference";
         
            end if;
      end;
      function As_Typed_String_Decl
        (Node : Gpr_Node'Class) return Typed_String_Decl
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Typed_String_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Typed_String_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to TypedStringDecl";
         
            end if;
      end;
      function As_Variable_Decl
        (Node : Gpr_Node'Class) return Variable_Decl
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Variable_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Variable_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to VariableDecl";
         
            end if;
      end;
      function As_Variable_Reference
        (Node : Gpr_Node'Class) return Variable_Reference
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Variable_Reference;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_Variable_Reference_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to VariableReference";
         
            end if;
      end;
      function As_With_Decl
        (Node : Gpr_Node'Class) return With_Decl
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_With_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_With_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to WithDecl";
         
            end if;
      end;
      function As_With_Decl_List
        (Node : Gpr_Node'Class) return With_Decl_List
      is
         N : constant Bare_Gpr_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_With_Decl_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Gpr_With_Decl_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Gpr_Parser: invalid type conversion from "
              & Node.Kind_Name
              & " to WithDecl.list";
         
            end if;
      end;

   -----------------------
   -- Entity primitives --
   -----------------------

   ----------
   -- Hash --
   ----------

   function Hash
     (Node : Gpr_Node) return Ada.Containers.Hash_Type is
   begin
      Check_Safety_Net (Node);
      return Hash_Entity (Node.Internal);
   end Hash;

   ----------
   -- Kind --
   ----------

   function Kind (Node : Gpr_Node'Class) return Gpr_Node_Kind_Type
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Node.Internal.Node.Kind;
   end Kind;

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Node : Gpr_Node'Class) return String is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Kind_Name (Node.Internal.Node);
   end Kind_Name;

      


      
      function To_Public_Gpr_Node_Array
         (Value : Internal_Entity_Array_Access) return Gpr_Node_Array is
      begin
         return Result : Gpr_Node_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Node (Value.Items (I).Node, Value.Items (I).Info)
               ;
            end loop;
         end return;
      end;


      


      


      









         
   function Parent
     (Node : Gpr_Node'Class) return Gpr_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Gpr_Parser.Implementation.Parent
            (Bare_Gpr_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info);

   end;

         
   function Parents
     (Node : Gpr_Node'Class;
      With_Self : Boolean := True) return Gpr_Node_Array is
      


         Internal_Arg_With_Self : Boolean;
      Property_Result : Internal_Entity_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_With_Self :=
            With_Self;

      
      Property_Result :=
         Gpr_Parser.Implementation.Parents
            (Bare_Gpr_Node (Node.Internal.Node), Internal_Arg_With_Self, E_Info => Node.Internal.Info);

         return Result : constant Gpr_Node_Array :=
            To_Public_Gpr_Node_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function Children
     (Node : Gpr_Node'Class) return Gpr_Node_Array is
      


      Property_Result : Internal_Entity_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Gpr_Parser.Implementation.Children
            (Bare_Gpr_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Gpr_Node_Array :=
            To_Public_Gpr_Node_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function Token_Start
     (Node : Gpr_Node'Class) return Token_Reference is
      


      Property_Result : Token_Reference;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Gpr_Parser.Implementation.Token_Start
            (Bare_Gpr_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function Token_End
     (Node : Gpr_Node'Class) return Token_Reference is
      


      Property_Result : Token_Reference;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Gpr_Parser.Implementation.Token_End
            (Bare_Gpr_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function Child_Index
     (Node : Gpr_Node'Class) return Integer is
      


      Property_Result : Integer;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Gpr_Parser.Implementation.Child_Index
            (Bare_Gpr_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function Previous_Sibling
     (Node : Gpr_Node'Class) return Gpr_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Gpr_Parser.Implementation.Previous_Sibling
            (Bare_Gpr_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info);

   end;

         
   function Next_Sibling
     (Node : Gpr_Node'Class) return Gpr_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Gpr_Parser.Implementation.Next_Sibling
            (Bare_Gpr_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info);

   end;

         
   function Unit
     (Node : Gpr_Node'Class) return Analysis_Unit is
      


      Property_Result : Internal_Unit;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Gpr_Parser.Implementation.Unit
            (Bare_Gpr_Node (Node.Internal.Node));

         return Wrap_Unit (Property_Result);

   end;

         
   function Is_Ghost
     (Node : Gpr_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Gpr_Parser.Implementation.Is_Ghost
            (Bare_Gpr_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function Full_Sloc_Image
     (Node : Gpr_Node'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Gpr_Parser.Implementation.Full_Sloc_Image
            (Bare_Gpr_Node (Node.Internal.Node));

         return Result : constant Text_Type :=
            Property_Result.Content
         do
            Free_Internal;
         end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;










         
   

   function F_Subp_Kind
     (Node : Ada_Access_Subp'Class) return Ada_Entity_Kind
   is
      Result : Bare_Ada_Entity_Kind;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_Access_Subp_F_Subp_Kind (Node.Internal.Node);
         if Result = null then
            return No_Ada_Entity_Kind;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subp_Kind;

         function F_Subp_Kind
           (Node : Ada_Access_Subp'Class) return Gpr_Ada_Entity_Kind
         is (Ada_Entity_Kind'(Node.F_Subp_Kind).Kind);

         
   

   function F_Skips
     (Node : Ada_Access_Subp'Class) return Ada_Skip_List
   is
      Result : Bare_Ada_Skip_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_Access_Subp_F_Skips (Node.Internal.Node);
         if Result = null then
            return No_Ada_Skip_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Skips;















         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Ada_Context_Clause_List'Class; Index : Positive) return Ada_Context_Clause
         is
            Result : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Ada_Context_Clause;
         end List_Child;

         

         function Ada_Context_Clause_List_First (Node : Ada_Context_Clause_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Ada_Context_Clause_List_Next
           (Node : Ada_Context_Clause_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Ada_Context_Clause_List_Has_Element
           (Node : Ada_Context_Clause_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Ada_Context_Clause_List_Element
           (Node : Ada_Context_Clause_List; Cursor : Positive) return Ada_Context_Clause'Class
         is
            Child : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Ada_Context_Clause'(Child.As_Ada_Context_Clause);
         end;


























         
   

   function F_Skips
     (Node : Ada_Generic'Class) return Gpr_Node
   is
      Result : Bare_Gpr_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_Generic_F_Skips (Node.Internal.Node);
         if Result = null then
            return No_Gpr_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Skips;







         
   

   function F_Generic_Stub
     (Node : Ada_Library_Item'Class) return Ada_Generic
   is
      Result : Bare_Ada_Generic;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_Library_Item_F_Generic_Stub (Node.Internal.Node);
         if Result = null then
            return No_Ada_Generic;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Generic_Stub;


         
   

   function F_Separate
     (Node : Ada_Library_Item'Class) return Ada_Separate
   is
      Result : Bare_Ada_Separate;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_Library_Item_F_Separate (Node.Internal.Node);
         if Result = null then
            return No_Ada_Separate;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Separate;


         
   

   function F_Main
     (Node : Ada_Library_Item'Class) return Ada_Main
   is
      Result : Bare_Ada_Main;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_Library_Item_F_Main (Node.Internal.Node);
         if Result = null then
            return No_Ada_Main;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Main;







         
   

   function F_Name
     (Node : Ada_Main'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_Main_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;







         
   

   function F_Has_Private
     (Node : Ada_Pkg'Class) return Private_Node
   is
      Result : Bare_Private_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_Pkg_F_Has_Private (Node.Internal.Node);
         if Result = null then
            return No_Private_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Private;

         function F_Has_Private (Node : Ada_Pkg'Class) return Boolean
         is (Private_Node'(Node.F_Has_Private).Kind
             = Gpr_Private_Present);












         
   

   function F_Skips
     (Node : Ada_Pragma'Class) return Ada_Skip_List
   is
      Result : Bare_Ada_Skip_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_Pragma_F_Skips (Node.Internal.Node);
         if Result = null then
            return No_Ada_Skip_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Skips;







         
   

   function F_Context_Clauses
     (Node : Ada_Prelude'Class) return Ada_Context_Clause_List
   is
      Result : Bare_Ada_Context_Clause_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_Prelude_F_Context_Clauses (Node.Internal.Node);
         if Result = null then
            return No_Ada_Context_Clause_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Context_Clauses;


         
   

   function F_Library_Item
     (Node : Ada_Prelude'Class) return Ada_Library_Item
   is
      Result : Bare_Ada_Library_Item;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_Prelude_F_Library_Item (Node.Internal.Node);
         if Result = null then
            return No_Ada_Library_Item;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Library_Item;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Ada_Prelude_Node_List'Class; Index : Positive) return Ada_Prelude_Node
         is
            Result : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Ada_Prelude_Node;
         end List_Child;

         

         function Ada_Prelude_Node_List_First (Node : Ada_Prelude_Node_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Ada_Prelude_Node_List_Next
           (Node : Ada_Prelude_Node_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Ada_Prelude_Node_List_Has_Element
           (Node : Ada_Prelude_Node_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Ada_Prelude_Node_List_Element
           (Node : Ada_Prelude_Node_List; Cursor : Positive) return Ada_Prelude_Node'Class
         is
            Child : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Ada_Prelude_Node'(Child.As_Ada_Prelude_Node);
         end;






         
   

   function F_Parent_Name
     (Node : Ada_Separate'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_Separate_F_Parent_Name (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Parent_Name;










         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Ada_Skip_List'Class; Index : Positive) return Ada_Skip
         is
            Result : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Ada_Skip;
         end List_Child;

         

         function Ada_Skip_List_First (Node : Ada_Skip_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Ada_Skip_List_Next
           (Node : Ada_Skip_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Ada_Skip_List_Has_Element
           (Node : Ada_Skip_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Ada_Skip_List_Element
           (Node : Ada_Skip_List; Cursor : Positive) return Ada_Skip'Class
         is
            Child : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Ada_Skip'(Child.As_Ada_Skip);
         end;






         
   

   function F_Subp_Kind
     (Node : Ada_Subp'Class) return Ada_Entity_Kind
   is
      Result : Bare_Ada_Entity_Kind;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_Subp_F_Subp_Kind (Node.Internal.Node);
         if Result = null then
            return No_Ada_Entity_Kind;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subp_Kind;

         function F_Subp_Kind
           (Node : Ada_Subp'Class) return Gpr_Ada_Entity_Kind
         is (Ada_Entity_Kind'(Node.F_Subp_Kind).Kind);






         
   

   function F_Skips
     (Node : Ada_Use'Class) return Ada_Skip_List
   is
      Result : Bare_Ada_Skip_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_Use_F_Skips (Node.Internal.Node);
         if Result = null then
            return No_Ada_Skip_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Skips;







         
   

   function F_Has_Limited
     (Node : Ada_With'Class) return Limited_Node
   is
      Result : Bare_Limited_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_With_F_Has_Limited (Node.Internal.Node);
         if Result = null then
            return No_Limited_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Limited;

         function F_Has_Limited (Node : Ada_With'Class) return Boolean
         is (Limited_Node'(Node.F_Has_Limited).Kind
             = Gpr_Limited_Present);


         
   

   function F_Has_Private
     (Node : Ada_With'Class) return Private_Node
   is
      Result : Bare_Private_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_With_F_Has_Private (Node.Internal.Node);
         if Result = null then
            return No_Private_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Private;

         function F_Has_Private (Node : Ada_With'Class) return Boolean
         is (Private_Node'(Node.F_Has_Private).Kind
             = Gpr_Private_Present);


         
   

   function F_Packages
     (Node : Ada_With'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_With_F_Packages (Node.Internal.Node);
         if Result = null then
            return No_Expr_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Packages;







         
   

   function F_Kind
     (Node : Ada_With_Formal'Class) return Ada_Entity_Kind
   is
      Result : Bare_Ada_Entity_Kind;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_With_Formal_F_Kind (Node.Internal.Node);
         if Result = null then
            return No_Ada_Entity_Kind;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Kind;

         function F_Kind
           (Node : Ada_With_Formal'Class) return Gpr_Ada_Entity_Kind
         is (Ada_Entity_Kind'(Node.F_Kind).Kind);

         
   

   function F_Skips
     (Node : Ada_With_Formal'Class) return Ada_Skip_List
   is
      Result : Bare_Ada_Skip_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ada_With_Formal_F_Skips (Node.Internal.Node);
         if Result = null then
            return No_Ada_Skip_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Skips;








         
   function P_As_Bool
     (Node : All_Qualifier'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Gpr_Parser.Implementation.Dispatcher_All_Qualifier_P_As_Bool
            (Bare_Gpr_Node (Node.Internal.Node));

         return Property_Result;

   end;















         
   

   function F_Attr_Name
     (Node : Attribute_Decl'Class) return Identifier
   is
      Result : Bare_Identifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Attribute_Decl_F_Attr_Name (Node.Internal.Node);
         if Result = null then
            return No_Identifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Attr_Name;


         
   

   function F_Attr_Index
     (Node : Attribute_Decl'Class) return Gpr_Node
   is
      Result : Bare_Gpr_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Attribute_Decl_F_Attr_Index (Node.Internal.Node);
         if Result = null then
            return No_Gpr_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Attr_Index;


         
   

   function F_Expr
     (Node : Attribute_Decl'Class) return Term_List
   is
      Result : Bare_Term_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Attribute_Decl_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Term_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Attribute_Name
     (Node : Attribute_Reference'Class) return Identifier
   is
      Result : Bare_Identifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Attribute_Reference_F_Attribute_Name (Node.Internal.Node);
         if Result = null then
            return No_Identifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Attribute_Name;


         
   

   function F_Attribute_Index
     (Node : Attribute_Reference'Class) return Gpr_Node
   is
      Result : Bare_Gpr_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Attribute_Reference_F_Attribute_Index (Node.Internal.Node);
         if Result = null then
            return No_Gpr_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Attribute_Index;







         
   

   function F_Function_Name
     (Node : Builtin_Function_Call'Class) return Identifier
   is
      Result : Bare_Identifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Builtin_Function_Call_F_Function_Name (Node.Internal.Node);
         if Result = null then
            return No_Identifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Function_Name;


         
   

   function F_Parameters
     (Node : Builtin_Function_Call'Class) return Terms
   is
      Result : Bare_Terms;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Builtin_Function_Call_F_Parameters (Node.Internal.Node);
         if Result = null then
            return No_Terms;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Parameters;







         
   

   function F_Var_Ref
     (Node : Case_Construction'Class) return Variable_Reference
   is
      Result : Bare_Variable_Reference;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Case_Construction_F_Var_Ref (Node.Internal.Node);
         if Result = null then
            return No_Variable_Reference;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Var_Ref;


         
   

   function F_Items
     (Node : Case_Construction'Class) return Case_Item_List
   is
      Result : Bare_Case_Item_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Case_Construction_F_Items (Node.Internal.Node);
         if Result = null then
            return No_Case_Item_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Items;







         
   

   function F_Choice
     (Node : Case_Item'Class) return Choices
   is
      Result : Bare_Choices;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Case_Item_F_Choice (Node.Internal.Node);
         if Result = null then
            return No_Choices;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Choice;


         
   

   function F_Decls
     (Node : Case_Item'Class) return Gpr_Node_List
   is
      Result : Bare_Gpr_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Case_Item_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Gpr_Node_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Case_Item_List'Class; Index : Positive) return Case_Item
         is
            Result : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Case_Item;
         end List_Child;

         

         function Case_Item_List_First (Node : Case_Item_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Case_Item_List_Next
           (Node : Case_Item_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Case_Item_List_Has_Element
           (Node : Case_Item_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Case_Item_List_Element
           (Node : Case_Item_List; Cursor : Positive) return Case_Item'Class
         is
            Child : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Case_Item'(Child.As_Case_Item);
         end;





         

         function Gpr_Node_List_First (Node : Gpr_Node_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Gpr_Node_List_Next
           (Node : Gpr_Node_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Gpr_Node_List_Has_Element
           (Node : Gpr_Node_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Gpr_Node_List_Element
           (Node : Gpr_Node_List; Cursor : Positive) return Gpr_Node'Class
         is
            Child : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Gpr_Node'(Child.As_Gpr_Node);
         end;











         
   

   function F_Project
     (Node : Compilation_Unit'Class) return Project
   is
      Result : Bare_Project;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Compilation_Unit_F_Project (Node.Internal.Node);
         if Result = null then
            return No_Project;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Project;















         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Expr_List'Class; Index : Positive) return Expr
         is
            Result : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Expr;
         end List_Child;

         

         function Expr_List_First (Node : Expr_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Expr_List_Next
           (Node : Expr_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Expr_List_Has_Element
           (Node : Expr_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Expr_List_Element
           (Node : Expr_List; Cursor : Positive) return Expr'Class
         is
            Child : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Expr'(Child.As_Expr);
         end;














         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Identifier_List'Class; Index : Positive) return Identifier
         is
            Result : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Identifier;
         end List_Child;

         

         function Identifier_List_First (Node : Identifier_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Identifier_List_Next
           (Node : Identifier_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Identifier_List_Has_Element
           (Node : Identifier_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Identifier_List_Element
           (Node : Identifier_List; Cursor : Positive) return Identifier'Class
         is
            Child : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Identifier'(Child.As_Identifier);
         end;







         
   function P_As_Bool
     (Node : Limited_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Gpr_Parser.Implementation.Dispatcher_Limited_Node_P_As_Bool
            (Bare_Gpr_Node (Node.Internal.Node));

         return Property_Result;

   end;

























         
   

   function F_Pkg_Name
     (Node : Package_Decl'Class) return Identifier
   is
      Result : Bare_Identifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Package_Decl_F_Pkg_Name (Node.Internal.Node);
         if Result = null then
            return No_Identifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Pkg_Name;


         
   

   function F_Pkg_Spec
     (Node : Package_Decl'Class) return Gpr_Node
   is
      Result : Bare_Gpr_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Package_Decl_F_Pkg_Spec (Node.Internal.Node);
         if Result = null then
            return No_Gpr_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Pkg_Spec;







         
   

   function F_Extended_Name
     (Node : Package_Extension'Class) return Identifier_List
   is
      Result : Bare_Identifier_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Package_Extension_F_Extended_Name (Node.Internal.Node);
         if Result = null then
            return No_Identifier_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Extended_Name;







         
   

   function F_Renamed_Name
     (Node : Package_Renaming'Class) return Identifier_List
   is
      Result : Bare_Identifier_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Package_Renaming_F_Renamed_Name (Node.Internal.Node);
         if Result = null then
            return No_Identifier_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Renamed_Name;







         
   

   function F_Extension
     (Node : Package_Spec'Class) return Package_Extension
   is
      Result : Bare_Package_Extension;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Package_Spec_F_Extension (Node.Internal.Node);
         if Result = null then
            return No_Package_Extension;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Extension;


         
   

   function F_Decls
     (Node : Package_Spec'Class) return Gpr_Node_List
   is
      Result : Bare_Gpr_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Package_Spec_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Gpr_Node_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;


         
   

   function F_End_Name
     (Node : Package_Spec'Class) return Identifier
   is
      Result : Bare_Identifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Package_Spec_F_End_Name (Node.Internal.Node);
         if Result = null then
            return No_Identifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_End_Name;







         
   

   function F_Prefix
     (Node : Prefix'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Prefix_F_Prefix (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Prefix;


         
   

   function F_Suffix
     (Node : Prefix'Class) return Identifier
   is
      Result : Bare_Identifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Prefix_F_Suffix (Node.Internal.Node);
         if Result = null then
            return No_Identifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Suffix;








         
   function P_As_Bool
     (Node : Private_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Gpr_Parser.Implementation.Dispatcher_Private_Node_P_As_Bool
            (Bare_Gpr_Node (Node.Internal.Node));

         return Property_Result;

   end;















         
   

   function F_Context_Clauses
     (Node : Project'Class) return With_Decl_List
   is
      Result : Bare_With_Decl_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Project_F_Context_Clauses (Node.Internal.Node);
         if Result = null then
            return No_With_Decl_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Context_Clauses;


         
   

   function F_Project_Decl
     (Node : Project'Class) return Project_Declaration
   is
      Result : Bare_Project_Declaration;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Project_F_Project_Decl (Node.Internal.Node);
         if Result = null then
            return No_Project_Declaration;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Project_Decl;







         
   

   function F_Qualifier
     (Node : Project_Declaration'Class) return Project_Qualifier
   is
      Result : Bare_Project_Qualifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Project_Declaration_F_Qualifier (Node.Internal.Node);
         if Result = null then
            return No_Project_Qualifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Qualifier;

         function F_Qualifier
           (Node : Project_Declaration'Class) return Gpr_Project_Qualifier
         is (Project_Qualifier'(Node.F_Qualifier).Kind);

         
   

   function F_Project_Name
     (Node : Project_Declaration'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Project_Declaration_F_Project_Name (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Project_Name;


         
   

   function F_Extension
     (Node : Project_Declaration'Class) return Project_Extension
   is
      Result : Bare_Project_Extension;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Project_Declaration_F_Extension (Node.Internal.Node);
         if Result = null then
            return No_Project_Extension;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Extension;


         
   

   function F_Decls
     (Node : Project_Declaration'Class) return Gpr_Node_List
   is
      Result : Bare_Gpr_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Project_Declaration_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Gpr_Node_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;


         
   

   function F_End_Name
     (Node : Project_Declaration'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Project_Declaration_F_End_Name (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_End_Name;







         
   

   function F_Is_All
     (Node : Project_Extension'Class) return All_Qualifier
   is
      Result : Bare_All_Qualifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Project_Extension_F_Is_All (Node.Internal.Node);
         if Result = null then
            return No_All_Qualifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Is_All;

         function F_Is_All (Node : Project_Extension'Class) return Boolean
         is (All_Qualifier'(Node.F_Is_All).Kind
             = Gpr_All_Qualifier_Present);


         
   

   function F_Path_Name
     (Node : Project_Extension'Class) return String_Literal
   is
      Result : Bare_String_Literal;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Project_Extension_F_Path_Name (Node.Internal.Node);
         if Result = null then
            return No_String_Literal;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Path_Name;










































         
   

   function F_Attr_Ref
     (Node : Project_Reference'Class) return Attribute_Reference
   is
      Result : Bare_Attribute_Reference;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Project_Reference_F_Attr_Ref (Node.Internal.Node);
         if Result = null then
            return No_Attribute_Reference;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Attr_Ref;












         
   

   function F_Str_Lit
     (Node : String_Literal_At'Class) return String_Literal
   is
      Result : Bare_String_Literal;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.String_Literal_At_F_Str_Lit (Node.Internal.Node);
         if Result = null then
            return No_String_Literal;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Str_Lit;


         
   

   function F_At_Lit
     (Node : String_Literal_At'Class) return Num_Literal
   is
      Result : Bare_Num_Literal;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.String_Literal_At_F_At_Lit (Node.Internal.Node);
         if Result = null then
            return No_Num_Literal;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_At_Lit;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : String_Literal_List'Class; Index : Positive) return String_Literal
         is
            Result : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_String_Literal;
         end List_Child;

         

         function String_Literal_List_First (Node : String_Literal_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function String_Literal_List_Next
           (Node : String_Literal_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function String_Literal_List_Has_Element
           (Node : String_Literal_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function String_Literal_List_Element
           (Node : String_Literal_List; Cursor : Positive) return String_Literal'Class
         is
            Child : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return String_Literal'(Child.As_String_Literal);
         end;









         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Term_List_List'Class; Index : Positive) return Term_List
         is
            Result : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Term_List;
         end List_Child;

         

         function Term_List_List_First (Node : Term_List_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Term_List_List_Next
           (Node : Term_List_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Term_List_List_Has_Element
           (Node : Term_List_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Term_List_List_Element
           (Node : Term_List_List; Cursor : Positive) return Term_List'Class
         is
            Child : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Term_List'(Child.As_Term_List);
         end;






         
   

   function F_Terms
     (Node : Terms'Class) return Term_List_List
   is
      Result : Bare_Term_List_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Terms_F_Terms (Node.Internal.Node);
         if Result = null then
            return No_Term_List_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Terms;







         
   

   function F_Var_Type_Name
     (Node : Type_Reference'Class) return Identifier_List
   is
      Result : Bare_Identifier_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Type_Reference_F_Var_Type_Name (Node.Internal.Node);
         if Result = null then
            return No_Identifier_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Var_Type_Name;







         
   

   function F_Type_Id
     (Node : Typed_String_Decl'Class) return Identifier
   is
      Result : Bare_Identifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Typed_String_Decl_F_Type_Id (Node.Internal.Node);
         if Result = null then
            return No_Identifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Type_Id;


         
   

   function F_String_Literals
     (Node : Typed_String_Decl'Class) return String_Literal_List
   is
      Result : Bare_String_Literal_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Typed_String_Decl_F_String_Literals (Node.Internal.Node);
         if Result = null then
            return No_String_Literal_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_String_Literals;







         
   

   function F_Var_Name
     (Node : Variable_Decl'Class) return Identifier
   is
      Result : Bare_Identifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Variable_Decl_F_Var_Name (Node.Internal.Node);
         if Result = null then
            return No_Identifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Var_Name;


         
   

   function F_Var_Type
     (Node : Variable_Decl'Class) return Type_Reference
   is
      Result : Bare_Type_Reference;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Variable_Decl_F_Var_Type (Node.Internal.Node);
         if Result = null then
            return No_Type_Reference;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Var_Type;


         
   

   function F_Expr
     (Node : Variable_Decl'Class) return Term_List
   is
      Result : Bare_Term_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Variable_Decl_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Term_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Variable_Name
     (Node : Variable_Reference'Class) return Identifier_List
   is
      Result : Bare_Identifier_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Variable_Reference_F_Variable_Name (Node.Internal.Node);
         if Result = null then
            return No_Identifier_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Variable_Name;


         
   

   function F_Attribute_Ref
     (Node : Variable_Reference'Class) return Attribute_Reference
   is
      Result : Bare_Attribute_Reference;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Variable_Reference_F_Attribute_Ref (Node.Internal.Node);
         if Result = null then
            return No_Attribute_Reference;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Attribute_Ref;







         
   

   function F_Is_Limited
     (Node : With_Decl'Class) return Limited_Node
   is
      Result : Bare_Limited_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.With_Decl_F_Is_Limited (Node.Internal.Node);
         if Result = null then
            return No_Limited_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Is_Limited;

         function F_Is_Limited (Node : With_Decl'Class) return Boolean
         is (Limited_Node'(Node.F_Is_Limited).Kind
             = Gpr_Limited_Present);


         
   

   function F_Path_Names
     (Node : With_Decl'Class) return String_Literal_List
   is
      Result : Bare_String_Literal_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.With_Decl_F_Path_Names (Node.Internal.Node);
         if Result = null then
            return No_String_Literal_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Path_Names;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : With_Decl_List'Class; Index : Positive) return With_Decl
         is
            Result : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_With_Decl;
         end List_Child;

         

         function With_Decl_List_First (Node : With_Decl_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function With_Decl_List_Next
           (Node : With_Decl_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function With_Decl_List_Has_Element
           (Node : With_Decl_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function With_Decl_List_Element
           (Node : With_Decl_List; Cursor : Positive) return With_Decl'Class
         is
            Child : Gpr_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return With_Decl'(Child.As_With_Decl);
         end;




   --------------------
   -- Children_Count --
   --------------------

   function Children_Count
     (Node : Gpr_Node'Class) return Natural is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Children_Count (Node.Internal.Node);
   end Children_Count;

   -----------------------
   -- First_Child_Index --
   -----------------------

   function First_Child_Index
     (Node : Gpr_Node'Class) return Natural is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return First_Child_Index (Node.Internal.Node);
   end First_Child_Index;

   ----------------------
   -- Last_Child_Index --
   ----------------------

   function Last_Child_Index
     (Node : Gpr_Node'Class) return Natural is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Last_Child_Index (Node.Internal.Node);
   end Last_Child_Index;

   -----------------
   -- First_Child --
   -----------------

   function First_Child
     (Node : Gpr_Node'Class) return Gpr_Node is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

      return Node.Child (First_Child_Index (Node.Internal.Node));
   end First_Child;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child
     (Node : Gpr_Node'Class) return Gpr_Node is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

      return Node.Child (Last_Child_Index (Node.Internal.Node));
   end Last_Child;

   ---------------
   -- Get_Child --
   ---------------

   procedure Get_Child
     (Node            : Gpr_Node'Class;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Gpr_Node)
   is
      N : Bare_Gpr_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Get_Child (Node.Internal.Node, Index, Index_In_Bounds, N);
      Result := Wrap_Node (N, Node.Internal.Info);
   end Get_Child;

   -----------
   -- Child --
   -----------

   function Child
     (Node  : Gpr_Node'Class;
      Index : Positive) return Gpr_Node
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Wrap_Node (Child (Node.Internal.Node, Index), Node.Internal.Info);
   end Child;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Node : Gpr_Node'Class) return Source_Location_Range is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Sloc_Range (Node.Internal.Node);
   end Sloc_Range;

   -------------
   -- Compare --
   -------------

   function Compare
     (Node : Gpr_Node'Class;
      Sloc : Source_Location) return Relative_Position is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Compare (Node.Internal.Node, Sloc);
   end Compare;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Node : Gpr_Node'Class;
      Sloc : Source_Location) return Gpr_Node is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Wrap_Node (Lookup (Node.Internal.Node, Sloc));
   end Lookup;

   ----------
   -- Text --
   ----------

   function Text (Node : Gpr_Node'Class) return Text_Type is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Implementation.Text (Node.Internal.Node);
   end Text;

   -----------------
   -- Token_Range --
   -----------------

   function Token_Range
     (Node : Gpr_Node'Class) return Token_Iterator is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Token_Iterator'(Node.As_Gpr_Node,
                             Node.Internal.Node.Token_End_Index);
   end Token_Range;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : Gpr_Node'Class;
      Show_Slocs  : Boolean := True;
      Line_Prefix : String := "") is
   begin
      Check_Safety_Net (Node);
      Print (Node.Internal.Node, Show_Slocs, Line_Prefix);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia
     (Node : Gpr_Node'Class; Line_Prefix : String := "") is
   begin
      Check_Safety_Net (Node);
      PP_Trivia (Node.Internal.Node, Line_Prefix);
   end PP_Trivia;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Node  : Gpr_Node'Class;
      Visit : access function (Node : Gpr_Node'Class)
              return Visit_Status)
      return Visit_Status
   is
      Info : constant Internal_Entity_Info := Node.Internal.Info;

      -------------
      -- Wrapper --
      -------------

      function Wrapper (Node : Bare_Gpr_Node) return Visit_Status
      is
         Public_Node : constant Gpr_Node :=
           Wrap_Node (Bare_Gpr_Node (Node), Info);
      begin
         return Visit (Public_Node);
      end Wrapper;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Traverse (Node.Internal.Node, Wrapper'Access);
   end Traverse;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (Node  : Gpr_Node'Class;
      Visit : access function (Node : Gpr_Node'Class)
                               return Visit_Status)
   is
      Result_Status : Visit_Status;
      pragma Unreferenced (Result_Status);
   begin
      Result_Status := Traverse (Node, Visit);
   end Traverse;

   --------------------------------
   -- Assign_Names_To_Logic_Vars --
   --------------------------------

   procedure Assign_Names_To_Logic_Vars (Node : Gpr_Node'Class)
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Assign_Names_To_Logic_Vars (Node.Internal.Node);
   end Assign_Names_To_Logic_Vars;

   -------------------------
   -- Children_And_Trivia --
   -------------------------

   function Children_And_Trivia
     (Node : Gpr_Node'Class) return Children_Array
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      declare
         Bare_Result : constant Bare_Children_Array :=
            Children_And_Trivia (Unwrap_Node (Node));
         Result      : Children_Array (Bare_Result'Range);
      begin
         for I in Bare_Result'Range loop
            declare
               BR : Bare_Child_Record renames Bare_Result (I);
               R  : Child_Record renames Result (I);
            begin
               case BR.Kind is
                  when Child =>
                     R := (Child, Wrap_Node (BR.Node));
                  when Trivia =>
                     R := (Trivia, BR.Trivia);
               end case;
            end;
         end loop;
         return Result;
      end;
   end Children_And_Trivia;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Self : Token_Iterator) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node);
      return Token_Start (Self.Node);
   end First_Token;

   ----------------
   -- Next_Token --
   ----------------

   function Next_Token
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node);
      return Next (Tok);
   end Next_Token;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Self : Token_Iterator; Tok : Token_Reference) return Boolean is
   begin
      Check_Safety_Net (Self.Node);
      return Get_Token_Index (Tok).Token <= Self.Last;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node);
      return Tok;
   end Element;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Context : in out Analysis_Context) is
   begin
      Context.Internal := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Context : in out Analysis_Context) is
   begin
      Inc_Ref (Unwrap_Context (Context));
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Context : in out Analysis_Context) is
      Ctx : Internal_Context := Unwrap_Context (Context);
   begin
      Dec_Ref (Ctx);
      Context.Internal := null;
   end Finalize;

   ----------------------------------------------------
   -- Soft links for public/internal type converters --
   ----------------------------------------------------

   function Wrap_Context (Context : Internal_Context) return Analysis_Context;
   function Unwrap_Context
     (Context : Analysis_Context'Class) return Internal_Context;

   function Wrap_Unit (Unit : Internal_Unit) return Analysis_Unit;
   function Unwrap_Unit (Unit : Analysis_Unit'Class) return Internal_Unit;

   function Wrap_Node
     (Node : Bare_Gpr_Node;
      Info : Internal_Entity_Info := No_Entity_Info)
      return Gpr_Node;
   function Unwrap_Node
     (Node : Gpr_Node'Class) return Bare_Gpr_Node;
   function Unwrap_Entity
     (Entity : Gpr_Node'Class) return Internal_Entity;

   ------------------
   -- Wrap_Context --
   ------------------

   function Wrap_Context (Context : Internal_Context) return Analysis_Context
   is
   begin
      Inc_Ref (Context);
      return (Ada.Finalization.Controlled with
              Internal => Internal_Context_Access (Context));
   end Wrap_Context;

   --------------------
   -- Unwrap_Context --
   --------------------

   function Unwrap_Context
     (Context : Analysis_Context'Class) return Internal_Context
   is (Internal_Context (Context.Internal));

   ---------------
   -- Wrap_Unit --
   ---------------

   function Wrap_Unit (Unit : Internal_Unit) return Analysis_Unit
   is (if Unit = null
       then No_Analysis_Unit
       else (Internal => Internal_Unit_Access (Unit),
             Context  => Wrap_Context (Context (Unit))));

   -----------------
   -- Unwrap_Unit --
   -----------------

   function Unwrap_Unit (Unit : Analysis_Unit'Class) return Internal_Unit
   is (Internal_Unit (Unit.Internal));

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Self : Gpr_Node'Class) is
      R  : Env_Rebindings renames Self.Internal.Info.Rebindings;
      SN : Node_Safety_Net renames Self.Safety_Net;
   begin
      if SN.Context = null then
         return;
      end if;

      --  Check that SN's context has not been released (see the Context_Pool)
      if SN.Context.Serial_Number /= SN.Context_Serial then
         raise Stale_Reference_Error with "context was released";

      --  Then check that the unit version is the same
      elsif SN.Unit.Unit_Version /= SN.Unit_Version then
         raise Stale_Reference_Error with "unit was reparsed";

      --  Then check that the R rebindings reference, if not-null, is not stale
      elsif R /= null and then R.Version /= SN.Rebindings_Version then
         raise Stale_Reference_Error with "related unit was reparsed";
      end if;
   end Check_Safety_Net;

   ---------------
   -- Wrap_Node --
   ---------------

   function Wrap_Node
     (Node : Bare_Gpr_Node;
      Info : Internal_Entity_Info := No_Entity_Info)
      return Gpr_Node is
   begin
      if Node = null then
         return No_Gpr_Node;
      end if;

      declare
         Unit               : constant Internal_Unit := Node.Unit;
         Context            : constant Internal_Context := Unit.Context;
         Rebindings_Version : constant Version_Number :=
           (if Info.Rebindings = null
            then 0
            else Info.Rebindings.Version);
      begin
         return ((Internal   => (Node, Info),
                  Safety_Net => (Context            => Context,
                                 Context_Serial     => Context.Serial_Number,
                                 Unit               => Unit,
                                 Unit_Version       => Unit.Unit_Version,
                                 Rebindings_Version => Rebindings_Version)));
      end;
   end;

   -----------------
   -- Unwrap_Node --
   -----------------

   function Unwrap_Node
     (Node : Gpr_Node'Class) return Bare_Gpr_Node
   is (Node.Internal.Node);

   -------------------
   -- Unwrap_Entity --
   -------------------

   function Unwrap_Entity
     (Entity : Gpr_Node'Class) return Internal_Entity
   is ((Entity.Internal));

   


begin
   Public_Converters.Wrap_Context := Wrap_Context'Access;
   Public_Converters.Unwrap_Context := Unwrap_Context'Access;
   Public_Converters.Wrap_Unit := Wrap_Unit'Access;
   Public_Converters.Unwrap_Unit := Unwrap_Unit'Access;
   Public_Converters.Wrap_Node := Wrap_Node'Access;
   Public_Converters.Unwrap_Node := Unwrap_Node'Access;
   Public_Converters.Unwrap_Entity := Unwrap_Entity'Access;
end Gpr_Parser.Analysis;
