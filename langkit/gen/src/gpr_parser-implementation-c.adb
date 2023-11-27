
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--










with Ada.Finalization;
pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
use Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");

with System.Memory;
use type System.Address;

with GNATCOLL.Iconv;

with Gpr_Parser_Support.Diagnostics; use Gpr_Parser_Support.Diagnostics;
with Gpr_Parser_Support.Text;        use Gpr_Parser_Support.Text;

with Gpr_Parser.Private_Converters;
use Gpr_Parser.Private_Converters;


          with Gpr_Parser_Support.Errors;


package body Gpr_Parser.Implementation.C is

   --  Avoid hiding from $.Lexer
   subtype Token_Data_Type is Common.Token_Data_Type;

   --------------------
   -- Event handlers --
   --------------------

   type C_Event_Handler is limited new
      Ada.Finalization.Limited_Controlled
      and Internal_Event_Handler
   with record
      Ref_Count           : Natural;
      Data                : System.Address;
      Destroy_Func        : gpr_event_handler_destroy_callback;
      Unit_Requested_Func : gpr_event_handler_unit_requested_callback;
      Unit_Parsed_Func    : gpr_event_handler_unit_parsed_callback;
   end record;

   overriding procedure Finalize (Self : in out C_Event_Handler);
   overriding procedure Inc_Ref (Self : in out C_Event_Handler);
   overriding function Dec_Ref (Self : in out C_Event_Handler) return Boolean;

   overriding procedure Unit_Requested_Callback
     (Self               : in out C_Event_Handler;
      Context            : Internal_Context;
      Name               : Text_Type;
      From               : Internal_Unit;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean);

   overriding procedure Unit_Parsed_Callback
     (Self     : in out C_Event_Handler;
      Context  : Internal_Context;
      Unit     : Internal_Unit;
      Reparsed : Boolean);

   ------------------
   -- File readers --
   ------------------

   type C_File_Reader is limited new
      Ada.Finalization.Limited_Controlled
      and Internal_File_Reader
   with record
      Ref_Count    : Natural;
      Data         : System.Address;
      Destroy_Func : gpr_file_reader_destroy_callback;
      Read_Func    : gpr_file_reader_read_callback;
   end record;

   type C_File_Reader_Access is access all C_File_Reader;

   overriding procedure Finalize (Self : in out C_File_Reader);
   overriding procedure Inc_Ref (Self : in out C_File_Reader);
   overriding function Dec_Ref (Self : in out C_File_Reader) return Boolean;
   overriding procedure Read
     (Self        : C_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);

   function Value_Or_Empty (S : chars_ptr) return String
   --  If S is null, return an empty string. Return Value (S) otherwise.
   is (if S = Null_Ptr
       then ""
       else Value (S));

   Last_Exception : gpr_exception_Ptr := null;

   ----------
   -- Free --
   ----------

   procedure Free (Address : System.Address) is
      procedure C_Free (Address : System.Address)
        with Import        => True,
             Convention    => C,
             External_Name => "free";
   begin
      C_Free (Address);
   end Free;

   -------------------------
   -- Analysis primitives --
   -------------------------

   function gpr_allocate_analysis_context
     return gpr_analysis_context is
   begin
      Clear_Last_Exception;
      begin
         return Allocate_Context;
      exception
         when Exc : others =>
            Set_Last_Exception (Exc);
            return null;
      end;
   end;

   procedure gpr_initialize_analysis_context
     (Context       : gpr_analysis_context;
      Charset       : chars_ptr;
      File_Reader   : gpr_file_reader;
      Unit_Provider : gpr_unit_provider;
      Event_Handler : gpr_event_handler;
      With_Trivia   : int;
      Tab_Stop      : int) is
   begin
      Clear_Last_Exception;

      declare
         C : constant String :=
           (if Charset = Null_Ptr
            then "iso-8859-1"
            else Value (Charset));
      begin
         Initialize_Context
            (Context       => Context,
             Charset       => C,
             File_Reader   => Unwrap_Private_File_Reader (File_Reader),
             Unit_Provider => Unwrap_Private_Provider (Unit_Provider),
             Event_Handler => Unwrap_Private_Event_Handler (Event_Handler),
             With_Trivia   => With_Trivia /= 0,
             Tab_Stop      => Natural (Tab_Stop));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function gpr_context_incref
     (Context : gpr_analysis_context) return gpr_analysis_context is
   begin
      Clear_Last_Exception;
      Inc_Ref (Context);
      return Context;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   procedure gpr_context_decref
     (Context : gpr_analysis_context)
   is
      Context_Var : Internal_Context := Context;
   begin
      Clear_Last_Exception;
      Dec_Ref (Context_Var);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function gpr_context_symbol
     (Context : gpr_analysis_context;
      Text    : access gpr_text;
      Symbol  : access gpr_symbol_type) return int
   is
      Raw_Text : Text_Type (1 .. Natural (Text.Length))
         with Import, Address => Text.Chars;
   begin
      Clear_Last_Exception;
      Symbol.all := Wrap_Symbol (Lookup_Symbol (Context, Raw_Text));
      return 1;
   exception
      when Invalid_Symbol_Error =>
         return 0;
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   procedure gpr_context_discard_errors_in_populate_lexical_env
     (Context : gpr_analysis_context;
      Discard : int) is
   begin
      Clear_Last_Exception;
      Discard_Errors_In_Populate_Lexical_Env (Context, Discard /= 0);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function gpr_get_analysis_unit_from_file
     (Context           : gpr_analysis_context;
      Filename, Charset : chars_ptr;
      Reparse           : int;
      Rule              : gpr_grammar_rule) return gpr_analysis_unit is
   begin
      Clear_Last_Exception;

      return Get_From_File
        (Context,
         Value (Filename),
         Value_Or_Empty (Charset),
         Reparse /= 0,
         Rule);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   function gpr_get_analysis_unit_from_buffer
     (Context           : gpr_analysis_context;
      Filename, Charset : chars_ptr;
      Buffer            : chars_ptr;
      Buffer_Size       : size_t;
      Rule              : gpr_grammar_rule) return gpr_analysis_unit is
   begin
      Clear_Last_Exception;

      declare
         Buffer_Str : String (1 .. Natural (Buffer_Size))
            with Import, Address => Convert (Buffer);
      begin
         return Get_From_Buffer
           (Context,
            Value (Filename),
            Value_Or_Empty (Charset),
            Buffer_Str,
            Rule);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;


   procedure gpr_unit_root
     (Unit     : gpr_analysis_unit;
      Result_P : gpr_node_Ptr) is
   begin
      Clear_Last_Exception;

      Result_P.all := (Unit.Ast_Root, No_Entity_Info);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure gpr_unit_first_token
     (Unit  : gpr_analysis_unit;
      Token : access gpr_token) is
   begin
      Clear_Last_Exception;

      declare
         T : constant Token_Reference := First_Token (Unit);
      begin
         Token.all := Wrap (T);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure gpr_unit_last_token
     (Unit  : gpr_analysis_unit;
      Token : access gpr_token) is
   begin
      Clear_Last_Exception;

      declare
         T : constant Token_Reference := Last_Token (Unit);
      begin
         Token.all := Wrap (T);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function gpr_unit_token_count
     (Unit : gpr_analysis_unit) return int is
   begin
      Clear_Last_Exception;

      return int (Token_Count (Unit));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return -1;
   end;

   function gpr_unit_trivia_count
     (Unit : gpr_analysis_unit) return int is
   begin
      Clear_Last_Exception;

      return int (Trivia_Count (Unit));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return -1;
   end;

   procedure gpr_unit_lookup_token
     (Unit   : gpr_analysis_unit;
      Sloc   : access gpr_source_location;
      Result : access gpr_token) is
   begin
      Clear_Last_Exception;

      declare
         S   : constant Source_Location := Unwrap (Sloc.all);
         Tok : constant Token_Reference := Lookup_Token (Unit, S);
      begin
         Result.all := Wrap (Tok);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure gpr_unit_dump_lexical_env
     (Unit : gpr_analysis_unit) is
   begin
      Clear_Last_Exception;
      Dump_Lexical_Env (Unit);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function gpr_unit_filename
     (Unit : gpr_analysis_unit) return chars_ptr is
   begin
      Clear_Last_Exception;

      return New_String (Get_Filename (Unit));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return Null_Ptr;
   end;

   function gpr_unit_diagnostic_count
     (Unit : gpr_analysis_unit) return unsigned is
   begin
      Clear_Last_Exception;

      return unsigned (Unit.Diagnostics.Length);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function gpr_unit_diagnostic
     (Unit         : gpr_analysis_unit;
      N            : unsigned;
      Diagnostic_P : access gpr_diagnostic) return int
   is
   begin
      Clear_Last_Exception;

      if N < unsigned (Unit.Diagnostics.Length) then
         declare
            D_In  : Diagnostic renames Unit.Diagnostics (Natural (N) + 1);
            D_Out : gpr_diagnostic renames Diagnostic_P.all;
         begin
            D_Out.Sloc_Range := Wrap (D_In.Sloc_Range);
            D_Out.Message := Wrap (D_In.Message);
            return 1;
         end;
      else
         return 0;
      end if;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function gpr_unit_context
     (Unit : gpr_analysis_unit) return gpr_analysis_context is
   begin
      Clear_Last_Exception;
      return Unit.Context;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   procedure gpr_unit_reparse_from_file
     (Unit : gpr_analysis_unit; Charset : chars_ptr) is
   begin
      Clear_Last_Exception;

      Reparse (Unit, Value_Or_Empty (Charset));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure gpr_unit_reparse_from_buffer
     (Unit        : gpr_analysis_unit;
      Charset     : chars_ptr;
      Buffer      : chars_ptr;
      Buffer_Size : size_t) is
   begin
      Clear_Last_Exception;

      declare
         Buffer_Str : String (1 .. Natural (Buffer_Size))
            with Import, Address => Convert (Buffer);
      begin
         Reparse (Unit, Value_Or_Empty (Charset), Buffer_Str);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function gpr_unit_populate_lexical_env
     (Unit : gpr_analysis_unit
   ) return int is
   begin
      Clear_Last_Exception;
      Populate_Lexical_Env
        (Unit, 1);
      return 1;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

   Node_Kind_Names : constant array (Gpr_Node_Kind_Type) of Text_Access :=
     (Gpr_All_Qualifier_Absent => new Text_Type'(To_Text ("AllQualifierAbsent")), Gpr_All_Qualifier_Present => new Text_Type'(To_Text ("AllQualifierPresent")), Gpr_Attribute_Decl => new Text_Type'(To_Text ("AttributeDecl")), Gpr_Attribute_Reference => new Text_Type'(To_Text ("AttributeReference")), Gpr_Case_Item_List => new Text_Type'(To_Text ("CaseItemList")), Gpr_Gpr_Node_List => new Text_Type'(To_Text ("GprNodeList")), Gpr_Choices => new Text_Type'(To_Text ("Choices")), Gpr_Term_List => new Text_Type'(To_Text ("TermList")), Gpr_Identifier_List => new Text_Type'(To_Text ("IdentifierList")), Gpr_String_Literal_List => new Text_Type'(To_Text ("StringLiteralList")), Gpr_Term_List_List => new Text_Type'(To_Text ("TermListList")), Gpr_With_Decl_List => new Text_Type'(To_Text ("WithDeclList")), Gpr_Builtin_Function_Call => new Text_Type'(To_Text ("BuiltinFunctionCall")), Gpr_Case_Construction => new Text_Type'(To_Text ("CaseConstruction")), Gpr_Case_Item => new Text_Type'(To_Text ("CaseItem")), Gpr_Compilation_Unit => new Text_Type'(To_Text ("CompilationUnit")), Gpr_Empty_Decl => new Text_Type'(To_Text ("EmptyDecl")), Gpr_Prefix => new Text_Type'(To_Text ("Prefix")), Gpr_Identifier => new Text_Type'(To_Text ("Identifier")), Gpr_Num_Literal => new Text_Type'(To_Text ("NumLiteral")), Gpr_String_Literal => new Text_Type'(To_Text ("StringLiteral")), Gpr_Limited_Absent => new Text_Type'(To_Text ("LimitedAbsent")), Gpr_Limited_Present => new Text_Type'(To_Text ("LimitedPresent")), Gpr_Others_Designator => new Text_Type'(To_Text ("OthersDesignator")), Gpr_Package_Decl => new Text_Type'(To_Text ("PackageDecl")), Gpr_Package_Extension => new Text_Type'(To_Text ("PackageExtension")), Gpr_Package_Renaming => new Text_Type'(To_Text ("PackageRenaming")), Gpr_Package_Spec => new Text_Type'(To_Text ("PackageSpec")), Gpr_Project => new Text_Type'(To_Text ("Project")), Gpr_Project_Declaration => new Text_Type'(To_Text ("ProjectDeclaration")), Gpr_Project_Extension => new Text_Type'(To_Text ("ProjectExtension")), Gpr_Project_Qualifier_Abstract => new Text_Type'(To_Text ("ProjectQualifierAbstract")), Gpr_Project_Qualifier_Aggregate => new Text_Type'(To_Text ("ProjectQualifierAggregate")), Gpr_Project_Qualifier_Aggregate_Library => new Text_Type'(To_Text ("ProjectQualifierAggregateLibrary")), Gpr_Project_Qualifier_Configuration => new Text_Type'(To_Text ("ProjectQualifierConfiguration")), Gpr_Project_Qualifier_Library => new Text_Type'(To_Text ("ProjectQualifierLibrary")), Gpr_Project_Qualifier_Standard => new Text_Type'(To_Text ("ProjectQualifierStandard")), Gpr_String_Literal_At => new Text_Type'(To_Text ("StringLiteralAt")), Gpr_Terms => new Text_Type'(To_Text ("Terms")), Gpr_Type_Reference => new Text_Type'(To_Text ("TypeReference")), Gpr_Typed_String_Decl => new Text_Type'(To_Text ("TypedStringDecl")), Gpr_Variable_Decl => new Text_Type'(To_Text ("VariableDecl")), Gpr_Variable_Reference => new Text_Type'(To_Text ("VariableReference")), Gpr_With_Decl => new Text_Type'(To_Text ("WithDecl")));

   function gpr_node_kind
     (Node : gpr_node_Ptr) return gpr_node_kind_enum is
   begin
      Clear_Last_Exception;

      declare
         K : constant Gpr_Node_Kind_Type := Node.Node.Kind;
      begin
         return gpr_node_kind_enum (K'Enum_Rep);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return gpr_node_kind_enum'First;
   end;

   procedure gpr_kind_name
     (Kind : gpr_node_kind_enum; Result : access gpr_text) is
   begin
      Clear_Last_Exception;

      declare
         K    : constant Gpr_Node_Kind_Type := Gpr_Node_Kind_Type'Enum_Val (Kind);
         Name : Text_Access renames Node_Kind_Names (K);
      begin
         Result.all := (Chars        => Name.all'Address,
                        Length       => Name'Length,
                        Is_Allocated => 0);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function gpr_node_unit
     (Node : gpr_node_Ptr) return gpr_analysis_unit is
   begin
      Clear_Last_Exception;
      return Node.Node.Unit;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   function gpr_is_equivalent
     (L, R : gpr_node_Ptr) return gpr_bool
   is
   begin
      Clear_Last_Exception;
      return gpr_bool (Boolean'Pos (Compare_Entity (L.all, R.all)));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function gpr_hash
     (Node : gpr_node_Ptr) return uint32_t
   is
   begin
      Clear_Last_Exception;
      return uint32_t (Hash_Entity (Node.all));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function gpr_is_token_node
     (Node : gpr_node_Ptr) return int is
   begin
      Clear_Last_Exception;
      return Boolean'Pos (Is_Token_Node (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function gpr_is_synthetic
     (Node : gpr_node_Ptr) return int is
   begin
      Clear_Last_Exception;
      return Boolean'Pos (Is_Synthetic (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   procedure gpr_node_image
     (Node : gpr_node_Ptr; Result : access gpr_text) is
   begin
      Clear_Last_Exception;
      declare
         Img : constant Text_Type := Text_Image (Node.all);
      begin
         Result.all := Wrap_Alloc (Img);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure gpr_node_text
     (Node : gpr_node_Ptr;
      Text : access gpr_text) is
   begin
      Clear_Last_Exception;
      Text.all := Wrap_Alloc (Implementation.Text (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure gpr_node_sloc_range
     (Node         : gpr_node_Ptr;
      Sloc_Range_P : access gpr_source_location_range) is
   begin
      Clear_Last_Exception;

      Sloc_Range_P.all := Wrap (Sloc_Range (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure gpr_lookup_in_node
     (Node   : gpr_node_Ptr;
      Sloc   : gpr_source_location;
      Result : gpr_node_Ptr) is
   begin
      Clear_Last_Exception;

      declare
         S : constant Source_Location := Unwrap (Sloc);
      begin
         Result.all := (Lookup (Node.Node, S), Node.Info);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function gpr_node_children_count
     (Node : gpr_node_Ptr) return unsigned is
   begin
      Clear_Last_Exception;
      return unsigned (Children_Count (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function gpr_node_child
     (Node    : gpr_node_Ptr;
      N       : unsigned;
      Child_P : gpr_node_Ptr) return int is
   begin
      Clear_Last_Exception;

      declare
         Result : Bare_Gpr_Node;
         Exists : Boolean;
      begin
         if N > unsigned (Natural'Last) then
            return 0;
         end if;
         Get_Child (Node.Node, Natural (N) + 1, Exists, Result);
         if Exists then
            Child_P.all := (Result, Node.Info);
            return 1;
         else
            return 0;
         end if;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function gpr_text_to_locale_string
     (Text : gpr_text) return System.Address is
   begin
      Clear_Last_Exception;

      declare
         use GNATCOLL.Iconv;

         Input_Byte_Size : constant size_t := 4 * Text.Length;

         Output_Byte_Size : constant size_t := Input_Byte_Size + 1;
         --  Assuming no encoding will take more than 4 bytes per character, 4
         --  times the size of the input text plus one null byte should be
         --  enough to hold the result. This is a development helper anyway, so
         --  we don't have performance concerns.

         Result : constant System.Address := System.Memory.Alloc
           (System.Memory.size_t (Output_Byte_Size));
         --  Buffer we are going to return to the caller. We use
         --  System.Memory.Alloc so that users can call C's "free" function in
         --  order to free it.

         Input : String (1 .. Natural (Input_Byte_Size));
         for Input'Address use Text.Chars;

         Output : String (1 .. Natural (Output_Byte_Size));
         for Output'Address use Result;

         State                     : Iconv_T;
         Input_Index, Output_Index : Positive := 1;
         Status                    : Iconv_Result;

         From_Code : constant String :=
           (if System."=" (System.Default_Bit_Order, System.Low_Order_First)
            then UTF32LE
            else UTF32BE);

      begin
         --  GNATCOLL.Iconv raises Constraint_Error exceptions for empty
         --  strings, so handle them ourselves.

         if Input_Byte_Size = 0 then
            Output (1) := ASCII.NUL;
         end if;

         --  Encode to the locale. Don't bother with error checking...

         Set_Locale;
         State := Iconv_Open
           (To_Code         => Locale,
            From_Code       => From_Code,
            Transliteration => True,
            Ignore          => True);
         Iconv (State, Input, Input_Index, Output, Output_Index, Status);
         Iconv_Close (State);

         --  Don't forget the trailing NULL character to keep C programs happy
         Output (Output_Index) := ASCII.NUL;

         return Result;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return System.Null_Address;
   end;

   ----------
   -- Wrap --
   ----------

   function Wrap (S : Unbounded_Wide_Wide_String) return gpr_text is
      Chars  : Big_Wide_Wide_String_Access;
      Length : Natural;
   begin
      Get_Wide_Wide_String (S, Chars, Length);
      return (Chars.all'Address, size_t (Length), 0);
   end Wrap;

   ------------------------
   -- Set_Last_Exception --
   ------------------------

   procedure Set_Last_Exception (Exc : Exception_Occurrence) is
   begin
      Set_Last_Exception (Exception_Identity (Exc), Exception_Message (Exc));
   end Set_Last_Exception;

   ------------------------
   -- Set_Last_Exception --
   ------------------------

   procedure Set_Last_Exception (Id : Exception_Id; Message : String) is
   begin
      --  If it's the first time, allocate room for the exception information

      if Last_Exception = null then
         Last_Exception := new gpr_exception;

      --  If it is not the first time, free memory allocated for the last
      --  exception.

      elsif Last_Exception.Information /= Null_Ptr then
         Free (Last_Exception.Information);
      end if;

      --  Get the kind corresponding to Exc

      if Id = Gpr_Parser_Support.Errors.File_Read_Error'Identity then
         Last_Exception.Kind := Exception_File_Read_Error;
         Last_Exception.Information := New_String (Message);
      elsif Id = Gpr_Parser_Support.Errors.Introspection.Bad_Type_Error'Identity then
         Last_Exception.Kind := Exception_Bad_Type_Error;
         Last_Exception.Information := New_String (Message);
      elsif Id = Gpr_Parser_Support.Errors.Introspection.Out_Of_Bounds_Error'Identity then
         Last_Exception.Kind := Exception_Out_Of_Bounds_Error;
         Last_Exception.Information := New_String (Message);
      elsif Id = Gpr_Parser_Support.Errors.Invalid_Input'Identity then
         Last_Exception.Kind := Exception_Invalid_Input;
         Last_Exception.Information := New_String (Message);
      elsif Id = Gpr_Parser_Support.Errors.Invalid_Symbol_Error'Identity then
         Last_Exception.Kind := Exception_Invalid_Symbol_Error;
         Last_Exception.Information := New_String (Message);
      elsif Id = Gpr_Parser_Support.Errors.Invalid_Unit_Name_Error'Identity then
         Last_Exception.Kind := Exception_Invalid_Unit_Name_Error;
         Last_Exception.Information := New_String (Message);
      elsif Id = Gpr_Parser_Support.Errors.Native_Exception'Identity then
         Last_Exception.Kind := Exception_Native_Exception;
         Last_Exception.Information := New_String (Message);
      elsif Id = Gpr_Parser_Support.Errors.Precondition_Failure'Identity then
         Last_Exception.Kind := Exception_Precondition_Failure;
         Last_Exception.Information := New_String (Message);
      elsif Id = Gpr_Parser_Support.Errors.Property_Error'Identity then
         Last_Exception.Kind := Exception_Property_Error;
         Last_Exception.Information := New_String (Message);
      elsif Id = Gpr_Parser_Support.Errors.Rewriting.Template_Args_Error'Identity then
         Last_Exception.Kind := Exception_Template_Args_Error;
         Last_Exception.Information := New_String (Message);
      elsif Id = Gpr_Parser_Support.Errors.Rewriting.Template_Format_Error'Identity then
         Last_Exception.Kind := Exception_Template_Format_Error;
         Last_Exception.Information := New_String (Message);
      elsif Id = Gpr_Parser_Support.Errors.Rewriting.Template_Instantiation_Error'Identity then
         Last_Exception.Kind := Exception_Template_Instantiation_Error;
         Last_Exception.Information := New_String (Message);
      elsif Id = Gpr_Parser_Support.Errors.Stale_Reference_Error'Identity then
         Last_Exception.Kind := Exception_Stale_Reference_Error;
         Last_Exception.Information := New_String (Message);
      elsif Id = Gpr_Parser_Support.Errors.Syntax_Error'Identity then
         Last_Exception.Kind := Exception_Syntax_Error;
         Last_Exception.Information := New_String (Message);
      elsif Id = Gpr_Parser_Support.Errors.Unknown_Charset'Identity then
         Last_Exception.Kind := Exception_Unknown_Charset;
         Last_Exception.Information := New_String (Message);
      elsif Id = Gpr_Parser_Support.Errors.Unparsing.Malformed_Tree_Error'Identity then
         Last_Exception.Kind := Exception_Malformed_Tree_Error;
         Last_Exception.Information := New_String (Message);
      else
         Last_Exception.Kind := Exception_Native_Exception;
         Last_Exception.Information := New_String (Message);
      end if;
   end Set_Last_Exception;

   --------------------------
   -- Clear_Last_Exception --
   --------------------------

   procedure Clear_Last_Exception is
   begin
      if Last_Exception /= null then
         Free (Last_Exception.Information);
      end if;
   end Clear_Last_Exception;

   function gpr_get_last_exception return gpr_exception_Ptr
   is
   begin
      if Last_Exception = null
         or else Last_Exception.Information = Null_Ptr
      then
         return null;
      else
         return Last_Exception;
      end if;
   end;

   function gpr_exception_name
     (Kind : gpr_exception_kind) return chars_ptr is
   begin
      return New_String (Kind'Image);
   end;

   function gpr_token_get_kind
     (Token : gpr_token) return int is
   begin
      Clear_Last_Exception;
      declare
         T : constant Token_Reference := Unwrap (Token);
         D : constant Token_Data_Type := Data (T);
      begin
         return Kind (D)'Enum_Rep;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function gpr_token_kind_name (Kind : int) return chars_ptr
   is
      K : Token_Kind;
   begin
      begin
         K := Token_Kind'Enum_Val (Kind);
      exception
         when Exc : Constraint_Error =>
            Set_Last_Exception (Exc);
            return Null_Ptr;
      end;

      return New_String (Token_Kind_Name (K));
   end;

   procedure gpr_token_sloc_range
     (Token : gpr_token; Result : access gpr_source_location_range) is
   begin
      Clear_Last_Exception;
      declare
         T : constant Token_Reference := Unwrap (Token);
         D : constant Token_Data_Type := Data (T);
      begin
         Result.all := Wrap (Sloc_Range (D));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure gpr_token_next
     (Token      : gpr_token;
      Next_Token : access gpr_token)
   is
   begin
      Clear_Last_Exception;
      declare
         T  : constant Token_Reference := Unwrap (Token);
         NT : constant Token_Reference := Next (T);
      begin
         Next_Token.all := Wrap (NT);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure gpr_token_previous
     (Token          : gpr_token;
      Previous_Token : access gpr_token)
   is
   begin
      Clear_Last_Exception;
      declare
         T  : constant Token_Reference := Unwrap (Token);
         PT : constant Token_Reference := Previous (T);
      begin
         Previous_Token.all := Wrap (PT);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function gpr_token_range_text
     (First, Last : gpr_token;
      Text        : access gpr_text) return int
   is
   begin
      Clear_Last_Exception;
      declare
         FD : constant Token_Data_Type := Data (Unwrap (First));
         LD : constant Token_Data_Type := Data (Unwrap (Last));

         First_Source_Buffer, Last_Source_Buffer : Text_Cst_Access;
         First_Index, Ignored_First              : Positive;
         Last_Index, Ignored_Last                : Natural;
      begin
         Extract_Token_Text
           (FD, First_Source_Buffer, First_Index, Ignored_Last);
         Extract_Token_Text
           (LD, Last_Source_Buffer, Ignored_First, Last_Index);
         if First_Source_Buffer /= Last_Source_Buffer then
            return 0;
         end if;
         Text.all := Wrap (First_Source_Buffer, First_Index, Last_Index);
         return 1;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function gpr_token_is_equivalent
     (Left  : gpr_token;
      Right : gpr_token) return gpr_bool
   is
   begin
      Clear_Last_Exception;
         declare
         L  : constant Token_Reference := Unwrap (Left);
         R  : constant Token_Reference := Unwrap (Right);
      begin
         return gpr_bool (Boolean'Pos (Is_Equivalent (L, R)));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   ----------------
   -- Wrap_Alloc --
   ----------------

   function Wrap_Alloc (S : Text_Type) return gpr_text is
      T : Text_Access := new Text_Type'(S);
   begin
      return gpr_text'(T.all'Address, T.all'Length, Is_Allocated => 1);
   end Wrap_Alloc;

   ----------------
   -- Wrap_Alloc --
   ----------------

   function Wrap_Alloc (S : Unbounded_Wide_Wide_String) return gpr_text is
      Chars     : Big_Wide_Wide_String_Access;
      Length    : Natural;
      Allocated : Text_Access;
   begin
      Get_Wide_Wide_String (S, Chars, Length);
      Allocated := new Text_Type (1 .. Length);
      Allocated.all := Chars (1 .. Length);
      return (Allocated.all'Address, Allocated.all'Length, 1);
   end Wrap_Alloc;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (S     : Text_Cst_Access;
      First : Positive;
      Last  : Natural) return gpr_text
   is
      Substring : Text_Type renames S (First .. Last);
   begin
      return (if First > Last
              then (Chars        => System.Null_Address,
                    Length       => 0,
                    Is_Allocated => 0)
              else (Chars        => S (First)'Address,
                    Length       => Substring'Length,
                    Is_Allocated => 0));
   end Wrap;

   procedure gpr_destroy_text (T : access gpr_text) is
   begin
      Clear_Last_Exception;
      declare
         use System;
      begin
         if T.Is_Allocated /= 0 and then T.Chars /= System.Null_Address then
            declare
               TT : Text_Type (1 .. Natural (T.Length));
               for TT'Address use T.Chars;
               TA : Text_Access := TT'Unrestricted_Access;
            begin
               Free (TA);
            end;
            T.Chars := System.Null_Address;
         end if;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure gpr_symbol_text
     (Symbol : access gpr_symbol_type; Text : access gpr_text) is
   begin
      Clear_Last_Exception;
      declare
         Sym    : constant Symbol_Type := Unwrap_Symbol (Symbol.all);
         Result : constant Text_Type :=
           (if Sym = null then "" else Image (Sym));
      begin
         Text.all := Wrap_Alloc (Result);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function gpr_create_big_integer
     (Text : access gpr_text) return gpr_big_integer is
   begin
      Clear_Last_Exception;
      declare
         T      : Text_Type (1 .. Natural (Text.Length))
            with Import, Address => Text.Chars;
         Image  : constant String := Gpr_Parser_Support.Text.Image (T);
         Result : constant Big_Integer_Type := Create_Big_Integer (Image);
      begin
         return Wrap_Big_Integer (Result);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return gpr_big_integer (System.Null_Address);
   end gpr_create_big_integer;

   procedure gpr_big_integer_text
     (Bigint : gpr_big_integer; Text : access gpr_text) is
   begin
      Clear_Last_Exception;
      declare
         BI    : constant Big_Integer_Type := Unwrap_Big_Integer (Bigint);
         Image : constant String := BI.Value.Image;
      begin
         Text.all := Wrap_Alloc (To_Text (Image));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure gpr_big_integer_decref
     (Bigint : gpr_big_integer) is
   begin
      Clear_Last_Exception;
      declare
         BI : Big_Integer_Type := Unwrap_Big_Integer (Bigint);
      begin
         Dec_Ref (BI);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure gpr_get_versions
     (Version, Build_Date : access chars_ptr)
   is
   begin
      Clear_Last_Exception;
      Version.all := New_String (Gpr_Parser.Version);
      Build_Date.all := New_String (Gpr_Parser.Build_Date);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function gpr_create_string
     (Content : System.Address; Length : int) return gpr_string_type
   is
      Value : Text_Type (1 .. Integer (Length))
        with Import, Address => Content;
   begin
      Clear_Last_Exception;
      return Create_String (Value);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   procedure gpr_string_dec_ref (Self : gpr_string_type) is
   begin
      Clear_Last_Exception;
      declare
         Self_Var : String_Type := Self;
      begin
         Dec_Ref (Self_Var);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure gpr_dec_ref_unit_provider
     (Provider : gpr_unit_provider) is
   begin
      Clear_Last_Exception;
      declare
         P : Internal_Unit_Provider_Access :=
            Unwrap_Private_Provider (Provider);
      begin
         Dec_Ref (P);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function gpr_create_event_handler
     (Data                : System.Address;
      Destroy_Func        : gpr_event_handler_destroy_callback;
      Unit_Requested_Func : gpr_event_handler_unit_requested_callback;
      Unit_Parsed_Func    : gpr_event_handler_unit_parsed_callback)
      return gpr_event_handler is
   begin
      Clear_Last_Exception;
      declare
         Result : constant Internal_Event_Handler_Access :=
           new C_Event_Handler'
             (Ada.Finalization.Limited_Controlled with
              Ref_Count           => 1,
              Data                => Data,
              Destroy_Func        => Destroy_Func,
              Unit_Requested_Func => Unit_Requested_Func,
              Unit_Parsed_Func    => Unit_Parsed_Func);
      begin
         return Wrap_Private_Event_Handler (Result);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return gpr_event_handler (System.Null_Address);
   end;

   procedure gpr_dec_ref_event_handler
     (Handler : gpr_event_handler) is
   begin
      Clear_Last_Exception;
      declare
         P : Internal_Event_Handler_Access :=
            Unwrap_Private_Event_Handler (Handler);
      begin
         Dec_Ref (P);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out C_File_Reader) is
   begin
      Self.Destroy_Func (Self.Data);
   end Finalize;

   -------------
   -- Inc_Ref --
   -------------

   overriding procedure Inc_Ref (Self : in out C_File_Reader) is
   begin
      Self.Ref_Count := Self.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   overriding function Dec_Ref (Self : in out C_File_Reader) return Boolean is
   begin
      Self.Ref_Count := Self.Ref_Count - 1;
      if Self.Ref_Count = 0 then
         return True;
      else
         return False;
      end if;
   end Dec_Ref;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self        : C_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      C_Filename : chars_ptr := New_String (Filename);
      C_Charset  : chars_ptr := New_String (Charset);
      C_Read_BOM : constant int := (if Read_BOM then 1 else 0);

      C_Contents   : aliased gpr_text;
      C_Diagnostic : aliased gpr_diagnostic :=
        (Sloc_Range => <>,
         Message    => (Chars        => Null_Address,
                        Length       => 0,
                        Is_Allocated => 0));
   begin
      Self.Read_Func.all
        (Self.Data, C_Filename, C_Charset, C_Read_BOM, C_Contents'Access,
         C_Diagnostic'Access);

      if C_Diagnostic.Message.Chars = Null_Address then

         --  If there is a diagnostic (an error), there is no content to return

         declare
            Message : Text_Type (1 .. Natural (C_Diagnostic.Message.Length))
               with Import,
                    Convention => Ada,
                    Address    => C_Diagnostic.Message.Chars;
         begin
            Append (Diagnostics,
                    Unwrap (C_Diagnostic.Sloc_Range),
                    Message);
         end;

      else
         --  Otherwise, create a copy of the buffer

         declare
            Buffer : Text_Type (1 .. Natural (C_Contents.Length))
               with Import, Convention => Ada, Address => C_Contents.Chars;
         begin
            Contents.Buffer := new Text_Type (Buffer'Range);
            Contents.First := Buffer'First;
            Contents.Last := Buffer'Last;
            Contents.Buffer.all := Buffer;
         end;
      end if;

      Free (C_Filename);
      Free (C_Charset);
   end Read;

   function gpr_create_file_reader
     (Data         : System.Address;
      Destroy_Func : gpr_file_reader_destroy_callback;
      Read_Func    : gpr_file_reader_read_callback) return gpr_file_reader
   is
   begin
      Clear_Last_Exception;
      declare
         Result : constant C_File_Reader_Access := new C_File_Reader'
           (Ada.Finalization.Limited_Controlled with
            Ref_Count    => 1,
            Data         => Data,
            Destroy_Func => Destroy_Func,
            Read_Func    => Read_Func);
      begin
         return Wrap_Private_File_Reader (Internal_File_Reader_Access (Result));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return gpr_file_reader (System.Null_Address);
   end;

   procedure gpr_dec_ref_file_reader
     (File_Reader : gpr_file_reader) is
   begin
      Clear_Last_Exception;
      declare
         P : Internal_File_Reader_Access :=
            Unwrap_Private_File_Reader (File_Reader);
      begin
         Dec_Ref (P);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   


   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out C_Event_Handler) is
   begin
      if Self.Destroy_Func /= null then
          Self.Destroy_Func (Self.Data);
      end if;
   end Finalize;

   -------------
   -- Inc_Ref --
   -------------

   overriding procedure Inc_Ref (Self : in out C_Event_Handler) is
   begin
      Self.Ref_Count := Self.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   overriding function Dec_Ref (Self : in out C_Event_Handler) return Boolean
   is
   begin
      Self.Ref_Count := Self.Ref_Count - 1;
      if Self.Ref_Count = 0 then
         return True;
      else
         return False;
      end if;
   end Dec_Ref;

   -----------------------------
   -- Unit_Requested_Callback --
   -----------------------------

   overriding procedure Unit_Requested_Callback
     (Self               : in out C_Event_Handler;
      Context            : Internal_Context;
      Name               : Text_Type;
      From               : Internal_Unit;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean)
   is
      Name_Access : constant Text_Cst_Access := Name'Unrestricted_Access;
      C_Name      : aliased constant gpr_text := Wrap (Name_Access);
   begin
      Self.Unit_Requested_Func
        (Self.Data,
         Context,
         C_Name'Access,
         From,
         (if Found then 1 else 0),
         (if Is_Not_Found_Error then 1 else 0));
   end Unit_Requested_Callback;

   --------------------------
   -- Unit_Parsed_Callback --
   --------------------------

   overriding procedure Unit_Parsed_Callback
     (Self     : in out C_Event_Handler;
      Context  : Internal_Context;
      Unit     : Internal_Unit;
      Reparsed : Boolean)
   is
   begin
      Self.Unit_Parsed_Func
        (Self.Data, Context, Unit, (if Reparsed then 1 else 0));
   end Unit_Parsed_Callback;

   


   ----------
   -- Wrap --
   ----------

   function Wrap (Token : Token_Reference) return gpr_token is
   begin
      if Token = No_Token then
         return (Token_Data   => null,
                 Token_Index  => -1,
                 Trivia_Index => -1,
                 others       => <>);
      end if;

      declare
         Index : constant Token_Or_Trivia_Index := Get_Token_Index (Token);
      begin
         return (Context         => Get_Token_Context (Token),
                 Token_Data      => Get_Token_TDH (Token),
                 Token_Index     => int (Index.Token),
                 Trivia_Index    => int (Index.Trivia));
      end;
   end Wrap;

   ------------
   -- Unwrap --
   ------------

   function Unwrap (Token : gpr_token) return Token_Reference is
   begin
      return (if Token.Token_Data = null
              then No_Token
              else Wrap_Token_Reference
                     (Token.Context,
                      Token.Token_Data,
                      (Token  => Token_Index (Token.Token_Index),
                       Trivia => Token_Index (Token.Trivia_Index))));
   end Unwrap;

   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

           

   

   
   

   function gpr_gpr_node_parent
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity;
         begin
            Result := Gpr_Parser.Implementation.Parent (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_gpr_node_parent;


           

   

   
   

   function gpr_gpr_node_parents
     (Node : gpr_node_Ptr;

         With_Self :
            
            gpr_bool;

      Value_P : access gpr_node_array) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
         
         Unwrapped_With_Self : constant Boolean :=
               With_Self /= 0
         ;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Array_Access;
         begin
            Result := Gpr_Parser.Implementation.Parents (Unwrapped_Node, With_Self => Unwrapped_With_Self, E_Info => Node.Info);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_gpr_node_parents;


           

   

   
   

   function gpr_gpr_node_children
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node_array) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Array_Access;
         begin
            Result := Gpr_Parser.Implementation.Children (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_gpr_node_children;


           

   

   
   

   function gpr_gpr_node_token_start
     (Node : gpr_node_Ptr;


      Value_P : access gpr_token) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Token_Reference;
         begin
            Result := Gpr_Parser.Implementation.Token_Start (Unwrapped_Node);

            Value_P.all :=
                   Wrap (Result)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_gpr_node_token_start;


           

   

   
   

   function gpr_gpr_node_token_end
     (Node : gpr_node_Ptr;


      Value_P : access gpr_token) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Token_Reference;
         begin
            Result := Gpr_Parser.Implementation.Token_End (Unwrapped_Node);

            Value_P.all :=
                   Wrap (Result)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_gpr_node_token_end;


           

   

   
   

   function gpr_gpr_node_child_index
     (Node : gpr_node_Ptr;


      Value_P : access int) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Integer;
         begin
            Result := Gpr_Parser.Implementation.Child_Index (Unwrapped_Node);

            Value_P.all :=
                   int (Result)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_gpr_node_child_index;


           

   

   
   

   function gpr_gpr_node_previous_sibling
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity;
         begin
            Result := Gpr_Parser.Implementation.Previous_Sibling (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_gpr_node_previous_sibling;


           

   

   
   

   function gpr_gpr_node_next_sibling
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity;
         begin
            Result := Gpr_Parser.Implementation.Next_Sibling (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_gpr_node_next_sibling;


           

   

   
   

   function gpr_gpr_node_unit
     (Node : gpr_node_Ptr;


      Value_P : access gpr_analysis_unit) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Unit;
         begin
            Result := Gpr_Parser.Implementation.Unit (Unwrapped_Node);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_gpr_node_unit;


           

   

   
   

   function gpr_gpr_node_is_ghost
     (Node : gpr_node_Ptr;


      Value_P : access gpr_bool) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Boolean;
         begin
            Result := Gpr_Parser.Implementation.Is_Ghost (Unwrapped_Node);

            Value_P.all :=
                   gpr_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_gpr_node_is_ghost;


           

   

   
   

   function gpr_gpr_node_full_sloc_image
     (Node : gpr_node_Ptr;


      Value_P : access gpr_string_type) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : String_Type;
         begin
            Result := Gpr_Parser.Implementation.Full_Sloc_Image (Unwrapped_Node);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_gpr_node_full_sloc_image;


           

   

   
   

   function gpr_all_qualifier_p_as_bool
     (Node : gpr_node_Ptr;


      Value_P : access gpr_bool) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_All_Qualifier then

         declare
            

            Result : Boolean;
         begin
            Result := Gpr_Parser.Implementation.Dispatcher_All_Qualifier_P_As_Bool (Unwrapped_Node);

            Value_P.all :=
                   gpr_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_all_qualifier_p_as_bool;


           

   

   
   

   function gpr_attribute_decl_f_attr_name
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Attribute_Decl_Range then

         declare
            

            Result : Bare_Identifier;
         begin
            Result := Attribute_Decl_F_Attr_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_attribute_decl_f_attr_name;


           

   

   
   

   function gpr_attribute_decl_f_attr_index
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Attribute_Decl_Range then

         declare
            

            Result : Bare_Gpr_Node;
         begin
            Result := Attribute_Decl_F_Attr_Index (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_attribute_decl_f_attr_index;


           

   

   
   

   function gpr_attribute_decl_f_expr
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Attribute_Decl_Range then

         declare
            

            Result : Bare_Term_List;
         begin
            Result := Attribute_Decl_F_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_attribute_decl_f_expr;


           

   

   
   

   function gpr_attribute_reference_f_attribute_name
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Attribute_Reference_Range then

         declare
            

            Result : Bare_Identifier;
         begin
            Result := Attribute_Reference_F_Attribute_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_attribute_reference_f_attribute_name;


           

   

   
   

   function gpr_attribute_reference_f_attribute_index
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Attribute_Reference_Range then

         declare
            

            Result : Bare_Gpr_Node;
         begin
            Result := Attribute_Reference_F_Attribute_Index (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_attribute_reference_f_attribute_index;


           

   

   
   

   function gpr_builtin_function_call_f_function_name
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Builtin_Function_Call_Range then

         declare
            

            Result : Bare_Identifier;
         begin
            Result := Builtin_Function_Call_F_Function_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_builtin_function_call_f_function_name;


           

   

   
   

   function gpr_builtin_function_call_f_parameters
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Builtin_Function_Call_Range then

         declare
            

            Result : Bare_Terms;
         begin
            Result := Builtin_Function_Call_F_Parameters (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_builtin_function_call_f_parameters;


           

   

   
   

   function gpr_case_construction_f_var_ref
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Case_Construction_Range then

         declare
            

            Result : Bare_Variable_Reference;
         begin
            Result := Case_Construction_F_Var_Ref (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_case_construction_f_var_ref;


           

   

   
   

   function gpr_case_construction_f_items
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Case_Construction_Range then

         declare
            

            Result : Bare_Case_Item_List;
         begin
            Result := Case_Construction_F_Items (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_case_construction_f_items;


           

   

   
   

   function gpr_case_item_f_choice
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Case_Item_Range then

         declare
            

            Result : Bare_Choices;
         begin
            Result := Case_Item_F_Choice (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_case_item_f_choice;


           

   

   
   

   function gpr_case_item_f_decls
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Case_Item_Range then

         declare
            

            Result : Bare_Gpr_Node_List;
         begin
            Result := Case_Item_F_Decls (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_case_item_f_decls;


           

   

   
   

   function gpr_compilation_unit_f_project
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Compilation_Unit_Range then

         declare
            

            Result : Bare_Project;
         begin
            Result := Compilation_Unit_F_Project (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_compilation_unit_f_project;


           

   

   
   

   function gpr_prefix_f_prefix
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Prefix_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Prefix_F_Prefix (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_prefix_f_prefix;


           

   

   
   

   function gpr_prefix_f_suffix
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Prefix_Range then

         declare
            

            Result : Bare_Identifier;
         begin
            Result := Prefix_F_Suffix (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_prefix_f_suffix;


           

   

   
   

   function gpr_limited_node_p_as_bool
     (Node : gpr_node_Ptr;


      Value_P : access gpr_bool) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Limited_Node then

         declare
            

            Result : Boolean;
         begin
            Result := Gpr_Parser.Implementation.Dispatcher_Limited_Node_P_As_Bool (Unwrapped_Node);

            Value_P.all :=
                   gpr_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_limited_node_p_as_bool;


           

   

   
   

   function gpr_package_decl_f_pkg_name
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Package_Decl_Range then

         declare
            

            Result : Bare_Identifier;
         begin
            Result := Package_Decl_F_Pkg_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_package_decl_f_pkg_name;


           

   

   
   

   function gpr_package_decl_f_pkg_spec
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Package_Decl_Range then

         declare
            

            Result : Bare_Gpr_Node;
         begin
            Result := Package_Decl_F_Pkg_Spec (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_package_decl_f_pkg_spec;


           

   

   
   

   function gpr_package_extension_f_extended_name
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Package_Extension_Range then

         declare
            

            Result : Bare_Identifier_List;
         begin
            Result := Package_Extension_F_Extended_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_package_extension_f_extended_name;


           

   

   
   

   function gpr_package_renaming_f_renamed_name
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Package_Renaming_Range then

         declare
            

            Result : Bare_Identifier_List;
         begin
            Result := Package_Renaming_F_Renamed_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_package_renaming_f_renamed_name;


           

   

   
   

   function gpr_package_spec_f_extension
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Package_Spec_Range then

         declare
            

            Result : Bare_Package_Extension;
         begin
            Result := Package_Spec_F_Extension (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_package_spec_f_extension;


           

   

   
   

   function gpr_package_spec_f_decls
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Package_Spec_Range then

         declare
            

            Result : Bare_Gpr_Node_List;
         begin
            Result := Package_Spec_F_Decls (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_package_spec_f_decls;


           

   

   
   

   function gpr_package_spec_f_end_name
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Package_Spec_Range then

         declare
            

            Result : Bare_Identifier;
         begin
            Result := Package_Spec_F_End_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_package_spec_f_end_name;


           

   

   
   

   function gpr_project_f_context_clauses
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Project_Range then

         declare
            

            Result : Bare_With_Decl_List;
         begin
            Result := Project_F_Context_Clauses (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_project_f_context_clauses;


           

   

   
   

   function gpr_project_f_project_decl
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Project_Range then

         declare
            

            Result : Bare_Project_Declaration;
         begin
            Result := Project_F_Project_Decl (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_project_f_project_decl;


           

   

   
   

   function gpr_project_declaration_f_qualifier
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Project_Declaration_Range then

         declare
            

            Result : Bare_Project_Qualifier;
         begin
            Result := Project_Declaration_F_Qualifier (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_project_declaration_f_qualifier;


           

   

   
   

   function gpr_project_declaration_f_project_name
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Project_Declaration_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Project_Declaration_F_Project_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_project_declaration_f_project_name;


           

   

   
   

   function gpr_project_declaration_f_extension
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Project_Declaration_Range then

         declare
            

            Result : Bare_Project_Extension;
         begin
            Result := Project_Declaration_F_Extension (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_project_declaration_f_extension;


           

   

   
   

   function gpr_project_declaration_f_decls
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Project_Declaration_Range then

         declare
            

            Result : Bare_Gpr_Node_List;
         begin
            Result := Project_Declaration_F_Decls (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_project_declaration_f_decls;


           

   

   
   

   function gpr_project_declaration_f_end_name
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Project_Declaration_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Project_Declaration_F_End_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_project_declaration_f_end_name;


           

   

   
   

   function gpr_project_extension_f_is_all
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Project_Extension_Range then

         declare
            

            Result : Bare_All_Qualifier;
         begin
            Result := Project_Extension_F_Is_All (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_project_extension_f_is_all;


           

   

   
   

   function gpr_project_extension_f_path_name
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Project_Extension_Range then

         declare
            

            Result : Bare_String_Literal;
         begin
            Result := Project_Extension_F_Path_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_project_extension_f_path_name;


           

   

   
   

   function gpr_string_literal_at_f_str_lit
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_String_Literal_At_Range then

         declare
            

            Result : Bare_String_Literal;
         begin
            Result := String_Literal_At_F_Str_Lit (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_string_literal_at_f_str_lit;


           

   

   
   

   function gpr_string_literal_at_f_at_lit
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_String_Literal_At_Range then

         declare
            

            Result : Bare_Num_Literal;
         begin
            Result := String_Literal_At_F_At_Lit (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_string_literal_at_f_at_lit;


           

   

   
   

   function gpr_terms_f_terms
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Terms_Range then

         declare
            

            Result : Bare_Term_List_List;
         begin
            Result := Terms_F_Terms (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_terms_f_terms;


           

   

   
   

   function gpr_type_reference_f_var_type_name
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Type_Reference_Range then

         declare
            

            Result : Bare_Identifier_List;
         begin
            Result := Type_Reference_F_Var_Type_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_type_reference_f_var_type_name;


           

   

   
   

   function gpr_typed_string_decl_f_type_id
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Typed_String_Decl_Range then

         declare
            

            Result : Bare_Identifier;
         begin
            Result := Typed_String_Decl_F_Type_Id (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_typed_string_decl_f_type_id;


           

   

   
   

   function gpr_typed_string_decl_f_string_literals
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Typed_String_Decl_Range then

         declare
            

            Result : Bare_String_Literal_List;
         begin
            Result := Typed_String_Decl_F_String_Literals (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_typed_string_decl_f_string_literals;


           

   

   
   

   function gpr_variable_decl_f_var_name
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Variable_Decl_Range then

         declare
            

            Result : Bare_Identifier;
         begin
            Result := Variable_Decl_F_Var_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_variable_decl_f_var_name;


           

   

   
   

   function gpr_variable_decl_f_var_type
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Variable_Decl_Range then

         declare
            

            Result : Bare_Type_Reference;
         begin
            Result := Variable_Decl_F_Var_Type (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_variable_decl_f_var_type;


           

   

   
   

   function gpr_variable_decl_f_expr
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Variable_Decl_Range then

         declare
            

            Result : Bare_Term_List;
         begin
            Result := Variable_Decl_F_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_variable_decl_f_expr;


           

   

   
   

   function gpr_variable_reference_f_variable_name
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Variable_Reference_Range then

         declare
            

            Result : Bare_Identifier_List;
         begin
            Result := Variable_Reference_F_Variable_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_variable_reference_f_variable_name;


           

   

   
   

   function gpr_variable_reference_f_attribute_ref
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_Variable_Reference_Range then

         declare
            

            Result : Bare_Attribute_Reference;
         begin
            Result := Variable_Reference_F_Attribute_Ref (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_variable_reference_f_attribute_ref;


           

   

   
   

   function gpr_with_decl_f_is_limited
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_With_Decl_Range then

         declare
            

            Result : Bare_Limited_Node;
         begin
            Result := With_Decl_F_Is_Limited (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_with_decl_f_is_limited;


           

   

   
   

   function gpr_with_decl_f_path_names
     (Node : gpr_node_Ptr;


      Value_P : access gpr_node) return int

   is
      Unwrapped_Node : constant Bare_Gpr_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Gpr_With_Decl_Range then

         declare
            

            Result : Bare_String_Literal_List;
         begin
            Result := With_Decl_F_Path_Names (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end gpr_with_decl_f_path_names;



         






         



function gpr_node_array_create (Length : int) return Internal_Entity_Array_Access is
begin
   Clear_Last_Exception;
   return Create_Internal_Entity_Array (Natural (Length));
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
      return null;
end gpr_node_array_create;

procedure gpr_node_array_inc_ref (A : Internal_Entity_Array_Access) is
begin
   Clear_Last_Exception;
   Inc_Ref (A);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end;

procedure gpr_node_array_dec_ref (A : Internal_Entity_Array_Access) is
begin
   Clear_Last_Exception;
   declare
      A_Var : Internal_Entity_Array_Access := A;
   begin
      Dec_Ref (A_Var);
   end;
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end;




end Gpr_Parser.Implementation.C;
