
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with System;

with GNATCOLL.Iconv;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Gpr_Parser_Support.Generic_API; use Gpr_Parser_Support.Generic_API;
with Gpr_Parser_Support.Generic_API.Analysis;
use Gpr_Parser_Support.Generic_API.Analysis;
with Gpr_Parser_Support.Internal.Analysis;
with Gpr_Parser_Support.Internal.Conversions;

with Gpr_Parser.Generic_API;
with Gpr_Parser.Implementation; use Gpr_Parser.Implementation;
with Gpr_Parser.Lexer_Implementation;
use Gpr_Parser.Lexer_Implementation;
with Gpr_Parser.Private_Converters;


package body Gpr_Parser.Common is

   Is_Token_Node_Kind : constant array (Gpr_Node_Kind_Type) of Boolean :=
     (Gpr_Ada_Access_Subp => False, Gpr_Ada_Pragma => False, Gpr_Ada_Use => False, Gpr_Ada_With => False, Gpr_Ada_Entity_Kind_Function => False, Gpr_Ada_Entity_Kind_Package => False, Gpr_Ada_Entity_Kind_Procedure => False, Gpr_Ada_Generic => False, Gpr_Ada_Library_Item => False, Gpr_Ada_Pkg => False, Gpr_Ada_Pkg_Body => False, Gpr_Ada_Subp => False, Gpr_Ada_Prelude => False, Gpr_Ada_Separate => False, Gpr_Ada_Skip => False, Gpr_Ada_With_Formal => False, Gpr_All_Qualifier_Absent => False, Gpr_All_Qualifier_Present => False, Gpr_Attribute_Decl => False, Gpr_Attribute_Reference => False, Gpr_Ada_Context_Clause_List => False, Gpr_Ada_Prelude_Node_List => False, Gpr_Ada_Skip_List => False, Gpr_Case_Item_List => False, Gpr_Expr_List => False, Gpr_Gpr_Node_List => False, Gpr_Choices => False, Gpr_Term_List => False, Gpr_Identifier_List => False, Gpr_String_Literal_List => False, Gpr_Term_List_List => False, Gpr_With_Decl_List => False, Gpr_Builtin_Function_Call => False, Gpr_Case_Construction => False, Gpr_Case_Item => False, Gpr_Compilation_Unit => False, Gpr_Empty_Decl => False, Gpr_Prefix => False, Gpr_Identifier => True, Gpr_Num_Literal => True, Gpr_String_Literal => True, Gpr_Limited_Absent => False, Gpr_Limited_Present => False, Gpr_Others_Designator => False, Gpr_Package_Decl => False, Gpr_Package_Extension => False, Gpr_Package_Renaming => False, Gpr_Package_Spec => False, Gpr_Private_Absent => False, Gpr_Private_Present => False, Gpr_Project => False, Gpr_Project_Declaration => False, Gpr_Project_Extension => False, Gpr_Project_Qualifier_Abstract => False, Gpr_Project_Qualifier_Aggregate => False, Gpr_Project_Qualifier_Aggregate_Library => False, Gpr_Project_Qualifier_Configuration => False, Gpr_Project_Qualifier_Library => False, Gpr_Project_Qualifier_Standard => False, Gpr_Project_Reference => False, Gpr_String_Literal_At => False, Gpr_Terms => False, Gpr_Type_Reference => False, Gpr_Typed_String_Decl => False, Gpr_Variable_Decl => False, Gpr_Variable_Reference => False, Gpr_With_Decl => False);
   --  For each node kind, return whether it is a node that contains only a
   --  single token.

   Is_Error_Node_Kind : constant array (Gpr_Node_Kind_Type) of Boolean :=
     (Gpr_Ada_Access_Subp => False, Gpr_Ada_Pragma => False, Gpr_Ada_Use => False, Gpr_Ada_With => False, Gpr_Ada_Entity_Kind_Function => False, Gpr_Ada_Entity_Kind_Package => False, Gpr_Ada_Entity_Kind_Procedure => False, Gpr_Ada_Generic => False, Gpr_Ada_Library_Item => False, Gpr_Ada_Pkg => False, Gpr_Ada_Pkg_Body => False, Gpr_Ada_Subp => False, Gpr_Ada_Prelude => False, Gpr_Ada_Separate => False, Gpr_Ada_Skip => True, Gpr_Ada_With_Formal => False, Gpr_All_Qualifier_Absent => False, Gpr_All_Qualifier_Present => False, Gpr_Attribute_Decl => False, Gpr_Attribute_Reference => False, Gpr_Ada_Context_Clause_List => False, Gpr_Ada_Prelude_Node_List => False, Gpr_Ada_Skip_List => False, Gpr_Case_Item_List => False, Gpr_Expr_List => False, Gpr_Gpr_Node_List => False, Gpr_Choices => False, Gpr_Term_List => False, Gpr_Identifier_List => False, Gpr_String_Literal_List => False, Gpr_Term_List_List => False, Gpr_With_Decl_List => False, Gpr_Builtin_Function_Call => False, Gpr_Case_Construction => False, Gpr_Case_Item => False, Gpr_Compilation_Unit => False, Gpr_Empty_Decl => False, Gpr_Prefix => False, Gpr_Identifier => False, Gpr_Num_Literal => False, Gpr_String_Literal => False, Gpr_Limited_Absent => False, Gpr_Limited_Present => False, Gpr_Others_Designator => False, Gpr_Package_Decl => False, Gpr_Package_Extension => False, Gpr_Package_Renaming => False, Gpr_Package_Spec => False, Gpr_Private_Absent => False, Gpr_Private_Present => False, Gpr_Project => False, Gpr_Project_Declaration => False, Gpr_Project_Extension => False, Gpr_Project_Qualifier_Abstract => False, Gpr_Project_Qualifier_Aggregate => False, Gpr_Project_Qualifier_Aggregate_Library => False, Gpr_Project_Qualifier_Configuration => False, Gpr_Project_Qualifier_Library => False, Gpr_Project_Qualifier_Standard => False, Gpr_Project_Reference => False, Gpr_String_Literal_At => False, Gpr_Terms => False, Gpr_Type_Reference => False, Gpr_Typed_String_Decl => False, Gpr_Variable_Decl => False, Gpr_Variable_Reference => False, Gpr_With_Decl => False);
   --  For each node kind, return whether it is an error node

   function Wrap_Token_Reference
     (Context : Internal_Context;
      TDH     : Token_Data_Handler_Access;
      Index   : Token_Or_Trivia_Index) return Token_Reference;
   function Get_Token_Context (Token : Token_Reference) return Internal_Context;
   function Get_Token_Unit (Token : Token_Reference) return Internal_Unit;
   function Get_Token_TDH
     (Token : Token_Reference) return Token_Data_Handler_Access;
   function Get_Token_Index
     (Token : Token_Reference) return Token_Or_Trivia_Index;
   procedure Extract_Token_Text
     (Token         : Token_Data_Type;
      Source_Buffer : out Text_Cst_Access;
      First         : out Positive;
      Last          : out Natural);
   --  Implementations for converters soft-links

   function From_Generic (Token : Lk_Token) return Common.Token_Reference
     with Export, External_Name => "Gpr_Parser__from_generic_token";
   function To_Generic (Token : Common.Token_Reference) return Lk_Token
     with Export, External_Name => "Gpr_Parser__to_generic_token";
   --  Implementation for converters hard-links in Private_Converters

   function "+" is new Ada.Unchecked_Conversion
     (Gpr_Parser_Support.Internal.Analysis.Internal_Context, Internal_Context);
   function "+" is new Ada.Unchecked_Conversion
     (Internal_Context, Gpr_Parser_Support.Internal.Analysis.Internal_Context);

   function Rewrap_Token
     (Origin : Token_Reference;
      Index  : Token_Or_Trivia_Index) return Token_Reference;
   --  Create a token reference for ``Index`` using the token data handler
   --  reference from ``Origin``.

   Token_Kind_To_Literals : constant array (Token_Kind) of Text_Access := (
   

         Gpr_All => new Text_Type'("all"),
         
         Gpr_Abstract => new Text_Type'("abstract"),
         
         Gpr_At => new Text_Type'("at"),
         
         Gpr_Case => new Text_Type'("case"),
         
         Gpr_End => new Text_Type'("end"),
         
         Gpr_For => new Text_Type'("for"),
         
         Gpr_Is => new Text_Type'("is"),
         
         Gpr_Limited => new Text_Type'("limited"),
         
         Gpr_Private => new Text_Type'("private"),
         
         Gpr_Null => new Text_Type'("null"),
         
         Gpr_Others => new Text_Type'("others"),
         
         Gpr_Package => new Text_Type'("package"),
         
         Gpr_Renames => new Text_Type'("renames"),
         
         Gpr_Type => new Text_Type'("type"),
         
         Gpr_Use => new Text_Type'("use"),
         
         Gpr_Pragma => new Text_Type'("pragma"),
         
         Gpr_When => new Text_Type'("when"),
         
         Gpr_With => new Text_Type'("with"),
         
         Gpr_Extends => new Text_Type'("extends"),
         
         Gpr_Par_Open => new Text_Type'("("),
         
         Gpr_Par_Close => new Text_Type'(")"),
         
         Gpr_Semicolon => new Text_Type'(";"),
         
         Gpr_Colon => new Text_Type'(":"),
         
         Gpr_Comma => new Text_Type'(","),
         
         Gpr_Dot => new Text_Type'("."),
         
         Gpr_Amp => new Text_Type'("&"),
         
         Gpr_Tick => new Text_Type'("'"),
         
         Gpr_Pipe => new Text_Type'("|"),
         
         Gpr_Assign => new Text_Type'(":="),
         
         Gpr_Arrow => new Text_Type'("=>"),
         
      others => new Text_Type'("")
   );

   Token_Kind_Names : constant array (Token_Kind) of String_Access := (
          Gpr_Identifier =>
             new String'("Identifier")
              ,
          Gpr_All =>
             new String'("All")
              ,
          Gpr_Abstract =>
             new String'("Abstract")
              ,
          Gpr_At =>
             new String'("At")
              ,
          Gpr_Case =>
             new String'("Case")
              ,
          Gpr_End =>
             new String'("End")
              ,
          Gpr_For =>
             new String'("For")
              ,
          Gpr_Is =>
             new String'("Is")
              ,
          Gpr_Limited =>
             new String'("Limited")
              ,
          Gpr_Private =>
             new String'("Private")
              ,
          Gpr_Null =>
             new String'("Null")
              ,
          Gpr_Others =>
             new String'("Others")
              ,
          Gpr_Package =>
             new String'("Package")
              ,
          Gpr_Renames =>
             new String'("Renames")
              ,
          Gpr_Type =>
             new String'("Type")
              ,
          Gpr_Use =>
             new String'("Use")
              ,
          Gpr_Pragma =>
             new String'("Pragma")
              ,
          Gpr_When =>
             new String'("When")
              ,
          Gpr_With =>
             new String'("With")
              ,
          Gpr_Extends =>
             new String'("Extends")
              ,
          Gpr_Par_Open =>
             new String'("Par_Open")
              ,
          Gpr_Par_Close =>
             new String'("Par_Close")
              ,
          Gpr_Semicolon =>
             new String'("Semicolon")
              ,
          Gpr_Colon =>
             new String'("Colon")
              ,
          Gpr_Comma =>
             new String'("Comma")
              ,
          Gpr_Dot =>
             new String'("Dot")
              ,
          Gpr_Amp =>
             new String'("Amp")
              ,
          Gpr_Tick =>
             new String'("Tick")
              ,
          Gpr_Pipe =>
             new String'("Pipe")
              ,
          Gpr_Assign =>
             new String'("Assign")
              ,
          Gpr_Arrow =>
             new String'("Arrow")
              ,
          Gpr_String =>
             new String'("String")
              ,
          Gpr_Number =>
             new String'("Number")
              ,
          Gpr_Label =>
             new String'("Label")
              ,
          Gpr_Char =>
             new String'("Char")
              ,
          Gpr_Comment =>
             new String'("Comment")
              ,
          Gpr_Whitespace =>
             new String'("Whitespace")
              ,
          Gpr_Termination =>
             new String'("Termination")
              ,
          Gpr_Lexing_Failure =>
             new String'("Lexing_Failure")
   );

   ---------------------
   -- Token_Kind_Name --
   ---------------------

   function Token_Kind_Name (Token_Id : Token_Kind) return String is
     (Token_Kind_Names (Token_Id).all);

   ------------------------
   -- Token_Kind_Literal --
   ------------------------

   function Token_Kind_Literal (Token_Id : Token_Kind) return Text_Type is
     (Token_Kind_To_Literals (Token_Id).all);

   -----------------------
   -- Token_Error_Image --
   -----------------------

   function Token_Error_Image (Token_Id : Token_Kind) return String is
      Literal : constant Text_Type := Token_Kind_Literal (Token_Id);
   begin
      return (if Literal /= ""
              then "'" & Image (Literal) & "'"
              else Token_Kind_Name (Token_Id));
   end Token_Error_Image;

   function To_Token_Kind (Raw : Raw_Token_Kind) return Token_Kind
   is (Token_Kind'Val (Raw));

   function From_Token_Kind (Kind : Token_Kind) return Raw_Token_Kind
   is (Token_Kind'Pos (Kind));

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Kind : Gpr_Node_Kind_Type) return Boolean is
   begin
      return Is_Token_Node_Kind (Kind);
   end Is_Token_Node;

   -------------------
   -- Is_Error_Node --
   -------------------

   function Is_Error_Node (Kind : Gpr_Node_Kind_Type) return Boolean is
   begin
      return Is_Error_Node_Kind (Kind);
   end Is_Error_Node;

   ------------------
   -- Is_List_Node --
   ------------------

   function Is_List_Node (Kind : Gpr_Node_Kind_Type) return Boolean is
   begin
         return Kind in Gpr_Base_List;
   end Is_List_Node;

   ------------------
   -- Rewrap_Token --
   ------------------

   function Rewrap_Token
     (Origin : Token_Reference;
      Index  : Token_Or_Trivia_Index) return Token_Reference is
   begin
      return (if Index = No_Token_Or_Trivia_Index
              then No_Token
              else (Origin.TDH, Index, Origin.Safety_Net));
   end Rewrap_Token;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Self : Token_Reference) is
      SN  : Token_Safety_Net renames Self.Safety_Net;
      Ctx : constant Internal_Context := +SN.Context;
   begin
      if Self.TDH /= null
         and then (Ctx.Serial_Number /= SN.Context_Version
                   or else Self.TDH.Version /= SN.TDH_Version)
      then
         raise Stale_Reference_Error;
      end if;
   end Check_Safety_Net;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Token_Reference) return Boolean is
      pragma Assert (Left.TDH = Right.TDH);
   begin
      Check_Safety_Net (Left);
      Check_Safety_Net (Right);
      if Left.Index.Token < Right.Index.Token then
         return True;

      elsif Left.Index.Token = Right.Index.Token then
         return Left.Index.Trivia < Right.Index.Trivia;

      else
         return False;
      end if;
   end "<";

   ----------
   -- Next --
   ----------

   function Next
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference is
   begin
      Check_Safety_Net (Token);
      return (if Token.TDH = null
              then No_Token
              else Rewrap_Token (Token,
                                 Next (Token.Index, Token.TDH.all,
                                       Exclude_Trivia)));
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference is
   begin
      Check_Safety_Net (Token);
      return (if Token.TDH = null
              then No_Token
              else Rewrap_Token (Token,
                                 Previous (Token.Index, Token.TDH.all,
                                           Exclude_Trivia)));
   end Previous;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol (Token : Token_Reference) return Symbol_Type is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return Get_Symbol (Token.Index, Token.TDH.all);
   end Get_Symbol;

   ----------
   -- Data --
   ----------

   function Data (Token : Token_Reference) return Token_Data_Type is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return Convert (Token.TDH.all, Token, Raw_Data (Token));
   end Data;

   ----------
   -- Text --
   ----------

   function Text (Token : Token_Reference) return Text_Type is
      RD : constant Stored_Token_Data := Raw_Data (Token);
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return Token.TDH.Source_Buffer (RD.Source_First .. RD.Source_Last);
   end Text;

   ----------
   -- Text --
   ----------

   function Text (First, Last : Token_Reference) return Text_Type is
      FD, LD : Token_Data_Type;
   begin
      Check_Safety_Net (First);
      Check_Safety_Net (Last);
      if First.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      if First.TDH /= Last.TDH then
         raise Precondition_Failure with
            "token arguments must belong to the same source";
      end if;
      FD := Data (First);
      LD := Data (Last);
      return FD.Source_Buffer.all (FD.Source_First .. LD.Source_Last);
   end Text;

   ----------
   -- Kind --
   ----------

   function Kind (Token_Data : Token_Data_Type) return Token_Kind is
   begin
      return Token_Data.Kind;
   end Kind;

   ---------------
   -- Is_Trivia --
   ---------------

   function Is_Trivia (Token : Token_Reference) return Boolean is
   begin
      Check_Safety_Net (Token);
      return Token.Index.Trivia /= No_Token_Index;
   end Is_Trivia;

   ---------------
   -- Is_Trivia --
   ---------------

   function Is_Trivia (Token_Data : Token_Data_Type) return Boolean is
   begin
      return Token_Data.Is_Trivia;
   end Is_Trivia;

   -----------
   -- Index --
   -----------

   function Index (Token : Token_Reference) return Token_Index is
   begin
      Check_Safety_Net (Token);
      return (if Token.Index.Trivia = No_Token_Index
              then Token.Index.Token
              else Token.Index.Trivia);
   end Index;

   -----------
   -- Index --
   -----------

   function Index (Token_Data : Token_Data_Type) return Token_Index is
   begin
      return Token_Data.Index;
   end Index;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Token_Data : Token_Data_Type) return Source_Location_Range
   is
   begin
      return Token_Data.Sloc_Range;
   end Sloc_Range;

   ---------------------
   -- Origin_Filename --
   ---------------------

   function Origin_Filename (Token : Token_Reference) return String is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return +Token.TDH.Filename.Full_Name;
   end Origin_Filename;

   --------------------
   -- Origin_Charset --
   --------------------

   function Origin_Charset (Token : Token_Reference) return String is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return To_String (Token.TDH.Charset);
   end Origin_Charset;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent (L, R : Token_Reference) return Boolean is
      DL : constant Token_Data_Type := Data (L);
      DR : constant Token_Data_Type := Data (R);
      TL : constant Text_Type := Text (L);
      TR : constant Text_Type := Text (R);
   begin
      return DL.Kind = DR.Kind and then TL = TR;
   end Is_Equivalent;

   -----------
   -- Image --
   -----------

   function Image (Token : Token_Reference) return String is
      D : constant Token_Data_Type := Data (Token);
   begin
      return ("<Token Kind=" & Token_Kind_Name (D.Kind) &
              " Text=" & Image (Text (Token), With_Quotes => True) & ">");
   end Image;

   --------------
   -- Raw_Data --
   --------------

   function Raw_Data (T : Token_Reference) return Stored_Token_Data is
   begin
      Check_Safety_Net (T);
      if T.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return
        (if T.Index.Trivia = No_Token_Index
         then Token_Vectors.Get (T.TDH.Tokens, Natural (T.Index.Token))
         else Trivia_Vectors.Get (T.TDH.Trivias, Natural (T.Index.Trivia)).T);
   end Raw_Data;

   -------------
   -- Convert --
   -------------

   function Convert
     (TDH      : Token_Data_Handler;
      Token    : Token_Reference;
      Raw_Data : Stored_Token_Data) return Token_Data_Type is
   begin
      Check_Safety_Net (Token);
      return (Kind          => To_Token_Kind (Raw_Data.Kind),
              Is_Trivia     => Token.Index.Trivia /= No_Token_Index,
              Index         => (if Token.Index.Trivia = No_Token_Index
                                then Token.Index.Token
                                else Token.Index.Trivia),
              Source_Buffer => Text_Cst_Access (TDH.Source_Buffer),
              Source_First  => Raw_Data.Source_First,
              Source_Last   => Raw_Data.Source_Last,
              Sloc_Range    => Sloc_Range (TDH, Raw_Data));
   end Convert;


   ------------------
   -- From_Generic --
   ------------------

   function From_Generic (Token : Lk_Token) return Common.Token_Reference is
      use Gpr_Parser_Support.Internal.Conversions;
      Id         : Any_Language_Id;
      Data       : Gpr_Parser_Support.Internal.Analysis.Internal_Token;
      Safety_Net : Gpr_Parser_Support.Internal.Analysis.Token_Safety_Net;
   begin
      Unwrap_Token (Token, Id, Data, Safety_Net);
      pragma Assert (Id = Generic_API.Self_Id);
      return (Data.TDH,
              Data.Index,
              (Safety_Net.Context,
               Safety_Net.Context_Version,
               Safety_Net.TDH_Version));
   end From_Generic;

   ----------------
   -- To_Generic --
   ----------------

   function To_Generic (Token : Common.Token_Reference) return Lk_Token is
      use Gpr_Parser_Support.Internal.Conversions;
   begin
      return Wrap_Token
        (Generic_API.Self_Id,
         (Token.TDH, Token.Index),
         (Token.Safety_Net.Context,
          Token.Safety_Net.Context_Version,
          Token.Safety_Net.TDH_Version));
   end To_Generic;


   --------------------------
   -- Wrap_Token_Reference --
   --------------------------

   function Wrap_Token_Reference
     (Context : Internal_Context;
      TDH     : Token_Data_Handler_Access;
      Index   : Token_Or_Trivia_Index) return Token_Reference is
   begin
      if Index = No_Token_Or_Trivia_Index then
         return No_Token;
      end if;

      declare
         SN : constant Token_Safety_Net :=
           (Context         => +Context,
            Context_Version => Context.Serial_Number,
            TDH_Version     => TDH.Version);
      begin
        return (TDH, Index, SN);
      end;
   end Wrap_Token_Reference;

   --------------------
   -- Get_Token_Unit --
   --------------------

   function Get_Token_Unit (Token : Token_Reference) return Internal_Unit is
      function "+" is new Ada.Unchecked_Conversion
        (System.Address, Internal_Unit);
   begin
      if Token = No_Token then
         raise Precondition_Failure with "null token argument";
      end if;
      Check_Safety_Net (Token);
      return +Token.TDH.Owner;
   end Get_Token_Unit;

   -----------------------
   -- Get_Token_Context --
   -----------------------

   function Get_Token_Context
     (Token : Token_Reference) return Internal_Context is
   begin
      return +Token.Safety_Net.Context;
   end Get_Token_Context;

   -------------------
   -- Get_Token_TDH --
   -------------------

   function Get_Token_TDH
     (Token : Token_Reference) return Token_Data_Handler_Access is
   begin
      return Token.TDH;
   end Get_Token_TDH;

   ---------------------
   -- Get_Token_Index --
   ---------------------

   function Get_Token_Index
     (Token : Token_Reference) return Token_Or_Trivia_Index is
   begin
      return Token.Index;
   end Get_Token_Index;

   ------------------------
   -- Extract_Token_Text --
   ------------------------

   procedure Extract_Token_Text
     (Token         : Token_Data_Type;
      Source_Buffer : out Text_Cst_Access;
      First         : out Positive;
      Last          : out Natural) is
   begin
      Source_Buffer := Token.Source_Buffer;
      First := Token.Source_First;
      Last := Token.Source_Last;
   end Extract_Token_Text;


begin
   --  Check that we actually have full Libiconv support: as nothing works
   --  without it, we explicitly check support here instead of letting
   --  user-unfriendly errors happen during lexing.

   if not GNATCOLL.Iconv.Has_Iconv then
      raise Program_Error with "Libiconv is not available";
   end if;


   Private_Converters.Wrap_Token_Reference := Wrap_Token_Reference'Access;
   Private_Converters.Get_Token_Context := Get_Token_Context'Access;
   Private_Converters.Get_Token_Unit := Get_Token_Unit'Access;
   Private_Converters.Get_Token_TDH := Get_Token_TDH'Access;
   Private_Converters.Get_Token_Index := Get_Token_Index'Access;
   Private_Converters.Extract_Token_Text := Extract_Token_Text'Access;
end Gpr_Parser.Common;
