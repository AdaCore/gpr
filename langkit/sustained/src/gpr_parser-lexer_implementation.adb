
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--




with GNATCOLL.VFS;

with Gpr_Parser_Support.File_Readers; use Gpr_Parser_Support.File_Readers;
with Gpr_Parser_Support.Slocs;        use Gpr_Parser_Support.Slocs;
with Gpr_Parser_Support.Text;         use Gpr_Parser_Support.Text;

with Gpr_Parser_Support.Symbols;
use Gpr_Parser_Support.Symbols;

with Gpr_Parser.Common;
with Gpr_Parser.Implementation; use Gpr_Parser.Implementation;
use Gpr_Parser.Implementation.Precomputed_Symbols;
with Gpr_Parser.Lexer_State_Machine;
use Gpr_Parser.Lexer_State_Machine;

with Gpr_Parser_Support.Symbols;

package body Gpr_Parser.Lexer_Implementation is

   use Token_Vectors, Trivia_Vectors, Integer_Vectors;

   procedure Extract_Tokens_From_Text_Buffer
     (Contents    : Decoded_File_Contents;
      With_Trivia : Boolean;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Helper for the Extract_Tokens procedure

   generic
      With_Trivia : Boolean;
   procedure Process_All_Tokens
     (Contents    : Decoded_File_Contents;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector);

   function Force_Symbol
     (TDH : Token_Data_Handler;
      T   : in out Stored_Token_Data) return Symbol_Type;
   --  If T has a symbol, return it. Otherwise, force its symbolization and
   --  return the symbol.

   ------------------------
   -- Process_All_Tokens --
   ------------------------

   procedure Process_All_Tokens
     (Contents    : Decoded_File_Contents;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is

      Token    : Lexed_Token;
      Token_Id : Token_Kind := Gpr_Termination;
      Symbol   : Thin_Symbol;

      Last_Token_Last : Natural := Contents.Buffer'First - 1;
      --  Index in TDH.Source_Buffer for the last character of the previous
      --  token. Used to process chunks of ignored text.


      Last_Token_Was_Trivia : Boolean := False;
      --  Whether the last item we added to TDH was a trivia

      procedure Append_Token (Data : Stored_Token_Data) with Inline;
      --  Append a token to TDH and update Last_Token_Was_Trivia accordingly

      procedure Append_Trivia (Data : Stored_Token_Data) with Inline;
      --  If trivia are disabled, do nothing. Otherwise, append a trivia to TDH
      --  and update Last_Token_Was_Trivia and the token/trivia mapping in TDH
      --  accordingly.

      function Source_First return Positive is (Token.Text_First);
      --  Index in TDH.Source_Buffer for the first character corresponding to
      --  the current token.

      function Source_Last return Natural is (Token.Text_Last);
      --  Likewise, for the last character

      ------------------
      -- Append_Token --
      ------------------

      procedure Append_Token (Data : Stored_Token_Data) is
      begin
         --  By default, the current token will have no trivia
         Append (TDH.Tokens_To_Trivias, Integer (No_Token_Index));

         TDH.Tokens.Append (Data);
         Last_Token_Was_Trivia := False;
      end Append_Token;

      -------------------
      -- Append_Trivia --
      -------------------

      procedure Append_Trivia (Data : Stored_Token_Data) is
      begin
         if not With_Trivia then
            return;
         end if;

         --  If the last item added to TDH was a trivia, extend the current
         --  trivia chain. Otherwise, update the Tokens_To_Trivias map to state
         --  that the trivia we are about to add is the first trivia that comes
         --  after the last token.

         if Last_Token_Was_Trivia then
            TDH.Trivias.Last_Element.all.Has_Next := True;
         else
            TDH.Tokens_To_Trivias.Last_Element.all :=
               TDH.Trivias.Last_Index + 1;
         end if;
         TDH.Trivias.Append ((Has_Next => False,
                              T        => Data));
         Last_Token_Was_Trivia := True;
      end Append_Trivia;

      State : Lexer_State;

   begin
      Initialize (State, Contents.Buffer, Contents.First, Contents.Last);
      Token := Last_Token (State);

      --  The first entry in the Tokens_To_Trivias map is for leading trivias
      TDH.Tokens_To_Trivias.Append (Integer (No_Token_Index));

      while Has_Next (State) loop
         Next_Token (State, Token);


         Token_Id := Token.Kind;
         Symbol := No_Thin_Symbol;

         case Token_Id is

            when Gpr_Abstract | Gpr_All | Gpr_Amp | Gpr_Arrow | Gpr_Assign | Gpr_At | Gpr_Case | Gpr_Char | Gpr_Colon | Gpr_Comma | Gpr_Dot | Gpr_End | Gpr_Extends | Gpr_For | Gpr_Identifier | Gpr_Is | Gpr_Limited | Gpr_Null | Gpr_Others | Gpr_Package | Gpr_Par_Close | Gpr_Par_Open | Gpr_Pipe | Gpr_Pragma | Gpr_Private | Gpr_Renames | Gpr_Semicolon | Gpr_Tick | Gpr_Type | Gpr_Use | Gpr_When | Gpr_With =>
               if TDH.Symbols /= No_Symbol_Table then
                  declare
                     Bounded_Text : Text_Type renames
                        Contents.Buffer (Token.Text_First .. Token.Text_Last);

                     Symbol_Res : constant Symbolization_Result :=
                           Gpr_Parser_Support.Symbols.Fold_Case (Bounded_Text);
                  begin
                     if Symbol_Res.Success then
                        Symbol := Find (TDH.Symbols, Symbol_Res.Symbol);
                     else
                        Append
                          (Diagnostics,
                           Make_Range
                             (Get_Sloc (TDH, Token.Text_First),
                              Get_Sloc (TDH, Token.Text_Last)),
                           Symbol_Res.Error_Message);
                     end if;
                  end;
               end if;

            when Gpr_Comment | Gpr_Lexing_Failure | Gpr_Whitespace =>
               Append_Trivia ((Kind         => From_Token_Kind (Token_Id),
                               Source_First => Source_First,
                               Source_Last  => Source_Last,
                               Symbol       => No_Thin_Symbol));

               if Token_Id = Gpr_Lexing_Failure then
                  Append
                    (Diagnostics,
                     Make_Range
                       (Get_Sloc (TDH, Token.Text_First),
                        Get_Sloc (TDH, Token.Text_Last)),
                     "Invalid token, ignored");
               end if;

               goto Dont_Append;

            when others =>
               null;

         end case;

         Append_Token
           ((Kind         => From_Token_Kind (Token_Id),
             Source_First => Source_First,
             Source_Last  => Source_Last,
             Symbol       => Symbol));


         Last_Token_Last := Source_Last;

         <<Dont_Append>>
      end loop;

   end Process_All_Tokens;

   procedure Process_All_Tokens_With_Trivia is new Process_All_Tokens (True);
   procedure Process_All_Tokens_No_Trivia is new Process_All_Tokens (False);

   -------------------------------------
   -- Extract_Tokens_From_Text_Buffer --
   -------------------------------------

   procedure Extract_Tokens_From_Text_Buffer
     (Contents    : Decoded_File_Contents;
      With_Trivia : Boolean;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector) is
   begin
      --  In the case we are reparsing an analysis unit, we want to get rid of
      --  the tokens from the old one.

      Reset
        (TDH, Contents.Buffer, Contents.First, Contents.Last);

      if With_Trivia then
         Process_All_Tokens_With_Trivia (Contents, TDH, Diagnostics);
      else
         Process_All_Tokens_No_Trivia (Contents, TDH, Diagnostics);
      end if;
   end Extract_Tokens_From_Text_Buffer;

   --------------------
   -- Extract_Tokens --
   --------------------

   procedure Extract_Tokens
     (Input       : Internal_Lexer_Input;
      With_Trivia : Boolean;
      File_Reader : access Implementation.Internal_File_Reader'Class;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      use type GNATCOLL.VFS.Filesystem_String;

      Contents : Decoded_File_Contents;
   begin
      --  It should not be possible to end up here with anything else than a
      --  file when there is a file reader, as it would mean that the file
      --  reader will be by-passed.
      pragma Assert (File_Reader = null or else Input.Kind = File);

      case Input.Kind is
         when File =>
            declare
               Filename : constant String := +Input.Filename.Full_Name.all;
               Charset  : constant String := To_String (Input.Charset);
            begin
               --  Use the file reader if there is one, otherwise just read the
               --  source file on the filesystem.
               if File_Reader = null then
                  Direct_Read
                    (Filename, Charset, Input.Read_BOM, Contents, Diagnostics);
               else
                  File_Reader.Read
                    (Filename, Charset, Input.Read_BOM, Contents, Diagnostics);
               end if;
            end;

            Extract_Tokens_From_Text_Buffer
              (Contents, With_Trivia, TDH, Diagnostics);
            TDH.Filename := Input.Filename;
            TDH.Charset := Input.Charset;

         when Bytes_Buffer =>
            declare
               Bytes : String (1 .. Input.Bytes_Count)
                  with Import, Address => Input.Bytes;
            begin
               Decode_Buffer
                 (Bytes, To_String (Input.Charset), Input.Read_BOM, Contents,
                  Diagnostics);
            end;
            Extract_Tokens_From_Text_Buffer
              (Contents, With_Trivia, TDH, Diagnostics);
            TDH.Filename := GNATCOLL.VFS.No_File;
            TDH.Charset := Input.Charset;

         when Text_Buffer =>
            Contents.Buffer := new Text_Type (1 .. Input.Text_Count);
            Contents.First := Contents.Buffer'First;
            Contents.Last := Contents.Buffer'Last;

            declare
               Text_View : Text_Type (1 .. Input.Text_Count)
                  with Import, Address => Input.Text;
            begin
               Contents.Buffer.all := Text_View;
               Extract_Tokens_From_Text_Buffer
                 (Contents, With_Trivia, TDH, Diagnostics);
               TDH.Filename := GNATCOLL.VFS.No_File;
               TDH.Charset := Null_Unbounded_String;
            end;
      end case;
   end Extract_Tokens;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Token : Token_Or_Trivia_Index;
      TDH   : Token_Data_Handler) return Symbol_Type
   is
      subtype Token_Data_Reference is Token_Vectors.Element_Access;

      Token_Data : constant Token_Data_Reference :=
        (if Token.Trivia = No_Token_Index
         then Token_Data_Reference
           (TDH.Tokens.Get_Access (Natural (Token.Token)))
         else Token_Data_Reference'
           (TDH.Trivias.Get_Access (Natural (Token.Trivia) - 1).T'Access));
   begin
      return Force_Symbol (TDH, Token_Data.all);
   end Get_Symbol;

   ------------------
   -- Force_Symbol --
   ------------------

   function Force_Symbol
     (TDH : Token_Data_Handler;
      T   : in out Stored_Token_Data) return Symbol_Type is
   begin
      if T.Symbol = No_Thin_Symbol then
         declare
            Text   : Text_Type renames
               TDH.Source_Buffer (T.Source_First ..  T.Source_Last);
            Symbol : constant Symbolization_Result :=
                  Gpr_Parser_Support.Symbols.Fold_Case (Text)
            ;
         begin
            --  This function is run as part of semantic analysis: there is
            --  currently no way to report errors from here, so just discard
            --  canonicalization issues here.
            if Symbol.Success then
               T.Symbol := Find (TDH.Symbols, Symbol.Symbol);
            end if;
         end;
      end if;
      return Get_Symbol (TDH.Symbols, T.Symbol);
   end Force_Symbol;

end Gpr_Parser.Lexer_Implementation;
