--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with System;

with Ada.Characters.Conversions;

with Gpr_Parser_Support.File_Readers; use Gpr_Parser_Support.File_Readers;
with Gpr_Parser_Support.Diagnostics;  use Gpr_Parser_Support.Diagnostics;
with Gpr_Parser.Lexer_State_Machine;  use Gpr_Parser.Lexer_State_Machine;
with Gpr_Parser.Common;               use Gpr_Parser.Common;
with Gpr_Parser_Support.Text;         use Gpr_Parser_Support.Text;
with Gpr_Parser_Support.Slocs;        use Gpr_Parser_Support.Slocs;
with Gpr_Parser_Support.Token_Data_Handlers;
use Gpr_Parser_Support.Token_Data_Handlers;
with Gpr_Parser.Public_Converters; use Gpr_Parser.Public_Converters;
with Gpr_Parser.Implementation;    use Gpr_Parser.Implementation;
with Gpr_Parser.Analysis;

package body Gpr_Parser.Basic_Ada_Parser is

   package GPS renames Gpr_Parser_Support;

   --------------------------
   -- Parse_Context_Clause --
   --------------------------

   procedure Parse_Context_Clauses
     (Filename       : String;
      Context        : Gpr_Parser.Analysis.Analysis_Context'Class;
      Charset        : String := "UTF-8";
      Log_Error      : access procedure (Message : String);
      With_Clause_CB : access procedure (Unit_Name  : String;
                                         Is_Limited : Boolean) := null;
      Unit_Name_CB   : access procedure (Unit_Name     : String;
                                         Separate_From : String;
                                         Lib_Item_Type : Library_Item_Type;
                                         Generic_Unit  : Boolean) := null;
      No_Body_CB               : access procedure := null)
   is
      Contents     : Decoded_File_Contents;
      Read_BOM     : Boolean          := Charset'Length = 0;
      State        : Lexer_State;
      Internal_Ctx : Internal_Context := Unwrap_Context (Context);
      Has_Error    : Boolean := False;

      subtype Specific_Identifier is Text_Type;

      Procedure_Token : aliased Specific_Identifier := "procedure";
      Function_Token  : aliased Specific_Identifier := "function";
      Generic_Token   : aliased Specific_Identifier := "generic";
      Separate_Token  : aliased Specific_Identifier := "separate";
      No_Body_Token   : aliased Specific_Identifier := "no_body";
      Body_Token      : aliased Specific_Identifier := "body";
      Access_Token    : aliased Specific_Identifier := "access";
      Protected_Token : aliased Specific_Identifier := "protected";
      Task_Token      : aliased Specific_Identifier := "task";

      type Specific_Identifier_Access is access all Specific_Identifier;

      type Specific_Identifier_Access_Array is
        array (Positive range <>) of Specific_Identifier_Access;

      type Token_Kind_Array is array (Positive range <>) of Token_Kind;

      procedure Log_Error_Internal (Message : String);

      function Skip_Until
        (Kinds       :     Token_Kind_Array;
         T           : out Lexed_Token;
         Identifiers :     Specific_Identifier_Access_Array := (2 .. 1 => <>);
         Expected    :     Boolean                          := True)
         return Boolean;
      --  Skip tokens until one of the specified token is encountered. Because
      --  identifiers token regroup several specific tokens, like separate,
      --  generic, etc, we may want to skip until specific identifiers. Last
      --  token read is stored in T. Identifiers is the list of specific
      --  identifier. It only has effect if one of the specified tokens kind in
      --  Kinds is Gpr_Identifier. If Expected is True, then if the last token
      --  read is an identifier, then it must match with one of the specified
      --  identifiers. Otherwise, the last identifier token read must NOT match
      --  with all the specified identifiers. If all tokens have been read
      --  without finding one of the expected tokens, then False is returned.

      function Parse_Library_Item_And_Subunit_Pre_Cond
         return Boolean;
      --  Pre condition function for Parse_Library_Item_And_Subunit. Ensure
      --  that the current token is valid as a first library item or subunit
      --  token.

      procedure Parse_Library_Item_And_Subunit with
        Pre => Parse_Library_Item_And_Subunit_Pre_Cond;
      --  Parse the library item unit name, or the subunit and the parent unit
      --  name. Call Unit_Name_CB if it is not null.

      function Parse_Static_Name (T : in out Lexed_Token) return Natural with
        Pre => Last_Token (State).Kind = Gpr_Identifier;
      --  Return the last token text of the static name which begins at the
      --  current identifier token.

      procedure Parse_With_Clause with
        Pre =>
         Last_Token (State).Kind = Gpr_With
         or else Last_Token (State).Kind = Gpr_Limited;
      --  Parse "with" clause, and calls With_Clause_CB if the callback is
      --  not null.

      ------------------------
      -- Log_Error_Internal --
      ------------------------

      procedure Log_Error_Internal (Message : String) is
      begin
         Has_Error := True;

         if Log_Error /= null then
            Log_Error (Message);
         end if;
      end Log_Error_Internal;

      ------------------------------------
      -- Parse_Library_Item_And_Subunit --
      ------------------------------------

      procedure Parse_Library_Item_And_Subunit is

         T                   : Lexed_Token := Last_Token (State);
         Separate_From_First : Natural     := 0;
         Separate_From_Last  : Natural     := 0;
         Generic_Unit        : Boolean     := False;
         Lib_Item_Type       : Library_Item_Type;

         procedure Skip_Generic with
           Pre =>
            T.Kind = Gpr_Identifier
            and then To_Lower (Contents.Buffer (T.Text_First .. T.Text_Last)) =
              Generic_Token;
         --  Skip tokens related to the generic part of a unit

         procedure Parse_Separate with
           Pre =>
            T.Kind = Gpr_Identifier
            and then To_Lower (Contents.Buffer (T.Text_First .. T.Text_Last)) =
              Separate_Token;
         --  Set Separate_From_First and Separate_From_Last with indexes of the
         --  separate argument.

         function Parse_Unit_Pre_Cond
            return Boolean;
         --  Pre-condition function for Parse_Unit. Ensure that the current
         --  token is either "package", "function" or "procedure".

         procedure Parse_Unit with
           Pre => Parse_Unit_Pre_Cond;
         --  Parse the mandatory part of the library item which contains the
         --  compilation unit name. Calls the library item callback if it is
         --  not null.

         procedure Parse_Separate is
         begin
            if not Skip_Until ((1 => Gpr_Par_Open), T) then
               Log_Error_Internal
                 ("no opening parenthesis after the 'separate' keyword");
               return;
            end if;

            if not Skip_Until ((1 => Gpr_Identifier), T) then
               Log_Error_Internal
                 ("no identifier after the 'separate' keyword");
               return;
            end if;

            Separate_From_First := T.Text_First;
            Separate_From_Last  := Parse_Static_Name (T);

            if not Skip_Until
              ((Gpr_Identifier, Gpr_Package), T,
               (Function_Token'Access, Procedure_Token'Access,
                Task_Token'Access, Protected_Token'Access))
            then
               Log_Error_Internal
                 ("missing the subunit kind (package / function / procedure)");
               return;
            end if;
         end Parse_Separate;

         ----------------
         -- Parse_Unit --
         ----------------

         procedure Parse_Unit is
         begin
            --  At this step, current token is either "function", "procedure",
            --  "package", "protected" or "task". Skip token until the next
            --  identifier is different from body, as it contains the unit
            --  name.

            if not Skip_Until
              ((1 => Gpr_Identifier), T, (1 => Body_Token'Access),
               Expected => False)
            then
               Log_Error_Internal
                 ("missing the unit or subunit identifier");

               return;
            end if;

            declare
               Unit_First : Positive := T.Text_First;
               Unit_Last  : Positive := Parse_Static_Name (T);

            begin
               declare
                  use Gpr_Parser_Support.Text;

                  U_Name : String :=
                             Gpr_Parser_Support.Text.Encode
                               (Contents.Buffer (Unit_First .. Unit_Last),
                                Charset);
                  Sep_Name : constant String :=
                               (if Separate_From_First /= 0
                                then Encode (Contents.Buffer
                                  (Separate_From_First .. Separate_From_Last),
                                  Charset)
                                else "");

               begin
                  Unit_Name_CB
                    (Unit_Name     => U_Name,
                     Separate_From => Sep_Name,
                     Lib_Item_Type => Lib_Item_Type,
                     Generic_Unit  => Generic_Unit);
               end;
            end;
         end Parse_Unit;

         -------------------------
         -- Parse_Unit_Pre_Cond --
         -------------------------

         function Parse_Unit_Pre_Cond return Boolean is
            T           : Lexed_Token := Last_Token (State);
            Valid_Token : Boolean     := False;

         begin
            if T.Kind = Gpr_Package then
               Valid_Token := True;

            elsif T.Kind = Gpr_Identifier then
               declare
                  Token_Text : Text_Type :=
                                 To_Lower
                                   (Contents.Buffer
                                      (T.Text_First .. T.Text_Last));

               begin
                  if Token_Text = Procedure_Token
                    or else Token_Text = Function_Token
                  then
                     Valid_Token := True;
                  end if;
               end;
            end if;

            return Valid_Token;
         end Parse_Unit_Pre_Cond;

         ------------------
         -- Skip_Generic --
         ------------------

         procedure Skip_Generic is
         begin
            --  With and access content should be skipped, but as it may
            --  contain the "function", "procedure" or "package" keyword,
            --  we can not skip these keywords.

            loop
               if not Skip_Until
                 ((Gpr_Identifier, Gpr_With, Gpr_Package), T,
                  (Function_Token'Access, Procedure_Token'Access,
                   Access_Token'Access))
               then
                  Log_Error_Internal
                    ("failed to skip the generic formal parameters part");

                  exit;
               end if;

               if T.Kind = Gpr_Package then
                  --  "Package" not preceded by a with token is the start of
                  --  the mandatory library item part.

                  exit;

               elsif T.Kind = Gpr_Identifier then
                  declare
                     T_Content : Text_Type :=
                       To_Lower
                         (Contents.Buffer (T.Text_First .. T.Text_Last));

                  begin
                     exit when T_Content = Function_Token
                       or else T_Content = Procedure_Token;
                     --  If we reach this part of the code, then the current
                     --  token shall be an access token.

                     if not Skip_Until ((1 => Gpr_Semicolon), T) then
                        Log_Error_Internal
                          ("in formal generic parameters, missing ':' after " &
                             "variable declaration");

                        exit;
                     end if;
                  end;

               elsif T.Kind = Gpr_With then
                  if not Skip_Until ((1 => Gpr_Semicolon), T) then
                     Log_Error_Internal
                       ("missing ';' after a formal generic parameter " &
                          "declaration");
                     exit;
                  end if;
               end if;
            end loop;
         end Skip_Generic;

      begin
         --  Optional generic part

         if T.Kind = Gpr_Identifier
           and then To_Lower (Contents.Buffer (T.Text_First .. T.Text_Last)) =
             Generic_Token
         then
            Skip_Generic;
            Generic_Unit := True;
            --  Optional separate part

         elsif T.Kind = Gpr_Identifier
           and then To_Lower (Contents.Buffer (T.Text_First .. T.Text_Last)) =
             Separate_Token
         then
            Parse_Separate;
         end if;

         --  Mandatory library item part begins with: - function / procedure -
         --  package body - [private] package In subunit case, so if separate
         --  is present, the tokens we can expect are: - procedure / function
         --  - package body - task body - protected body.

         if T.Kind = Gpr_Package then
            Lib_Item_Type := Is_Package;

         else
            Lib_Item_Type := Is_Subprogram;
         end if;

         Parse_Unit;
      end Parse_Library_Item_And_Subunit;

      ---------------------------------------------
      -- Parse_Library_Item_And_Subunit_Pre_Cond --
      ---------------------------------------------

      function Parse_Library_Item_And_Subunit_Pre_Cond
         return Boolean
      is
         T           : Lexed_Token := Last_Token (State);
         Valid_Token : Boolean     := False;

      begin
         if T.Kind = Gpr_Package then
            Valid_Token := True;

         elsif T.Kind = Gpr_Identifier then
            declare
               Token_Text : Text_Type :=
                            To_Lower (Contents.Buffer
                              (T.Text_First .. T.Text_Last));

            begin
               if Token_Text = Procedure_Token
                 or else Token_Text = Function_Token
                 or else Token_Text = Separate_Token
                 or else Token_Text = Generic_Token
               then
                  Valid_Token := True;
               end if;
            end;
         end if;

         return Valid_Token;
      end Parse_Library_Item_And_Subunit_Pre_Cond;

      -----------------------
      -- Parse_Static_Name --
      -----------------------

      function Parse_Static_Name (T : in out Lexed_Token) return Natural is
         Static_Name_Expected_Token : Token_Kind := Gpr_Dot;
         Last                       : Natural    := T.Text_Last;

      begin
         while Has_Next (State) loop
            Next_Token (State, T);

            if T.Kind /= Static_Name_Expected_Token then
               return Last;

            else
               Last := T.Text_Last;

               if Static_Name_Expected_Token = Gpr_Dot then
                  Static_Name_Expected_Token := Gpr_Identifier;

               else
                  Static_Name_Expected_Token := Gpr_Dot;
               end if;
            end if;
         end loop;

         return Last;
      end Parse_Static_Name;

      -----------------------
      -- Parse_With_Clause --
      -----------------------

      procedure Parse_With_Clause
      is
         T                 : Lexed_Token := Last_Token (State);
         Withed_Unit_First : Positive;
         Withed_Unit_Last  : Positive;
         Lim               : Boolean     := False;

      begin
         if T.Kind = Gpr_Limited then
            Lim := True;

            if not Skip_Until ((1 => Gpr_With), T) then
               Log_Error_Internal
                 ("expecting a with token after 'limited'");

               return;
            end if;
         end if;

         --  "With" token location is returned as source location, even for
         --  several units in the same with clause.

         loop
            if not Skip_Until ((1 => Gpr_Identifier), T) then
               Log_Error_Internal
                 ("with clause is missing a unit identifier");

               return;
            end if;
            --  Current token shall be an identifier

            Withed_Unit_First := T.Text_First;
            Withed_Unit_Last  := Parse_Static_Name (T);

            if With_Clause_CB /= null then
               declare
                  Name : String :=
                    Gpr_Parser_Support.Text.Encode
                      (Contents.Buffer (Withed_Unit_First .. Withed_Unit_Last),
                       Charset);

               begin
                  With_Clause_CB (Name, Lim);
               end;
            end if;

            if T.Kind /= Gpr_Semicolon and then T.Kind /= Gpr_Comma then
               if not Skip_Until ((Gpr_Semicolon, Gpr_Comma), T) then
                  Log_Error_Internal
                    ("missing ';' or ',' in a with clause");

                  return;
               end if;
            end if;

            if T.Kind = Gpr_Semicolon then

               --  No more unit within this "with" clause

               exit;
            end if;
         end loop;
      end Parse_With_Clause;

      ----------------
      -- Skip_Until --
      ----------------

      function Skip_Until
        (Kinds       :     Token_Kind_Array;
         T           : out Lexed_Token;
         Identifiers :     Specific_Identifier_Access_Array := (2 .. 1 => <>);
         Expected    :     Boolean                          := True)
         return Boolean is
      begin
         while Has_Next (State) loop
            Next_Token (State, T);

            for I in Kinds'Range loop
               if Kinds (I) = T.Kind then
                  --  In case of identifiers, two scenarios: - If a list of
                  --  identifiers has been specified,
                  --    then current identifier is checked.
                  --  - If the list of expected identifiers is empty,
                  --    then only check for the identifier token kind.

                  if T.Kind = Gpr_Identifier and then Identifiers'Length > 0
                  then
                     declare
                        Text : Text_Type :=
                          To_Lower
                            (Contents.Buffer (T.Text_First .. T.Text_Last));

                     begin
                        --  First case: we expect to find a specific identifier

                        if Expected then
                           for J in Identifiers'Range loop
                              if Text = Identifiers (J).all then
                                 return True;
                              end if;
                           end loop;

                        else
                           --  Second case: We expect to find all identifier
                           --  except the identifiers specified.

                           declare
                              Found : Boolean := False;

                           begin
                              --  Check all the unwanted identifiers

                              for J in Identifiers'Range loop
                                 if Text = Identifiers (J).all then
                                    Found := True;
                                 end if;
                              end loop;
                              --  Current token does not match with one of the
                              --  unwanted identifiers.

                              if not Found then
                                 return True;
                              end if;
                           end;
                        end if;
                     end;

                  else
                     --  Current token is one of the expected tokens

                     return True;
                  end if;
               end if;
            end loop;
         end loop;

         --  Token was not found and Lexer is empty

         return False;
      end Skip_Until;

   begin
      --  Fill the Contents buffer with specified file content. If a file
      --  reader is provided, it is used instead to read source file.

      declare
         use Ada.Characters.Conversions;

         Diagnostics : GPS.Diagnostics.Diagnostics_Vectors.Vector;
      begin

         if Internal_Ctx.File_Reader /= null then
            Read
              (Internal_Ctx.File_Reader.all, Filename, Charset, Read_BOM,
               Contents, Diagnostics);
         else
            Direct_Read (Filename, Charset, Read_BOM, Contents, Diagnostics);
         end if;

         if not Diagnostics.Is_Empty then
            for D of Diagnostics loop
               Log_Error_Internal (To_String (To_Text (D.Message)));
            end loop;

            return;
         end if;
      end;

      --  Initialize the lexer

      Initialize (State, Contents.Buffer, Contents.First, Contents.Last);

      while Has_Next (State) and then not Has_Error loop

         --  Parse 0, 1 or more withed clauses, and one library item or
         --  subunit. The parser may detect compilation units for invalid
         --  files, as tokens are skipped until a wanted token is found.

         declare
            T : Lexed_Token;

         begin
            if not Skip_Until
                ((Gpr_With, Gpr_Identifier, Gpr_Package, Gpr_Limited,
                  Gpr_Pragma),
                 T,
                 (Procedure_Token'Access, Function_Token'Access,
                  Separate_Token'Access, Generic_Token'Access))
            then
               Log_Error_Internal ("Unexpected preamble");

               exit;
            end if;

            if T.Kind = Gpr_With or else T.Kind = Gpr_Limited then
               Parse_With_Clause;

            elsif T.Kind = Gpr_Identifier or else T.Kind = Gpr_Package then
               Parse_Library_Item_And_Subunit;

               --  We do not need to parse after the library item and subunit
               --  parsing, because "with" clauses precede them. Moreover, as
               --  we do not support multi-unit here, only one compilation unit
               --  can exist per file.

               exit;

            elsif T.Kind = Gpr_Pragma then

               --  Two cases:
               --  - pragma No_Body: The source can only contain this pragma
               --    and possibly comments. The parsing is ended.
               --  - pragma XXX: No effect on parsing. Note that the parsing is
               --  correct only if the file is valid. If the file has a "with
               --  XXX; pragma No_Body", then the file will be categorized has
               --  No_Body, despite it is an invalid file.

               if not Skip_Until ((1 => Gpr_Identifier), T) then
                  Log_Error_Internal
                    ("pragma should be followed by an identifier");

                  exit;

               else
                  declare
                     Text : Text_Type :=
                              To_Lower
                                (Contents.Buffer
                                   (T.Text_First .. T.Text_Last));

                  begin
                     if Text = No_Body_Token then
                        if No_Body_CB /= null then
                           No_Body_CB.all;
                        end if;

                        exit;
                     end if;
                  end;
               end if;
            end if;
         end;
      end loop;

      Gpr_Parser_Support.Text.Free (Contents.Buffer);
   end Parse_Context_Clauses;

end Gpr_Parser.Basic_Ada_Parser;
