--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with System;

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

   --------------------------
   -- Parse_Context_Clause --
   --------------------------

   procedure Parse_Context_Clauses
     (Filename       : String;
      Diagnostics    : in out Diagnostics_Vectors.Vector;
      Context        : Gpr_Parser.Analysis.Analysis_Context'Class;
      Charset        : String           := "ISO-8859-1";
      With_Clause_CB : access procedure
        (Unit_Name  : String;
         Source_Loc : Gpr_Parser_Support.Slocs.Source_Location;
         Is_Limited : Boolean) := null;
      Unit_Name_CB   : access procedure
        (Unit_Name     : String;
         Separate_From : String := "";
         Source_Loc    : Gpr_Parser_Support.Slocs.Source_Location;
         Lib_Item_Type : Gpr_Parser.Basic_Ada_Parser.Library_Item_Type;
         Generic_Unit  : Boolean) := null;
      On_No_Body_CB  : access procedure := null)
   is
      Contents     : Decoded_File_Contents;
      Read_BOM     : Boolean          := Charset'Length = 0;
      State        : Lexer_State;
      Token_DH     : Token_Data_Handler;
      Internal_Ctx : Internal_Context := Unwrap_Context (Context);

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

      procedure Log_Error (T       : Lexed_Token; Message : String) with
        Pre => Initialized (Token_DH);
      --  Append message to diagnostics with token source location

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

      ---------------
      -- Log_Error --
      ---------------

      procedure Log_Error (T : Lexed_Token; Message : String) is

         function Get_Sloc_Range (T : Lexed_Token)
           return Source_Location_Range with
           Pre => Initialized (Token_DH);
         --  Obtain source location range from a lexer token

         function Get_Sloc_Range
           (T : Lexed_Token)
            return Source_Location_Range is

            Sloc_Start : Source_Location := Get_Sloc (Token_DH, T.Text_First);
            Sloc_End   : Source_Location := Get_Sloc (Token_DH, T.Text_Last);

         begin
            --  We can not obtain source location range from Get_Token, because
            --  the token data handler is empty. Only lines and columns have
            --  been computed with the Reset procedure. Thus, only Get_Sloc
            --  is available.

            return
              (Start_Line   => Sloc_Start.Line,
               Start_Column => Sloc_Start.Column, End_Line => Sloc_End.Line,
               End_Column   => Sloc_End.Column);
         end Get_Sloc_Range;

         Err_Sloc : Source_Location_Range := Get_Sloc_Range (T);

      begin
         Append (Diagnostics, Err_Sloc, To_Text (Message));
      end Log_Error;

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
            declare
               Err_Token_Save : Lexed_Token := T;

            begin
               if not Skip_Until ((1 => Gpr_Par_Open), T) then
                  Log_Error
                    (Err_Token_Save,
                     "Error during parsing: Failed to find the opening " &
                     "parenthese after the 'separate' token.");
                  return;
               end if;
            end;

            declare
               Err_Token_Save : Lexed_Token := T;

            begin
               if not Skip_Until ((1 => Gpr_Identifier), T) then
                  Log_Error
                    (Err_Token_Save,
                     "Error during parsing: Failed to find the identifier " &
                     "after the 'separate' opening parenthese.");
                  return;
               end if;
            end;

            Separate_From_First := T.Text_First;
            Separate_From_Last  := Parse_Static_Name (T);

            declare
               Err_Token_Save : Lexed_Token := T;

            begin
               if not Skip_Until
                   ((Gpr_Identifier, Gpr_Package), T,
                    (Function_Token'Access, Procedure_Token'Access,
                     Task_Token'Access, Protected_Token'Access))
               then
                  Log_Error
                    (Err_Token_Save,
                     "Error during parsing: Failed to find mandatory " &
                     "token for a subunit (package / function / procedure).");
                  return;
               end if;
            end;
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

            declare
               Err_Token_Save : Lexed_Token := T;

            begin
               if not Skip_Until
                   ((1 => Gpr_Identifier), T, (1 => Body_Token'Access),
                    Expected => False)
               then
                  Log_Error
                    (Err_Token_Save,
                     "Error during parsing: Failed to find identifier for " &
                     "the mandatory library item / subunit part.");
                  return;
               end if;
            end;

            declare
               Unit_First : Positive := T.Text_First;
               Unit_Last  : Positive := Parse_Static_Name (T);

            begin
               declare
                  U_Name : String :=
                           Gpr_Parser_Support.Text.Encode
                             (Contents.Buffer (Unit_First .. Unit_Last), Charset);
                  Sloc   : Gpr_Parser_Support.Slocs.Source_Location :=
                           Get_Sloc (Token_DH, Unit_First);

               begin
                  if Separate_From_First /= 0 then
                     declare

                        Sep_Name : String :=
                          Gpr_Parser_Support.Text.Encode
                            (Contents.Buffer
                               (Separate_From_First .. Separate_From_Last),
                             Charset);

                     begin
                        Unit_Name_CB
                          (Unit_Name     => U_Name,
                           Separate_From => Sep_Name,
                           Source_Loc    => Sloc,
                           Lib_Item_Type => Lib_Item_Type,
                           Generic_Unit  => Generic_Unit);
                     end;

                  else
                     Unit_Name_CB
                       (Unit_Name     => U_Name,
                        Source_Loc    => Sloc,
                        Lib_Item_Type => Lib_Item_Type,
                        Generic_Unit  => Generic_Unit);
                  end if;
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
                    To_Lower (Contents.Buffer (T.Text_First .. T.Text_Last));

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
               declare
                  Err_Token_Save : Lexed_Token := T;

               begin
                  if not Skip_Until
                      ((Gpr_Identifier, Gpr_With, Gpr_Package), T,
                       (Function_Token'Access, Procedure_Token'Access,
                        Access_Token'Access))
                  then
                     Log_Error
                       (Err_Token_Save,
                        "Error during parsing: Failed to skip generic.");
                     exit;
                  end if;
               end;

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

                     declare
                        Err_Token_Save : Lexed_Token := T;

                     begin
                        if not Skip_Until ((1 => Gpr_Semicolon), T) then
                           Log_Error
                             (Err_Token_Save,
                              "Error during parsing: Failed to skip generic. " &
                              "Semicolon not eventually found after an 'access' token");
                           exit;
                        end if;
                     end;
                  end;

               elsif T.Kind = Gpr_With then
                  declare
                     Err_Token_Save : Lexed_Token := T;

                  begin
                     if not Skip_Until ((1 => Gpr_Semicolon), T) then
                        Log_Error
                          (Err_Token_Save,
                           "Error during parsing: Failed to skip generic. Semicolon " &
                           "not eventually found after a 'with' token");
                        exit;
                     end if;
                  end;
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
         Sloc              : Source_Location;

      begin
         if T.Kind = Gpr_Limited then
            Lim := True;

            declare
               Err_Token_Save : Lexed_Token := T;

            begin
               if not Skip_Until ((1 => Gpr_With), T) then
                  Log_Error
                    (Err_Token_Save,
                     "Error during parsing: Limited token should " &
                     "be followed by a with token.");
                  return;
               end if;
            end;
         end if;

         --  "With" token location is returned as source location, even for
         --  several units in the same with clause.

         if With_Clause_CB /= null then
            Sloc := Get_Sloc (Token_DH, T.Text_First);
         end if;

         loop
            declare
               Err_Token_Save : Lexed_Token := T;

            begin
               if not Skip_Until ((1 => Gpr_Identifier), T) then
                  Log_Error
                    (Err_Token_Save,
                     "Error during parsing: With token should be " &
                     "followed eventually by an identifier. End of " &
                     "file obtained instead.");

                  return;
               end if;
            end;
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
                  With_Clause_CB (Name, Sloc, Lim);
               end;
            end if;

            if T.Kind /= Gpr_Semicolon and then T.Kind /= Gpr_Comma then
               declare
                  Err_Token_Save : Lexed_Token := T;

               begin
                  if not Skip_Until ((Gpr_Semicolon, Gpr_Comma), T) then
                     Log_Error
                       (Err_Token_Save,
                        "Error during parsing: Withed unit shall be " &
                        "eventually followed by a comma when several " &
                        "units are specified, or a semicolon otherwise.");

                     return;
                  end if;
               end;
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

      if Internal_Ctx.File_Reader /= null then
         Read
           (Internal_Ctx.File_Reader.all, Filename, Charset, Read_BOM,
            Contents, Diagnostics);
      else
         Direct_Read (Filename, Charset, Read_BOM, Contents, Diagnostics);
      end if;

      --  Initialize the lexer

      Initialize (State, Contents.Buffer, Contents.First, Contents.Last);

      --  Initialize the token data handler, with which source location (slocs)
      --  are obtained.

      Initialize
        (Token_DH, Internal_Ctx.Symbols, System.Null_Address,
         Internal_Ctx.Tab_Stop);

      --  Associate source buffer to token data handler, and compute new lines
      --  indexes.

      Reset (Token_DH, Contents.Buffer, Contents.First, Contents.Last);

      while Has_Next (State) loop

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
               Append
                 (Diagnostics, No_Source_Location_Range,
                  To_Text
                    ("Error during parsing: Failed to find " &
                     "a first valid token for with clause, " &
                     "library item or subunit in the file " & Filename));

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

               declare
                  Err_Token_Save : Lexed_Token := T;

               begin
                  if not Skip_Until ((1 => Gpr_Identifier), T) then
                     Log_Error
                       (Err_Token_Save,
                        "pragma should be followed by an identifier");

                     exit;
                  else

                     declare
                        Text : Text_Type :=
                               To_Lower
                                 (Contents.Buffer
                                   (T.Text_First .. T.Text_Last));

                     begin
                        if Text = No_Body_Token then
                           if On_No_Body_CB /= null then
                              On_No_Body_CB.all;
                           end if;

                           exit;
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;

      Free (Token_DH);
   end Parse_Context_Clauses;

end Gpr_Parser.Basic_Ada_Parser;
