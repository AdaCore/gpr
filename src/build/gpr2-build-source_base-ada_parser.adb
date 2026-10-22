--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Conversions;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GNAT.Byte_Order_Mark;

with GNATCOLL.Iconv;
with GNATCOLL.Mmap;

with GPR2.Build.Unit_Info;
with GPR2.Log;
with GPR2.Containers;

package body GPR2.Build.Source_Base.Ada_Parser is

   package Lexer is

      --  Scan syntax directly from Latin-1 or UTF-8 bytes

      type Token_Kind is
        (Tok_Termination,
         Tok_Failure,
         Tok_Identifier,
         Tok_With,
         Tok_Limited,
         Tok_Package,
         Tok_Pragma,
         Tok_Keyword,
         --  Other reserved words, distinct from identifiers matched by text
         Tok_Dot,
         Tok_Comma,
         Tok_Semicolon,
         Tok_Par_Open,
         Tok_Delimiter,
         Tok_Number,
         Tok_String,
         Tok_Whitespace,
         Tok_Comment);

      type Token is record
         Kind       : Token_Kind;
         Text_First : Positive;
         Text_Last  : Natural;
      end record;

      type State is limited private;

      function Encoding_Length (Buffer : String; First : Positive)
                               return Natural;
      --  Length of a bracket or Hex escape, or zero if absent or malformed

      procedure Initialize (Self : out State; Buffer : String);
      --  Buffer must also be passed to Next_Token

      function Last_Token (Self : State) return Token;

      function Has_Next (Self : State) return Boolean;

      procedure Next_Token
        (Self : in out State; Buffer : String; Tok : out Token)
        with Pre => Has_Next (Self);

   private

      type State is limited record
         Last       : Natural;
         Has_Next   : Boolean;
         Last_Token : Token;
      end record;
   end Lexer;

   package body Lexer is

      subtype Blank is Character
        with Static_Predicate => Blank in ' ' | ASCII.HT .. ASCII.CR;

      subtype Digit is Character range '0' .. '9';

      subtype Hex_Digit is Character
        with Static_Predicate => Hex_Digit in Digit | 'a' .. 'f' | 'A' .. 'F';

      subtype Letter is Character
        with Static_Predicate =>
          Letter in 'a' .. 'z' | 'A' .. 'Z' | '_'
                    | Character'Val (16#80#) .. Character'Last;

      subtype Alphanumeric is Character
        with Static_Predicate =>
          Alphanumeric in Letter | Digit;

      function Lower (C : Character) return Character
      is (if C in 'A' .. 'Z'
          then Character'Val (Character'Pos (C) + 32)
          else C);

      function Keyword (Word : String) return Token_Kind;
      --  Return Word's token kind, ignoring case

      ---------------------
      -- Encoding_Length --
      ---------------------

      function Encoding_Length (Buffer : String; First : Positive)
                               return Natural
      is
         Idx : Natural;
      begin
         if Buffer (First) = ASCII.ESC then
            if Buffer'Last - First >= 4
              and then
                (for all J in First + 1 .. First + 4 =>
                   Buffer (J) in Hex_Digit)
            then
               return 5;
            else
               return 0;
            end if;
         end if;

         if Buffer'Last - First < 5
           or else Buffer (First) /= '['
           or else Buffer (First + 1) /= '"'
         then
            return 0;
         end if;

         Idx := First + 2;
         while Idx <= Buffer'Last and then Buffer (Idx) in Hex_Digit loop
            Idx := Idx + 1;
         end loop;

         if Idx - First - 2 in 2 | 4 | 6 | 8
           and then Idx < Buffer'Last
           and then Buffer (Idx) = '"'
           and then Buffer (Idx + 1) = ']'
         then
            return Idx - First + 2;
         end if;

         return 0;
      end Encoding_Length;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Self : State) return Boolean is (Self.Has_Next);

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (Self : out State; Buffer : String) is
      begin
         Self.Last       := Buffer'Last;
         Self.Has_Next   := True;
         Self.Last_Token := (Tok_Termination, Buffer'First, Buffer'First - 1);
      end Initialize;

      -------------
      -- Keyword --
      -------------

      function Keyword (Word : String) return Token_Kind is
         Folded : String (1 .. Word'Length);
      begin
         --  "abstract" is the longest reserved word here

         if Word'Length not in 2 .. 8 then
            return Tok_Identifier;
         end if;

         for J in Word'Range loop
            Folded (J - Word'First + 1) := Lower (Word (J));
         end loop;

         case Folded'Length is
            when 2 =>
               if Folded in "at" | "is" then
                  return Tok_Keyword;
               end if;

            when 3 =>
               if Folded in "all" | "end" | "for" | "use" then
                  return Tok_Keyword;
               end if;

            when 4 =>
               if Folded = "with" then
                  return Tok_With;
               elsif Folded in "case" | "null" | "type" | "when" then
                  return Tok_Keyword;
               end if;

            when 6 =>
               if Folded = "pragma" then
                  return Tok_Pragma;
               elsif Folded = "others" then
                  return Tok_Keyword;
               end if;

            when 7 =>
               if Folded = "limited" then
                  return Tok_Limited;
               elsif Folded = "package" then
                  return Tok_Package;
               elsif Folded in "private" | "renames" | "extends" then
                  return Tok_Keyword;
               end if;

            when 8 =>
               if Folded = "abstract" then
                  return Tok_Keyword;
               end if;

            when others =>
               null;
         end case;

         return Tok_Identifier;
      end Keyword;

      ----------------
      -- Last_Token --
      ----------------

      function Last_Token (Self : State) return Token is (Self.Last_Token);

      ----------------
      -- Next_Token --
      ----------------

      procedure Next_Token
        (Self : in out State; Buffer : String; Tok : out Token)
      is
         S     : String renames Buffer;
         Last  : constant Natural  := Self.Last;
         First : constant Positive := Self.Last_Token.Text_Last + 1;
         Idx   : Natural           := First;
         Match : Natural;

      begin
         if First > Last then
            Tok := (Tok_Termination, First, First - 1);
            Self.Has_Next := False;
            Self.Last_Token := Tok;

            return;
         end if;

         case S (First) is
            when Blank =>
               while Idx <= Last and then S (Idx) in Blank loop
                  Idx := Idx + 1;
               end loop;

               Tok := (Tok_Whitespace, First, Idx - 1);

            when '-' =>
               if First < Last and then S (First + 1) = '-' then
                  Idx := First + 2;

                  while Idx <= Last
                    and then S (Idx) not in ASCII.LF .. ASCII.CR
                  loop
                     Idx := Idx + 1;
                  end loop;

                  Tok := (Tok_Comment, First, Idx - 1);
               else
                  Tok := (Tok_Failure, First, First);
               end if;

            when '"' =>
               --  Keep the last closing quote when scanning doubled quotes

               Idx := First + 1;
               Match := 0;

               Scan_String :
               while Idx <= Last loop
                  if S (Idx) = '"' then
                     Match := Idx;

                     exit Scan_String when
                       Idx = Last or else S (Idx + 1) /= '"';

                     Idx := Idx + 2;

                  elsif S (Idx) = ASCII.LF then
                     exit Scan_String;

                  else
                     Idx := Idx + 1;
                  end if;
               end loop Scan_String;

               if Match = 0 then
                  Tok := (Tok_Failure, First, First);
               else
                  Tok := (Tok_String, First, Match);
               end if;

            when Digit =>
               Match := First;
               Idx := First + 1;

               Scan_Number :
               loop
                  if Idx <= Last and then S (Idx) in Digit then
                     Match := Idx;
                     Idx := Idx + 1;

                  elsif Idx < Last
                    and then S (Idx) = '_'
                    and then S (Idx + 1) in Digit
                  then
                     Idx := Idx + 2;
                     Match := Idx - 1;

                  else
                     exit Scan_Number;
                  end if;
               end loop Scan_Number;

               Tok := (Tok_Number, First, Match);

            when Letter | '[' | ASCII.ESC =>
               Idx := First;

               loop
                  while Idx <= Last and then S (Idx) in Alphanumeric loop
                     Idx := Idx + 1;
                  end loop;

                  exit when Idx > Last or else S (Idx) not in '[' | ASCII.ESC;

                  Match := Encoding_Length (S, Idx);
                  if Match = 0 then
                     if S (Idx) = ASCII.ESC
                       or else
                         (Idx > First
                          and then Idx < Last
                          and then S (Idx + 1) = '"')
                     then
                        raise Ada.Strings.UTF_Encoding.Encoding_Error
                          with "invalid wide character encoding in identifier";
                     end if;
                     exit;
                  end if;
                  Idx := Idx + Match;
               end loop;

               if Idx = First then
                  Tok := (Tok_Failure, First, First);
               else
                  Tok := (Keyword (S (First .. Idx - 1)), First, Idx - 1);
               end if;

            when ':' =>
               if First < Last and then S (First + 1) = '=' then
                  Tok := (Tok_Delimiter, First, First + 1);
               else
                  Tok := (Tok_Delimiter, First, First);
               end if;

            when '=' =>
               if First < Last and then S (First + 1) = '>' then
                  Tok := (Tok_Delimiter, First, First + 1);
               else
                  Tok := (Tok_Failure, First, First);
               end if;

            when '(' => Tok := (Tok_Par_Open, First, First);
            when ',' => Tok := (Tok_Comma, First, First);
            when '.' => Tok := (Tok_Dot, First, First);
            when ';' => Tok := (Tok_Semicolon, First, First);

            when ')' | '&' | ''' | '|' =>
               Tok := (Tok_Delimiter, First, First);

            when others =>
               Tok := (Tok_Failure, First, First);
         end case;

         Self.Last_Token := Tok;
      end Next_Token;
   end Lexer;

   use Lexer;

   package Token_Vectors is new Ada.Containers.Vectors
     (Natural, Token);

   subtype Token_Vector is Token_Vectors.Vector;

   -------------
   -- Compute --
   -------------

   procedure Compute
     (File_Reader      : GPR2.File_Readers.File_Reader_Reference;
      Data             : in out Source_Base.Object'Class;
      Get_Withed_Units : Boolean;
      Success          : out Boolean)
   is
      U_Withed : Containers.Name_Set;
      W_Found  : Containers.Name_Set;

      Parsed   : Boolean := False;
      --  True when a parser callback updates Data

      procedure No_Body_CB;

      procedure Parse_Buffer (Buffer : String);

      procedure Register
        (Name         : String;
         Parents_Only : Boolean := False);
      --  Register Name and its parents

      procedure Unit_Name_CB
        (Unit_Name     : String;
         Separate_From : String;
         Generic_Unit  : Boolean);
      --  Update Data's unit and its dependencies

      procedure With_Clause_CB
        (Unit_Name  : String;
         Is_Limited : Boolean);
      --  Collect dependencies for Unit_Name_CB

      ----------------
      -- No_Body_CB --
      ----------------

      procedure No_Body_CB is
      begin
         Data.Update_Unit
           (Unit_Info.Create
              (Unit_Name => No_Name,
               Index     => No_Index,
               Kind      => S_No_Body,
               Parsed    => True));
         Data.Kind := S_No_Body;
         Parsed    := True;
      end No_Body_CB;

      ------------------
      -- Parse_Buffer --
      ------------------

      procedure Parse_Buffer (Buffer : String) is
      begin
         Parse_Context_Clauses
           (Buffer         => Buffer,
            Log_Error      => null, --  Ignore parsing errors for now
            With_Clause_CB => (if Get_Withed_Units
                               then With_Clause_CB'Access
                               else null),
            Unit_Name_CB   => Unit_Name_CB'Access,
            No_Body_CB     => No_Body_CB'Access);
      end Parse_Buffer;

      --------------
      -- Register --
      --------------

      procedure Register
        (Name         : String;
         Parents_Only : Boolean := False)
      is
         N        : constant Name_Type := Name_Type (Name);
         B_Name   : constant String    :=
                      GPR2.Path_Name.Base_Name (Name);
         Position : Containers.Name_Type_Set.Cursor;
         Inserted : Boolean;

      begin
         if not Parents_Only then
            W_Found.Insert (N, Position, Inserted);

            if Inserted then
               U_Withed.Insert (N);
            end if;
         end if;

         if B_Name /= Name then
            Register (B_Name);
         end if;
      end Register;

      ------------------
      -- Unit_Name_CB --
      ------------------

      procedure Unit_Name_CB
        (Unit_Name     : String;
         Separate_From : String;
         Generic_Unit  : Boolean)
      is
         pragma Unreferenced (Generic_Unit);
      begin

         --  Refine the kind inferred from naming: body or separate

         if Separate_From /= "" then
            Data.Kind := S_Separate;
         elsif Data.Kind = S_Separate then
            Data.Kind := S_Body;
         end if;

         if Separate_From = "" then
            --  Make parent units visible for child units

            Register (Unit_Name, True);
         end if;

         declare
            U_Name : constant Name_Type :=
                       (if Separate_From'Length > 0
                        then Name_Type (Separate_From)
                        else Name_Type (Unit_Name));
            Sep_Name : constant Optional_Name_Type :=
                         (if Separate_From'Length > 0
                          then Optional_Name_Type (Unit_Name)
                          else "");

            CU       : constant Unit_Info.Object :=
                         Unit_Info.Create
                           (Unit_Name     => U_Name,
                            Index         => No_Index,
                            Kind          => Data.Kind,
                            Separate_Name => Sep_Name,
                            Dependencies  => U_Withed,
                            Parsed        => True);

         begin
            --  Replace the unit inferred from naming with the parsed unit

            Data.Update_Unit (CU);

            Parsed := True;
         end;

      end Unit_Name_CB;

      --------------------
      -- With_Clause_CB --
      --------------------

      procedure With_Clause_CB
        (Unit_Name  : String;
         Is_Limited : Boolean)
      is
         pragma Unreferenced (Is_Limited);
      begin
         Register (Unit_Name);
      end With_Clause_CB;

      use type GPR2.File_Readers.File_Reader_Reference;
      use type GPR2.File_Readers.Text_Access;


   begin
      if File_Reader = GPR2.File_Readers.No_File_Reader_Reference then
         declare
            File   : GNATCOLL.Mmap.Mapped_File :=
                       GNATCOLL.Mmap.Invalid_Mapped_File;
            Region : GNATCOLL.Mmap.Mapped_Region :=
                       GNATCOLL.Mmap.Invalid_Mapped_Region;
         begin
            File := GNATCOLL.Mmap.Open_Read
                      (Data.Path_Name.String_Value);
            Region := GNATCOLL.Mmap.Read (File);

            declare
               Buffer : constant String
                          (1 .. GNATCOLL.Mmap.Last (Region))
                 with Import  => True,
                      Address =>
                        GNATCOLL.Mmap.Data (Region).all'Address;
            begin
               --  Actual encoding used in this buffer will be handled by
               --  Parse_Context_Clauses, no need to try a translation right
               --  here, and pass the raw buffer directly.
               Parse_Buffer (Buffer);
            end;

            GNATCOLL.Mmap.Free (Region);
            GNATCOLL.Mmap.Close (File);
         exception
            when others =>
               Parsed := False;
               GNATCOLL.Mmap.Free (Region);
               GNATCOLL.Mmap.Close (File);
         end;

      else
         --  Custom readers provide decoded contents

         declare
            Contents : GPR2.File_Readers.Decoded_File_Contents :=
                         (Buffer => null, First => 1, Last => 0);
            Diags    : GPR2.Log.Object;
         begin
            GPR2.File_Readers.Read
              (Self        => File_Reader.Get,
               Filename    => Data.Path_Name.String_Value,
               Charset     => "iso-8859-1",
               Read_BOM    => True,
               Contents    => Contents,
               Diagnostics => Diags);

            if not Diags.Has_Error and then Contents.Buffer /= null then
               --  Ensure the BOM is included so that Parse_Context_Clauses
               --  properly reads it and treat the source as utf-8 encoded
               Parse_Buffer
                 (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode
                    (Contents.Buffer (Contents.First .. Contents.Last),
                     Output_BOM => True));
            end if;

            GPR2.File_Readers.Free (Contents.Buffer);
         exception
            when others =>
               Parsed := False;
               GPR2.File_Readers.Free (Contents.Buffer);
         end;
      end if;

      if not Parsed
        and then Data.Has_Unit_At (No_Index)
        and then Data.Unit.Is_Parsed
      then
         declare
            Old : Unit_Info.Object := Data.Unit;
         begin
            Old.Set_Parsed_State (False);
            Data.Update_Unit (Old);
         end;
      end if;

      Success := Parsed;
   end Compute;

   ---------------------------
   -- Parse_Context_Clauses --
   ---------------------------

   procedure Parse_Context_Clauses
     (Buffer         : String;
      Log_Error      : access procedure (Message : String);
      With_Clause_CB : access procedure (Unit_Name  : String;
                                         Is_Limited : Boolean) := null;
      Unit_Name_CB   : access procedure (Unit_Name     : String;
                                         Separate_From : String;
                                         Generic_Unit  : Boolean) := null;
      No_Body_CB     : access procedure := null)
   is
      Has_Error : Boolean := False;

      function Decode_UTF
        (Input : String; BOM : GNAT.Byte_Order_Mark.BOM_Kind) return String;

      procedure Log_Error_Internal (Message : String);

      procedure Parse (Source : String; UTF8 : Boolean);

      ----------------
      -- Decode_UTF --
      ----------------

      function Decode_UTF
        (Input : String; BOM : GNAT.Byte_Order_Mark.BOM_Kind) return String
      is
         use GNAT.Byte_Order_Mark;
         use type GNATCOLL.Iconv.Iconv_Result;

         Charset : constant String :=
           (case BOM is
               when UTF16_LE => GNATCOLL.Iconv.UTF16LE,
               when UTF16_BE => GNATCOLL.Iconv.UTF16BE,
               when UTF32_LE => GNATCOLL.Iconv.UTF32LE,
               when others   => GNATCOLL.Iconv.UTF32BE);
         Output  : String (1 .. 2 * Input'Length);
         In_Idx  : Positive := Input'First;
         Out_Idx : Positive := Output'First;
         Result  : GNATCOLL.Iconv.Iconv_Result;
         State   : constant GNATCOLL.Iconv.Iconv_T :=
                     GNATCOLL.Iconv.Iconv_Open (From_Code => Charset);
      begin
         if Input'Length = 0 then
            GNATCOLL.Iconv.Iconv_Close (State);
            return "";
         end if;

         GNATCOLL.Iconv.Iconv
           (State, Input, In_Idx, Output, Out_Idx, Result);

         if Result /= GNATCOLL.Iconv.Success then
            raise Ada.Strings.UTF_Encoding.Encoding_Error
              with "invalid " & Charset & " source";
         end if;

         return Converted : constant String := Output (1 .. Out_Idx - 1) do
            GNATCOLL.Iconv.Iconv_Close (State);
         end return;
      exception
         when others =>
            GNATCOLL.Iconv.Iconv_Close (State);
            raise;
      end Decode_UTF;

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

      -----------
      -- Parse --
      -----------

      procedure Parse (Source : String; UTF8 : Boolean) is
         Buffer : String renames Source;
         State  : Lexer.State;

         type Name_Array is
           array (Positive range <>) of access constant String;
         type Kind_Array is array (Positive range <>) of Token_Kind;

         Procedure_Name : aliased constant String := "procedure";
         Function_Name  : aliased constant String := "function";
         Generic_Name   : aliased constant String := "generic";
         Separate_Name  : aliased constant String := "separate";
         No_Body_Name   : aliased constant String := "no_body";
         Body_Name      : aliased constant String := "body";
         Access_Name    : aliased constant String := "access";
         Protected_Name : aliased constant String := "protected";
         Task_Name      : aliased constant String := "task";

         function Text (T : Token) return String
         is (To_Lower_ASCII
               (Buffer (T.Text_First .. T.Text_Last)));

         function Name_Without_Whitespaces
           (First_Idx, Last_Idx : Natural;
            Whitespaces         : Token_Vector) return String;
         --  Remove whitespace/comments and decode character escapes to UTF-8

         function Skip_Until
           (Kinds    : Kind_Array;
            T        : out Token;
            Names    : Name_Array := (2 .. 1 => <>);
            Expected : Boolean    := True) return Boolean;
         --  Find a token of Kinds. For identifiers, Names selects matches
         --  when Expected is True, and excludes them otherwise. An empty
         --  Names accepts any identifier. Return False at end of input.

         function Parse_Library_Item_And_Subunit_Pre_Cond return Boolean;

         procedure Parse_Library_Item_And_Subunit
           with Pre => Parse_Library_Item_And_Subunit_Pre_Cond;
         --  Report the unit name and any subunit parent through Unit_Name_CB

         function Parse_Static_Name
           (T           : in out Token;
            Whitespaces : out Token_Vector) return Natural
           with Pre => Last_Token (State).Kind = Tok_Identifier;
         --  Return the name's last index and collect whitespace/comment spans

         procedure Parse_With_Clause
           with Pre => Last_Token (State).Kind in Tok_With | Tok_Limited;

         ------------------------------
         -- Name_Without_Whitespaces --
         ------------------------------

         function Name_Without_Whitespaces
           (First_Idx, Last_Idx : Natural;
            Whitespaces         : Token_Vector) return String
         is
            Result    : String (1 .. Last_Idx - First_Idx + 1);
            Len       : Natural := 0;
            Cursor    : Natural := First_Idx;
            Non_ASCII : Boolean := False;

            function Decode_Escapes return String;
            procedure Take (From, To : Natural);

            --------------------
            -- Decode_Escapes --
            --------------------

            function Decode_Escapes return String is
               Decoded : Wide_Wide_String (1 .. Len);
               Length  : Natural := 0;
               Idx     : Positive := 1;
               First   : Positive;
               Span    : Natural;
            begin
               loop
                  First := Idx;
                  while Idx <= Len
                    and then Result (Idx) not in '[' | ASCII.ESC
                  loop
                     Idx := Idx + 1;
                  end loop;

                  if Idx > First then
                     declare
                        Plain : constant Wide_Wide_String :=
                          (if UTF8
                           then Ada.Strings.UTF_Encoding.Wide_Wide_Strings.
                             Decode (Result (First .. Idx - 1))
                           else Ada.Characters.Conversions.
                             To_Wide_Wide_String
                               (Result (First .. Idx - 1)));
                     begin
                        Decoded (Length + 1 .. Length + Plain'Length) := Plain;
                        Length := Length + Plain'Length;
                     end;
                  end if;

                  exit when Idx > Len;
                  Span := Encoding_Length (Result (1 .. Len), Idx);
                  if Span = 0 then
                     raise Ada.Strings.UTF_Encoding.Encoding_Error
                       with "invalid wide character encoding in identifier";
                  end if;

                  Length := Length + 1;
                  First := Idx + (if Result (Idx) = ASCII.ESC then 1 else 2);
                  Decoded (Length) := Wide_Wide_Character'Val
                    (Natural'Value
                       ("16#" & Result
                          (First .. Idx + Span
                             - (if Result (Idx) = ASCII.ESC then 1 else 3))
                        & '#'));
                  Idx := Idx + Span;
               end loop;

               return Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode
                 (Decoded (1 .. Length));
            end Decode_Escapes;

            ----------
            -- Take --
            ----------

            procedure Take (From, To : Natural) is
            begin
               if From <= To then
                  Result (Len + 1 .. Len + To - From + 1) :=
                    Buffer (From .. To);
                  Len := Len + To - From + 1;
               end if;
            end Take;

         begin
            for W of Whitespaces loop
               Take (Cursor, W.Text_First - 1);
               Cursor := W.Text_Last + 1;
            end loop;

            Take (Cursor, Last_Idx);

            for C of Result (1 .. Len) loop
               if C in '[' | ASCII.ESC then
                  return Decode_Escapes;
               elsif C >= Character'Val (16#80#) then
                  Non_ASCII := True;
               end if;
            end loop;

            if Non_ASCII then
               if UTF8 then
                  --  Below we check that the sequence is actual UTF-8:
                  --  the call to Decode will raise Encoding_Error if not.
                  declare
                     Decoded : constant Wide_Wide_String :=
                       Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode
                         (Result (1 .. Len));
                     pragma Unreferenced (Decoded);
                  begin
                     return Result (1 .. Len);
                  end;
               else
                  --  Latin-1 to utf8 conversion
                  return Ada.Strings.UTF_Encoding.Strings.Encode
                    (Result (1 .. Len));
               end if;
            end if;

            --  Plain ASCII needs no conversion
            return Result (1 .. Len);
         end Name_Without_Whitespaces;

         ------------------------------------
         -- Parse_Library_Item_And_Subunit --
         ------------------------------------

         procedure Parse_Library_Item_And_Subunit is
            T                    : Token := Last_Token (State);
            Separate_From_First  : Natural := 0;
            Separate_From_Last   : Natural := 0;
            Separate_Whitespaces : Token_Vector;
            Generic_Unit         : Boolean := False;

            procedure Skip_Generic;
            procedure Parse_Separate;
            function Parse_Unit_Pre_Cond return Boolean;
            procedure Parse_Unit with Pre => Parse_Unit_Pre_Cond;

            --------------------
            -- Parse_Separate --
            --------------------

            procedure Parse_Separate is
            begin
               if not Skip_Until ((1 => Tok_Par_Open), T) then
                  Log_Error_Internal
                    ("no opening parenthesis after the 'separate' keyword");

                  return;
               end if;

               if not Skip_Until ((1 => Tok_Identifier), T) then
                  Log_Error_Internal
                    ("no identifier after the 'separate' keyword");

                  return;
               end if;

               Separate_From_First := T.Text_First;
               Separate_From_Last  :=
                 Parse_Static_Name (T, Separate_Whitespaces);

               if not Skip_Until
                 ((Tok_Identifier, Tok_Package), T,
                  (Function_Name'Access, Procedure_Name'Access,
                   Task_Name'Access, Protected_Name'Access))
               then
                  Log_Error_Internal
                    ("missing the subunit kind " &
                       "(package / function / procedure)");
               end if;
            end Parse_Separate;

            ----------------
            -- Parse_Unit --
            ----------------

            procedure Parse_Unit is
               Whitespaces : Token_Vector;
            begin
               --  Skip an optional "body" before the unit name

               if not Skip_Until
                 ((Tok_Identifier, Tok_String), T, (1 => Body_Name'Access),
                  Expected => False)
               then
                  Log_Error_Internal
                    ("missing the unit or subunit identifier");

                  return;
               end if;

               if T.Kind = Tok_String then
                  --  Leave invalid operator units for the compiler to diagnose
                  return;
               end if;

               declare
                  Unit_First : constant Positive := T.Text_First;
                  Unit_Last  : constant Natural  :=
                                 Parse_Static_Name (T, Whitespaces);
               begin
                  Unit_Name_CB
                    (Unit_Name     =>
                       Name_Without_Whitespaces
                         (Unit_First, Unit_Last, Whitespaces),
                     Separate_From =>
                       (if Separate_From_First /= 0
                        then Name_Without_Whitespaces
                               (Separate_From_First, Separate_From_Last,
                                Separate_Whitespaces)
                        else ""),
                     Generic_Unit  => Generic_Unit);
               end;
            end Parse_Unit;

            -------------------------
            -- Parse_Unit_Pre_Cond --
            -------------------------

            function Parse_Unit_Pre_Cond return Boolean is
               C : constant Token := Last_Token (State);
            begin
               if C.Kind = Tok_Package then
                  return True;

               elsif C.Kind = Tok_Identifier then
                  --  Task and protected bodies reach this through a subunit

                  return Text (C) in Procedure_Name | Function_Name
                                   | Task_Name | Protected_Name;
               end if;

               return False;
            end Parse_Unit_Pre_Cond;

            ------------------
            -- Skip_Generic --
            ------------------

            procedure Skip_Generic is
            begin
               --  Skip formal "with" and "access" declarations so their
               --  subprogram and package keywords do not start the unit.

               loop
                  if not Skip_Until
                    ((Tok_Identifier, Tok_With, Tok_Package), T,
                     (Function_Name'Access, Procedure_Name'Access,
                      Access_Name'Access))
                  then
                     Log_Error_Internal
                       ("failed to skip the generic formal parameters part");

                     exit;
                  end if;

                  if T.Kind = Tok_Package then
                     --  A package outside a formal "with" starts the unit

                     exit;

                  elsif T.Kind = Tok_Identifier then
                     exit when Text (T) in Function_Name | Procedure_Name;

                     --  Skip the formal access declaration

                     if not Skip_Until ((1 => Tok_Semicolon), T) then
                        Log_Error_Internal
                          ("in formal generic parameters, missing ':' after " &
                             "variable declaration");

                        exit;
                     end if;

                  elsif T.Kind = Tok_With then
                     if not Skip_Until ((1 => Tok_Semicolon), T) then
                        Log_Error_Internal
                          ("missing ';' after a formal generic parameter " &
                             "declaration");

                        exit;
                     end if;
                  end if;
               end loop;
            end Skip_Generic;

         begin
            if T.Kind = Tok_Identifier and then Text (T) = Generic_Name then
               Skip_Generic;
               Generic_Unit := True;

            elsif T.Kind = Tok_Identifier
              and then Text (T) = Separate_Name
            then
               Parse_Separate;
            end if;

            Parse_Unit;
         end Parse_Library_Item_And_Subunit;

         ---------------------------------------------
         -- Parse_Library_Item_And_Subunit_Pre_Cond --
         ---------------------------------------------

         function Parse_Library_Item_And_Subunit_Pre_Cond return Boolean is
            T : constant Token := Last_Token (State);
         begin
            if T.Kind = Tok_Package then
               return True;

            elsif T.Kind = Tok_Identifier then
               return Text (T) in
                 Procedure_Name | Function_Name | Separate_Name | Generic_Name;
            end if;

            return False;
         end Parse_Library_Item_And_Subunit_Pre_Cond;

         -----------------------
         -- Parse_Static_Name --
         -----------------------

         function Parse_Static_Name
           (T           : in out Token;
            Whitespaces : out Token_Vector) return Natural
         is
            Expected : Token_Kind := Tok_Dot;
            Last     : Natural    := T.Text_Last;
         begin
            Whitespaces.Clear;

            while Has_Next (State) loop
               Next_Token (State, Buffer, T);

               if T.Kind in Tok_Whitespace | Tok_Comment then
                  Whitespaces.Append (T);

               elsif T.Kind /= Expected then
                  return Last;

               else
                  Last := T.Text_Last;
                  Expected :=
                    (if Expected = Tok_Dot then Tok_Identifier else Tok_Dot);
               end if;
            end loop;

            return Last;
         end Parse_Static_Name;

         -----------------------
         -- Parse_With_Clause --
         -----------------------

         procedure Parse_With_Clause is
            T           : Token := Last_Token (State);
            First       : Positive;
            Last        : Positive;
            Is_Limited  : Boolean := False;
            Whitespaces : Token_Vector;

         begin
            if T.Kind = Tok_Limited then
               Is_Limited := True;

               if not Skip_Until ((1 => Tok_With), T) then
                  Log_Error_Internal
                    ("expecting a with token after 'limited'");

                  return;
               end if;
            end if;

            loop
               if not Skip_Until ((1 => Tok_Identifier), T) then
                  Log_Error_Internal
                    ("with clause is missing a unit identifier");

                  return;
               end if;

               First := T.Text_First;
               Last  := Parse_Static_Name (T, Whitespaces);

               if With_Clause_CB /= null then
                  With_Clause_CB
                    (Name_Without_Whitespaces (First, Last, Whitespaces),
                     Is_Limited);
               end if;

               if T.Kind not in Tok_Semicolon | Tok_Comma then
                  if not Skip_Until ((Tok_Semicolon, Tok_Comma), T) then
                     Log_Error_Internal
                       ("missing ';' or ',' in a with clause");

                     return;
                  end if;
               end if;

               exit when T.Kind = Tok_Semicolon;
            end loop;
         end Parse_With_Clause;

         ----------------
         -- Skip_Until --
         ----------------

         function Skip_Until
           (Kinds    : Kind_Array;
            T        : out Token;
            Names    : Name_Array := (2 .. 1 => <>);
            Expected : Boolean    := True) return Boolean is
         begin
            while Has_Next (State) loop
               Next_Token (State, Buffer, T);

               for K of Kinds loop
                  if K = T.Kind then
                     if T.Kind = Tok_Identifier and then Names'Length > 0 then
                        declare
                           Content : constant String := Text (T);
                           Found   : Boolean := False;
                        begin
                           for N of Names loop
                              if Content = N.all then
                                 Found := True;
                              end if;
                           end loop;

                           if Found = Expected then
                              return True;
                           end if;
                        end;

                     else
                        return True;
                     end if;
                  end if;
               end loop;
            end loop;

            return False;
         end Skip_Until;

      begin
         Lexer.Initialize (State, Buffer);

         while Has_Next (State) and then not Has_Error loop

            --  Scan context clauses and the unit header without validating
            --  the rest of the source.

            declare
               T : Token;
            begin
               if not Skip_Until
                 ((Tok_With, Tok_Identifier, Tok_Package, Tok_Limited,
                   Tok_Pragma),
                  T,
                  (Procedure_Name'Access, Function_Name'Access,
                   Separate_Name'Access, Generic_Name'Access))
               then
                  Log_Error_Internal ("Unexpected preamble");

                  exit;
               end if;

               if T.Kind in Tok_With | Tok_Limited then
                  if With_Clause_CB /= null then
                     Parse_With_Clause;

                  elsif not Skip_Until ((1 => Tok_Semicolon), T) then
                     Log_Error_Internal
                       ("missing ';' or ',' in a with clause");

                     exit;
                  end if;

               elsif T.Kind in Tok_Identifier | Tok_Package then
                  Parse_Library_Item_And_Subunit;

                  --  Only the first unit header is needed

                  exit;

               elsif T.Kind = Tok_Pragma then
                  --  Stop at pragma No_Body; ignore other pragmas

                  if not Skip_Until ((1 => Tok_Identifier), T) then
                     Log_Error_Internal
                       ("pragma should be followed by an identifier");

                     exit;

                  elsif Text (T) = No_Body_Name then
                     if No_Body_CB /= null then
                        No_Body_CB.all;
                     end if;

                     exit;
                  end if;
               end if;
            end;
         end loop;
      end Parse;

   begin
      declare
         use GNAT.Byte_Order_Mark;

         BOM : BOM_Kind;
         Len : Natural;
      begin
         Read_BOM (Buffer, Len, BOM);

         --  The parser itself works fine for all 8-bit based encodings, since
         --  the keywords are all plain ascii characters. UTF16 and UTF32 on
         --  the other hand require a conversion before being parsed.
         --
         --  For sources with no BOM we expect latin-1 here and the identifiers
         --  will be translated to utf-8 before the corresponding callback is
         --  called.

         case BOM is
            when Unknown =>
               Parse (Buffer, UTF8 => False);

            when UTF8_All =>
               Parse (Buffer (Buffer'First + Len .. Buffer'Last),
                      UTF8 => True);

            when UTF16_LE | UTF16_BE | UTF32_LE | UTF32_BE =>
               Parse
                 (Decode_UTF
                    (Buffer (Buffer'First + Len .. Buffer'Last), BOM),
                  UTF8 => True);

            when others =>
               Log_Error_Internal ("unsupported source encoding");
         end case;
      end;
   exception
      when E : others =>
         Log_Error_Internal
           ("exception while parsing" & ASCII.LF
            & Ada.Exceptions.Exception_Information (E));
   end Parse_Context_Clauses;

end GPR2.Build.Source_Base.Ada_Parser;
