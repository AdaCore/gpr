
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--




package body Gpr_Parser.Lexer_State_Machine is

   Is_Trivia : constant array (Token_Kind) of Boolean := (
      Gpr_Termination => False, Gpr_Lexing_Failure => True, Gpr_Identifier => False, Gpr_All => False, Gpr_Abstract => False, Gpr_At => False, Gpr_Case => False, Gpr_End => False, Gpr_For => False, Gpr_Is => False, Gpr_Limited => False, Gpr_Private => False, Gpr_Null => False, Gpr_Others => False, Gpr_Package => False, Gpr_Renames => False, Gpr_Type => False, Gpr_Use => False, Gpr_Pragma => False, Gpr_When => False, Gpr_With => False, Gpr_Extends => False, Gpr_Par_Open => False, Gpr_Par_Close => False, Gpr_Semicolon => False, Gpr_Colon => False, Gpr_Comma => False, Gpr_Dot => False, Gpr_Amp => False, Gpr_Tick => False, Gpr_Pipe => False, Gpr_Assign => False, Gpr_Arrow => False, Gpr_String => False, Gpr_Number => False, Gpr_Label => False, Gpr_Char => False, Gpr_Comment => True, Gpr_Whitespace => True
   );

   type Character_Range is record
      First, Last : Character_Type;
   end record;

   type Character_Range_Array is array (Positive range <>) of Character_Range;
   --  Sorted list of dijoint character ranges

   pragma Warnings (Off, "referenced");
   function Contains
     (Char : Character_Type; Ranges : Character_Range_Array) return Boolean;
   --  Return whether Char is included in the given ranges
   pragma Warnings (On, "referenced");

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : out Lexer_State;
      Input       : Text_Access;
      Input_First : Positive;
      Input_Last  : Natural) is
   begin
      Self.Input := Input;
      Self.Input_First := Input_First;
      Self.Input_Last := Input_Last;
      Self.Has_Next := True;
      Self.Last_Token := (Kind       => Gpr_Termination,
                          Text_First => Input_First,
                          Text_Last  => Input_First - 1);
      Self.Last_Token_Kind := Gpr_Termination;
   end Initialize;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Self : Lexer_State) return Lexed_Token is
   begin
      return Self.Last_Token;
   end Last_Token;

   --------------
   -- Has_Next --
   --------------

   function Has_Next (Self : Lexer_State) return Boolean is
   begin
      return Self.Has_Next;
   end Has_Next;

   --------------
   -- Contains --
   --------------

   function Contains
     (Char : Character_Type; Ranges : Character_Range_Array) return Boolean
   is
      Low  : Natural := Ranges'First;
      High : Natural := Ranges'Last;
   begin
      while Low <= High loop
         declare
            Middle : constant Natural := (Low + High) / 2;
            R      : Character_Range renames Ranges (Middle);
         begin
            if Char < R.First then
               High := Middle - 1;
            elsif Char > R.Last then
               Low := Middle + 1;
            else
               return True;
            end if;
         end;
      end loop;
      return False;
   end Contains;



   ----------------
   -- Next_Token --
   ----------------

   procedure Next_Token
     (Self : in out Lexer_State; Token : out Lexed_Token)
   is
      Input : constant Text_Access := Self.Input;

      First_Index : Positive;
      --  Index of the first input character for the token to return

      Index : Positive;
      --  Index for the next input character to be analyzed

      Match_Index : Natural;
      --  If we found a match, index for its last character. Otherwise, zero.

      Match_Ignore : Boolean;
      --  If we found a match, whether we must ignore it and restart the
      --  automaton after its character range.

      Match_Kind : Token_Kind;
      --  If we found a match and it is not ignored, kind for the token to
      --  emit. Meaningless otherwise.
   begin
      First_Index := Self.Last_Token.Text_Last + 1;

      <<Start>>
      Index := First_Index;
      Match_Index := 0;
      Match_Ignore := False;



         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#9#) .. Character_Type'Val (16#a#) | Character_Type'Val (16#d#) | ' ' => goto State_1;
               when '"' => goto State_2;
               when '&' => goto State_3;
               when ''' => goto State_4;
               when '(' => goto State_5;
               when ')' => goto State_6;
               when ',' => goto State_7;
               when '-' => goto State_8;
               when '.' => goto State_9;
               when '0' .. '9' => goto State_10;
               when ':' => goto State_11;
               when ';' => goto State_12;
               when '=' => goto State_13;
               when 'A' | 'a' => goto State_14;
               when 'B' | 'D' | 'F' .. 'E' | 'G' .. 'H' | 'J' .. 'K' | 'M' | 'O' .. 'N' | 'P' .. 'O' | 'Q' | 'S' | 'U' .. 'T' | 'V' | 'X' .. 'Z' | '_' | 'b' | 'd' | 'f' .. 'e' | 'g' .. 'h' | 'j' .. 'k' | 'm' | 'o' .. 'n' | 'p' .. 'o' | 'q' | 's' | 'u' .. 't' | 'v' | 'x' .. 'z' => goto State_15;
               when 'C' | 'c' => goto State_16;
               when 'E' | 'e' => goto State_17;
               when 'F' | 'f' => goto State_18;
               when 'I' | 'i' => goto State_19;
               when 'L' | 'l' => goto State_20;
               when 'N' | 'n' => goto State_21;
               when 'O' | 'o' => goto State_22;
               when 'P' | 'p' => goto State_23;
               when 'R' | 'r' => goto State_24;
               when 'T' | 't' => goto State_25;
               when 'U' | 'u' => goto State_26;
               when 'W' | 'w' => goto State_27;
               when '|' => goto State_28;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_1>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Whitespace;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#9#) .. Character_Type'Val (16#a#) | Character_Type'Val (16#d#) | ' ' => goto State_29;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_2>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. Character_Type'Val (16#10ffff#) => goto State_30;
               when '"' => goto State_31;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_3>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Amp;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_4>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Tick;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_5>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Par_Open;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_6>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Par_Close;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_7>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Comma;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_8>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '-' => goto State_32;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_9>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Dot;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_10>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Number;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_33;
               when '_' => goto State_34;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_11>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Colon;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_35;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_12>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Semicolon;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_13>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '>' => goto State_36;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_14>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' | 'C' .. 'K' | 'M' .. 'S' | 'U' .. 'Z' | '_' | 'a' | 'c' .. 'k' | 'm' .. 's' | 'u' .. 'z' => goto State_37;
               when 'B' | 'b' => goto State_38;
               when 'L' | 'l' => goto State_39;
               when 'T' | 't' => goto State_40;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_15>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_16>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'B' .. 'Z' | '_' | 'b' .. 'z' => goto State_37;
               when 'A' | 'a' => goto State_41;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_17>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'M' | 'O' .. 'W' | 'Y' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'w' | 'y' .. 'z' => goto State_37;
               when 'N' | 'n' => goto State_42;
               when 'X' | 'x' => goto State_43;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_18>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'N' | 'P' .. 'Z' | '_' | 'a' .. 'n' | 'p' .. 'z' => goto State_37;
               when 'O' | 'o' => goto State_44;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_19>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'R' | 'T' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_37;
               when 'S' | 's' => goto State_45;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_20>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'H' | 'J' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'z' => goto State_37;
               when 'I' | 'i' => goto State_46;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_21>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'T' | 'V' .. 'Z' | '_' | 'a' .. 't' | 'v' .. 'z' => goto State_37;
               when 'U' | 'u' => goto State_47;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_22>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'S' | 'U' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_37;
               when 'T' | 't' => goto State_48;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_23>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'B' .. 'Q' | 'S' .. 'Z' | '_' | 'b' .. 'q' | 's' .. 'z' => goto State_37;
               when 'A' | 'a' => goto State_49;
               when 'R' | 'r' => goto State_50;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_24>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'D' | 'F' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_37;
               when 'E' | 'e' => goto State_51;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_25>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'X' | 'Z' | '_' | 'a' .. 'x' | 'z' => goto State_37;
               when 'Y' | 'y' => goto State_52;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_26>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'R' | 'T' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_37;
               when 'S' | 's' => goto State_53;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_27>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'G' | 'I' .. 'H' | 'J' .. 'Z' | '_' | 'a' .. 'g' | 'i' .. 'h' | 'j' .. 'z' => goto State_37;
               when 'H' | 'h' => goto State_54;
               when 'I' | 'i' => goto State_55;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_28>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Pipe;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_29>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Whitespace;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#9#) .. Character_Type'Val (16#a#) | Character_Type'Val (16#d#) | ' ' => goto State_29;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_30>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. Character_Type'Val (16#10ffff#) => goto State_30;
               when '"' => goto State_31;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_31>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_String;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_56;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_32>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Comment;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#10ffff#) => goto State_57;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_33>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Number;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_33;
               when '_' => goto State_34;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_34>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_33;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_35>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Assign;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_36>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Arrow;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_37>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_38>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'R' | 'T' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_37;
               when 'S' | 's' => goto State_58;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_39>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'K' | 'M' .. 'Z' | '_' | 'a' .. 'k' | 'm' .. 'z' => goto State_37;
               when 'L' | 'l' => goto State_59;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_40>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_At;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_41>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'R' | 'T' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_37;
               when 'S' | 's' => goto State_60;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_42>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'C' | 'E' .. 'Z' | '_' | 'a' .. 'c' | 'e' .. 'z' => goto State_37;
               when 'D' | 'd' => goto State_61;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_43>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'S' | 'U' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_37;
               when 'T' | 't' => goto State_62;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_44>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Q' | 'S' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_37;
               when 'R' | 'r' => goto State_63;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_45>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Is;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_46>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'L' | 'N' .. 'Z' | '_' | 'a' .. 'l' | 'n' .. 'z' => goto State_37;
               when 'M' | 'm' => goto State_64;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_47>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'K' | 'M' .. 'Z' | '_' | 'a' .. 'k' | 'm' .. 'z' => goto State_37;
               when 'L' | 'l' => goto State_65;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_48>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'G' | 'I' .. 'Z' | '_' | 'a' .. 'g' | 'i' .. 'z' => goto State_37;
               when 'H' | 'h' => goto State_66;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_49>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'B' | 'D' .. 'Z' | '_' | 'a' .. 'b' | 'd' .. 'z' => goto State_37;
               when 'C' | 'c' => goto State_67;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_50>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'B' .. 'H' | 'J' .. 'Z' | '_' | 'b' .. 'h' | 'j' .. 'z' => goto State_37;
               when 'A' | 'a' => goto State_68;
               when 'I' | 'i' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_51>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'M' | 'O' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_37;
               when 'N' | 'n' => goto State_70;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_52>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'O' | 'Q' .. 'Z' | '_' | 'a' .. 'o' | 'q' .. 'z' => goto State_37;
               when 'P' | 'p' => goto State_71;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_53>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'D' | 'F' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_37;
               when 'E' | 'e' => goto State_72;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_54>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'D' | 'F' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_37;
               when 'E' | 'e' => goto State_73;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_55>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'S' | 'U' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_37;
               when 'T' | 't' => goto State_74;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_56>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. Character_Type'Val (16#10ffff#) => goto State_30;
               when '"' => goto State_31;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_57>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Comment;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#10ffff#) => goto State_75;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_58>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'S' | 'U' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_37;
               when 'T' | 't' => goto State_76;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_59>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_All;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_60>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'D' | 'F' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_37;
               when 'E' | 'e' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_61>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_End;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_62>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'D' | 'F' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_37;
               when 'E' | 'e' => goto State_78;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_63>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_For;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_64>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'H' | 'J' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'z' => goto State_37;
               when 'I' | 'i' => goto State_79;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_65>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'K' | 'M' .. 'Z' | '_' | 'a' .. 'k' | 'm' .. 'z' => goto State_37;
               when 'L' | 'l' => goto State_80;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_66>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'D' | 'F' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_37;
               when 'E' | 'e' => goto State_81;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_67>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'J' | 'L' .. 'Z' | '_' | 'a' .. 'j' | 'l' .. 'z' => goto State_37;
               when 'K' | 'k' => goto State_82;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_68>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'F' | 'H' .. 'Z' | '_' | 'a' .. 'f' | 'h' .. 'z' => goto State_37;
               when 'G' | 'g' => goto State_83;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_69>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'U' | 'W' .. 'Z' | '_' | 'a' .. 'u' | 'w' .. 'z' => goto State_37;
               when 'V' | 'v' => goto State_84;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_70>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'B' .. 'Z' | '_' | 'b' .. 'z' => goto State_37;
               when 'A' | 'a' => goto State_85;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_71>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'D' | 'F' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_37;
               when 'E' | 'e' => goto State_86;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_72>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Use;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_73>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'M' | 'O' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_37;
               when 'N' | 'n' => goto State_87;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_74>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'G' | 'I' .. 'Z' | '_' | 'a' .. 'g' | 'i' .. 'z' => goto State_37;
               when 'H' | 'h' => goto State_88;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_75>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Comment;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#10ffff#) => goto State_75;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_76>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Q' | 'S' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_37;
               when 'R' | 'r' => goto State_89;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_77>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Case;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_78>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'M' | 'O' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_37;
               when 'N' | 'n' => goto State_90;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_79>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'S' | 'U' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_37;
               when 'T' | 't' => goto State_91;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_80>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Null;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_81>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Q' | 'S' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_37;
               when 'R' | 'r' => goto State_92;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_82>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'B' .. 'Z' | '_' | 'b' .. 'z' => goto State_37;
               when 'A' | 'a' => goto State_93;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_83>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'L' | 'N' .. 'Z' | '_' | 'a' .. 'l' | 'n' .. 'z' => goto State_37;
               when 'M' | 'm' => goto State_94;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_84>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'B' .. 'Z' | '_' | 'b' .. 'z' => goto State_37;
               when 'A' | 'a' => goto State_95;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_85>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'L' | 'N' .. 'Z' | '_' | 'a' .. 'l' | 'n' .. 'z' => goto State_37;
               when 'M' | 'm' => goto State_96;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_86>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Type;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_87>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_When;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_88>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_With;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_89>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'B' .. 'Z' | '_' | 'b' .. 'z' => goto State_37;
               when 'A' | 'a' => goto State_97;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_90>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'C' | 'E' .. 'Z' | '_' | 'a' .. 'c' | 'e' .. 'z' => goto State_37;
               when 'D' | 'd' => goto State_98;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_91>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'D' | 'F' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_37;
               when 'E' | 'e' => goto State_99;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_92>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'R' | 'T' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_37;
               when 'S' | 's' => goto State_100;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_93>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'F' | 'H' .. 'Z' | '_' | 'a' .. 'f' | 'h' .. 'z' => goto State_37;
               when 'G' | 'g' => goto State_101;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_94>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'B' .. 'Z' | '_' | 'b' .. 'z' => goto State_37;
               when 'A' | 'a' => goto State_102;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_95>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'S' | 'U' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_37;
               when 'T' | 't' => goto State_103;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_96>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'D' | 'F' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_37;
               when 'E' | 'e' => goto State_104;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_97>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'B' | 'D' .. 'Z' | '_' | 'a' .. 'b' | 'd' .. 'z' => goto State_37;
               when 'C' | 'c' => goto State_105;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_98>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'R' | 'T' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_37;
               when 'S' | 's' => goto State_106;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_99>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'C' | 'E' .. 'Z' | '_' | 'a' .. 'c' | 'e' .. 'z' => goto State_37;
               when 'D' | 'd' => goto State_107;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_100>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Others;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_101>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'D' | 'F' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_37;
               when 'E' | 'e' => goto State_108;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_102>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Pragma;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_103>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'D' | 'F' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_37;
               when 'E' | 'e' => goto State_109;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_104>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'R' | 'T' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_37;
               when 'S' | 's' => goto State_110;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_105>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'S' | 'U' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_37;
               when 'T' | 't' => goto State_111;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_106>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Extends;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_107>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Limited;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_108>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Package;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_109>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Private;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_110>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Renames;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_111>>

               Match_Index := Index - 1;
               Match_Kind := Gpr_Abstract;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_37;

            when others =>

               goto Stop;
            end case;
         end;


      <<Stop>>
      --  We end up here as soon as the currently analyzed character was not
      --  accepted by any transitions from the current state. Two cases from
      --  there:

      if Match_Index = 0 then
         --  We haven't found a match. Just create an error token and plan to
         --  start a new token at the next character.
         if Index > Self.Input_Last then
            Token := (Gpr_Termination, Index, Index - 1);
            Self.Has_Next := False;
         else
            Token := (Gpr_Lexing_Failure, First_Index, First_Index);
         end if;

      elsif Match_Ignore then
         --  We found a match. It must be ignored: resume lexing to start right
         --  after the matched text.
         First_Index := Match_Index + 1;
         goto Start;

      else
         --  We found a match for which we must emit a token
         Token := (Match_Kind, First_Index, Match_Index);
      end if;

      Self.Last_Token := Token;
      if not Is_Trivia (Token.Kind) then
         Self.Last_Token_Kind := Token.Kind;
      end if;
   end Next_Token;

end Gpr_Parser.Lexer_State_Machine;
