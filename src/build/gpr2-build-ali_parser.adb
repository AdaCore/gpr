--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Text_IO;
with Ada.Exceptions;
with GNATCOLL.Buffer;
with GPR2.Message;
with GPR2.Source_Reference;

package body GPR2.Build.ALI_Parser is

   Scan_ALI_Error : exception;

   package GB renames GNATCOLL.Buffer;

   package IO is

      function Get_Token (File : in out GB.Reader) return String;
      --  Return next token available. The result is "" if LF or EOF are
      --  encountered before being able to read a token.

      procedure Next_Line (File : in out GB.Reader; Header : in out Character);
      --  Skip to the next (non-empty) line and sets Header.
      --  If no such line exists, Header is set to NUL.

   end IO;

   --------
   -- IO --
   --------

   package body IO is

      ---------------
      -- Get_Token --
      ---------------

      function Get_Token (File : in out GB.Reader) return String
      is

         subtype Delimiter is Character
           with Static_Predicate =>
             Delimiter in ASCII.HT | ASCII.CR | ASCII.LF | ASCII.EOT;
         subtype Delimiter_Or_Space is Character
           with Static_Predicate =>
             Delimiter_Or_Space in Delimiter | ' ';

         function Get_Quoted_Word return String;
         function Get_Word return String;

         ---------------------
         -- Get_Quoted_Word --
         ---------------------

         function Get_Quoted_Word return String is
            C  : Character := ASCII.NUL;
            First : Long_Long_Integer := GB.Current_Position (File);
            Last  : Long_Long_Integer := First;

            QN : Natural   := 1;
            --  Number of quotes seen
         begin
            --  Skip the first double quote

            First := First + 1;
            loop

               if not GB.Next (File, C) then
                  raise Scan_ALI_Error with
                     "end of file encountered during the parsing of quoted"
                     & " word '" & GB.Token (File, First, Last) & "'";
               end if;

               if C = '"' then
                  QN := QN + 1;

                  if QN = 3 then

                     --  Two quotes is escaped quote literal

                     Last := Last + 2;
                     QN := 1;
                  end if;

               elsif QN = 2 then
                  if C in Delimiter_Or_Space then

                     --  Had an ending quote followed by a delimiter: done
                     --  Two consecutive double quotes need to be replaced by
                     --  a single double quote.

                     declare
                        Token  : String := GB.Token (File, First, Last);
                        Result : String := Token;
                        Tok_Idx, Result_Idx : Natural := Token'First;
                     begin
                        while Tok_Idx <= Token'Last loop
                           Result (Result_Idx) := Token (Tok_Idx);

                           if Token (Tok_Idx) = '"' then

                              -- Skip the second double quote

                              Tok_Idx := Tok_Idx + 1;
                           end if;

                           Tok_Idx := Tok_Idx + 1;
                           Result_Idx := Result_Idx + 1;
                        end loop;
                        return Result (Result'First .. Result_Idx-1);
                     end;
                  else

                     --  Had an ending quote followed by regular characters:
                     --  error !

                     raise Scan_ALI_Error with
                       "wrong quoted format of '"
                       & GB.Token (File, First, Last) & ''';
                  end if;
               elsif C in ASCII.LF then
                  raise Scan_ALI_Error with
                    "wrong quoted format of '"
                    & GB.Token (File, First, Last) & ''';
               else
                  Last := Last + 1;
               end if;
            end loop;
         end Get_Quoted_Word;

         --------------
         -- Get_Word --
         --------------

         function Get_Word return String is
            C     : Character       := ASCII.NUL;
            First : constant Long_Long_Integer := GB.Current_Position (File);
            Last  : Long_Long_Integer         := First;
            Previous_Token_Is_Space : Boolean := False;
         begin
            loop

               if not GB.Next (File, C) then
                  return GB.Token (File, First, Last);
               end if;

               --  Two consecutive space means end of token
               if C = ' ' then
                  if Previous_Token_Is_Space then
                     exit;
                  else
                     Previous_Token_Is_Space := True;
                  end if;
               else
                  Previous_Token_Is_Space := False;
               end if;

               if C in Delimiter then
                  exit;
               end if;


               Last := Last + 1;
            end loop;

            return GB.Token (File, First, Last);
         end Get_Word;

         C  : Character := ASCII.NUL;

      begin
         Read_Token : loop

            if not GB.Next (File, C) then
               return "";
            end if;

            if C = ASCII.LF then
               return "";
            end if;

            if C not in Delimiter_Or_Space then
               if C = '"' then
                  return Get_Quoted_Word;
               else
                  return Get_Word;
               end if;
            end if;
         end loop Read_Token;
      end Get_Token;

      ---------------
      -- Next_Line --
      ---------------

      procedure Next_Line (File : in out GB.Reader; Header : in out Character)
      is
      begin
         loop
            if not GB.Next (File, Header) then
               Header := ASCII.NUL;

               return;
            end if;

            exit when Header = ASCII.LF;
         end loop;

         loop
            if not GB.Next (File, Header) then
               Header := ASCII.NUL;

               return;
            end if;

            exit when Header not in ASCII.CR | ASCII.LF;
         end loop;
      end Next_Line;
   end IO;

   ------------------
   -- Dependencies --
   ------------------

   procedure Dependencies
     (ALI_File  : GPR2.Path_Name.Object;
      Dep_Names : in out Dep_Vectors.Vector;
      Messages : in out GPR2.Log.Object)
   is

      procedure Parse_Dep (Reader : in out GB.Reader);
      --  Parse the source file name of the current dependency line

      ---------------
      -- Parse_Dep --
      ---------------

      procedure Parse_Dep (Reader : in out GB.Reader) is
         Char_After_D : Character;
      begin
         if not GB.Check (Reader, " ") then
            raise Scan_ALI_Error with "space expected after the 'D'"
            & " dependency character";
         end if;

         declare
            Source_File : constant String := IO.Get_Token (Reader);
         begin
            if Source_File = "" then
               raise Scan_ALI_Error with "missed dependency source file";
            end if;

            Dep_Names.Append(Source_File);
         end;
      end Parse_Dep;

      Reader : GB.Reader :=  GB.Open (String (ALI_File.Value));
   begin
      declare
         Word : Character := ASCII.NUL;
      begin

         --  Only the dependencies lines "D" are of interest, as they contain
         --  dependencies source names.

         loop
            IO.Next_Line (Reader, Word);

            case Word is
               when ASCII.NUL   =>
                  exit;

               when 'D' =>
                  Parse_Dep (Reader);

               when others =>
                  null;
            end case;
         end loop;

         GB.Finalize (Reader);

      exception
         when E : others =>

            Messages.Append
              (GPR2.Message.Create (
               GPR2.Message.Error,
               "ALI parser error: " & Ada.Exceptions.Exception_Message (E),
               GPR2.Source_Reference.Object
                 (GPR2.Source_Reference.Create (ALI_File.Value, 0, 0))));

            GB.Finalize (Reader);
      end;
   end Dependencies;
end GPR2.Build.ALI_Parser;
