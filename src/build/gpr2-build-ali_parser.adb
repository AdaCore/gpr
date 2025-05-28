--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Exceptions;
with GNATCOLL.Buffer;
with GNATCOLL.Traces;
with Ada.Text_IO;
with Ada.Strings.Fixed;

package body GPR2.Build.ALI_Parser is

   Scan_ALI_Error : exception;

   package GB renames GNATCOLL.Buffer;
   package GT renames GNATCOLL.Traces;

   Traces : constant GT.Trace_Handle :=
              GT.Create ("GPR.BUILD.ALI_PARSER", GNATCOLL.Traces.Off);

   package IO is

      function Get_Token
        (File : in out GB.Reader;
         EOL  :    out Boolean) return String;
      --  Return next token available. The result is "" if LF or EOF are
      --  encountered before being able to read a token.

      procedure Next_Line (File : in out GB.Reader; Header : out Character);
      --  Skip to the next (non-empty) line and sets Header.
      --  If no lines have been read before this call, then skip to the
      --  first non-empty and set Header.
      --  If no such line exists, Header is set to NUL.

   end IO;

   --------
   -- IO --
   --------

   package body IO is

      ---------------
      -- Get_Token --
      ---------------

      function Get_Token
        (File : in out GB.Reader;
         EOL  :    out Boolean) return String
      is
         subtype End_Of_Line is Character
           with Static_Predicate =>
             End_Of_Line in ASCII.CR | ASCII.LF | ASCII.EOT;
         subtype Delimiter is Character
           with Static_Predicate =>
             Delimiter in ASCII.HT | End_Of_Line;
         subtype Delimiter_Or_Space is Character
           with Static_Predicate =>
             Delimiter_Or_Space in Delimiter | ' ';

         function Get_Quoted_Word return String;
         function Get_Word return String;

         ---------------------
         -- Get_Quoted_Word --
         ---------------------

         function Get_Quoted_Word return String is
            C     : Character := ASCII.NUL;
            First : Long_Long_Integer := GB.Current_Position (File);
            Last  : Long_Long_Integer := First;

            QN    : Natural   := 1;
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

                     EOL := C in End_Of_Line;

                     declare
                        Token               : constant String :=
                                                GB.Token (File, First, Last);
                        Result              : String := Token;
                        Tok_Idx, Result_Idx : Natural := Token'First;
                     begin
                        while Tok_Idx <= Token'Last loop
                           Result (Result_Idx) := Token (Tok_Idx);

                           if Token (Tok_Idx) = '"' then
                              --  Skip the second double quote
                              Tok_Idx := Tok_Idx + 1;
                           end if;

                           Tok_Idx    := Tok_Idx + 1;
                           Result_Idx := Result_Idx + 1;
                        end loop;

                        return Result (Result'First .. Result_Idx - 1);
                     end;

                  else
                     --  Had an ending quote followed by regular characters:
                     --  error !

                     raise Scan_ALI_Error
                       with "wrong quoted format of '"
                          & GB.Token (File, First, Last) & ''';
                  end if;

               elsif C in ASCII.LF then
                  raise Scan_ALI_Error
                    with "wrong quoted format of '"
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
            C                       : Character := ASCII.NUL;
            First                   : constant Long_Long_Integer :=
                                        GB.Current_Position (File);
            Last                    : Long_Long_Integer := First;
            Previous_Token_Is_Space : Boolean := False;
         begin
            loop
               if not GB.Next (File, C) then
                  return GB.Token (File, First, Last);
               end if;

               --  Two consecutive space means end of token
               if C = ' ' then
                  if Previous_Token_Is_Space then
                     Last := Last - 1;
                     EOL := False;
                     exit;
                  else
                     Previous_Token_Is_Space := True;
                  end if;

               else
                  Previous_Token_Is_Space := False;
               end if;

               if C in Delimiter then
                  EOL := C in End_Of_Line;
                  exit;
               end if;

               Last := Last + 1;
            end loop;

            return GB.Token (File, First, Last);
         end Get_Word;

         C  : Character := ASCII.NUL;

      begin
         EOL := False;

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

      procedure Next_Line (File : in out GB.Reader; Header : out Character) is
      begin
         if File.Is_End_Of_Data then
            Header := ASCII.NUL;
            return;
         end if;

         if File.Current_Position = 0 then

            --  No character have been read by the reader

            if not GB.Next (File, Header) then
               Header := ASCII.NUL;
               return;
            end if;

         else
            Header := File.Current_Char;

            loop
               exit when Header = ASCII.LF;

               if not GB.Next (File, Header) then
                  Header := ASCII.NUL;
                  return;
               end if;
            end loop;
         end if;

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

   function Dependencies
     (ALI_File  : GPR2.Path_Name.Object;
      Dep_Names : in out GPR2.Containers.Filename_Set) return Boolean
   is
      procedure Parse_Dep (Reader : in out GB.Reader);
      --  Parse a D line to extract the source

      procedure Parse_With (Reader : in out GB.Reader);
      --  Parse a W line to extract the source

      EOL : Boolean := False;

      ---------------
      -- Parse_Dep --
      ---------------

      procedure Parse_Dep (Reader : in out GB.Reader) is
      begin
         if not GB.Check (Reader, " ") then
            raise Scan_ALI_Error with "space expected after the 'D'"
              & " dependency character";
         end if;

         declare
            Source_File : constant String := IO.Get_Token (Reader, EOL);
         begin
            if Source_File = "" then
               raise Scan_ALI_Error with "missed dependency source file";
            end if;

            Dep_Names.Include (Filename_Type (Source_File));
         end;
      end Parse_Dep;

      ---------------
      -- Parse_Dep --
      ---------------

      procedure Parse_With (Reader : in out GB.Reader) is
      begin
         if not GB.Check (Reader, " ") then
            raise Scan_ALI_Error with "space expected after the 'W'"
              & " withed unit character";
         end if;

         --  ??? Not backward compatible with ali files produced by
         --  the oldest compilers.

         declare
            Unit_Name : constant String := IO.Get_Token (Reader, EOL);
            Filename  : constant String :=
                          (if not EOL then IO.Get_Token (Reader, EOL) else "");
         begin
            if Unit_Name = "" then
               raise Scan_ALI_Error with "missed withed unit name";
            end if;

            if Filename /= "" then
               Dep_Names.Include (Filename_Type (Filename));
            end if;
         end;
      end Parse_With;

      Reader : GB.Reader :=  GB.Open (String (ALI_File.Value));
      Word   : Character := ASCII.NUL;
   begin
      --  Only the dependencies lines "D" are of interest, as they contain
      --  dependencies source names.

      loop
         if not EOL then
            IO.Next_Line (Reader, Word);
         elsif not GB.Next (Reader, Word) then
            return True;
         end if;

         EOL := False;

         case Word is
            when ASCII.NUL   =>
               exit;

            when 'D' =>
               Parse_Dep (Reader);

            when 'W' | 'Y' =>
                  Parse_With (Reader);

            when 'X' =>
               exit;

            when others =>
               null;
         end case;
      end loop;

      GB.Finalize (Reader);

      return True;

   exception
      when E : others =>
         GNATCOLL.Traces.Trace
           (Traces,
            "ALI parser error: " & Ada.Exceptions.Exception_Message (E));
         GB.Finalize (Reader);

         return False;
   end Dependencies;

   -------------
   -- Imports --
   -------------

   function Imports
     (ALI_File : GPR2.Path_Name.Object;
      Imports  : in out GPR2.Containers.Name_Set) return Boolean
   is

      procedure Parse_With (Reader : in out GB.Reader);
      --  Parse the source file name of the current dependency line

      EOL : Boolean := False;

      ---------------
      -- Parse_Dep --
      ---------------

      procedure Parse_With (Reader : in out GB.Reader) is
      begin
         if not GB.Check (Reader, " ") then
            raise Scan_ALI_Error
              with "space expected after the 'W'"
                 & " withed unit character";
         end if;

         --  ??? Not backward compatible with ali files produced by
         --  the oldest compilers.

         declare
            Unit_Name : constant String := IO.Get_Token (Reader, EOL);
         begin
            if Unit_Name = "" then
               raise Scan_ALI_Error with "missed withed unit name";
            end if;

            if Unit_Name'Length >= 3
              and then Unit_Name (Unit_Name'Last) in 's' | 'b'
              and then Unit_Name (Unit_Name'Last - 1) = '%'
            then
               Imports.Include
                 (Name_Type
                    (Unit_Name (Unit_Name'First .. Unit_Name'Last - 2)));
            else
               raise Scan_ALI_Error with
                 "withed unit name does not end with '%s'";
            end if;
         end;
      end Parse_With;

      Reader : GB.Reader :=  GB.Open (String (ALI_File.Value));
      Word   : Character := ASCII.NUL;

   begin
      --  Only the dependencies lines "D" are of interest, as they contain
      --  dependencies source names.

      loop
         if not EOL then
            IO.Next_Line (Reader, Word);
         elsif not GB.Next (Reader, Word) then
            return True;
         end if;

         EOL := False;

         case Word is
            when ASCII.NUL =>
               exit;

            when 'D' =>
               exit;
               --  ??? Add other cases that are after the withed units

            when 'W' | 'Y' =>
               Parse_With (Reader);

            when others =>
               null;
         end case;
      end loop;

      GB.Finalize (Reader);

      return True;
   exception
      when E : others =>
         GNATCOLL.Traces.Trace
           (Traces,
            "ALI parser error: " & Ada.Exceptions.Exception_Message (E));
         GB.Finalize (Reader);

         return False;
   end Imports;

   --------------
   -- Switches --
   --------------

   function Switches
     (ALI_File : GPR2.Path_Name.Object) return GPR2.Containers.Value_List
   is
      Result      : GPR2.Containers.Value_List;
      Reader      : GB.Reader :=  GB.Open (ALI_File.String_Value);
      Word        : Character := ASCII.NUL;
      In_Switches : Boolean := False;
      EOL         : Boolean := False;
   begin
      loop
         if not EOL then
            IO.Next_Line (Reader, Word);
         elsif not GB.Next (Reader, Word) then
            return Result;
         end if;

         EOL := False;

         if not In_Switches and then Word = 'A' then
            In_Switches := True;
         elsif In_Switches and then Word /= 'A' then
            exit;
         end if;

         if In_Switches then
            Result.Append (IO.Get_Token (Reader, EOL));
         end if;
      end loop;

      GB.Finalize (Reader);

      return Result;

   exception
      when E : others =>
         GNATCOLL.Traces.Trace
           (Traces,
            "ALI parser error: " & Ada.Exceptions.Exception_Message (E));
         GB.Finalize (Reader);

         return Result;
   end Switches;

   ----------------
   -- Unit_Flags --
   ----------------

   function Unit_Flags
     (ALI_File : GPR2.Path_Name.Object) return Units_Flags_Set
   is

      procedure Parse_Flags (Reader : in out GB.Reader);
      --  Parse the source file name of the current dependency line

      R   : Units_Flags_Set := (others => (others => False));
      EOL : Boolean := False;

      -----------------
      -- Parse_Flase --
      -----------------

      procedure Parse_Flags (Reader : in out GB.Reader) is
         pragma Warnings (Off);
      begin
         if not GB.Check (Reader, " ") then
            raise Scan_ALI_Error
              with "space expected after the 'U'"
                 & " withed unit character";
         end if;

         declare
            Unit_Name : constant String := IO.Get_Token (Reader, EOL);
         begin
            if Unit_Name = "" then
               raise Scan_ALI_Error with "missed unit name";
            end if;

            if Unit_Name'Length >= 3
              and then Unit_Name (Unit_Name'Last) in 's' | 'b'
              and then Unit_Name (Unit_Name'Last - 1) = '%'
            then
               --  Read the flags at end of line
               declare
                  SB : constant Spec_Body :=
                         (if Unit_Name (Unit_Name'Last) = 's'
                          then U_Spec
                          else U_Body);
                  S  : constant String := IO.Get_Token (Reader, EOL)
                    with Unreferenced;
                  --  Source
                  F  : constant String := IO.Get_Token (Reader, EOL)
                    with Unreferenced;
                  --  Checksum & flags
                  I  : Positive := F'First;
               begin
                  --  Skip checksum

                  while F (I) not in ' ' | ASCII.HT | ASCII.LF loop
                     I := I + 1;
                  end loop;

                  I := I + 1;

                  --  Read flags

                  while I < F'Last loop
                     case F (I) is
                        when 'B' =>
                           case F (I + 1) is
                              when 'N' =>
                                 R (SB) (Body_Needed_For_SAL) := True;
                              when 'D' =>
                                 R (SB) (Elaborate_Body_Desirable) := True;
                              when others =>
                                 null;
                           end case;

                        when 'D' =>
                           case F (I + 1) is
                              when 'E' =>
                                 R (SB) (Dynamic_Elab) := True;
                              when others =>
                                 null;
                           end case;

                        when 'E' =>
                           case F (I + 1) is
                              when 'B' =>
                                 R (SB) (Elaborate_Body) := True;
                              when 'E' =>
                                 R (SB) (Set_Elab_Entity) := True;
                              when others =>
                                 null;
                           end case;

                        when 'G' =>
                           case F (I + 1) is
                              when 'E' =>
                                 R (SB) (Is_Generic) := True;
                              when others =>
                                 null;
                           end case;

                        when 'I' =>
                           case F (I + 1) is
                              when 'S' =>
                                 R (SB) (Init_Scalars) := True;
                              when others =>
                                 null;
                           end case;

                        when 'N' =>
                           case F (I + 1) is
                              when 'E' =>
                                 R (SB) (No_Elab) := True;
                              when others =>
                                 null;
                           end case;

                        when 'P' =>
                           case F (I + 1) is
                              when 'F' =>
                                 R (SB) (Has_Finalizer) := True;
                              when 'R' =>
                                 R (SB) (Preelab) := True;
                              when 'U' =>
                                 R (SB) (Pure) := True;
                              when others =>
                                 null;
                           end case;

                        when 'R' =>
                           case F (I + 1) is
                              when 'A' =>
                                 R (SB) (Has_RACW) := True;
                              when 'C' =>
                                 R (SB) (RCI) := True;
                              when 'T' =>
                                 R (SB) (Remote_Types) := True;
                              when others =>
                                 null;
                           end case;

                        when 'S' =>
                           case F (I + 1) is
                              when 'E' =>
                                 R (SB) (Serious_Errors) := True;
                              when 'P' =>
                                 R (SB) (Shared_Passive) := True;
                              when others =>
                                 null;
                           end case;

                        when others =>
                           null;
                     end case;

                     I := I + 2;
                  end loop;
               end;

            else
               raise Scan_ALI_Error with
                 "withed unit name does not end with '%s'";
            end if;
         end;
      end Parse_Flags;

      Reader : GB.Reader := GB.Open (String (ALI_File.Value));
      Word   : Character := ASCII.NUL;

   begin
      --  Only the units lines "U" are of interest, as they contain
      --  flags for source spec/body.

      loop
         if not EOL then
            IO.Next_Line (Reader, Word);
         elsif not GB.Next (Reader, Word) then
            return R;
         end if;

         EOL := False;

         case Word is
            when ASCII.NUL =>
               exit;

            when 'D' =>
               exit;
               --  Units are before dependencies

            when 'U' =>
               Parse_Flags (Reader);

            when others =>
               null;
         end case;
      end loop;

      GB.Finalize (Reader);

      return R;
   exception
      when E : others =>
         GNATCOLL.Traces.Trace
           (Traces,
            "ALI parser error: " & Ada.Exceptions.Exception_Message (E));
         GB.Finalize (Reader);

         return R;
   end Unit_Flags;

   -------------
   -- Version --
   -------------

   function Version (ALI_File : GPR2.Path_Name.Object) return String is
      use Ada.Text_IO;

      File  : File_Type;
      Line  : String (1 .. 1_000);
      Last  : Natural;
      Start : Natural;
   begin
      if not ALI_File.Exists then
         return "";
      end if;

      Open
         (File,
         In_File,
         ALI_File.String_Value);
      Get_Line (File, Line, Last);
      Close (File);
      Start := Ada.Strings.Fixed.Index (Line (1 .. Last), " v");

      if Start /= 0 then
         return (Line (Start + 2 .. Last - 1));
      end if;

      return "";
   end Version;

end GPR2.Build.ALI_Parser;
