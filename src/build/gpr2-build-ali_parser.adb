--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Exceptions;
with Ada.Strings.Fixed;

with GNATCOLL.Buffer;
with GNATCOLL.OS.FS;
with GNATCOLL.Traces;

package body GPR2.Build.ALI_Parser is

   Scan_ALI_Error : exception;

   package GB renames GNATCOLL.Buffer;
   package GT renames GNATCOLL.Traces;

   Traces : constant GT.Trace_Handle :=
     GT.Create ("GPR.BUILD.ALI_PARSER", GNATCOLL.Traces.Off);

   type Unit_Header is record
      Name       : Unbounded_String;
      Part       : Spec_Body;
      Kind       : Library_Unit_Kind := Unknown;
      Attributes : Unit_Attributes := (others => False);
   end record;

   function Parse_Unit_Header
     (Reader : in out GB.Reader; EOL : in out Boolean) return Unit_Header;

   package IO is

      type Token_Mode is
        (Whitespace_Separated, Double_Space_Separated, Rest_Of_Line);

      function Get_Token
        (File : in out GB.Reader;
         EOL  : out Boolean;
         Mode : Token_Mode := Whitespace_Separated) return String;
      --  Return the next token. Mode controls unquoted token delimiters:
      --    Whitespace_Separated: a space, tab, or line end.
      --    Double_Space_Separated: two spaces, a tab, or line end.
      --    Rest_Of_Line: line end only.
      --  Quoted tokens retain spaces and tabs regardless of Mode. Return ""
      --  when the line ends before a token starts. Set EOL at line end or EOF.

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
         EOL  : out Boolean;
         Mode : Token_Mode := Whitespace_Separated) return String
      is
         subtype End_Of_Line is Character
           with Static_Predicate =>
             End_Of_Line in ASCII.CR | ASCII.LF | ASCII.EOT;
         subtype Delimiter is Character
           with Static_Predicate => Delimiter in ASCII.HT | End_Of_Line;
         subtype Delimiter_Or_Space is Character
           with Static_Predicate => Delimiter_Or_Space in Delimiter | ' ';

         function Get_Quoted_Word return String;
         function Get_Word return String;

         ---------------------
         -- Get_Quoted_Word --
         ---------------------

         function Get_Quoted_Word return String is
            C     : Character := ASCII.NUL;
            First : Long_Long_Integer := GB.Current_Position (File);
            Last  : Long_Long_Integer := First;

            QN : Natural := 1;
            --  Number of quotes seen
         begin
            --  Skip the first double quote

            First := First + 1;
            loop

               if not GB.Next (File, C) then
                  raise Scan_ALI_Error
                    with
                      "end of file encountered during the parsing of quoted"
                      & " word '"
                      & GB.Token (File, First, Last)
                      & "'";
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

                           Tok_Idx := Tok_Idx + 1;
                           Result_Idx := Result_Idx + 1;
                        end loop;

                        return Result (Result'First .. Result_Idx - 1);
                     end;

                  else
                     --  Had an ending quote followed by regular characters:
                     --  error !

                     raise Scan_ALI_Error
                       with
                         "wrong quoted format of '"
                         & GB.Token (File, First, Last)
                         & ''';
                  end if;

               elsif C in ASCII.LF then
                  raise Scan_ALI_Error
                    with
                      "wrong quoted format of '"
                      & GB.Token (File, First, Last)
                      & ''';
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
                  EOL := True;
                  return GB.Token (File, First, Last);
               end if;

               --  Older ALIs delimit unquoted filenames with two spaces

               if Mode = Double_Space_Separated and then C = ' ' then
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

               if (Mode = Whitespace_Separated
                   and then C in Delimiter_Or_Space)
                 or else
                   (Mode = Double_Space_Separated and then C in Delimiter)
                 or else (Mode = Rest_Of_Line and then C in End_Of_Line)
               then
                  EOL := C in End_Of_Line;
                  exit;
               end if;

               Last := Last + 1;
            end loop;

            return GB.Token (File, First, Last);
         end Get_Word;

         C : Character := ASCII.NUL;

      begin
         EOL := False;

         Read_Token :
         loop
            if not GB.Next (File, C) then
               EOL := True;
               return "";
            end if;

            if C in End_Of_Line then
               EOL := True;
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

            if Header not in ASCII.CR | ASCII.LF then
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

   ------------
   -- Create --
   ------------

   function Create
     (Path : GPR2.Path_Name.Object) return Object is
   begin
      return Self : Object do
         Self.Path := Path;
      end return;
   end Create;

   -----------
   -- Parse --
   -----------

   function Parse (Self : in out Object) return Boolean is
      procedure Parse_Dep (Reader : in out GB.Reader);
      --  Parse a D line to extract the source and unit part, when available

      procedure Parse_With (Reader : in out GB.Reader);
      --  Parse the source file name of the current dependency line

      EOL    : Boolean := False;
      Cur_SB : Spec_Body := U_Spec;

      ---------------
      -- Parse_Dep --
      ---------------

      procedure Parse_Dep (Reader : in out GB.Reader) is
      begin
         if not GB.Check (Reader, " ") then
            raise Scan_ALI_Error
              with "space expected after the 'D'" & " dependency character";
         end if;

         declare
            Source_File : constant String :=
              IO.Get_Token (Reader, EOL, Mode => IO.Double_Space_Separated);
         begin
            if Source_File = "" then
               raise Scan_ALI_Error with "missed dependency source file";
            end if;

            GPR2.Build.Dependencies.Include
              (Self.Dependencies, Filename_Type (Source_File));

            if EOL or else IO.Get_Token (Reader, EOL) = "" then
               raise Scan_ALI_Error with "missing dependency timestamp";
            end if;

            if EOL or else IO.Get_Token (Reader, EOL) = "" then
               raise Scan_ALI_Error with "missing dependency checksum";
            end if;

            if EOL then
               return;
            end if;

            declare
               Unit_Name : constant String := IO.Get_Token (Reader, EOL);
            begin
               if Unit_Name = "" then
                  return;
               elsif Unit_Name'Length < 3
                 or else Unit_Name (Unit_Name'Last) not in 's' | 'b'
                 or else Unit_Name (Unit_Name'Last - 1) /= '%'
               then
                  --  Older ALI formats may omit the unit kind or use a
                  --  separate-unit name. Keep the filename dependency.
                  return;
               end if;

               GPR2.Build.Dependencies.Include
                 (Self.Dependencies, Filename_Type (Source_File),
                  (Name_Length => Unit_Name'Length - 2,
                   Name        =>
                     Name_Type
                       (Unit_Name (Unit_Name'First .. Unit_Name'Last - 2)),
                   Part        =>
                     (if Unit_Name (Unit_Name'Last) = 's'
                      then S_Spec else S_Body)));
            end;
         end;
      end Parse_Dep;

      ----------------
      -- Parse_With --
      ----------------

      procedure Parse_With (Reader : in out GB.Reader) is
      begin
         if not GB.Check (Reader, " ") then
            raise Scan_ALI_Error
              with "space expected after the 'W'" & " withed unit character";
         end if;

         --  ??? Not backward compatible with ali files produced by
         --  the oldest compilers.

         declare
            Unit_Name : constant String := IO.Get_Token (Reader, EOL);
            --  EOL is True here exactly when Unit_Name was the last token
            --  on this line. The compiler recorded a bare "W/Y/Z unit_name%s")
            Has_File  : constant Boolean := not EOL;
         begin
            if Unit_Name = "" then
               raise Scan_ALI_Error with "missed withed unit name";
            end if;

            if Unit_Name'Length >= 3
              and then Unit_Name (Unit_Name'Last) in 's' | 'b'
              and then Unit_Name (Unit_Name'Last - 1) = '%'
            then
               declare
                  Name : constant Name_Type :=
                           Name_Type
                             (Unit_Name
                                (Unit_Name'First .. Unit_Name'Last - 2));
               begin
                  if Cur_SB = U_Spec then
                     Self.Spec_Imports.Include (Name);
                  else
                     Self.Body_Imports.Include (Name);
                  end if;

                  if not Has_File then
                     Self.Unresolved_Withs.Include (Name);
                  end if;
               end;
            else
               raise Scan_ALI_Error
                 with "withed unit name does not end with '%s'";
            end if;
         end;
      end Parse_With;

   begin
      if Self.Is_Parsed then
         return True;
      end if;

      declare
         Reader : GB.Reader := GB.Open (Self.Path.String_Value);
         Word   : Character := ASCII.NUL;
         Header : Unit_Header;
      begin

         --  Clear the object, to remove potential obsolete values
         Self.Reset;

         --  Only the dependencies lines "D" are of interest, as they contain
         --  dependencies source names.

         loop
            if not EOL then
               IO.Next_Line (Reader, Word);
            elsif not GB.Next (Reader, Word) then
               exit;
            end if;

            EOL := False;

            case Word is
               when ASCII.NUL =>
                  exit;

               when 'D' =>
                  Parse_Dep (Reader);

               when 'L' =>
                  declare
                     Value : constant String :=
                       IO.Get_Token (Reader, EOL, Mode => IO.Rest_Of_Line);
                  begin
                     Self.Linker_Options.Append (Value);
                  end;

               when 'U' =>
                  Header := Parse_Unit_Header (Reader, EOL);
                  Cur_SB := Header.Part;

                  Self.Units.Append
                    (Unit_Info'
                       (Name_Length => Length (Header.Name),
                        Name        => Name_Type (To_String (Header.Name)),
                        Part        => Header.Part,
                        Kind        => Header.Kind,
                        Attributes  => Header.Attributes));

                  if Cur_SB = U_Spec then
                     Self.Needs_Body :=
                       Header.Attributes (Body_Needed_For_SAL);
                  end if;

               when 'V' =>
                  declare
                     Version : constant String :=
                       IO.Get_Token (Reader, EOL, Mode => IO.Rest_Of_Line);
                     Start   : Natural;
                  begin
                     for J in Version'Range loop
                        if Version (J) = 'v' then
                           Start := J;
                        elsif J = Version'Last then
                           Self.Version := +Version (Start .. J);
                           exit;
                        elsif Version (J) = '"' then
                           Self.Version := +Version (Start .. J - 1);
                           exit;
                        end if;
                     end loop;
                  end;

               when 'W' | 'Y' | 'Z' =>
                  --  'W' = with
                  --  'Y' = limited_with
                  --  'Z' = with only reachable via a generic instantiation
                  --  All three are real closure dependencies.
                  Parse_With (Reader);

               when 'X' =>
                  --  nothing to do after the first X line: internal cross
                  --  references
                  exit;

               when others =>
                  null;
            end case;
         end loop;

         GB.Finalize (Reader);
         Self.Is_Parsed := True;
         return True;
      exception
         when E : others =>
            GNATCOLL.Traces.Trace
            (Traces,
               "ALI parser error: " & Ada.Exceptions.Exception_Message (E));
            GB.Finalize (Reader);
            return False;
      end;
   exception
      when GB.Invalid_Reader =>
         GNATCOLL.Traces.Trace
              (Traces, "File " & Self.Path.String_Value & " is invalid," &
               " aborting parsing");
         return False;
   end Parse;

   -----------------------
   -- Parse_Unit_Header --
   -----------------------

   function Parse_Unit_Header
     (Reader : in out GB.Reader; EOL : in out Boolean) return Unit_Header
   is
      R : Unit_Header := (others => <>);
   begin
      if not GB.Check (Reader, " ") then
         raise Scan_ALI_Error
           with "space expected after the 'U'" & " withed unit character";
      end if;

      declare
         Unit_Name : constant String := IO.Get_Token (Reader, EOL);
      begin
         if Unit_Name = "" then
            raise Scan_ALI_Error with "missed unit name";
         end if;

         if Unit_Name'Length < 3
           or else Unit_Name (Unit_Name'Last) not in 's' | 'b'
           or else Unit_Name (Unit_Name'Last - 1) /= '%'
         then
            raise Scan_ALI_Error
              with "withed unit name does not end with '%s'";
         end if;

         if Unit_Name (Unit_Name'Last) = 's' then
            R.Part := U_Spec;
         else
            R.Part := U_Body;
         end if;

         R.Name := To_Unbounded_String
           (Unit_Name (Unit_Name'First .. Unit_Name'Last - 2));
      end;

      --  Read the attributes at end of line
      declare
         S : constant String :=
           IO.Get_Token (Reader, EOL, Mode => IO.Double_Space_Separated)
         with Unreferenced;
         --  Source
         F : constant String :=
           IO.Get_Token (Reader, EOL, Mode => IO.Rest_Of_Line);
         --  Checksum & attributes
         I : Positive := F'First;
      begin
         --  Skip checksum

         while I <= F'Last
           and then F (I) not in ' ' | ASCII.HT | ASCII.LF
         loop
            I := I + 1;
         end loop;

         --  Read attributes

         while I < F'Last loop
            while I < F'Last
              and then F (I) in ' ' | ASCII.HT | ASCII.LF
            loop
               I := I + 1;
            end loop;

            exit when I >= F'Last;

            case F (I) is
               when 'B' =>
                  case F (I + 1) is
                     when 'N' =>
                        R.Attributes (Body_Needed_For_SAL) := True;

                     when 'D' =>
                        R.Attributes (Elaborate_Body_Desirable) := True;

                     when others =>
                        null;
                  end case;

               when 'D' =>
                  case F (I + 1) is
                     when 'E' =>
                        R.Attributes (Dynamic_Elab) := True;

                     when others =>
                        null;
                  end case;

               when 'E' =>
                  case F (I + 1) is
                     when 'B' =>
                        R.Attributes (Elaborate_Body) := True;

                     when 'E' =>
                        R.Attributes (Set_Elab_Entity) := True;

                     when others =>
                        null;
                  end case;

               when 'G' =>
                  case F (I + 1) is
                     when 'E' =>
                        R.Attributes (Is_Generic) := True;

                     when others =>
                        null;
                  end case;

               when 'I' =>
                  case F (I + 1) is
                     when 'S' =>
                        R.Attributes (Init_Scalars) := True;

                     when others =>
                        null;
                  end case;

               when 'N' =>
                  case F (I + 1) is
                     when 'E' =>
                        R.Attributes (No_Elab) := True;

                     when others =>
                        null;
                  end case;

               when 'P' =>
                  case F (I + 1) is
                     when 'F' =>
                        R.Attributes (Has_Finalizer) := True;

                     when 'R' =>
                        R.Attributes (Preelab) := True;

                     when 'U' =>
                        R.Attributes (Pure) := True;

                     when 'K' =>
                        R.Kind := Is_Package;

                     when others =>
                        null;
                  end case;

               when 'R' =>
                  case F (I + 1) is
                     when 'A' =>
                        R.Attributes (Has_RACW) := True;

                     when 'C' =>
                        R.Attributes (RCI) := True;

                     when 'T' =>
                        R.Attributes (Remote_Types) := True;

                     when others =>
                        null;
                  end case;

               when 'S' =>
                  case F (I + 1) is
                     when 'E' =>
                        R.Attributes (Serious_Errors) := True;

                     when 'P' =>
                        R.Attributes (Shared_Passive) := True;

                     when 'U' =>
                        R.Kind := Is_Subprogram;

                     when others =>
                        null;
                  end case;

               when others =>
                  null;
            end case;

            I := I + 2;
         end loop;
      end;

      return R;
   end Parse_Unit_Header;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out Object) is
   begin
      Self := (Path => Self.Path, others => <>);
   end Reset;

   --------------
   -- Switches --
   --------------

   function Switches
     (ALI_File : GPR2.Path_Name.Object) return GPR2.Containers.Value_List
   is
      Result      : GPR2.Containers.Value_List;
      Reader      : GB.Reader := GB.Open (ALI_File.String_Value);
      Word        : Character := ASCII.NUL;
      In_Switches : Boolean := False;
      EOL         : Boolean := False;
   begin
      loop
         IO.Next_Line (Reader, Word);

         if not In_Switches and then Word = 'A' then
            In_Switches := True;
         elsif In_Switches and then Word /= 'A' then
            exit;
         end if;

         if In_Switches then
            Result.Append
              (IO.Get_Token (Reader, EOL, Mode => IO.Rest_Of_Line));
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

   -------------
   -- Version --
   -------------

   function Version (ALI_File : GPR2.Path_Name.Object) return String is
      use GNATCOLL.OS.FS;

      File   : File_Descriptor;
      Line   : String (1 .. 100);
      Length : Natural
      with Unreferenced;
      Last   : Natural;
      Start  : Natural;
   begin
      if not ALI_File.Exists then
         return "";
      end if;

      File := Open (ALI_File.String_Value);
      Length := Read (File, Line);
      Close (File);

      Last := GNATCOLL.Utils.Line_End (Line, 1);

      Start := Ada.Strings.Fixed.Index (Line (1 .. Last), " v");

      if Start /= 0 then
         return (Line (Start + 2 .. Last - 1));
      end if;

      return "";
   end Version;

end GPR2.Build.ALI_Parser;
