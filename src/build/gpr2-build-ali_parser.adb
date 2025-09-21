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

   type Unit_Flags_Kind is
     (Elaborate_Body_Desirable, --  BD
      Body_Needed_For_SAL,      --  BN
      Dynamic_Elab,             --  DE
      Elaborate_Body,           --  EB
      Set_Elab_Entity,          --  EE
      Is_Generic,               --  GE
      Init_Scalars,             --  IS
      No_Elab,                  --  NE
      Has_Finalizer,            --  PF
      Preelab,                  --  PR
      Pure,                     --  PU
      RCI,                      --  RC
      Remote_Types,             --  RT
      Has_RACW,                 --  RA
      Serious_Errors,           --  SE
      Shared_Passive);          --  SP

   type Unit_Flags_Set is array (Unit_Flags_Kind) of Boolean
   with Default_Component_Value => False;

   type Unit_Flags_Part is record
      SB    : Spec_Body;
      Flags : Unit_Flags_Set;
   end record;

   function Parse_Flags
     (Reader : in out GB.Reader; EOL : in out Boolean) return Unit_Flags_Part;

   package IO is

      function Get_Token
        (File : in out GB.Reader; EOL : out Boolean) return String;
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
        (File : in out GB.Reader; EOL : out Boolean) return String
      is
         subtype End_Of_Line is Character
         with
           Static_Predicate => End_Of_Line in ASCII.CR | ASCII.LF | ASCII.EOT;
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

         C : Character := ASCII.NUL;

      begin
         EOL := False;

         Read_Token :
         loop
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

   ------------
   -- Create --
   ------------

   function Create
     (Path : GPR2.Path_Name.Object; Parse : Boolean) return Object is
   begin
      return Self : Object do
         Self.Path := Path;
         if Parse then
            Self.Parse;
         end if;
      end return;
   end Create;

   -----------
   -- Parse --
   -----------

   procedure Parse (Self : in out Object) is
      procedure Parse_Dep (Reader : in out GB.Reader);
      --  Parse a D line to extract the source

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
            Source_File : constant String := IO.Get_Token (Reader, EOL);
         begin
            if Source_File = "" then
               raise Scan_ALI_Error with "missed dependency source file";
            end if;

            Self.Dependencies.Include (Filename_Type (Source_File));
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
            Filename  : constant String :=
              (if not EOL then IO.Get_Token (Reader, EOL) else "");
         begin
            if Unit_Name = "" then
               raise Scan_ALI_Error with "missed withed unit name";
            end if;

            if Unit_Name'Length >= 3
              and then Unit_Name (Unit_Name'Last) in 's' | 'b'
              and then Unit_Name (Unit_Name'Last - 1) = '%'
            then
               if Cur_SB = U_Spec then
                  Self.Spec_Imports.Include
                    (Name_Type
                       (Unit_Name (Unit_Name'First .. Unit_Name'Last - 2)));
               else
                  Self.Body_Imports.Include
                    (Name_Type
                       (Unit_Name (Unit_Name'First .. Unit_Name'Last - 2)));
               end if;
            else
               raise Scan_ALI_Error
                 with "withed unit name does not end with '%s'";
            end if;

            if Filename /= "" then
               Self.Dependencies.Include (Filename_Type (Filename));
            end if;
         end;
      end Parse_With;

      Reader : GB.Reader := GB.Open (Self.Path.String_Value);
      Word   : Character := ASCII.NUL;
      Flags  : Unit_Flags_Part;

   begin
      --  Clear the object, to remove potential obsolete values
      Self := (Path => Self.Path, others => <>);

      --  Only the dependencies lines "D" are of interest, as they contain
      --  dependencies source names.

      loop
         if not EOL then
            IO.Next_Line (Reader, Word);
         elsif not GB.Next (Reader, Word) then
            Self.Is_Parsed := True;

            return;
         end if;

         EOL := False;

         case Word is
            when ASCII.NUL =>
               exit;

            when 'D' =>
               Parse_Dep (Reader);

            when 'U' =>
               Flags := Parse_Flags (Reader, EOL);
               Cur_SB := Flags.SB;

               if Cur_SB = U_Spec then
                  Self.Needs_Body := Flags.Flags (Body_Needed_For_SAL);
               end if;

            when 'V' =>
               declare
                  Version : constant String := IO.Get_Token (Reader, EOL);
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

            when 'W' | 'Y' =>
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

   exception
      when E : others =>
         GNATCOLL.Traces.Trace
           (Traces,
            "ALI parser error: " & Ada.Exceptions.Exception_Message (E));
         GB.Finalize (Reader);

         Self.Is_Parsed := False;
         return;
   end Parse;

   -----------------
   -- Parse_Flase --
   -----------------

   function Parse_Flags
     (Reader : in out GB.Reader; EOL : in out Boolean) return Unit_Flags_Part
   is
      R : Unit_Flags_Part;
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
            R.SB := U_Spec;
         else
            R.SB := U_Body;
         end if;
      end;

      --  Read the flags at end of line
      declare
         S : constant String := IO.Get_Token (Reader, EOL)
         with Unreferenced;
         --  Source
         F : constant String := IO.Get_Token (Reader, EOL);
         --  Checksum & flags
         I : Positive := F'First;
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
                        R.Flags (Body_Needed_For_SAL) := True;

                     when 'D' =>
                        R.Flags (Elaborate_Body_Desirable) := True;

                     when others =>
                        null;
                  end case;

               when 'D' =>
                  case F (I + 1) is
                     when 'E' =>
                        R.Flags (Dynamic_Elab) := True;

                     when others =>
                        null;
                  end case;

               when 'E' =>
                  case F (I + 1) is
                     when 'B' =>
                        R.Flags (Elaborate_Body) := True;

                     when 'E' =>
                        R.Flags (Set_Elab_Entity) := True;

                     when others =>
                        null;
                  end case;

               when 'G' =>
                  case F (I + 1) is
                     when 'E' =>
                        R.Flags (Is_Generic) := True;

                     when others =>
                        null;
                  end case;

               when 'I' =>
                  case F (I + 1) is
                     when 'S' =>
                        R.Flags (Init_Scalars) := True;

                     when others =>
                        null;
                  end case;

               when 'N' =>
                  case F (I + 1) is
                     when 'E' =>
                        R.Flags (No_Elab) := True;

                     when others =>
                        null;
                  end case;

               when 'P' =>
                  case F (I + 1) is
                     when 'F' =>
                        R.Flags (Has_Finalizer) := True;

                     when 'R' =>
                        R.Flags (Preelab) := True;

                     when 'U' =>
                        R.Flags (Pure) := True;

                     when others =>
                        null;
                  end case;

               when 'R' =>
                  case F (I + 1) is
                     when 'A' =>
                        R.Flags (Has_RACW) := True;

                     when 'C' =>
                        R.Flags (RCI) := True;

                     when 'T' =>
                        R.Flags (Remote_Types) := True;

                     when others =>
                        null;
                  end case;

               when 'S' =>
                  case F (I + 1) is
                     when 'E' =>
                        R.Flags (Serious_Errors) := True;

                     when 'P' =>
                        R.Flags (Shared_Passive) := True;

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
   end Parse_Flags;

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
