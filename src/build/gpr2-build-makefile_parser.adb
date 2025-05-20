--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Exceptions;
with GNATCOLL.Buffer;
with GNATCOLL.Traces;

package body GPR2.Build.Makefile_Parser is

   Scan_Makefile_Error : exception;

   package GB renames GNATCOLL.Buffer;

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create
                ("GPR.BUILD.MAKEFILE_PARSER", GNATCOLL.Traces.Off);

   package IO is

      function Fetch_Dependency
        (File : in out GB.Reader) return String;
      --  Find a dependency in a line or a multi line if '\' is found.
      --  ": a.c b.c" or
      --  ": a.c \
      --  "b.c"
      --  will return "a.c" and subsequent calls will one-by-one return each
      --  source file ("b.c" in this example).
      --  Returns the source file.

      function Next_Object
        (File   : in out GB.Reader;
         Header :    out Character) return String;
      --  Find ": " or ":<ASCII.HT>" in the buffer File.
      --  Returns the object name or an empty string if there is no match.
      --  Sets the buffer current char in Header corresponding to the char at
      --  the index of the match. Sets Header to ASCII.NUL if there is no
      --  match.
   end IO;

   --------
   -- IO --
   --------

   package body IO is

      ----------------------
      -- Fetch_Dependency --
      ----------------------

      function Fetch_Dependency (File : in out GB.Reader) return String
      is
         C            : Character         := File.Current_Char;
         Start        : Long_Long_Integer := 0;

         EOL_Extra_Offset : Boolean := False;
         Escaped          : Boolean := False;
         Expected_Dep     : Boolean := False;
         Multi_Line       : Boolean := False;
         Skipping         : Boolean := False;
         Splited_Path     : Boolean := False;
         Started          : Boolean := False;

         Path         : Unbounded_String;
      begin
         loop
            if File.Current_Char in '\' then
               if File.Check ("" & ASCII.CR)
                 or else File.Check ("" & ASCII.LF)
               then
                  if not Skipping then
                     --  We found a '\' at the end of a line, we will expect to
                     --  find other dependencies in the next line.
                     Multi_Line := True;
                  end if;

               else
                  --  We have a '\' not at the end of the line, this means it's
                  --  either an escape character to take into account, or a
                  --  path separator for windows.

                  --  Note, since GPR2.On_Windows is evaluated statically
                  --  depending on the actual host, the if statement below is
                  --  optimized by the toolchain leading to a warning "this
                  --  code can never be executed". We thus need to kill the
                  --  warning.

                  pragma Warnings (Off, "this code can never be executed*");
                  if GPR2.On_Windows then
                     --  On Windows, a '\' is part of the path name,
                     --  except when it is followed by another '\' or ' '.
                     --  Although "\\" at the beginning of a path is valid.
                     if Started
                       and then (File.Check ("\") or else File.Check (" "))
                     then
                        --  If we started, raise the Splited_Path flag so we
                        --  know we have to fully construct the path rather
                        --  than fetching the File.Token entirely.
                        Splited_Path := True;

                        --  Construct a part of the path
                        Append
                          (Path,
                           File.Token (Start, File.Current_Position - 2));

                        Start := File.Current_Position;

                        if File.Current_Char in ' ' then
                           --  If we have a ' ' after the '\' then we have to
                           --  escape it so it does not terminate the path
                           --  retrivial.
                           Escaped := True;
                        end if;
                     end if;

                  else
                     if Started then
                        --  If we started, raise the Splited_Path flag so we
                        --  know we have to fully construct the path rather
                        --  than fetching the File.Token entirely.
                        Splited_Path := True;

                        --  Construct a part of the path
                        Append
                          (Path,
                           File.Token (Start, File.Current_Position - 1));
                     end if;

                     --  Move the index to the next char directly, '\' won't be
                     --  part of the path.
                     Start := File.Current_Position + 1;

                     if File.Check (" ") then
                        --  If we have a ' ' after the '\' then we have to
                        --  escape it so it does not terminate the path
                        --  retrivial.
                        Escaped := True;
                     end if;
                  end if;
                  pragma Warnings (On, "this code can never be executed*");
               end if;
            end if;

            if File.Current_Char in ASCII.CR | ASCII.LF then
               --  If we were skipping the previous line, a new line resets the
               --  flag
               if Skipping then
                  Skipping := False;
               end if;

               --  Make sure we read every "EOL" available char
               if File.Current_Char in ASCII.CR then
                  if File.Check ("" & ASCII.LF) then
                     --  if we read an ASCII.CR and ASCII.LF we need to add
                     --  an extra offset.
                     EOL_Extra_Offset := True;
                  end if;
               end if;

               --  We have a multi line indicator, verify we are not escaping
               --  the current line.
               --  If we are skipping, we are still expecting to find a new
               --  dependency.
               if Multi_Line and then File.Check ("#") then
                  Skipping := True;
               end if;

               --  We have a multi line indicator and the line is not to be
               --  skipped
               declare
                  Offset : constant Long_Long_Integer :=
                             (if EOL_Extra_Offset then 2 else 1);
                  Finish : constant Long_Long_Integer :=
                             File.Current_Position - Offset;
               begin
                  if Multi_Line and then not Skipping then
                     if Started then
                        if Splited_Path then
                           Append (Path, File.Token (Start, Finish));
                           return To_String (Path);
                        else
                           return File.Token (Start, Finish);
                        end if;
                     else
                        --  Put the flag down
                        Multi_Line   := False;
                        --  We are expecting to find a dependency in this line
                        Expected_Dep := True;
                     end if;
                  else
                     if Started then
                        --  Return the (expected) dependency
                        Expected_Dep := False;

                        if Splited_Path then
                           Append (Path, File.Token (Start, Finish));
                           return To_String (Path);
                        else
                           return File.Token (Start, Finish);
                        end if;

                     else
                        --  We reached an end of line without finding
                        --  a dependency.
                        if Expected_Dep then
                           --  If we were expecting to found one, this is a
                           --  parsing error.
                           raise Scan_Makefile_Error with
                             "Multi-line separator detected, dependency source"
                             & " file not found";
                        end if;
                        --  Otherwise it probably means we were skipping the
                        --  line
                     end if;
                  end if;
               end;

            elsif File.Current_Char in ' ' then
               --  Found the dependency separator
               if Escaped then
                  --  Do nothing and lower the flag
                  Escaped := False;
               elsif Started then
                  --  If a dependency started, then return it.
                  --  Otherwise we may have extra ' ' before reaching the
                  --  actual interesting data.

                  --  Skip the extra space characters before that
                  declare
                     Last : constant Long_Long_Integer :=
                              File.Current_Position - 1;
                  begin
                     loop
                        exit when not GB.Next (File, C);
                        exit when C /= ' ';
                     end loop;

                     if Splited_Path then
                        Append (Path, File.Token (Start, Last));
                        return To_String (Path);
                     else
                        return File.Token (Start, Last);
                     end if;
                  end;
               end if;

            else
               --  We found a char of interest, start the dependency and save
               --  the starting index.
               if not Started and not Skipping then
                  Start := File.Current_Position;
                  Started := True;
               end if;
            end if;

            if not GB.Next (File, C) then
               --  Couldn't read the next char
               raise Scan_Makefile_Error;
            end if;
         end loop;
      end Fetch_Dependency;

      -----------------
      -- Next_Object --
      -----------------

      function Next_Object
        (File        : in out GB.Reader;
         Header      :    out Character) return String
      is
         First              : Long_Long_Integer := 0;
         Last               : Long_Long_Integer := 0;

         EOF                : Boolean := False;
         First_Char         : Boolean := True;
         Non_Ignorable_Data : Boolean := False;
         Object_Found       : Boolean := False;
         Skipping           : Boolean := False;
      begin
         loop
            --  No more data to read, exit the search
            if File.Is_End_Of_Data then
               Header := ASCII.NUL;
               EOF    := True;
            end if;

            --  Could not read the next char, exit the search
            if not GB.Next (File, Header) then
               Header := ASCII.NUL;
               EOF    := True;
            end if;

            --  We are reading the first char of a new search or a new line
            if First_Char
              or else Header in ASCII.CR | ASCII.LF | ASCII.NUL
            then
               --  If we were skipping the previous line, a new line resets the
               --  flag.
               if Skipping then
                  Skipping := False;
               end if;

               --  Make sure we read every "EOL" available char
               if Header in ASCII.CR then
                  if File.Check ("" & ASCII.LF) then
                     null;
                  end if;

                  --  We found some "non-ignorable" char on the previous line
                  --  without matching with an object detection pattern, this
                  --  is a parsing error.
                  if Non_Ignorable_Data then
                     raise Scan_Makefile_Error with "no colon";
                  end if;

               elsif Header in ASCII.LF then
                  --  We found some "non-ignorable" char on the previous line
                  --  without matching with an object detection pattern, this
                  --  is a parsing error.
                  if Non_Ignorable_Data then
                     raise Scan_Makefile_Error with "no colon";
                  end if;

               elsif Header in ASCII.NUL then
                  --  We found some "non-ignorable" char on the previous line
                  --  without matching with an object detection pattern, this
                  --  is a parsing error.
                  if Non_Ignorable_Data then
                     raise Scan_Makefile_Error with "no colon";
                  end if;
               end if;

               --  Look for a skipping symbol '#'
               if (First_Char
                   and then Header in '#')
                 or else File.Check ("#")
               then
                  --  The rest of the line will not be parsed
                  Skipping := True;
               else
                  --  Save the first "useful" index for a later purpose
                  First := File.Current_Position;
               end if;

               First_Char := False;
            end if;

            --  If when reading a new line we didn't encounter a '#' at the
            --  beginning
            if not Skipping then

               --  Looking for the ": " or ":<ASCII.HT>" pattern in order to
               --  determine we have an object to find dependencies for.
               if Header in ':'
                 and then
                   (File.Check (" ")
                    or else File.Check ("" & ASCII.HT))
               then
                  --  Found the pattern, save indexes in order to retrieve the
                  --  name of the object.

                  --  This allows to escape ' ' before and after the name of
                  --  the object :
                  --  "     bla.o   : "
                  --   ^--- First    ^--- Last
                  --  will return "bla.o"
                  Last := File.Current_Position;
                  loop
                     --  File.Current_Position moved when matching ": "
                     --  so we have to start from two steps back
                     Last := Last - 2;
                     exit when
                       Last = First
                       or else File.Token (Last, Last) /= " ";
                  end loop;

                  loop
                     exit when
                       First = Last
                       or else not
                         (File.Token (First, First) = "" & ASCII.CR
                          or else File.Token (First, First) = "" & ASCII.LF
                          or else File.Token (First, First) = " ");
                     First := First + 1;
                  end loop;

                  --  Raise the flag to exit the object search
                  Object_Found := True;

               elsif Header not in ASCII.CR | ASCII.LF | ASCII.HT | ' ' then
                  --  We have some "not ignorable" information in the makefile
                  --  that aren't escaped with '#' at the start of the line.
                  --  This is considered a parsing error.
                  Non_Ignorable_Data := True;
               end if;
            end if;

            --  Exit the object search when we find one or we read the entire
            --  file without finding one.
            exit when Object_Found or else EOF;
         end loop;

         --  Return the actual object name, or an empty string if we reached
         --  EOF before finding one.
         return (if EOF then "" else File.Token (First, Last));
      end Next_Object;

   end IO;

   ------------------
   -- Dependencies --
   ------------------

   function Dependencies
     (Makefile  : GPR2.Path_Name.Object;
      Object    : GPR2.Path_Name.Object;
      Dep_Names : in out GPR2.Containers.Filename_Set;
      Strict    : Boolean := False) return Boolean
   is
      Reader      : GB.Reader := GB.Open (String (Makefile.Value));
      Header      : Character := ASCII.NUL;
      EOF         : Boolean   := False;

      function Is_Time_Stamp (S : String) return Boolean;
      --  Return True if S has the format of a Timestamp

      -------------------
      -- Is_Time_Stamp --
      -------------------

      function Is_Time_Stamp (S : String) return Boolean is
         Result : Boolean := False;
      begin
         if S'Length = 14 then
            Result := True;

            for J in S'Range loop
               if S (J) not in '0' .. '9' then
                  Result := False;
                  exit;
               end if;
            end loop;
         end if;

         return Result;
      end Is_Time_Stamp;
   begin
      loop
         if EOF then
            exit;
         else
            --  Look for a "<object_file>: " pattern
            declare
               Object_Name : constant String :=
                               IO.Next_Object (Reader, Header);
            begin
               if Object_Name /= "" then

                  --  Check Object_Name coherence with the Path_Name.Object of
                  --  the corresponding object file.
                  --  This takes into account the filename casing of the
                  --  underlying host assured by the type of Filename_Type.
                  if not (Object.Simple_Name = Simple_Name (Object_Name)) then
                     raise Scan_Makefile_Error with
                       "expected object file name "
                       & String (Object.Simple_Name) & ", got "
                       & String (Object_Name);
                  end if;

               elsif Header not in ASCII.NUL then
                  raise Scan_Makefile_Error with "unexpected object";
               end if;
            end;
         end if;

         case Header is
            when ASCII.NUL   =>
               --  Either we finished reading the file, terminate the parsing
               EOF := True;

            when ':' =>
               Fetch_All_Dependencies : loop
                  exit Fetch_All_Dependencies when
                    Reader.Current_Char in ASCII.CR | ASCII.LF;

                  --  Fetch a dependency
                  declare
                     Token : constant String := IO.Fetch_Dependency (Reader);
                  begin
                     if not Is_Time_Stamp (Token) then
                        declare
                           --  Check the dependency has a valid simple name.
                           SN : constant Simple_Name :=
                                  Path_Name.Simple_Name
                                    (Filename_Type (Token));
                        begin
                           if SN'Length = 0 then
                              raise Scan_Makefile_Error with
                                "invalid dependency file '" & Token & ''';
                           else
                              Dep_Names.Include (Filename_Type (Token));
                           end if;
                        end;
                     end if;
                  end;
               end loop Fetch_All_Dependencies;

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
            "Makefile parser error: " & Ada.Exceptions.Exception_Message (E));
         GB.Finalize (Reader);
         if Strict then
            raise Scan_Makefile_Error with
              "dependency file " & String (Makefile.Simple_Name)
              & " has wrong format"
              & (if Ada.Exceptions.Exception_Message (E)'Length /= 0
                 then " : " & Ada.Exceptions.Exception_Message (E)
                 else "");
         else
            return False;
         end if;
   end Dependencies;

end GPR2.Build.Makefile_Parser;
