------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with GNAT.Calendar;

with GPR2.ALI.With_Data;

package body GPR2.ALI.ALI_Data is

   package IO is

      use Ada.Streams;

      Size_Chunk : constant := 2_048;
      --  Size of the chunk read by the implementation

      type Handle is record
         FD      : Stream_IO.File_Type;
         Buffer  : Stream_Element_Array (1 .. Size_Chunk);
         Current : Stream_Element_Offset := -1;
         Last    : Stream_Element_Offset := -1;
         Line    : Positive := 1;
         At_LF   : Boolean := False;
      end record;

      procedure Open
        (File     : in out Handle;
         Filename : GPR2.Path_Name.Object)
        with Post => Stream_IO.Is_Open (File.FD)
        and then File.Current = 0
        and then File.Last >= 0;
      --  Open Filename and initialize the corresponding handle

      procedure Close (File : in out Handle)
        with Post => not Stream_IO.Is_Open (File.FD);

      function Get_Token
        (File : in out Handle; Stop_At_LF : Boolean := False) return String
        with Pre => Stream_IO.Is_Open (File.FD);
      --  Get next token on the file.
      --  If Stop_At_LF is True, then no token will be read after the current
      --  line is finished; calling again with Stop_At_LF => False (through a
      --  call to Next_Line for instance) allows further reading.
      --  The result is "" if either we got LF in Stop_At_LF mode, or EOF.

      procedure Next_Line (File : in out Handle; Header : in out Character)
        with Pre => Stream_IO.Is_Open (File.FD);
      --  Skips to the next (non-empty) line and sets Header.
      --  If no such line exists, Header is set to NUL.

      function At_LF (File : Handle) return Boolean;

   end IO;

   --------
   -- IO --
   --------

   package body IO is

      procedure Fill_Buffer (File : in out Handle)
        with Inline, Post => File.Current = 0;
      --  Read a chunk of data in the buffer or nothing if there is no more
      --  data to read.

      -----------
      -- At_LF --
      -----------

      function At_LF (File : Handle) return Boolean is
        (File.At_LF);

      -----------
      -- Close --
      -----------

      procedure Close (File : in out Handle) is
      begin
         Stream_IO.Close (File.FD);
      end Close;

      -----------------
      -- Fill_Buffer --
      -----------------

      procedure Fill_Buffer (File : in out Handle) is
      begin
         Stream_IO.Read (File.FD, File.Buffer, File.Last);
         File.Current := 0;
      end Fill_Buffer;

      ---------------
      -- Get_Token --
      ---------------

      function Get_Token
        (File : in out Handle; Stop_At_LF : Boolean := False) return String
      is
         function Next_Char return Character with Inline;
         --  Get next char in buffer

         subtype Content_Index is Natural range 0 .. 1_024;
         subtype Content_Range is Content_Index range 1 .. Content_Index'Last;

         Tok  : String (Content_Range);
         Cur  : Content_Index := 0;
         P, C : Character := ASCII.NUL;
         pragma Unreferenced (P);

         procedure Get_Word with Inline;
         --  Read a word, result will be in Tok (Tok'First .. Cur)

         function Is_Sep (C : Character) return Boolean;

         --------------
         -- Get_Word --
         --------------

         procedure Get_Word is
            C : Character;
         begin
            loop
               C := Next_Char;

               if not Is_Sep (C) then
                  Cur := Cur + 1;
                  Tok (Cur) := C;

               else
                  File.Current := File.Current - 1;
                  exit;
               end if;
            end loop;
         end Get_Word;

         ------------
         -- Is_Sep --
         ------------

         function Is_Sep (C : Character) return Boolean is
         begin
            return Character'Pos (C) in 32 | 9  --  Space/Tab
              or else C in ASCII.LF | ASCII.EOT;
         end Is_Sep;

         ---------------
         -- Next_Char --
         ---------------

         function Next_Char return Character is
         begin
            if File.Current = File.Last then
               if Stream_IO.End_Of_File (File.FD) then
                  --  Nothing more to read
                  return ASCII.EOT;
               else
                  Fill_Buffer (File);
               end if;
            end if;

            File.Current := File.Current + 1;
            return Character'Val (File.Buffer (File.Current));
         end Next_Char;

      begin
         Read_Token : loop
            if Stop_At_LF and then File.At_LF then
               return "";
            end if;

            File.At_LF := False;

            C := Next_Char;

            Cur := 1;
            Tok (Cur) := C;

            if C = ASCII.EOT then
               Cur := 0;
               exit Read_Token;

            elsif C = ASCII.LF then
               File.At_LF := True;
               File.Line := File.Line + 1;

            elsif not Is_Sep (C) then
               Get_Word;
               exit Read_Token;
            end if;

            P := C;
         end loop Read_Token;

         return Tok (1 .. Cur);
      end Get_Token;

      ---------------
      -- Next_Line --
      ---------------

      procedure Next_Line (File : in out Handle; Header : in out Character)
      is
         Old_Line : constant Positive := File.Line;
         At_LF    : constant Boolean  := File.At_LF;
         Current  : constant Stream_Element_Offset := File.Current;
      begin
         loop
            declare
               Tok : constant String := Get_Token (File);
            begin
               if Tok = "" then
                  Header := ASCII.NUL;
                  return;

               elsif File.Line > Old_Line
                 or else (At_LF and then File.Line = Old_Line)
                 --  This second condition is for the case we hit LF in a
                 --  previous call to Get_Token, with Stop_At_LF => True.
                 --  The line number will then be unchanged.
                 or else (Current = 0 and then File.Line = 1)
               --  This third condition is for the special case of the very
               --  first read.
               then
                  Header := Tok (1);
                  return;
               end if;
            end;
         end loop;
      end Next_Line;

      ----------
      -- Open --
      ----------

      procedure Open
        (File     : in out Handle;
         Filename : GPR2.Path_Name.Object) is
      begin
         Stream_IO.Open (File.FD, Stream_IO.In_File, Filename.Value);
         Fill_Buffer (File);
         File.Line := 1;
      end Open;

   end IO;

   -------------
   -- Dep_For --
   -------------

   function Dep_For
     (Self : Object; File : Simple_Name) return Dependency_Data.Object is
   begin
      if Self.Sdeps_Map.Contains (File) then
         return Self.Sdeps (Self.Sdeps_Map (File));
      else
         return Dependency_Data.Undefined;
      end if;
   end Dep_For;

   ---------------
   -- Print_ALI --
   ---------------

   procedure Print_ALI (Self : Object) is
      use Ada.Text_IO;
   begin

      Put_Line ("Ofile: " & (-Self.Ofile_Full_Name));

      Put ("Args: ");
      for A of Self.Args loop
         Put (A & ", ");
      end loop;
      New_Line;

      Put_Line ("Units: ");
      for U of Self.Units loop
         Put_Line ("   " & String (U.Uname));

         for W of U.Withs loop
            Put_Line ("   -> " & String (W.Uname));
         end loop;

      end loop;

      Put_Line ("Dependencies: ");
      for D of Self.Sdeps loop
         Put_Line ("   " & String (D.Sfile));
      end loop;

      Put_Line ("GNAT version: " & (-Self.GNAT_Version));

      Put_Line ("Compil errors: " & Self.Compile_Errors'Image);

      Put_Line ("No object: " & Self.No_Object'Image);
   end Print_ALI;

   --------------
   -- Scan_ALI --
   --------------

   function Scan_ALI (File : Path_Name.Object) return Object
   is
      Scan_ALI_Error : exception;

      Handle : IO.Handle;

      Result : Object := Object'
        (Ofile_Full_Name => +File.Value,
         Args            => Value_Type_List.Empty_Vector,
         Units           => Unit_Data.List.Empty_List,
         Sdeps           => Dependency_Data.List.Empty_List,
         Sdeps_Map       => Empty_Sdep_Map,
         GNAT_Version    => Null_Unbounded_String,
         Compile_Errors  => False,
         No_Object       => False);

      procedure Fill_Arg;

      procedure Fill_Dep;

      procedure Fill_GNAT_Version;

      procedure Fill_Unit;

      procedure Fill_With (Header : Character);

      --------------
      -- Fill_Arg --
      --------------

      procedure Fill_Arg is
         Tok : constant String := IO.Get_Token (Handle, Stop_At_LF => True);
      begin
         if Tok = "" then
            raise Scan_ALI_Error;
         else
            Result.Args.Append (Tok);
         end if;
      end Fill_Arg;

      --------------
      -- Fill_Dep --
      --------------

      procedure Fill_Dep is
         D : Dependency_Data.Object;

         function Checksum (S : String) return Word;

         function Get_Token return String;

         function Time_Stamp (S : String) return Ada.Calendar.Time;

         function Checksum (S : String) return Word is
         begin
            if S'Length /= 8 then
               raise Scan_ALI_Error;
            end if;

            declare
               Chk : Word := 0;

            begin
               for C of S loop
                  if C in '0' .. '9' then
                     Chk := Chk * 16 +
                              Character'Pos (C) - Character'Pos ('0');

                  elsif C in 'a' .. 'f' then
                     Chk := Chk * 16 +
                              Character'Pos (C) - Character'Pos ('a') + 10;

                  else
                     raise Scan_ALI_Error;
                  end if;
               end loop;

               return Chk;
            end;
         end Checksum;

         function Get_Token return String is
            Tok : constant String := IO.Get_Token (Handle, Stop_At_LF => True);
         begin
            if Tok = "" then
               raise Scan_ALI_Error;
            else
               return Tok;
            end if;
         end Get_Token;

         function Time_Stamp (S : String) return Ada.Calendar.Time is
            T : String (1 .. 14);
         begin
            if S'Length /= 14 then
               raise Scan_ALI_Error;
            end if;

            T := S;

            return GNAT.Calendar.Time_Of
              (Year       => Integer'Value (String (T (1 .. 4))),
               Month      => Integer'Value (String (T (5 .. 6))),
               Day        => Integer'Value (String (T (7 .. 8))),
               Hour       => Integer'Value (String (T (9 .. 10))),
               Minute     => Integer'Value (String (T (11 .. 12))),
               Second     => Integer'Value (String (T (13 .. 14))));
         end Time_Stamp;

         Sfile  : constant String            := Get_Token;
         Stamp  : constant Ada.Calendar.Time := Time_Stamp (Get_Token);
         Chksum : constant Word              := Checksum (Get_Token);

      begin
         --  Unit_Name, Unit_Kind

         declare
            Kind   : Kind_Type;
            Name   : constant String := Get_Token;
            Suffix : constant String :=
                       (if Name'Length > 2
                        then Name (Name'Last - 1 .. Name'Last)
                        else "");
         begin
            if Suffix = "%s" then
               Kind := S_Spec;
            elsif Suffix = "%b" then
               Kind := S_Body;
            elsif Name /= "" then
               Kind := S_Separate;
            else
               raise Scan_ALI_Error;
            end if;

            D := Dependency_Data.Create
              (Sfile     => Simple_Name (Sfile),
               Stamp     => Stamp,
               Checksum  => Chksum,
               Unit_Name => Name_Type (Name (Name'First .. Name'Last - 2)),
               Unit_Kind => Kind);
         end;

         Result.Sdeps.Append (D);

         Result.Sdeps_Map.Include
           (D.Sfile, Result.Sdeps.Last_Index);
         --  Here we use Include and not Insert, due to possible duplicate
         --  D lines in ALI files.
      end Fill_Dep;

      ------------------
      -- Fill_Version --
      ------------------

      procedure Fill_GNAT_Version is
         Version : Unbounded_String;
         Length  : Natural := 0;
      begin
         --  This field is quoted and should look like: "GNAT Lib v...".
         --  We get space-delimited words from the IO package and only the last
         --  one before LF is relevant.

         loop
            declare
               Tok : constant String :=
                       IO.Get_Token (Handle, Stop_At_LF => True);
            begin
               if Tok = "" then
                  exit;
               else
                  Version := +Tok;
                  Length  := Tok'Length;
               end if;
            end;
         end loop;

         --  We must at least have the initial 'v', the closing quote, and
         --  one character in the middle.

         if Length > 2 and then Element (Version, 1) = 'v'
           and then Element (Version, Length) = '"'
         then
            Result.GNAT_Version := +"GNAT " &
              Unbounded_Slice (Version, 2, Length - 1);
         else
            raise Scan_ALI_Error;
         end if;
      end Fill_GNAT_Version;

      ---------------
      -- Fill_Unit --
      ---------------

      procedure Fill_Unit is
         U : Unit_Data.Object;

         use Unit_Data;

         U_Flags : Flag_Array := Default_Flags;
      begin
         --  Uname, Sfile, Utype

         declare
            Tok1  : constant String :=
                      IO.Get_Token (Handle, Stop_At_LF => True);
            Tok2  : constant String :=
                      IO.Get_Token (Handle, Stop_At_LF => True);
            Utype : Unit_Type;

         begin
            if Tok1'Length < 3 then  --  At least "?%(b|s)"
               raise Scan_ALI_Error;
            end if;

            if Tok2 = "" then
               raise Scan_ALI_Error;
            end if;

            --  Set Utype. This will be adjusted after we finish reading the
            --  U lines, in case we have both spec and body.

            declare
               Suffix : constant String := Tok1 (Tok1'Last - 1 .. Tok1'Last);
            begin
               if Suffix = "%s" then
                  Utype := Is_Spec_Only;
               elsif Suffix = "%b" then
                  Utype := Is_Body_Only;
               else
                  raise Scan_ALI_Error;
               end if;
            end;

            U := Unit_Data.Create
              (Uname => Name_Type (Tok1 (1 .. Tok1'Last - 2)),
               Sfile => Simple_Name (Tok2),
               Utype => Utype);
         end;

         --  Flags (we skip the unit version)

         loop
            declare
               C1, C2 : Character;
               Tok    : constant String :=
                          IO.Get_Token (Handle, Stop_At_LF => True);
            begin
               exit when Tok = "";

               if Tok'Length = 2 then  --  This eliminates the version field

                  C1 := Tok (1);
                  C2 := Tok (2);

                  if C1 = 'B' then
                     --  Elaborate_Body_Desirable, Body_Needed_For_SAL

                     if C2 = 'D' then
                        U_Flags (Elaborate_Body_Desirable) := True;
                     elsif C2 = 'N' then
                        U_Flags (Body_Needed_For_SAL) := True;
                     end if;

                  elsif C1 = 'D' and then C2 = 'E' then
                     --  Dynamic_Elab

                     U_Flags (Dynamic_Elab) := True;

                  elsif C1 = 'E' and then C2 = 'B' then
                     --  Elaborate_Body

                     U_Flags (Elaborate_Body) := True;

                  elsif C1 = 'G' and then C2 = 'E' then
                     --  Is_Generic

                     U_Flags (Is_Generic) := True;

                  elsif C1 = 'I' and then C2 = 'S' then
                     --  Init_Scalars

                     U_Flags (Init_Scalars) := True;

                  elsif C1 = 'N' and then C2 = 'E' then
                     --  No_Elab

                     U_Flags (No_Elab) := True;

                  elsif C1 = 'P' then
                     --  Preelab, Pure, Unit_Kind

                     if C2 = 'R' then
                        U_Flags (Preelab) := True;
                     elsif C2 = 'U' then
                        U_Flags (Pure) := True;
                     elsif C2 = 'K' then
                        U.Set_Unit_Kind ('p');
                     end if;

                  elsif C1 = 'R' then
                     --  RCI, Remote_Types, Has_RACW

                     if C2 = 'C' then
                        U_Flags (RCI) := True;
                     elsif C2 = 'T' then
                        U_Flags (Remote_Types) := True;
                     elsif C2 = 'A' then
                        U_Flags (Has_RACW) := True;
                     end if;

                  elsif C1 = 'S' then
                     --  Shared_Passive, Unit_Kind

                     if C2 = 'P' then
                        U_Flags (Shared_Passive) := True;
                     elsif C2 = 'U' then
                        U.Set_Unit_Kind ('s');
                     end if;
                  end if;
               end if;
            end;
         end loop;

         U.Set_Flags (U_Flags);

         Result.Units.Append (U);
      end Fill_Unit;

      ---------------
      -- Fill_With --
      ---------------

      procedure Fill_With (Header : Character) is
         W : With_Data.Object;

         Impl : constant Boolean := (Header = 'Z');
         --  Implicit_With_From_Instantiation

         procedure P (Element : in out Unit_Data.Object);

         procedure P (Element : in out Unit_Data.Object) is
         begin
            Element.Add_With (W);
         end P;

      begin
         --  Uname, Ukind, Sfile, Afile

         declare
            N : constant String := IO.Get_Token (Handle, Stop_At_LF => True);
            S : constant String := IO.Get_Token (Handle, Stop_At_LF => True);
            A : constant String := IO.Get_Token (Handle, Stop_At_LF => True);
            Ukind : Kind_Type;
         begin
            if N'Length < 3 then  --  At least "?%(b|s)"
               raise Scan_ALI_Error;
            end if;

            declare
               Suffix : constant String := N (N'Last - 1 .. N'Last);
            begin
               if Suffix = "%s" then
                  Ukind := S_Spec;
               elsif Suffix = "%b" then
                  Ukind := S_Body;
               else
                  raise Scan_ALI_Error;
               end if;
            end;

            W := With_Data.Create
              (Uname => Name_Type (N (1 .. N'Last - 2)),
               Ukind => Ukind,
               Sfile => Optional_Name_Type (S),
               Afile => Optional_Name_Type (A),
               Implicit_With_From_Instantiation => Impl);
         end;

         Result.Units.Update_Element (Result.Units.Last_Index, P'Access);
      end Fill_With;

   begin
      --  Fill the ALI object up till the XREFs

      IO.Open (Handle, File);

      declare
         Header : Character;  --  Line header, set when calling Next_Line

         procedure Next_Line
           (Expected  : Character := ASCII.NUL;
            Allow_EOF : Boolean   := False)
           with Pre => not (Expected /= ASCII.NUL and then Allow_EOF);
         --  Wrapper to call IO.Next_Line

         ---------------
         -- Next_Line --
         ---------------

         procedure Next_Line
           (Expected  : Character := ASCII.NUL;
            Allow_EOF : Boolean   := False) is
         begin
            IO.Next_Line (Handle, Header);
            if (Expected /= ASCII.NUL and then Header /= Expected)
              or else (not Allow_EOF and then Header = ASCII.NUL)
            then
               raise Scan_ALI_Error;
            end if;
         end Next_Line;

         use type Ada.Containers.Count_Type;

      begin
         --  Version (V line, mandatory)

         Next_Line (Expected => 'V');
         Fill_GNAT_Version;

         --  Skip to Args (A lines), or Units (U lines) if there is none.
         --  There must be at least one U line.

         loop
            Next_Line;
            exit when Header in 'A' | 'U';
         end loop;

         --  Read Args

         while Header = 'A' loop
            Fill_Arg;
            Next_Line;
            exit when Header /= 'A';
         end loop;

         --  Skip to Units (U lines)

         loop
            Next_Line;
            exit when Header = 'U';
         end loop;

         --  Read Units + {Withs}

         loop
            if Result.Units.Length = 2 then
               raise Scan_ALI_Error;  --  Cannot have more than 2 units
            end if;

            Fill_Unit;
            Next_Line;

            --  For each unit, read the With list (W/Y/Z lines) if any

            while Header in 'W' | 'Y' | 'Z' loop
               Fill_With (Header);
               Next_Line;
            end loop;

            --  Skip to either the next U section, or the first D line.
            --  There must be at least one D line: the dependency to the
            --  unit itself.

            while Header not in 'U' | 'D' loop
               Next_Line;
            end loop;

            exit when Header /= 'U';
         end loop;

         --  If we have 2 units, the first one should be the body and the
         --  second one the spec. Update the Utype accordingly.

         if Result.Units.Length = 2 then
            declare
               use Unit_Data;

               procedure P1 (Element : in out Unit_Data.Object);
               procedure P2 (Element : in out Unit_Data.Object);

               procedure P1 (Element : in out Unit_Data.Object) is
               begin
                  if Element.Utype /= Is_Body_Only then
                     raise Scan_ALI_Error;
                  end if;
                  Element.Set_Utype (Is_Body);
               end P1;

               procedure P2 (Element : in out Unit_Data.Object) is
               begin
                  if Element.Utype /= Is_Spec_Only then
                     raise Scan_ALI_Error;
                  end if;
                  Element.Set_Utype (Is_Spec);
               end P2;
            begin
               Result.Units.Update_Element (1, P1'Access);
               Result.Units.Update_Element (2, P2'Access);
            end;
         end if;

         --  Read Deps

         while Header = 'D' loop
            Fill_Dep;
            Next_Line (Allow_EOF => True);  --  Only here we allow EOF
         end loop;

         IO.Close (Handle);
         return Result;

      exception
         when others =>
            IO.Close (Handle);
            return Undefined;
      end;
   end Scan_ALI;

end GPR2.ALI.ALI_Data;
