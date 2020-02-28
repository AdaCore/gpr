------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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

with Ada.Directories;
with Ada.Streams.Stream_IO;

with GPR2.Source_Info.Parser.Registry;
with GPR2.Source;

package body GPR2.Source_Info.Parser.ALI is

   Handle : Object;

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
         Filename : GPR2.Path_Name.Object'Class)
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

         Tok : String (Content_Range);
         Cur : Content_Index := 0;
         C   : Character     := ASCII.NUL;

         procedure Get_Word with Inline;
         --  Read a word, result will be in Tok (Tok'First .. Cur)

         function Is_Sep (C : Character) return Boolean is
           (C in ' ' | ASCII.HT | ASCII.CR | ASCII.LF | ASCII.EOT);
         --  The character is separator

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
         end loop Read_Token;

         return Tok (1 .. Cur);
      end Get_Token;

      ---------------
      -- Next_Line --
      ---------------

      procedure Next_Line (File : in out Handle; Header : in out Character) is
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
         Filename : GPR2.Path_Name.Object'Class) is
      begin
         Stream_IO.Open (File.FD, Stream_IO.In_File, Filename.Value);
         Fill_Buffer (File);
         File.Line := 1;
      end Open;

   end IO;

   -----------------
   -- Clear_Cache --
   -----------------

   overriding procedure Clear_Cache (Self : not null access Object) is
   begin
      Self.Cache.Clear;
   end Clear_Cache;

   -------------
   -- Compute --
   -------------

   overriding procedure Compute
     (Self   : not null access Object;
      Data   : in out Source_Info.Object'Class;
      Source : GPR2.Source.Object'Class;
      LI     : Path_Name.Object'Class    := GPR2.Path_Name.Undefined;
      View   : Project.View.Object'Class := Project.View.Undefined)
   is
      use all type GPR2.Unit.Library_Unit_Type;

      Scan_ALI_Error : exception;

      A_Handle : IO.Handle;

      --  Unit data

      subtype CU_Index is Natural range 0 .. 2;

      B_Name  : constant Name_Type := Source.Path_Name.Simple_Name;
      U_Name  : Unbounded_String;
      S_Name  : Unbounded_String;
      U_Kind  : Unit.Library_Unit_Type;
      U_Flags : Unit.Flags_Set := Unit.Default_Flags;
      Main    : Unit.Main_Type := Unit.None;
      Withs   : Source_Reference.Identifier.Set.Object;

      CUs     : array (CU_Index range 1 .. 2) of Unit.Object;
      CU_BN   : array (CU_Index range 1 .. 2) of Unbounded_String;
      --  Units' corresponding source base name

      CU_Idx  : CU_Index := 0;
      Current : CU_Index := 0;

      procedure Fill_Unit;
      --  Add all units defined in ALI (spec, body or both)

      procedure Fill_With
        with Post => Withs.Length > Withs'Old.Length;
      --  Add all withed units into Withs below

      procedure Set_Source_Info_Data (Unit : GPR2.Unit.Object);
      --  Set returned source infor data

      function Key
        (LI : Path_Name.Object'Class; Basename : Name_Type) return Name_Type
      is (Name_Type (LI.Value & '@' & String (Basename)));

      ---------------
      -- Fill_Unit --
      ---------------

      procedure Fill_Unit is
         L_Type : Unit.Library_Item_Type with Unreferenced;
      begin
         --  Uname, Sfile, Utype

         declare
            Tok1 : constant String :=
                     IO.Get_Token (A_Handle, Stop_At_LF => True);
            Tok2 : constant String :=
                     IO.Get_Token (A_Handle, Stop_At_LF => True);
         begin
            --  At least "?%(b|s)"

            if Tok1'Length < 3 and then Tok1 (Tok1'Last - 1) /= '%' then
               raise Scan_ALI_Error;
            end if;

            if Tok2 = "" then
               raise Scan_ALI_Error;
            end if;

            --  Set Utype. This will be adjusted after we finish reading the
            --  U lines, in case we have both spec and body.

            case Tok1 (Tok1'Last) is
               when 's'    => U_Kind := Unit.S_Spec_Only;
               when 'b'    => U_Kind := Unit.S_Body_Only;
               when others => raise Scan_ALI_Error;
            end case;

            U_Name := +Tok1 (1 .. Tok1'Last - 2);
            S_Name := +Tok2;
         end;

         --  Flags (we skip the unit version)

         loop
            declare
               use Unit;

               Tok    : constant String :=
                          IO.Get_Token (A_Handle, Stop_At_LF => True);
               C1, C2 : Character;

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
                     --  No_Elab_Code

                     U_Flags (No_Elab_Code) := True;

                  elsif C1 = 'P' then
                     --  Preelab, Pure, Unit_Kind

                     if C2 = 'R' then
                        U_Flags (Preelab) := True;
                     elsif C2 = 'U' then
                        U_Flags (Pure) := True;
                     elsif C2 = 'K' then
                        L_Type := Unit.Is_Package;
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
                        L_Type := Unit.Is_Subprogram;
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end Fill_Unit;

      ---------------
      -- Fill_With --
      ---------------

      procedure Fill_With is
         N : constant String := IO.Get_Token (A_Handle, Stop_At_LF => True);
         S : constant String :=
               IO.Get_Token (A_Handle, Stop_At_LF => True) with Unreferenced;
         A : constant String :=
               IO.Get_Token (A_Handle, Stop_At_LF => True) with Unreferenced;

         U_Last : constant Integer := N'Last - 2; -- Unit last character in N
         U_Kind : Unit.Library_Unit_Type with Unreferenced;
      begin
         --  At least "?%(b|s)"

         if U_Last <= 0 or else N (N'Last - 1) /= '%' then
            raise Scan_ALI_Error;
         end if;

         case N (N'Last) is
            when 's'    => U_Kind := Unit.S_Spec;
            when 'b'    => U_Kind := Unit.S_Body;
            when others => raise Scan_ALI_Error;
         end case;

         --  Insert the withed unit without the sloc information. This
         --  information is found at the end of the ALI file and will be
         --  setup later.

         declare
            --  ??? line & column are wrong, we should parse the xref
            Sloc : constant GPR2.Source_Reference.Object :=
                     GPR2.Source_Reference.Object
                       (Source_Reference.Create
                          (Source.Path_Name.Value, 1, 1));
            S    : constant Source_Reference.Identifier.Object'Class :=
                     Source_Reference.Identifier.Create
                       (Sloc => Sloc,
                        Text => Name_Type (N (1 .. U_Last)));
         begin
            Withs.Insert (S);
         end;
      end Fill_With;

      --------------------------
      -- Set_Source_Info_Data --
      --------------------------

      procedure Set_Source_Info_Data (Unit : GPR2.Unit.Object) is
      begin
         Data.CU_List.Append (Unit);
         Data.CU_Map.Insert (Unit.Index, Unit);
         Data.Kind := Unit.Kind;

         Data.Parsed := Source_Info.LI;
         Data.Is_Ada := True;
         Data.Timestamp := Directories.Modification_Time (LI.Value);
      end Set_Source_Info_Data;

      use GPR2.Unit;

   begin
      --  If this unit is in the cache, return now we don't want to parse
      --  again the whole file.

      if Self.Cache.Contains (Key (LI, B_Name)) then
         Set_Source_Info_Data (Self.Cache (Key (LI, B_Name)));
         return;
      end if;

      --  Fill the ALI object up till the XREFs

      IO.Open (A_Handle, LI);

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
            IO.Next_Line (A_Handle, Header);

            if (Expected /= ASCII.NUL and then Header /= Expected)
              or else (not Allow_EOF and then Header = ASCII.NUL)
            then
               raise Scan_ALI_Error;
            end if;
         end Next_Line;

      begin
         --  Version (V line, mandatory)

         Next_Line (Expected => 'V');

         --  Skip version string

         loop
            Next_Line;

            case Header is
               when 'M' =>
                  --  Scan main program type if exists

                  declare
                     Tok : constant String :=
                             IO.Get_Token (A_Handle, Stop_At_LF => True);
                  begin
                     if Tok = "F" then
                        Main := Unit.Is_Function;
                     elsif Tok = "P" then
                        Main := Unit.Is_Procedure;
                     else
                        raise Scan_ALI_Error;
                     end if;
                  end;

               when 'A' | 'U' =>
                  --  Skip to Args (A lines), or Units (U lines) if there is
                  --  none. There must be at least one U line.

                  exit;

               when others =>
                  null;
            end case;
         end loop;

         --  Read Args

         while Header = 'A' loop
            --  Skipp compilation arguments
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
            --  Cannot have more than 2 units:  the spec and the body

            if CU_Idx = 2 then
               raise Scan_ALI_Error;
            end if;

            Fill_Unit;
            Next_Line;

            --  For each unit, read the With list (W/Y/Z lines) if any

            while Header in 'W' | 'Y' | 'Z' loop
               --  Only record explicite with clauses
               if Header in 'W' | 'Y' then
                  Fill_With;
               end if;

               Next_Line;
            end loop;

            --  Record this unit

            CU_Idx := CU_Idx + 1;

            CUs (CU_Idx) :=
              Unit.Create
                (Name         => Name_Type (-U_Name),
                 Index        => 1,
                 Kind         => U_Kind,
                 Main         => Main,
                 Dependencies => Withs,
                 Sep_From     => "",
                 Flags        => U_Flags);

            CU_BN (CU_Idx) := S_Name;

            Withs.Clear;

            if Name_Type (-S_Name) = B_Name then
               Current := CU_Idx;
            end if;

            --  Kind of first unit is also recorded in Data.Kind

            if CU_Idx = 1 then
               Data.Kind := U_Kind;
            end if;

            --  Skip to either the next U section, or the first D line.
            --  There must be at least one D line: the dependency to the
            --  unit itself.

            while Header not in 'U' | 'D' loop
               Next_Line;
            end loop;

            exit when Header /= 'U';
         end loop;

         --  Look for X (cross-references) lines

         while Header not in 'X' loop
            Next_Line;
         end loop;

         IO.Close (A_Handle);

         --  If we have 2 units, the first one should be the body and the
         --  second one the spec. Update the u_kind accordingly.

         if CU_Idx = 2 then
            if CUs (1).Kind /= Unit.S_Body_Only then
               raise Scan_ALI_Error
                 with "Unit body is on the wrong position";
            end if;

            Unit.Update_Kind (CUs (1), Unit.S_Body);
            Data.Kind := Unit.S_Body;

            if CUs (2).Kind /= Unit.S_Spec_Only then
               raise Scan_ALI_Error
                 with "Unit spec is on the wrong position";
            end if;

            Unit.Update_Kind (CUs (2), Unit.S_Spec);
         end if;

         --  Record into the cache

         for K in 1 .. CU_Idx loop
            declare
               Cache_Key : constant Name_Type :=
                             Key (LI, Name_Type (-CU_BN (K)));
            begin
               if not Self.Cache.Contains (Cache_Key) then
                  Self.Cache.Insert (Cache_Key, CUs (K));
               end if;
            end;
         end loop;

         --  Set data for the current source

         if Current in CUs'Range then
            Set_Source_Info_Data (CUs (Current));
         else
            Data.Parsed := Source_Info.None;
         end if;

      exception
         when others =>
            IO.Close (A_Handle);
            Data.Parsed := Source_Info.None;
      end;
   end Compute;

begin
   GPR2.Source_Info.Parser.Registry.Register (Handle);
end GPR2.Source_Info.Parser.ALI;
