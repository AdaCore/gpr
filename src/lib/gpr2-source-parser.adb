------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2017, Free Software Foundation, Inc.            --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Strings.Maps.Constants;
with Ada.Streams.Stream_IO;

with GPR2.Source_Reference.Identifier;

package body GPR2.Source.Parser is

   use Ada;

   --  The following IO package is a temporary solution before the full and
   --  real implementation is based on LibAdaLang. Note that we do not want to
   --  parse the whole Ada syntax here at the moment. The elements that are
   --  needed are:
   --
   --  1. whether the package is a separate unit. the parsing is necessary as
   --  it is not possible to get this information while the body suffix and
   --  separate suffix are identical.
   --
   --  2. the actual unit name as declared in the package, this is needed to
   --  get correct unit name for krunched filenames.
   --
   --  3. the dependent units. we need the withed entities to be able to
   --  compute the full dependencies of a given unit to compile.
   --
   --  So basically we just need to get the context of the unit and we never
   --  parse the content passed the procedure, package or function declaration.

   package IO is

      use Ada.Streams;

      Size_Chunk : constant := 2_048;
      --  Size of the chunk read by the implementation

      type Handle is record
         FD      : Stream_IO.File_Type;
         Buffer  : Stream_Element_Array (1 .. Size_Chunk);
         Current : Stream_Element_Offset := -1;
         Last    : Stream_Element_Offset := -1;
         Is_Id   : Boolean; --  True when last parsing was an identifier
         Line    : Positive := 1;
      end record;

      procedure Open
        (File     : in out Handle;
         Filename : Path_Name_Type)
        with Post => Stream_IO.Is_Open (File.FD)
                     and then File.Current = 0
                     and then File.Last >= 0;
      --  Open Filename and initialize the corresponding handle

      procedure Close (File : in out Handle)
        with Post => not Stream_IO.Is_Open (File.FD);

      function Get_Token (File : in out Handle) return String
        with Pre => Stream_IO.Is_Open (File.FD);
      --  Get next token on the file

   end IO;

   -----------
   -- Check --
   -----------

   function Check (Filename : Path_Name_Type) return Data is

      H : IO.Handle;

      function Read_Unit return Unbounded_String with Inline;
      --  Read a unit name

      ---------------
      -- Read_Unit --
      ---------------

      function Read_Unit return Unbounded_String is
         Unit : Unbounded_String;
      begin
         Read_Unit : loop
            declare
               Tok : constant String := IO.Get_Token (H);
            begin
               exit Read_Unit when Tok in "" | "is" | "renames" | "with"
                 or else not (H.Is_Id or else Tok = ".");

               --  Skip token body as in "package body"

               if Tok /= "body" then
                  Unit := Unit & Tok;
               end if;
            end;
         end loop Read_Unit;

         return Unit;
      end Read_Unit;

      R : Data;
   begin
      IO.Open (H, Filename);

      Check_Context : loop
         declare
            Tok : constant String :=
                    Characters.Handling.To_Lower (IO.Get_Token (H));
         begin
            if Tok = "separate" then
               R.Is_Separate := True;

               --  Read the unit it is a separate which is surrounded by
               --  parenthesis.

               declare
                  Tok : constant String := IO.Get_Token (H);
               begin
                  if Tok = "(" then
                     R.Sep_From := Read_Unit;
                  end if;
               end;

            elsif Tok = "with" then
               declare
                  Unit : constant Unbounded_String := Read_Unit;
               begin
                  --  Check for a null unit, this can happen if the source is
                  --  partial or invalid.

                  if Unit /= Null_Unbounded_String then
                     R.W_Units.Insert
                       (Source_Reference.Identifier.Create
                          (Value (Filename), H.Line, 1,
                           Name_Type (To_String (Unit))));
                  end if;
               end;

            elsif Tok in "procedure" | "package" | "function" then
               R.Unit_Name := Read_Unit;
               exit Check_Context;
            end if;

            --  Stop parsing when reaching the unit declaration or when
            --  end-of-file.

            exit Check_Context when Tok = "";
         end;
      end loop Check_Context;

      IO.Close (H);
      return R;
   end Check;

   --------
   -- IO --
   --------

   package body IO is

      procedure Fill_Buffer (File : in out Handle)
        with Inline, Post => File.Current = 0;
      --  Read a chunk of data in the buffer or nothing if there is no more
      --  data to read.

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

      function Get_Token (File : in out Handle) return String is

         use all type Strings.Maps.Character_Set;

         Ada_Word : constant Strings.Maps.Character_Set :=
                      Strings.Maps.Constants.Alphanumeric_Set
                      or Strings.Maps.To_Set ("_");

         function Next_Char return Character with Inline;
         --  Get next char in buffer

         subtype Content_Index is Natural range 0 .. 1_024;
         subtype Content_Range is Content_Index range 1 .. Content_Index'Last;

         Tok  : String (Content_Range);
         Cur  : Content_Index := 0;
         P, C : Character := ASCII.NUL;

         procedure Skip_EOL with Inline;
         --  Skip chars until an end-of-line

         procedure Clear_Context
           with Inline,
                Post => P = ASCII.NUL and then C = ASCII.NUL;
         --  Clear current context

         procedure Get_Word with Inline;
         --  Read a work, result will be in Tok (Tok'First .. Cur)

         -------------------
         -- Clear_Context --
         -------------------

         procedure Clear_Context is
         begin
            P := ASCII.NUL;
            C := ASCII.NUL;
         end Clear_Context;

         --------------
         -- Get_Word --
         --------------

         procedure Get_Word is
            C : Character;
         begin
            loop
               C := Next_Char;

               if Strings.Maps.Is_In (C, Ada_Word) then
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

         --------------
         -- Skip_EOL --
         --------------

         procedure Skip_EOL is
         begin
            loop
               exit when Next_Char in ASCII.LF | ASCII.EOT;
            end loop;
         end Skip_EOL;

      begin
         File.Is_Id := False;

         Read_Token : loop
            C := Next_Char;

            Cur := 1;
            Tok (Cur) := C;

            if C = ASCII.EOT then
               Cur := 0;
               exit Read_Token;

            elsif P = '-' and then C = '-' then
               Skip_EOL;
               Clear_Context;
               File.Line := File.Line + 1;

            elsif Strings.Maps.Is_In (C, Ada_Word) then
               Get_Word;
               File.Is_Id := True;
               exit Read_Token;

            elsif C in '.' | ';' | '(' | ')' then
               exit Read_Token;

            elsif C in ASCII.LF then
               File.Line := File.Line + 1;
            end if;

            P := C;
         end loop Read_Token;

         return Tok (1 .. Cur);
      end Get_Token;

      ----------
      -- Open --
      ----------

      procedure Open
        (File     : in out Handle;
         Filename : Path_Name_Type) is
      begin
         Stream_IO.Open (File.FD, Stream_IO.In_File, Value (Filename));
         Fill_Buffer (File);
         File.Line := 1;
      end Open;

   end IO;

end GPR2.Source.Parser;
