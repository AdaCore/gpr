--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Exceptions;
with Ada.IO_Exceptions;

with GNATCOLL.OS.FS;
with GNATCOLL.Utils;

with GPR2.Build.Source;
with GPR2.Build.View_Db;
with GPR2.Message;
with GPR2.Project.Tree;
with GPR2.Project.View;

package body GPR2.Build.Object_Info.Parser.ALI is

   Scan_ALI_Error : exception;

   package IO is

      use GNATCOLL.OS;
      use type GNATCOLL.OS.FS.File_Descriptor;

      Size_Chunk : constant := 2_048;
      --  Size of the chunk read by the implementation

      Max_Token_Size : constant := 1024;
      subtype Token_Index is Natural range 0 .. Max_Token_Size;

      type Handle is record
         Path    : Path_Name.Object;
         FD      : FS.File_Descriptor;
         Buffer  : String (1 .. Size_Chunk);
         Token   : String (1 .. Max_Token_Size);
         Current : Integer := -1;
         Last    : Integer := -1;
         Line    : Positive := 1;
         At_LF   : Boolean := False;
      end record;

      procedure Open
        (File     : in out Handle;
         Filename : GPR2.Path_Name.Object'Class);
      --  Open Filename and initialize the corresponding handle

      procedure Close (File : in out Handle);

      function Get_Token
        (File           : in out Handle;
         Stop_At_LF     : Boolean := False;
         May_Be_Quoted  : Boolean := False;
         May_Have_Space : Boolean := False) return String
        with Pre => File.FD /= FS.Invalid_FD;
      --  Get next token on the file.
      --  If Stop_At_LF is True, then no token will be read after the current
      --  line is finished; calling again with Stop_At_LF => False (through a
      --  call to Next_Line for instance) allows further reading.
      --  The result is "" if either we got LF in Stop_At_LF mode, or EOF.

      procedure Next_Line
        (File      : in out Handle;
         Header    :    out Character;
         Expected  : Character := ASCII.NUL;
         Allow_EOF : Boolean   := False)
        with Pre => File.FD /= FS.Invalid_FD;
      --  Skips to the next (non-empty) line and sets Header.
      --  If no such line exists, Header is set to NUL.

   end IO;

   function Get_Checksum (A_Handle : in out IO.Handle) return Word;

   function To_Unit_Name
     (Current_View : GPR2.Project.View.Object;
      Sfile        : Simple_Name)
      return String;
   --  Compatibility mode for old GNAT compilers where the unit name and
   --  kind is not recorded as part of the Dependency lines.

   --------
   -- IO --
   --------

   package body IO is

      procedure Fill_Buffer (File : in out Handle)
        with Inline, Post => File.Current = 0;
      --  Read a chunk of data in the buffer or nothing if there is no more
      --  data to read.

      function Next_Char (File : in out Handle) return Character
        with Inline;

      -----------
      -- Close --
      -----------

      procedure Close (File : in out Handle) is
      begin
         if File.FD /= FS.Invalid_FD then
            FS.Close (File.FD);
            File.FD := FS.Invalid_FD;
         end if;
      end Close;

      -----------------
      -- Fill_Buffer --
      -----------------

      procedure Fill_Buffer (File : in out Handle) is
      begin
         File.Last := FS.Read (File.FD, Buffer => File.Buffer);
         File.Current := 0;
      end Fill_Buffer;

      ---------------
      -- Get_Token --
      ---------------

      function Get_Token
        (File           : in out Handle;
         Stop_At_LF     : Boolean := False;
         May_Be_Quoted  : Boolean := False;
         May_Have_Space : Boolean := False) return String
      is
         --  Get next char in buffer

         subtype Delimiter is Character
           with Static_Predicate =>
             Delimiter in ASCII.HT | ASCII.CR | ASCII.LF | ASCII.EOT;
         subtype Delimiter_Or_Space is Character
           with Static_Predicate =>
             Delimiter_Or_Space in Delimiter | ' ';

         Cur : Token_Index := 0;
         C   : Character     := ASCII.NUL;

      begin
         Read_Token : loop
            if Stop_At_LF and then File.At_LF then
               return "";
            end if;

            C := Next_Char (File);

            if C not in Delimiter_Or_Space then
               if May_Be_Quoted and then C = '"' then
                  --  Get Quoted word
                  declare
                     QN : Natural := 1;
                     --  Number of quotes seen
                  begin
                     loop
                        C := Next_Char (File);

                        if C = '"' then
                           QN := QN + 1;

                           if QN = 3 then
                              --  Two quotes is escaped quote literal
                              Cur := Cur + 1;
                              File.Token (Cur) := C;
                              QN := 1;
                           end if;

                        elsif QN = 2 then
                           if C in Delimiter_Or_Space then
                              --  Had an ending quote followed by a delimiter:
                              --  done

                              exit;

                           else
                              --  Had an ending quote followed by regular
                              --  characters: error !

                              raise Scan_ALI_Error with
                                "Wrong quoted format of '"
                                & File.Token (1 .. Cur) & ''';
                           end if;

                        elsif C in ASCII.LF | ASCII.EOT then
                           raise Scan_ALI_Error with
                             "Wrong quoted format of '" & File.Token (1 .. Cur)
                             & ''';

                        else
                           Cur := Cur + 1;
                           File.Token (Cur) := C;
                        end if;
                     end loop;
                  end;

               elsif May_Have_Space then
                  --  Word with potential space in it
                  declare
                     Next_C : Character;
                  begin
                     loop
                        Cur := Cur + 1;
                        File.Token (Cur) := C;

                        C := Next_Char (File);

                        if C = ' ' then
                           Next_C := Next_Char (File);

                           if Next_C = ' ' then
                              --  Two consecutive space means end of token

                              exit;
                           else
                              Cur := Cur + 1;
                              File.Token (Cur) := C;
                              C := Next_C;
                           end if;
                        end if;

                        exit when C in Delimiter;
                     end loop;
                  end;

               else
                  loop
                     Cur := Cur + 1;
                     File.Token (Cur) := C;

                     C := Next_Char (File);

                     exit when C in Delimiter_Or_Space;
                  end loop;
               end if;

               exit Read_Token;

            elsif C = ASCII.EOT then
               Cur := 0;
               exit Read_Token;
            end if;
         end loop Read_Token;

         return File.Token (1 .. Cur);
      end Get_Token;

      ---------------
      -- Next_Char --
      ---------------

      function Next_Char (File : in out Handle) return Character is
      begin
         if File.Current = File.Last then
            Fill_Buffer (File);

            if File.Last = 0 then
               --  Nothing more to read
               return ASCII.EOT;
            end if;
         end if;

         File.Current := File.Current + 1;

         declare
            Result : constant Character :=
                       File.Buffer (File.Current);
         begin
            if Result = ASCII.LF then
               File.At_LF := True;
               File.Line := File.Line + 1;
            else
               File.At_LF := False;
            end if;

            return Result;
         end;
      end Next_Char;

      ---------------
      -- Next_Line --
      ---------------

      procedure Next_Line
        (File      : in out Handle;
         Header    :    out Character;
         Expected  : Character := ASCII.NUL;
         Allow_EOF : Boolean   := False)
      is
         At_EOF : Boolean := False;
      begin
         Header := ASCII.NUL;

         while not File.At_LF loop
            declare
               C : constant Character := Next_Char (File);
            begin
               if C = ASCII.EOT then
                  Header := ASCII.NUL;
                  At_EOF := True;

                  exit;
               end if;
            end;
         end loop;

         --  Get first char after line break, taking care of CR/LF case

         while not At_EOF loop
            Header := Next_Char (File);

            if Header = ASCII.EOT then
               Header := ASCII.NUL;
               At_EOF := True;

               exit;
            end if;

            exit when Header not in ASCII.CR | ASCII.LF;
         end loop;

         if Expected /= ASCII.NUL
           and then Header /= ASCII.NUL
           and then Header /= Expected
         then
            raise Scan_ALI_Error with
              "Expected '" & Expected & "' but got '" & Header & "' in " &
              File.Path.Value;

         elsif not Allow_EOF and then At_EOF then
            raise Scan_ALI_Error with
              "Unexpected end of file in " & File.Path.Value;
         end if;
      end Next_Line;

      ----------
      -- Open --
      ----------

      procedure Open
        (File     : in out Handle;
         Filename : GPR2.Path_Name.Object'Class) is
      begin
         File.FD := FS.Open (Filename.Value, FS.Read_Mode);

         if File.FD = FS.Invalid_FD then
            raise Ada.IO_Exceptions.Use_Error with
              "Cannot read " & Filename.Value;
         end if;
         Fill_Buffer (File);
         File.Line := 1;
         File.At_LF := True;
      end Open;

   end IO;

         --  function Kind_Len return Natural is
         --    (if Kind in S_Spec | S_Body and then not Taken_From_Tree
         --     then 2 else 0);
         --  Length of suffix denoting dependency kind

   -------------
   -- Compute --
   -------------

   overriding procedure Compute
     (Self     : Object;
      Data     : in out Object_Info.Object'Class;
      Messages : in out Log.Object)
   is

      subtype CU_Index is Natural range 0 .. 2;

      A_Handle : IO.Handle;
      --  File handle

      Header   : Character;

      --  Unit data

      U_Kind  : Main_CU_Part;
      U_Flags : GPR2.Unit.Flags_Set;

      CU_Idx  : CU_Index := 0;

   begin
      if not Data.LI_Path.Is_Defined or else not Data.LI_Path.Exists then
         Data.Parsed := None;
         Data.Dependencies.Clear;

         for S of Data.Includes loop
            S.Clear;
         end loop;

         return;
      end if;

      --  Fill the ALI object up till the XREFs

      IO.Open (A_Handle, Data.LI_Path);

      --  Version (V line, mandatory)

      IO.Next_Line (A_Handle, Header, Expected => 'V');

      --  Skip version string

      loop
         IO.Next_Line (A_Handle, Header);

         case Header is
            when 'M' =>
               --  Scan main program type if exists

               declare
                  Tok : constant String :=
                          IO.Get_Token (A_Handle, Stop_At_LF => True);
               begin
                  if Tok = "F" then
                     Data.Main := GPR2.Unit.Is_Function;
                  elsif Tok = "P" then
                     Data.Main := GPR2.Unit.Is_Procedure;
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

      --  Skip to Units (U lines)

      while Header /= 'U' loop
         IO.Next_Line (A_Handle, Header);
      end loop;

      --  Read Units + {Withs}

      U_Loop :
      while Header = 'U' loop
         --  Cannot have more than 2 units:  the spec and the body

         if CU_Idx = 2 then
            raise Scan_ALI_Error;
         end if;

         --  Uname, Sfile, Utype

         declare
            Tok1 : constant String :=
                     IO.Get_Token (A_Handle, Stop_At_LF => True);
            Tok2 : constant String :=
                     IO.Get_Token (A_Handle,
                                   Stop_At_LF     => True,
                                   May_Be_Quoted  => True,
                                   May_Have_Space => True);
         begin
            --  At least "?%(b|s)"

            if Tok1'Length < 3 and then Tok1 (Tok1'Last - 1) /= '%' then
               raise Scan_ALI_Error with "Wrong unit name format " & Tok1;
            end if;

            if Tok2 = "" then
               raise Scan_ALI_Error with "File name absent in U line";
            end if;

            --  Set Utype. This will be adjusted after we finish reading the
            --  U lines, in case we have both spec and body.

            if CU_Idx = 0 then
               --  Update compilation unit name
               Data.CU.Name := +Tok1 (1 .. Tok1'Last - 2);
            end if;

            case Tok1 (Tok1'Last) is
               when 's'    => U_Kind := CU_Spec;
               when 'b'    => U_Kind := CU_Body;
               when others =>
                  raise Scan_ALI_Error with "Wrong unit name " & Tok1;
            end case;

            if U_Kind = CU_Spec then
               Data.CU.Spec.Source := +Tok2;
            else
               Data.CU.Implem.Source := +Tok2;
            end if;
         end;

         --  Unit Flags (we skip the unit version)

         U_Flags := GPR2.Unit.Default_Flags;

         U_Flags_Loop : loop
            declare
               use GPR2.Unit;

               Tok    : constant String :=
                          IO.Get_Token (A_Handle, Stop_At_LF => True);
               C1, C2 : Character;

            begin
               exit U_Flags_Loop when Tok = "";

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
                        case U_Kind is
                           when CU_Spec =>
                              Data.CU.Spec.L_Type := GPR2.Unit.Is_Package;
                           when CU_Body =>
                              Data.CU.Implem.L_Type := GPR2.Unit.Is_Package;
                        end case;
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
                        case U_Kind is
                           when CU_Spec =>
                              Data.CU.Spec.L_Type := GPR2.Unit.Is_Subprogram;
                           when CU_Body =>
                              Data.CU.Implem.L_Type := GPR2.Unit.Is_Subprogram;
                        end case;
                     end if;
                  end if;
               end if;
            end;
         end loop U_Flags_Loop;

         case U_Kind is
            when CU_Spec =>
               Data.CU.Spec.Flags := U_Flags;
            when CU_Body =>
               Data.CU.Implem.Flags := U_Flags;
         end case;

         --  Done parsing the unit info: now get the list of withed units

         IO.Next_Line (A_Handle, Header);

         --  For each unit, read the With list (W/Y/Z lines) if any

         Data.Includes (U_Kind).Clear;

         With_Loop :
         while Header in 'W' | 'Y' | 'Z' loop
            --  Only record explicite with clauses
            if Header in 'W' | 'Y' then
               declare
                  N : constant String := IO.Get_Token (A_Handle,
                                                       Stop_At_LF => True);
                  S : constant String := IO.Get_Token (A_Handle,
                                                       Stop_At_LF     => True,
                                                       May_Have_Space => True)
                    with Unreferenced;
                  A : constant String := IO.Get_Token (A_Handle,
                                                       Stop_At_LF     => True,
                                                       May_Have_Space => True)
                    with Unreferenced;

                  U_Last : constant Integer := N'Last - 2;
                  --  Unit last character in N
               begin
                  if U_Last <= 0
                    or else N (N'Last - 1) /= '%'
                    or else N (N'Last) not in 's' | 'b'
                  then
                     raise Scan_ALI_Error with
                       "Wrong unit name format """ & N & '"';
                  end if;

                  --  Insert the withed unit without the sloc information. This
                  --  information is found at the end of the ALI file and will
                  --  be setup later.

                  declare
                     --  ??? line & column are wrong, we should parse the xref
                     Sloc : constant GPR2.Source_Reference.Object :=
                              GPR2.Source_Reference.Object
                                (Source_Reference.Create
                                   (-(if U_Kind = CU_Spec
                                      then Data.CU.Spec.Source
                                      else Data.CU.Implem.Source), 1, 1));
                     Position : Include_Sets.Cursor;
                     Inserted : Boolean;
                  begin
                     --  With lines could be duplicated, ignore the next
                     --  duplicated one

                     Data.Includes (U_Kind).Insert
                       (Source_Reference.Identifier.Object
                          (Source_Reference.Identifier.Create
                               (Sloc => Sloc,
                                Text => Name_Type (N (1 .. U_Last)))),
                        Position, Inserted);
                  end;
               end;
            end if;

            IO.Next_Line (A_Handle, Header);
         end loop With_Loop;

         CU_Idx := CU_Idx + 1;

         --  Skip to either the next U section, or the first D line.
         --  There must be at least one D line: the dependency to the
         --  unit itself.

         while Header not in 'U' | 'D' loop
            IO.Next_Line (A_Handle, Header);
         end loop;
      end loop U_Loop;

      --  Read Deps

      Data.Dependencies.Clear;

      D_Loop :
      while Header = 'D' loop
         declare
            Sfile    : constant String            :=
                         IO.Get_Token (A_Handle, True, True, True);
            Stamp    : constant Ada.Calendar.Time :=
                         To_Time (IO.Get_Token (A_Handle, True));
            Chksum   : constant Word              := Get_Checksum (A_Handle);
            Uname    : constant String            :=
                         IO.Get_Token (A_Handle, Stop_At_LF => True);
            --  Could be empty on *.adc file dependency, preprocessor files or
            --  on GNAT version 7.2.2 and older.
            Name     : constant String :=
                         (if Uname = "" and then Chksum /= 0
                          -- before GNAT 7.2.2
                          then To_Unit_Name (Data.View, Simple_Name (Sfile))
                          else Uname);
            --  Unit_Name
            Suffix   : constant String :=
                         (if Uname'Length > 2
                          then Uname (Uname'Last - 1 .. Uname'Last)
                          else "");

            Kind     : CU_Part;
            Ignore   : Boolean := False;

         begin
            if Suffix = "%s" then
               Kind := CU_Spec;

            elsif Suffix = "%b" then
               Kind := CU_Body;

            elsif Name = "" then
               --  *.adc and preprocessor file dependencies
               Ignore := True;

            else
               Kind := CU_Separate;

               if GNATCOLL.Utils.Starts_With
                 (Name, Prefix => To_Lower (Unit_Name (Data)) & '.')
               then
                  Data.CU.Separates.Append
                    ((Sub_Unit => +Name (Name'First + Length (Data.CU.Name) + 1
                                           .. Name'Last),
                      Source   => +Sfile,
                      Stamp    => Stamp));
               end if;
            end if;

            if not Ignore then
               Data.Dependencies.Append
                 ((Name_Length  => Name'Length - Suffix'Length,
                   SFile_Length => Sfile'Length,
                   Unit_Kind    => Kind,
                   Stamp        => Stamp,
                   Checksum     => Chksum,
                   Unit_Name    => Name_Type (Name
                                    (Name'First .. Name'Last - Suffix'Length)),
                   Sfile        => Filename_Type (Sfile)));
            end if;

            IO.Next_Line (A_Handle, Header, Allow_EOF => True);
            --  Only here do we allow EOF
         end;
      end loop D_Loop;

      IO.Close (A_Handle);

   exception
      when E : others =>
         Messages.Append
           (Message.Create
              (Message.Warning,
               "ALI parser error: " & Ada.Exceptions.Exception_Message (E),
               Source_Reference.Create
                 (Data.LI_Path.Value, 1, 1)));

         IO.Close (A_Handle);
         Data.Parsed := None;
   end Compute;

   ------------------
   -- Get_Checksum --
   ------------------

   function Get_Checksum (A_Handle : in out IO.Handle) return Word is
      S   : constant String := IO.Get_Token (A_Handle);
      Chk : Word := 0;
      Hex : Word;
   begin
      if S'Length /= 8 then
         raise Scan_ALI_Error with
           "Wrong checksum length" & S'Length'Img;
      end if;

      for C of S loop
         case C is
            when '0' .. '9' =>
               Hex := Character'Pos (C) - Character'Pos ('0');

            when 'a' .. 'f' =>
               Hex := Character'Pos (C) - Character'Pos ('a') + 10;

            when others =>
               raise Scan_ALI_Error with
                 "Wrong character '" & C & "' in checksum";
         end case;

         Chk := Chk * 16 + Hex;
      end loop;

      return Chk;
   end Get_Checksum;

   ------------------
   -- To_Unit_Name --
   ------------------

   function To_Unit_Name
     (Current_View : GPR2.Project.View.Object;
      Sfile        : Simple_Name)
      return String
   is
      Path : constant Path_Name.Object :=
               Path_Name.Create_File (Sfile, Path_Name.No_Resolution);
      Db   : Build.View_Db.Object;
      Src  : Build.View_Db.Source_Context;
      Kind : Build.Unit_Kind;

   begin
      for V of Current_View.Namespace_Roots loop
         Db := Current_View.Tree.Artifacts_Database (V);
         Src := Db.Visible_Source (Sfile);
         exit when Src.Source.Is_Defined;
      end loop;

      --  The runtime is not always registerd in the tree, to speed up
      --  computation. So perform an explicit check here.

      if not Src.Source.Is_Defined
        and then Current_View.Tree.Has_Runtime_Project
        and then Current_View.Tree.Runtime_Project.Has_Source (Sfile)
      then
         Db := Current_View.Tree.Artifacts_Database
           (Current_View.Tree.Runtime_Project);
         Src := Db.Visible_Source (Sfile);
      end if;

      if Src.Source.Is_Defined then
         if Src.Source.Units.Is_Indexed_List then
            --  No way to disambiguate: pick up the first unit.
            --  Note: this is a corner case, as support for Multi-Unit
            --  sources with antique compilers is not really a user case.

            Kind := Src.Source.Kind (Multi_Unit_Index'First);

            return To_Lower
                     (Src.Source.Unit (Multi_Unit_Index'First).Unit_Name)
              & (if Kind = Build.S_Spec then "%s"
                 elsif Kind = Build.S_Body then "%b"
                 else "");

         else
            Kind := Src.Source.Kind (No_Index);

            return To_Lower (Src.Source.Unit (No_Index).Unit_Name);
         end if;

      else
         return String (Path.Base_Name);
      end if;
   end To_Unit_Name;

end GPR2.Build.Object_Info.Parser.ALI;
