--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with GNATCOLL.Utils;

with GPR2.Project.Unit_Info;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Source_Info.Parser.Registry;

package body GPR2.Source_Info.Parser.ALI is

   Handle : Object;

   Scan_ALI_Error : exception;

   function "+" (Item : String) return Unbounded_String
                 renames To_Unbounded_String;

   procedure Compute
     (Self   : in out Object;
      Data   : in out Source_Info.Object'Class;
      Source : GPR2.Path_Name.Object;
      LI     : Path_Name.Object'Class;
      U_Ref  : in out GPR2.Unit.Object;
      View   : Project.View.Object);
   --  Parse single ALI file

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
        (File           : in out Handle;
         Stop_At_LF     : Boolean := False;
         May_Be_Quoted  : Boolean := False;
         May_Have_Space : Boolean := False) return String
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

   end IO;

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
         if Stream_IO.Is_Open (File.FD) then
            Stream_IO.Close (File.FD);
         end if;
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
        (File           : in out Handle;
         Stop_At_LF     : Boolean := False;
         May_Be_Quoted  : Boolean := False;
         May_Have_Space : Boolean := False) return String
      is
         --  Get next char in buffer

         subtype Content_Index is Natural range 0 .. 1_024;
         subtype Content_Range is Content_Index range 1 .. Content_Index'Last;

         subtype Delimiter is Character
           with Static_Predicate =>
             Delimiter in ASCII.HT | ASCII.CR | ASCII.LF | ASCII.EOT;
         subtype Delimiter_Or_Space is Character
           with Static_Predicate =>
             Delimiter_Or_Space in Delimiter | ' ';

         Tok         : String (Content_Range);
         Cur         : Content_Index := 0;
         C           : Character     := ASCII.NUL;

         procedure Get_Quoted_Word;
         procedure Get_Word_With_Space;
         procedure Get_Word;
         --  Read a word, result will be in Tok (Tok'First .. Cur)

         ---------------------
         -- Get_Quoted_Word --
         ---------------------

         procedure Get_Quoted_Word is
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
                     Tok (Cur) := C;
                     QN := 1;
                  end if;

               elsif QN = 2 then
                  if C in Delimiter_Or_Space then
                     --  Had an ending quote followed by a delimiter: done

                     return;

                  else
                     --  Had an ending quote followed by regular characters:
                     --  error !

                     raise Scan_ALI_Error with
                       "Wrong quoted format of '" & Tok (Tok'First .. Cur)
                       & ''';
                  end if;

               elsif C in ASCII.LF | ASCII.EOT then
                  raise Scan_ALI_Error with
                    "Wrong quoted format of '" & Tok (Tok'First .. Cur)
                    & ''';

               else
                  Cur := Cur + 1;
                  Tok (Cur) := C;
               end if;
            end loop;
         end Get_Quoted_Word;

         --------------
         -- Get_Word --
         --------------

         procedure Get_Word is
         begin
            loop
               Cur := Cur + 1;
               Tok (Cur) := C;

               C := Next_Char (File);

               exit when C in Delimiter_Or_Space;
            end loop;
         end Get_Word;

         -------------------------
         -- Get_Word_With_Space --
         -------------------------

         procedure Get_Word_With_Space is
            Next_C : Character;
         begin
            loop
               Cur := Cur + 1;
               Tok (Cur) := C;

               C := Next_Char (File);

               if C = ' ' then
                  Next_C := Next_Char (File);

                  if Next_C = ' ' then
                     --  Two consecutive space means end of token

                     exit;
                  else
                     Cur := Cur + 1;
                     Tok (Cur) := C;
                     C := Next_C;
                  end if;
               end if;

               exit when C in Delimiter;
            end loop;
         end Get_Word_With_Space;

      begin
         Read_Token : loop
            if Stop_At_LF and then File.At_LF then
               return "";
            end if;

            C := Next_Char (File);

            if C not in Delimiter_Or_Space then
               if May_Be_Quoted and then C = '"' then
                  Get_Quoted_Word;
               elsif May_Have_Space then
                  Get_Word_With_Space;
               else
                  Get_Word;
               end if;

               exit Read_Token;

            elsif C = ASCII.EOT then
               Cur := 0;
               exit Read_Token;
            end if;
         end loop Read_Token;

         return Tok (1 .. Cur);
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
                       Character'Val (File.Buffer (File.Current));
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

      procedure Next_Line (File : in out Handle; Header : in out Character) is
      begin
         while not File.At_LF loop
            declare
               C : constant Character := Next_Char (File);
            begin
               if C = ASCII.EOT then
                  Header := ASCII.NUL;

                  return;
               end if;
            end;
         end loop;

         loop
            Header := Next_Char (File);
            if Header = ASCII.EOT then
               Header := ASCII.NUL;

               return;
            end if;

            exit when Header not in ASCII.CR | ASCII.LF;
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
         File.At_LF := True;
      end Open;

   end IO;

   -----------------
   -- Clear_Cache --
   -----------------

   overriding procedure Clear_Cache (Self : not null access Object) is
   begin
      Self.Cache.Clear;
      Self.Sep_Cache.Clear;
   end Clear_Cache;

   -------------
   -- Compute --
   -------------

   procedure Compute
     (Self   : in out Object;
      Data   : in out Source_Info.Object'Class;
      Source : GPR2.Path_Name.Object;
      LI     : Path_Name.Object'Class;
      U_Ref  : in out GPR2.Unit.Object;
      View   : Project.View.Object)
   is
      use all type GPR2.Unit.Library_Unit_Type;

      A_Handle : IO.Handle;

      --  Unit data

      subtype CU_Index is Natural range 0 .. 2;

      LI_Idx  : constant Unit_Index  := U_Ref.Index;
      B_Name  : constant Simple_Name := Source.Simple_Name;
      U_Name  : Unbounded_String;
      S_Name  : Unbounded_String;
      U_Kind  : GPR2.Unit.Library_Unit_Type;
      U_Flags : GPR2.Unit.Flags_Set         := GPR2.Unit.Default_Flags;
      Main    : GPR2.Unit.Main_Type         := GPR2.Unit.None;
      L_Type  : GPR2.Unit.Library_Item_Type := GPR2.Unit.Is_Package;
      Withs   : Source_Reference.Identifier.Set.Object;

      CUs     : array (CU_Index range 1 .. 2) of GPR2.Unit.Object;
      CU_TS   : array (CU_Index range 1 .. 2) of Ada.Calendar.Time;
      CU_CS   : array (CU_Index range 1 .. 2) of Word;
      CU_BN   : array (CU_Index range 1 .. 2) of Unbounded_String;
      --  Units' corresponding source base name

      CU_Idx  : CU_Index := 0;
      Current : CU_Index := 0;

      procedure Fill_Dep (Map : Dependency_Vectors_Ref.Reference_Type);
      --  Add dependency from D line

      procedure Fill_Unit;
      --  Add all units defined in ALI (spec, body or both)

      procedure Fill_With;
      --  Add all withed units into Withs below

      procedure Set_Source_Info_Data
        (Cache : Cache_Holder; Add_Deps : Boolean);
      --  Set returned source info data

      function Key
        (LI       : Path_Name.Object'Class;
         Basename : Simple_Name;
         Kind     : GPR2.Unit.Library_Unit_Type) return Cache_Key
      is (LI_Length  => LI.Value'Length,
          LI         => Filename_Type (LI.Value),
          Src_Length => Basename'Length,
          Src        => Basename,
          LI_Kind    => (case Kind is
                            when GPR2.Unit.S_Spec_Only => GPR2.Unit.S_Spec,
                            when GPR2.Unit.S_Body_Only => GPR2.Unit.S_Body,
                            when others => Kind));
      --  Do not distingush between S_Spec/Body_Only and S_Spec/Body because
      --  package without body can be inherited from extended project and body
      --  defined in extending project. The same with S_Spec, but for procedure
      --  or function.

      --------------
      -- Fill_Dep --
      --------------

      procedure Fill_Dep (Map : Dependency_Vectors_Ref.Reference_Type) is

         function Checksum (S : String) return Word;

         function Get_Token
           (May_Be_Quoted  : Boolean := False;
            May_Have_Space : Boolean := False) return String;

         function To_Unit_Name return String;
         --  This routine is needed only on GNAT version 7.2.2 and older,
         --  because unit name and kind is not defined in D lines.
         --  Get unit name from project tree if available.
         --  Put unresolved dependency into queue to resolve it later.

         --------------
         -- Checksum --
         --------------

         function Checksum (S : String) return Word is
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
         end Checksum;

         ---------------
         -- Get_Token --
         ---------------

         function Get_Token
           (May_Be_Quoted  : Boolean := False;
            May_Have_Space : Boolean := False) return String
         is
            Tok : constant String :=
                    IO.Get_Token
                      (A_Handle,
                       Stop_At_LF     => True,
                       May_Be_Quoted  => May_Be_Quoted,
                       May_Have_Space => May_Have_Space);
         begin
            if Tok = "" then
               raise Scan_ALI_Error with "Missed dependency field";
            else
               return Tok;
            end if;
         end Get_Token;

         Sfile  : constant String            := Get_Token (True, True);
         Stamp  : constant Ada.Calendar.Time := To_Time (Get_Token);
         Chksum : constant Word              := Checksum (Get_Token);
         Forth  : constant String            :=
                    IO.Get_Token (A_Handle, Stop_At_LF => True);
         --  Could be empty on *.adc file dependency, preprocessor files or on
         --  GNAT version 7.2.2 and older.

         use GPR2.Unit;

         Kind : Library_Unit_Type;
         Taken_From_Tree : Boolean := False;

         ------------------
         -- To_Unit_Name --
         ------------------

         function To_Unit_Name return String is
            Path : constant Path_Name.Object :=
                     Path_Name.Create_File
                       (Simple_Name (Sfile), Path_Name.No_Resolution);
            Tree : constant access Project.Tree.Object := View.Tree;
            View : Project.View.Object := Tree.Get_View (Path);
            Source : Project.Source.Object;
         begin
            --  The runtime is not always registerd in the tree, to speed up
            --  computation. So perform an explicit check here.

            if not View.Is_Defined
              and then Tree.Has_Runtime_Project
              and then Tree.Runtime_Project.Has_Source (Simple_Name (Sfile))
            then
               View := Tree.Runtime_Project;
            end if;

            if View.Is_Defined then
               Source := View.Source (Path);

               pragma Assert (Source.Is_Defined);

               Taken_From_Tree := True;
               if Source.Units.Is_Indexed_List then
                  --  No way to disambiguate: pick up the first unit.
                  --  Note: this is a corner case, as support for Multi-Unit
                  --  sources with antique compilers is not really a user case.

                  Kind := Source.Kind (Multi_Unit_Index'First);

                  return To_Lower (Source.Unit_Name (Multi_Unit_Index'First));

               else
                  Kind := Source.Kind (No_Index);

                  return To_Lower (Source.Unit_Name (No_Index));
               end if;

            else
               return String (Path.Base_Name);
            end if;
         end To_Unit_Name;

         Name : constant String :=
                  (if Forth = "" and then Chksum /= 0 -- GNAT 7.2.2 and older
                   then To_Unit_Name else Forth);
         --  Unit_Name

         function Kind_Len return Natural is
           (if Kind in S_Spec | S_Body and then not Taken_From_Tree
            then 2 else 0);
         --  Length of suffix denoting dependency kind

         Suffix : constant String :=
                     (if Forth'Length > 2
                      then Forth (Forth'Last - 1 .. Forth'Last)
                      else "");
         C_Cache  : Sep_Cache_Map.Cursor;
         Inserted : Boolean;
         H_Cache  : Cache_Holder;

      begin
         if Suffix = "%s" then
            Kind := S_Spec;

         elsif Suffix = "%b" then
            Kind := S_Body;

         elsif Name = "" then
            --  *.adc and preprocessor file dependencies

            return;

         elsif not Taken_From_Tree then
            Kind := S_Separate;

            if GNATCOLL.Utils.Starts_With
                 (Name, Prefix => To_Lower (CUs (1).Name) & '.')
            then
               H_Cache :=
                 (Create
                    (Name          => Name_Type (Name),
                     Index         => No_Index,   -- Unknown for now
                     Lib_Unit_Kind => S_Separate,
                     Lib_Item_Kind => Is_Package, -- No way to know from ALI
                     Main          => None,       -- No way to know from ALI
                     Dependencies  =>
                       Source_Reference.Identifier.Set.Empty_Set,
                     Sep_From      => Parent_Name (Name_Type (Name)),
                     Flags         => Default_Flags),
                  Chksum, Stamp, Data.Dependencies (LI_Idx));

               Self.Sep_Cache.Insert (Name, H_Cache, C_Cache, Inserted);

               if Inserted
                 and then not Taken_From_Tree
                 and then View.Is_Runtime
               then
                  --  We have to index separated unit in cache by the fake unit
                  --  name calculated from runtime source filename, a.excach
                  --  for a-excach.adb.

                  declare
                     U_Name : constant String := To_Unit_Name;
                  begin
                     if U_Name /= Name then
                        Self.Sep_Cache.Insert
                          (U_Name, H_Cache, C_Cache, Inserted);
                     end if;
                  end;
               end if;

               if not Inserted
                 and then Self.Sep_Cache (C_Cache) /= H_Cache
               then
                  raise Scan_ALI_Error with "subunit inconsistency " & Name &
                    ' ' &
                    (if Self.Sep_Cache (C_Cache).Unit.Separate_From
                     /= CUs (1).Name
                     then String (Self.Sep_Cache (C_Cache).Unit.Separate_From)
                     & " # " & String (CUs (1).Name) else "");
               end if;
            end if;
         end if;

         for Idx in CU_BN'Range loop
            if Sfile = CU_BN (Idx) then
               CU_TS (Idx) := Stamp;
               CU_CS (Idx) := Chksum;
            end if;
         end loop;

         Map.Append
           ((Name_Length  => Name'Length - Kind_Len,
             SFile_Length => Sfile'Length,
             Unit_Kind    => Kind,
             Stamp        => Stamp,
             Checksum     => Chksum,
             Unit_Name    => Name (Name'First .. Name'Last - Kind_Len),
             Sfile        => Sfile));
      end Fill_Dep;

      ---------------
      -- Fill_Unit --
      ---------------

      procedure Fill_Unit is
      begin
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

            case Tok1 (Tok1'Last) is
               when 's'    => U_Kind := GPR2.Unit.S_Spec_Only;
               when 'b'    => U_Kind := GPR2.Unit.S_Body_Only;
               when others =>
                  raise Scan_ALI_Error with "Wrong unit name " & Tok1;
            end case;

            U_Name := +Tok1 (1 .. Tok1'Last - 2);
            S_Name := +Tok2;
         end;

         --  Flags (we skip the unit version)

         loop
            declare
               use GPR2.Unit;

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
                        L_Type := GPR2.Unit.Is_Package;
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
                        L_Type := GPR2.Unit.Is_Subprogram;
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
         S : constant String := IO.Get_Token (A_Handle,
                                              Stop_At_LF     => True,
                                              May_Have_Space => True)
               with Unreferenced;
         A : constant String := IO.Get_Token (A_Handle,
                                              Stop_At_LF     => True,
                                              May_Have_Space => True)
               with Unreferenced;

         U_Last : constant Integer := N'Last - 2; -- Unit last character in N
         U_Kind : GPR2.Unit.Library_Unit_Type with Unreferenced;
      begin
         if U_Last <= 0
           or else N (N'Last - 1) /= '%'
           or else N (N'Last) not in 's' | 'b'
         then
            raise Scan_ALI_Error with "Wrong unit name format """ & N & '"';
         end if;

         --  Insert the withed unit without the sloc information. This
         --  information is found at the end of the ALI file and will be
         --  setup later.

         declare
            --  ??? line & column are wrong, we should parse the xref
            Sloc : constant GPR2.Source_Reference.Object :=
                     GPR2.Source_Reference.Object
                       (Source_Reference.Create (Source.Value, 1, 1));
            Position : Source_Reference.Identifier.Set.Cursor;
            Inserted : Boolean;
         begin
            --  With lines could be duplicated, ignore the next duplicated one

            Withs.Insert
              (Source_Reference.Identifier.Create
                 (Sloc => Sloc, Text => Name_Type (N (1 .. U_Last))),
              Position, Inserted);
         end;
      end Fill_With;

      --------------------------
      -- Set_Source_Info_Data --
      --------------------------

      procedure Set_Source_Info_Data
        (Cache    : Cache_Holder;
         Add_Deps : Boolean)
      is
         Position   : Unit_Dependencies.Cursor;
         Inserted   : Boolean;
      begin
         pragma Assert
           ((U_Ref.Kind in GPR2.Unit.Spec_Kind)
            = (Cache.Unit.Kind in GPR2.Unit.Spec_Kind),
            "cache inconsistent: " & String (U_Ref.Name)
            & Cache.Unit.Index'Img & ' ' & U_Ref.Kind'Img
            & ' ' & Cache.Unit.Kind'Img);

         U_Ref := Cache.Unit;

         --  Preserve the U_Ref index: we can't know the index from the ALI
         --  file, only the naming attributes of the project file define
         --  those.

         U_Ref.Update_Index (LI_Idx);

         Data.Language := Ada_Language;
         Data.Checksum := Cache.Checksum;

         if LI_Idx = No_Index then
            Data.LI_Timestamp := Cache.Timestamp;
            Data.Parsed       := Source_Info.LI;
         else
            Data.CU_Info (LI_Idx) := (Cache.Timestamp, Source_Info.LI);
         end if;

         if Add_Deps then
            Data.Dependencies.Insert
              (LI_Idx, Cache.Depends, Position, Inserted);
         end if;
      end Set_Source_Info_Data;

      use GPR2.Unit;

      In_Cache : Cache_Map.Cursor :=
                   Self.Cache.Find (Key (LI, B_Name, U_Ref.Kind));
      Inserted : Boolean;

   begin
      --  If this unit is in the cache, return now we don't want to parse
      --  again the whole file.

      if Cache_Map.Has_Element (In_Cache) then
         Set_Source_Info_Data
           (Cache_Map.Element (In_Cache), Add_Deps => True);
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

            if Expected /= ASCII.NUL and then Header /= Expected then
               raise Scan_ALI_Error with
                 "Expected '" & Expected & "' but got '" & Header & ''';
            end if;

            if not Allow_EOF and then Header = ASCII.NUL then
               raise Scan_ALI_Error with
                 "Unexpected end of file in " & LI.Value;
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
                        Main := GPR2.Unit.Is_Function;
                     elsif Tok = "P" then
                        Main := GPR2.Unit.Is_Procedure;
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

         while Header /= 'U' loop
            Next_Line;
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

            if U_Ref.Name /= Name_Type (-U_Name) then
               View.Reindex_Unit
                 (From => U_Ref.Name, To => Name_Type (-U_Name));
            end if;

            CUs (CU_Idx) :=
              GPR2.Unit.Create
                (Name          => Name_Type (-U_Name),
                 Index         => U_Ref.Index,
                 Lib_Unit_Kind => U_Kind,
                 Lib_Item_Kind => L_Type,
                 Main          => Main,
                 Dependencies  => Withs,
                 Sep_From      => No_Name,
                 Flags         => U_Flags);

            CU_BN (CU_Idx) := S_Name;

            Withs.Clear;

            if Simple_Name (-S_Name) = B_Name
              and then (U_Kind in GPR2.Unit.Spec_Kind)
                       = (U_Ref.Kind in GPR2.Unit.Spec_Kind)
            then
               Current := CU_Idx;
            end if;

            --  Skip to either the next U section, or the first D line.
            --  There must be at least one D line: the dependency to the
            --  unit itself.

            while Header not in 'U' | 'D' loop
               Next_Line;
            end loop;

            exit when Header /= 'U';
         end loop;

         if Current = 0 then
            if (CU_Idx = 1
                and then U_Ref.Kind = GPR2.Unit.S_Body
                and then CUs (1).Kind = GPR2.Unit.S_Spec_Only)
              or else
                (CU_Idx = 2
                 and then CU_BN (1) = CU_BN (2))
            then
               --  Body file with pragma No_Body

               View.Hide_Unit_Body (U_Ref.Name);
               View.Hide_Unit_Body (CUs (1).Name);
            end if;

            Data.Parsed := None;
            IO.Close (A_Handle);
            return;
         end if;

         --  Read Deps

         declare
            Data_Deps : Dependency_Vectors_Ref.Ref;
         begin
            Data_Deps.Set (Dependency_Vectors.Empty_Vector);
            Data.Dependencies.Insert (LI_Idx, Data_Deps);

            while Header = 'D' loop
               Fill_Dep (Data_Deps.Get);
               Next_Line (Allow_EOF => True);  --  Only here we allow EOF
            end loop;
         end;
         IO.Close (A_Handle);

         --  If we have 2 units, the first one should be the body and the
         --  second one the spec. Update the u_kind accordingly.

         if CU_Idx = 2 then
            pragma Assert (CUs (1).Name = CUs (2).Name);

            if CUs (1).Kind /= GPR2.Unit.S_Body_Only then
               raise Scan_ALI_Error
                 with "Unit body is on the wrong position";
            end if;

            if CUs (2).Kind /= GPR2.Unit.S_Spec_Only then
               raise Scan_ALI_Error
                 with "Unit spec is on the wrong position";
            end if;

            GPR2.Unit.Update_Kind (CUs (1), GPR2.Unit.S_Body);
            GPR2.Unit.Update_Kind (CUs (2), GPR2.Unit.S_Spec);
         end if;

         --  Record into the cache

         for K in 1 .. CU_Idx loop
            declare
               Cache : constant Cache_Holder :=
                         (CUs (K), CU_CS (K), CU_TS (K),
                          Data.Dependencies (LI_Idx));
            begin
               if Current = K then
                  Set_Source_Info_Data (Cache, Add_Deps => False);
               end if;

               Self.Cache.Insert
                 (Key (LI, Simple_Name (-CU_BN (K)), CUs (K).Kind),
                  Cache, In_Cache, Inserted);

               pragma Assert
                 (Inserted,
                  "couldn't record into cache: " & LI.Value
                  & ' ' & Image (Cache_Map.Key (In_Cache))
                  & Current'Img);
            end;
         end loop;

      exception
         when E : others =>
            Ada.Text_IO.Put_Line
              ("ALI parser error: " & LI.Value
               & ' ' & Ada.Exceptions.Exception_Information (E));

            IO.Close (A_Handle);
            Data.Parsed := None;
      end;
   end Compute;

   -------------
   -- Compute --
   -------------

   overriding procedure Compute
     (Self   : not null access Object;
      Data   : in out Source_Info.Object'Class;
      Source : Project.Source.Object)
   is
      View : constant Project.View.Object := Source.View;
      LI   : Path_Name.Object;
      File : constant Path_Name.Object := Source.Path_Name;

      procedure Check_Separated (SU : in out GPR2.Unit.Object);

      ---------------------
      -- Check_Separated --
      ---------------------

      procedure Check_Separated (SU : in out GPR2.Unit.Object) is

         Name     : constant Name_Type := SU.Name;
         Lown     : constant String := To_Lower (Name);
         FU       : Project.Unit_Info.Object;
         Src      : Project.Source.Object;
         Src_Idx  : Unit_Index;
         CS       : Sep_Cache_Map.Cursor := Self.Sep_Cache.Find (Lown);
         Last_Dot : Natural := Name'Last + 1;

         procedure Set_Data;
         --  Set some fields from cache to Source_Info.Object for subunit

         --------------
         -- Set_Data --
         --------------

         procedure Set_Data is
            Ref      : constant Sep_Cache_Map.Constant_Reference_Type :=
                         Self.Sep_Cache.Constant_Reference (CS);

         begin
            if SU.Name = Ref.Unit.Name then
               SU.Set_Separate_From (Ref.Unit.Separate_From);

            else
               --  Runtime krunched filename case

               SU := Ref.Unit;
            end if;

            if SU.Index = No_Index then
               Data.Parsed := Source_Info.LI;
               Data.LI_Timestamp := Ref.Timestamp;
            else
               Data.CU_Info (SU.Index) :=
                 (Ref.Timestamp, Source_Info.LI);
            end if;

            Data.Checksum := Ref.Checksum;
         end Set_Data;

      begin
         if Sep_Cache_Map.Has_Element (CS) then
            Set_Data;
            return;
         end if;

         --  The check must be done in a loop to catch separate of separates

         loop
            FU := Project.Unit_Info.Undefined;

            for J in reverse Name'First .. Last_Dot - 1 loop
               if Name (J) = '.' then
                  Last_Dot := J;
                  FU := View.Unit (Name (Name'First .. J - 1));
                  exit when FU.Is_Defined;
               end if;
            end loop;

            if not FU.Is_Defined or else not FU.Has_Body then
               return;
            end if;

            Src := View.Source (FU.Main_Body.Source);
            Src_Idx := FU.Main_Body.Index;

            declare
               LI_File : Path_Name.Object;
               CU      : constant GPR2.Unit.List.Reference_Type :=
                           Source_Info.Object'Class (Src).CU_List.Reference
                             (Src_Idx);
            begin
               if CU.Kind in GPR2.Unit.Body_Kind
                 and then CU.Name = FU.Name
               then
                  LI_File := GPR2.Project.Source.Artifact.Dependency
                    (Src, CU.Index, Actual_File => True);

                  if LI_File.Is_Defined then
                     Compute
                       (Self.all, Src, FU.Main_Body.Source, LI_File, CU, View);
                  end if;
               end if;
            end;

            if Src.Is_Parsed (Src_Idx) then
               CS := Self.Sep_Cache.Find (Lown);

               if Sep_Cache_Map.Has_Element (CS) then
                  Set_Data;
               end if;

               return;
            end if;
         end loop;
      end Check_Separated;

      use type GPR2.Unit.Library_Unit_Type;

   begin
      for CU of Data.CU_List loop
         LI := GPR2.Project.Source.Artifact.Dependency
           (Source, CU.Index,
            Actual_File   => True,
            Maybe_No_Body => True);

         --  if LI is not defined, check if the body has been parsed.
         --  Note that this happens in particular when the spec and body
         --  don't share the same basename.

         if not LI.Is_Defined and then CU.Kind = GPR2.Unit.S_Spec then
            declare
               Other : constant GPR2.Project.Source.Source_Part :=
                         Source.Other_Part (CU.Index);
            begin
               LI := GPR2.Project.Source.Artifact.Dependency
                 (Other.Source, Other.Index, Actual_File => True);
            end;
         end if;

         if LI.Is_Defined then
            Compute (Self.all, Data, File, LI, CU, View);
         elsif CU.Kind in GPR2.Unit.Body_Kind then
            Check_Separated (CU);
         end if;
      end loop;
   end Compute;

begin
   GPR2.Source_Info.Parser.Registry.Register (Handle);
end GPR2.Source_Info.Parser.ALI;
