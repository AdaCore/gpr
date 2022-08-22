--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Environment_Variables;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Regpat;

with GNATCOLL.Traces;

with GPR2.Containers;
with GPR2.Message;

package body GPR2.KB.Compiler_Iterator is

   Main_Trace : constant GNATCOLL.Traces.Trace_Handle :=
                  GNATCOLL.Traces.Create
                    ("KNOWLEDGE_BASE.COMPILER_ITERATOR",
                     GNATCOLL.Traces.Off);

   package Exec_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);

   package CDM renames Compiler_Description_Maps;

   procedure Foreach_Compiler_In_Dir
     (Iterator       : in out Object'Class;
      Base           : in out KB.Object;
      Directory      : String;
      From_Extra_Dir : Boolean;
      On_Target      : Targets_Set_Id;
      Path_Order     : Integer;
      Continue       : out Boolean);
   --  Find all known compilers in Directory, and call Iterator.Callback as
   --  appropriate.

   procedure Foreach_Language_Runtime
     (Iterator       : in out Object'Class;
      Base           : in out KB.Object;
      Name           : Unbounded_String;
      Executable     : Unbounded_String;
      Directory      : String;
      Prefix         : Unbounded_String;
      From_Extra_Dir : Boolean;
      On_Target      : Targets_Set_Id;
      Descr          : Compiler_Description;
      Path_Order     : Integer;
      Continue       : out Boolean);
   --  For each language/runtime parsed in Languages/Runtimes, create a new
   --  compiler in the list, if it matches Description

   procedure Get_Targets_Set
     (Base   : in out KB.Object;
      Target : String;
      Id     : out Targets_Set_Id);
   --  Get the target alias set id for a target.  If not already in the base,
   --  add it.

   -----------------------------
   -- Foreach_Compiler_In_Dir --
   -----------------------------

   procedure Foreach_Compiler_In_Dir
     (Iterator       : in out Object'Class;
      Base           : in out KB.Object;
      Directory      : String;
      From_Extra_Dir : Boolean;
      On_Target      : Targets_Set_Id;
      Path_Order     : Integer;
      Continue       : out Boolean)
   is
      use CDM;
      use Exec_Sets;
      use Ada.Directories;
      use GNAT.Regpat;
      use GNATCOLL.Traces;

      Executable_Pattern : constant String :=
                             (if On_Windows then "*.{exe,bat,cmd}" else "");
      Search             : Search_Type;
      Dir                : Directory_Entry_Type;
      Exec_Suffix        : OS_Lib.String_Access :=
                             GNAT.OS_Lib.Get_Executable_Suffix;
      Exec_Set           : Exec_Sets.Set;
   begin
      --  Since the name of an executable can be a regular expression, we need
      --  to look at all files in the directory to see if they match. This
      --  requires more system calls than if the name was always a simple
      --  string. So we first check which of the two algorithms should be used.

      Continue := True;

      Increase_Indent
        (Main_Trace,
         "Foreach compiler in "
         & Directory & " regexp="
         & Boolean'Image (Base.Check_Executable_Regexp)
         & " extra_dir=" & From_Extra_Dir'Img);

      if Base.Check_Executable_Regexp then
         begin
            Start_Search
              (Search    => Search,
               Directory => Directory,
               Pattern   => Executable_Pattern);
         exception
            when Ada.Directories.Name_Error =>
               Decrease_Indent
                 (Main_Trace, "No such directory:" & Directory);
               Continue := True;
               GNAT.OS_Lib.Free (Exec_Suffix);
               return;
            when Ada.Directories.Use_Error =>
               Decrease_Indent
                 (Main_Trace, "Directory not readable:" & Directory);
               Continue := True;
               GNAT.OS_Lib.Free (Exec_Suffix);
               return;
         end;

         loop
            exit when not More_Entries (Search);
            begin
               Get_Next_Entry (Search, Dir);
               Exec_Set.Include (Ada.Directories.Simple_Name (Dir));
            exception
               when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
                  null;
            end;
         end loop;

         For_All_Files_In_Dir : for Simple of Exec_Set loop
            for C in Base.Compilers.Iterate loop
               declare
                  Config  : constant CDM.Constant_Reference_Type :=
                              CDM.Constant_Reference (Base.Compilers, C);
                  Matches : Match_Array
                              (0 .. Integer'Max (0, Config.Prefix_Index));
                  Matched : Boolean;
                  Prefix  : Unbounded_String;
               begin
                  --  A language with no expected compiler => always match
                  if Config.Name = Null_Unbounded_String then
                     Increase_Indent
                       (Main_Trace,
                        String (Key (C))
                        & " requires no compiler");
                     Continue := True;

                     Foreach_Language_Runtime
                       (Iterator       => Iterator,
                        Base           => Base,
                        Name           =>
                          To_Unbounded_String (String (Key (C))),
                        Executable     => Null_Unbounded_String,
                        Directory      => "",
                        On_Target      => Unknown_Targets_Set,
                        Prefix         => Null_Unbounded_String,
                        From_Extra_Dir => From_Extra_Dir,
                        Descr          => Config,
                        Path_Order     => Path_Order,
                        Continue       => Continue);

                     Decrease_Indent (Main_Trace);

                     exit For_All_Files_In_Dir when not Continue;

                     Matched := False;

                  elsif not Config.Executable_Re.Is_Empty then
                     Match
                       (Config.Executable_Re.Element,
                        Data    => Simple,
                        Matches => Matches);
                     Matched := Matches (0) /= No_Match;

                  else
                     Matched :=
                       (To_String (Config.Executable) & Exec_Suffix.all) =
                         Simple;
                  end if;

                  if Matched then
                     Increase_Indent
                       (Main_Trace,
                        String (Key (C))
                        & " is candidate: filename=" & Simple);

                     if not Config.Executable_Re.Is_Empty
                       and then Config.Prefix_Index >= 0
                       and then Matches (Config.Prefix_Index) /= No_Match
                     then
                        Prefix := To_Unbounded_String
                          (Simple (Matches
                           (Config.Prefix_Index).First ..
                             Matches (Config.Prefix_Index).Last));
                     end if;

                     Continue := True;
                     Foreach_Language_Runtime
                       (Iterator       => Iterator,
                        Base           => Base,
                        Name           =>
                          To_Unbounded_String (String (Key (C))),
                        Executable     => To_Unbounded_String (Simple),
                        Directory      => Directory,
                        On_Target      => On_Target,
                        Prefix         => Prefix,
                        From_Extra_Dir => From_Extra_Dir,
                        Descr          => Config,
                        Path_Order     => Path_Order,
                        Continue       => Continue);

                     Decrease_Indent (Main_Trace);
                     exit For_All_Files_In_Dir when not Continue;
                  end if;
               end;
            end loop;

         end loop For_All_Files_In_Dir;
         Exec_Set.Clear;
      else
         --  Do not search all entries in the directory, but check explicitly
         --  for the compilers. This results in a lot less system calls, and
         --  thus is faster.

         for C in Base.Compilers.Iterate loop
            declare
               Config : constant CDM.Constant_Reference_Type :=
                          CDM.Constant_Reference (Base.Compilers, C);
               F      : constant String :=
                          GNAT.OS_Lib.Normalize_Pathname
                            (Name           => To_String (Config.Executable),
                             Directory      => Directory,
                             Resolve_Links  => False,
                             Case_Sensitive => On_Windows)
                          & Exec_Suffix.all;
            begin
               if Ada.Directories.Exists (F) then
                  Trace (Main_Trace, "--------------------------------------");
                  Trace (Main_Trace,
                         "Processing "
                         & To_String (Config.Name) & " in " & Directory);

                  Foreach_Language_Runtime
                    (Iterator       => Iterator,
                     Base           => Base,
                     Name           => To_Unbounded_String (String (Key (C))),
                     Executable     => Config.Executable,
                     Prefix         => Null_Unbounded_String,
                     From_Extra_Dir => From_Extra_Dir,
                     On_Target      => On_Target,
                     Directory      => Directory,
                     Descr          => Config,
                     Path_Order     => Path_Order,
                     Continue       => Continue);
                  exit when not Continue;
               end if;
            exception
               when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
                  null;
            end;
         end loop;
      end if;

      GNAT.OS_Lib.Free (Exec_Suffix);
      Decrease_Indent (Main_Trace);
   end Foreach_Compiler_In_Dir;

   ------------------------------
   -- Foreach_Compiler_In_Path --
   ------------------------------

   procedure Foreach_In_Path
     (Self       : in out Object'Class;
      Base       : in out KB.Object;
      On_Target  : Name_Type;
      Extra_Dirs : String := "")
   is
      use GNATCOLL.Traces;

      Selected_Targets_Set : Targets_Set_Id;
      Dirs                 : GPR2.Containers.Value_List;
      Map                  : GPR2.Containers.Value_Set;

      use GPR2.Containers.Value_Type_List;

      procedure Process_Path
        (Path            : String;
         Prefix          : Character;
         Prepend_To_List : Boolean);
      --  Add a directory to the list of directories to examine

      ------------------
      -- Process_Path --
      ------------------

      procedure Process_Path
        (Path            : String;
         Prefix          : Character;
         Prepend_To_List : Boolean)
      is
         use GPR2.Containers.Value_Type_Set;

         First, Last : Natural;
      begin
         First := Path'First;

         while First <= Path'Last loop
            --  Skip null entries on PATH
            if Path (First) = GNAT.OS_Lib.Path_Separator then
               First := First + 1;

            else
               Last := First + 1;
               while Last <= Path'Last
                 and then Path (Last) /= GNAT.OS_Lib.Path_Separator
               loop
                  Last := Last + 1;
               end loop;

               declare
                  --  Use a hash to make sure we do not parse the same
                  --  directory twice. This is both more efficient and avoids
                  --  duplicates in the final result list. To handle the case
                  --  of links (on linux for instance /usr/bin/X11 points to
                  --  ".", ie /usr/bin, and compilers would appear duplicated),
                  --  we resolve symbolic links. This call is also set to fold
                  --  to lower-case when appropriate

                  Normalized : constant Value_Type :=
                                 Name_As_Directory
                                   (GNAT.OS_Lib.Normalize_Pathname
                                      (Path (First .. Last - 1),
                                       Resolve_Links  => True,
                                       Case_Sensitive => False));
               begin
                  if not Contains (Map, Normalized) then
                     Include (Map, Normalized);

                     --  Rerun normalize_pathname without resolve_links so that
                     --  the displayed path looks familiar to the user (no ..,
                     --  ./ or quotes, but still using the path as shown in
                     --  $PATH)
                     declare
                        Final_Path : constant String :=
                                       GNAT.OS_Lib.Normalize_Pathname
                                         (Path (First .. Last - 1),
                                          Resolve_Links  => False,
                                          Case_Sensitive => False);
                     begin
                        --  Windows is somewhat slow at parsing directories, do
                        --  not look into any directory under C:\windows as
                        --  there is no compiler to be found there anyway.

                        if not On_Windows
                          or else
                            (Final_Path'Length > 10
                               and then
                             Characters.Handling.To_Lower (Final_Path
                               (Final_Path'First .. Final_Path'First + 9)) /=
                                "c:\windows")
                        then
                           Trace
                             (Main_Trace,
                              "Will examine " & Prefix & " " & Final_Path);

                           if Prepend_To_List then
                              Prepend (Dirs, Prefix & Final_Path);
                           else
                              Append (Dirs, Prefix & Final_Path);
                           end if;
                        end if;
                     end;
                  end if;
               end;

               First := Last + 1;
            end if;
         end loop;
      end Process_Path;

      Dir        : GPR2.Containers.Value_Type_List.Cursor;
      Path_Order : Positive := 1;
      Continue   : Boolean;

   begin
      --  Preprocess the list of directories that will be searched. When a
      --  directory appears both in Extra_Dirs and in Path, we prepend it to
      --  the PATH for optimization purposes: no need to look in all the PATH
      --  if the compiler(s) will match in that directory. However, this has
      --  the result that a command line with --config that specifies a path
      --  and one that doesn't might find the second compiler in the same
      --  path even if it is not the first one on the PATH. That's minor, and
      --  a workaround is for the user to specify path for all --config args.
      --
      --  We will also need to know later whether the directory comes from
      --  PATH or extra_dirs. If a directory appears in both, it is said to
      --  come from PATH, so that all its compilers are taken into account.
      --  As a special convention, the first character of the directory name is
      --  set to 'E' if the dir comes from extra_dirs, or 'P' if it comes from
      --  PATH.

      if Environment_Variables.Exists ("PATH") then
         Process_Path (Ada.Environment_Variables.Value ("PATH"), 'P', False);
      end if;

      if Extra_Dirs /= "" then
         Process_Path (Extra_Dirs, 'E', Prepend_To_List => True);
      end if;

      Get_Targets_Set (Base, String (On_Target), Selected_Targets_Set);

      Dir := First (Dirs);

      while Has_Element (Dir) loop
         declare
            P : constant String := Element (Dir);
         begin
            Foreach_Compiler_In_Dir
              (Iterator       => Self,
               Base           => Base,
               Directory      => P (P'First + 1 .. P'Last),
               From_Extra_Dir => P (P'First) = 'E',
               Path_Order     => Path_Order,
               On_Target      => Selected_Targets_Set,
               Continue       => Continue);
            exit when not Continue;
         end;

         Path_Order := Path_Order + 1;
         Next (Dir);
      end loop;
   end Foreach_In_Path;

   ------------------------------
   -- Foreach_Language_Runtime --
   ------------------------------

   procedure Foreach_Language_Runtime
     (Iterator       : in out Object'Class;
      Base           : in out KB.Object;
      Name           : Unbounded_String;
      Executable     : Unbounded_String;
      Directory      : String;
      Prefix         : Unbounded_String;
      From_Extra_Dir : Boolean;
      On_Target      : Targets_Set_Id;
      Descr          : Compiler_Description;
      Path_Order     : Integer;
      Continue       : out Boolean)
   is
      use External_Value_Nodes;
      use External_Value_Lists;

      use GNATCOLL.Traces;

      use GPR2.Containers.Value_Type_List;

      function Is_Windows_Executable (Filename : String) return Boolean;
      --  Verify that a given filename is indeed an executable

      ---------------------------
      -- Is_Windows_Executable --
      ---------------------------

      function Is_Windows_Executable (Filename : String) return Boolean is
         use GNAT.OS_Lib;

         type Byte is mod 256;
         for Byte'Size use 8;
         for Byte'Alignment use 1;
         type Bytes is array (Positive range <>) of Byte;

         Windows_Pattern : constant Bytes := (77, 90, 144, 0);

         Fd     : constant File_Descriptor := Open_Read (Filename, Binary);
         B      : Bytes (1 .. 4);
         N_Read : Integer;
      begin
         N_Read := Read (Fd, B'Address, 4);
         Close (Fd);

         if N_Read < 4 then
            return False;

         else
            if B = Windows_Pattern then
               return True;
            else
               return False;
            end if;
         end if;
      end Is_Windows_Executable;

      Exec_Suffix : GNAT.OS_Lib.String_Access;

      Target    : External_Value_Lists.List;
      Version   : External_Value_Lists.List;
      Languages : External_Value_Lists.List;
      Runtimes  : External_Value_Lists.List;
      Variables : External_Value_Lists.List;
      Comp      : Compiler;
      C, C2     : External_Value_Lists.Cursor;
      CS        : GPR2.Containers.Value_Type_List.Cursor;

      Drop_Compiler : Boolean := False;

   begin
      Continue := True;

      --  Verify that the compiler is indeed a real executable
      --  on Windows and not a cygwin symbolic link.
      --  This whole part is optimized out on non-windows hosts, so in order
      --  to keep the builds in debug mode a warning suppresion is needed.

      pragma Warnings (Off, "this code can never be executed");
      if On_Windows
           and then
         not Is_Windows_Executable
           (Directory & OS_Lib.Directory_Separator & To_String (Executable))
      then
         Continue := True;
         return;
      end if;
      pragma Warnings (On, "this code can never be executed");

      Comp.Name       := Name;
      Comp.Path       :=
        GPR2.Path_Name.Create_Directory
          (Filename_Type
             (GNAT.OS_Lib.Normalize_Pathname
                (Directory, Case_Sensitive => False)));
      Exec_Suffix := GNAT.OS_Lib.Get_Executable_Suffix;
      Comp.Base_Name :=
        To_Unbounded_String
          (GNAT.Directory_Operations.Base_Name
             (To_String (Executable), Suffix => Exec_Suffix.all));
      GNAT.OS_Lib.Free (Exec_Suffix);
      Comp.Path_Order := Path_Order;
      Comp.Prefix     := Prefix;
      Comp.Executable := Executable;

      --  Check the target first, for efficiency. If it doesn't match, no need
      --  to compute other attributes.

      if Executable /= Null_Unbounded_String then
         if not Is_Empty (Descr.Target.EV) then
            Get_External_Value
              ("target",
               Value            => Descr.Target,
               Comp             => Comp,
               Split_Into_Words => False,
               Calls_Cache      => Base.External_Calls_Cache,
               Messages         => Base.Messages,
               Processed_Value  => Target,
               Ignore_Compiler  => Drop_Compiler);

            if Drop_Compiler then
               return;
            end if;

            if not Is_Empty (Target) then
               Comp.Target :=
                 External_Value_Lists.Element (First (Target)).Value;
               Get_Targets_Set
                 (Base, To_String (Comp.Target), Comp.Targets_Set);
            else
               Trace (Main_Trace, "Target unknown for this compiler");
               Comp.Targets_Set := Unknown_Targets_Set;
               Continue := True;
               return;
            end if;

            if On_Target /= All_Target_Sets
              and then Comp.Targets_Set /= On_Target
            then
               Trace (Main_Trace,
                      "Target for this compiler does not match --target");
               Continue := True;
               return;
            end if;

         else
            Trace (Main_Trace, "Target unspecified, always match");
            Comp.Targets_Set := All_Target_Sets;
         end if;

         --  Then get the value of the remaining attributes. For most of them,
         --  we must be able to find a valid value, or the compiler is simply
         --  ignored

         Get_External_Value
           ("version",
            Value            => Descr.Version,
            Comp             => Comp,
            Split_Into_Words => False,
            Calls_Cache      => Base.External_Calls_Cache,
            Messages         => Base.Messages,
            Processed_Value  => Version,
            Ignore_Compiler  => Drop_Compiler);

            if Drop_Compiler then
               return;
            end if;

         if Is_Empty (Version) then
            Trace
              (Main_Trace,
               "Ignore compiler, since couldn't guess its version");
            Continue := True;
            return;
         end if;

         Comp.Version := External_Value_Lists.Element (First (Version)).Value;

         Get_External_Value
           ("variables",
            Value            => Descr.Variables,
            Comp             => Comp,
            Split_Into_Words => False,
            Calls_Cache      => Base.External_Calls_Cache,
            Messages         => Base.Messages,
            Processed_Value  => Variables,
            Ignore_Compiler  => Drop_Compiler);

            if Drop_Compiler then
               return;
            end if;

         C := First (Variables);

         while Has_Element (C) loop
            declare
               Ext : constant External_Value_Item :=
                       External_Value_Lists.Element (C);
            begin
               if Ext.Value = Null_Unbounded_String then
                  Trace
                    (Main_Trace,
                     "Ignore compiler since variable '"
                     & To_String (Ext.Extracted_From) & "' is empty");
                  Continue := True;
                  return;
               end if;

               if Variables_Maps.Contains
                 (Comp.Variables, Ext.Extracted_From)
               then
                  Base.Messages.Append
                    (GPR2.Message.Create
                       (GPR2.Message.Information,
                        "Variable '"
                        & To_String (Ext.Extracted_From)
                        & "' is already defined",
                        Sloc => Descr.Variables.Sloc));
               else
                  Variables_Maps.Insert
                    (Comp.Variables, Ext.Extracted_From, Ext.Value);
               end if;
            end;
            Next (C);
         end loop;
      end if;

      Get_External_Value
        ("languages",
         Value            => Descr.Languages,
         Comp             => Comp,
         Split_Into_Words => True,
         Calls_Cache      => Base.External_Calls_Cache,
         Messages         => Base.Messages,
         Processed_Value  => Languages,
         Ignore_Compiler  => Drop_Compiler);

      if Drop_Compiler then
         return;
      end if;

      if Is_Empty (Languages) then
         Trace
           (Main_Trace,
            "Ignore compiler, since no language could be computed");
         Continue := True;
         return;
      end if;

      if Executable /= Null_Unbounded_String then
         Get_External_Value
           ("runtimes",
            Value            => Descr.Runtimes,
            Comp             => Comp,
            Split_Into_Words => True,
            Merge_Same_Dirs  => True,
            Calls_Cache      => Base.External_Calls_Cache,
            Messages         => Base.Messages,
            Processed_Value  => Runtimes,
            Ignore_Compiler  => Drop_Compiler);

         if Drop_Compiler then
            return;
         end if;

         Comp.Default_Runtime := True;
         Comp.Any_Runtime := False;

         if not Is_Empty (Runtimes) then
            --  This loop makes sure that the default runtime appears first in
            --  the list (and thus is selected automatically when creating
            --  configuration non-interactively).

            Comp.Default_Runtime := False;
            Comp.Any_Runtime     := True;

            CS := First (Descr.Default_Runtimes);

            Defaults_Loop : while Has_Element (CS) loop
               C2 := First (Runtimes);
               while Has_Element (C2) loop
                  if To_String (External_Value_Lists.Element (C2).Value)
                    = GPR2.Containers.Value_Type_List.Element (CS)
                  then
                     Prepend (Runtimes, External_Value_Lists.Element (C2));
                     Delete (Runtimes, C2);
                     Comp.Default_Runtime := True;
                     exit Defaults_Loop;
                  end if;

                  Next (C2);
               end loop;

               Next (CS);
            end loop Defaults_Loop;
         end if;
      end if;

      C := First (Languages);

      while Has_Element (C) loop
         declare
            L : constant String :=
                  To_String (External_Value_Lists.Element (C).Value);
         begin
            Comp.Language := +Name_Type (L);
         end;

         --  First check if a runtime specified with option --config= will
         --  match.

         Callback
           (Self              => Iterator,
            Base              => Base,
            Comp              => Comp,
            Runtime_Specified => True,
            From_Extra_Dir    => From_Extra_Dir,
            Continue          => Continue);

         if not Continue then
            return;
         end if;

         if Is_Empty (Runtimes) then
            if Descr.Runtimes /= Null_External_Value then
               Trace
                 (Main_Trace,
                  "No runtime found where one is required for: "
                  & Comp.Path.Value);
            else
               Callback
                 (Self              => Iterator,
                  Base              => Base,
                  Comp              => Comp,
                  Runtime_Specified => False,
                  From_Extra_Dir    => From_Extra_Dir,
                  Continue          => Continue);

               if not Continue then
                  return;
               end if;
            end if;

         else
            C2 := First (Runtimes);

            while Has_Element (C2) loop
               Comp.Runtime     := External_Value_Lists.Element (C2).Value;
               Comp.Alt_Runtime :=
                 External_Value_Lists.Element (C2).Alternate;
               Comp.Runtime_Dir :=
                 External_Value_Lists.Element (C2).Extracted_From;
               Callback
                 (Self              => Iterator,
                  Base              => Base,
                  Comp              => Comp,
                  Runtime_Specified => False,
                  From_Extra_Dir    => From_Extra_Dir,
                  Continue          => Continue);

               if not Continue then
                  return;
               end if;

               Next (C2);
            end loop;
         end if;

         Next (C);
      end loop;
   end Foreach_Language_Runtime;

   ----------------------
   --  Get_Targets_Set --
   ----------------------

   procedure Get_Targets_Set
     (Base   : in out KB.Object;
      Target : String;
      Id     : out Targets_Set_Id) is
   begin
      Id := Query_Targets_Set (Base, Name_Type (Target));

      if Id /= Unknown_Targets_Set then
         return;
      end if;

      --  Create a new set
      declare
         Set : Target_Lists.List;
      begin
         GNATCOLL.Traces.Trace
           (Main_Trace, "create a new target set for " & Target);
         Set.Append
           (GNAT.Regpat.Compile ("^" & GNAT.Regpat.Quote (Target) & "$"));
         Base.Targets_Sets.Append ((To_Unbounded_String (Target), Set), 1);
         Id := Base.Targets_Sets.Last_Index;
      end;
   end Get_Targets_Set;

end GPR2.KB.Compiler_Iterator;
