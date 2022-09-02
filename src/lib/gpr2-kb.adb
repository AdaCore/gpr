--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Fixed;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;

with GNATCOLL.OS.Process;
with GNATCOLL.Traces;
with GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;

with GPR2.KB.Compiler_Iterator;
with GPR2.KB.Parsing;
with GPR2.Message;

package body GPR2.KB is

   Main_Trace : constant GNATCOLL.Traces.Trace_Handle :=
                  GNATCOLL.Traces.Create
                    ("KNOWLEDGE_BASE",
                     GNATCOLL.Traces.Off);

   Match_Trace : constant GNATCOLL.Traces.Trace_Handle :=
                   GNATCOLL.Traces.Create
                     ("KNOWLEDGE_BASE.MATHCING",
                      GNATCOLL.Traces.Off);

   No_Compatible_Compilers : exception;
   --  Raised when any combination of compilers found can form a supported
   --  configuration.

   procedure Complete_Command_Line_Compilers
     (Self             : in out Object;
      On_Target        : Name_Type;
      Filters          : Compiler_Lists.List;
      Compilers        : in out Compiler_Lists.List;
      Selected_Target  : in out Unbounded_String;
      Fallback         : Boolean;
      Errors           : out Log.Object);
   --  In batch mode, the configuration descriptions indicate what compilers
   --  should be selected. Each of these descriptions selects the first
   --  matching compiler available, and all descriptions must match a compiler.
   --  The information provided by the user does not have to be complete, and
   --  this procedure completes all missing information like version, runtime,
   --  and so on.
   --  Filters is the list specified by the user, and contains potentially
   --  partial information for each compiler. On output, Compilers is completed
   --  with the full information for all compilers in Filters.
   --  If any of the required compilers cannot be found corresponding
   --  diagnostic is passed through Errors.
   --  If no set of compatible compilers was found raises
   --  No_Compatible_Compilers.

   function Extra_Dirs_From_Filters
     (Filters : Compiler_Lists.List) return String;
   --  Compute the list of directories that should be prepended to the PATH
   --  when searching for compilers. These are all the directories that the
   --  user has explicitly specified in his filters.

   function Is_Language_With_No_Compiler
     (Self     : Object;
      Language : Language_Id) return Boolean;
   --  Given a language name (lower case), returns True if that language is
   --  known to require no compiler.

   function Is_Supported_Config
     (Self      : Object;
      Compilers : Compiler_Lists.List) return Boolean;
   --  Whether we know how to link code compiled with all the selected
   --  compilers.

   procedure Parse_All_Dirs
     (Processed_Value : out External_Value_Lists.List;
      Visited         : in out String_To_External_Value.Map;
      Current_Dir     : String;
      Path_To_Check   : String;
      Regexp          : Regpat.Pattern_Matcher;
      Regexp_Str      : String;
      Value_If_Match  : String;
      Group           : Integer;
      Group_Match     : String := "";
      Group_Count     : Natural := 0;
      Contents        : Pattern_Matcher_Holder;
      Merge_Same_Dirs : Boolean;
      Error_Sloc      : Source_Reference.Object;
      Messages        : in out Log.Object);
   --  Parses all subdirectories of Current_Dir for those that match
   --  Path_To_Check (see description of <directory>). When a match is found,
   --  the regexp is evaluated against the current directory, and the matching
   --  parenthesis group is appended to Append_To (comma-separated).
   --  If Group is -1, then Value_If_Match is used instead of the parenthesis
   --  group.
   --  Group_Match is the substring that matched Group (if it has been matched
   --  already). Group_Count is the number of parenthesis groups that have been
   --  processed so far. The idea is to compute the matching substring as we
   --  go, since the regexp might no longer match in the end, if for instance
   --  it includes ".." directories.
   --
   --  If Merge_Same_Dirs is True, then the values that come from a
   --  <directory> node will be merged (the last one is kept, other removed) if
   --  they point to the same physical directory (after normalizing names). In
   --  this case, Visited contains the list of normalized directory names.
   --
   --  Contents, if specified, is a regular expression. It indicates that any
   --  file matching the pattern should be parsed, and the first line matching
   --  that regexp should be used as the name of the file instead. This is a
   --  way to simulate symbolic links on platforms that do not use them.

   generic
      with function Callback (Var_Name, Index : String) return String;
   function Substitute_Variables
     (Str        : String;
      Error_Sloc : Source_Reference.Object;
      Messages   : in out Log.Object) return String;
   --  Substitutes variables in Str (their value is computed through Callback).
   --  Possible errors are stored in Messages.

   function Substitute_Variables_In_Compiler_Description
     (Str        : String;
      Comp       : Compiler;
      Error_Sloc : Source_Reference.Object;
      Messages   : in out Log.Object) return String;
   --  Substitutes the special "$..." names in compiler description

   function Substitute_Variables_In_Configuration
     (Self       : Object;
      Str        : String;
      Comps      : Compiler_Lists.List;
      Error_Sloc : Source_Reference.Object;
      Messages   : in out Log.Object) return String;
   --  Substitutes the special "$..." names in configuration

   function Get_Variable_Value
     (Comp : Compiler;
      Name : String) return String;
   --  Returns the value of a predefined or user-defined variable.
   --  If the variable is not defined a warning is emitted and an empty
   --  string is returned.

   function Get_String_No_Adalib (Str : String) return String;
   --  Returns the name without "adalib" at the end

   function To_String (Comp : Compiler) return String;
   --  Return a string representing the compiler. Used for diagnotics.

   function To_String (Compilers  : Compiler_Lists.List) return String;
   --  Return a string representing the list ofcompilers. Only Selected
   --  compilers are taken into account. Used for diagnotics.

   procedure Set_Selection
     (Compilers : in out Compiler_Lists.List;
      Cursor    : Compiler_Lists.Cursor;
      Selected  : Boolean);

   function Match
     (Filter    : Compilers_Filter_Lists.List;
      Compilers : Compiler_Lists.List) return Boolean;
   --  Returns True if all elements of Filter matches against Compilers.
   --  Filter represents the list of nides <configuration><compilers>.

   function Match
     (Filter    : Compilers_Filter;
      Compilers : Compiler_Lists.List) return Boolean;
   --  Returns True if at least one component of Filter matches against
   --  Compilers, taking into account the negate value.
   --  Filter represents a single node <configuration><compilers>.

   function Match
     (Filter    : Compiler_Filter;
      Compilers : Compiler_Lists.List) return Boolean;
   --  Returns True if at least on of the Compilers match the filter.
   --  Filter represents a single node <configuration><compilers><compiler>.

   function Match
     (Target_Filter : Double_String_Lists.List;
      Negate        : Boolean;
      Compilers     : Compiler_Lists.List) return Boolean;
   --  Return True if Filter matches the list of selected configurations

   function Generate_Configuration
     (Self      : Object;
      Compilers : Compiler_Lists.List;
      Target    : String;
      Errors    : in out Log.Object) return Unbounded_String;
   --  Generate the configuration string for the list of selected compilers

   package String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, String);

   procedure Merge_Config
     (Self       : Object;
      Packages   : in out String_Maps.Map;
      Compilers  : Compiler_Lists.List;
      Config     : String;
      Error_Sloc : Source_Reference.Object;
      Errors     : in out Log.Object);
   --  Merge the contents of Config into Packages, so that each attributes ends
   --  up in the right package, and the packages are not duplicated.

   procedure Update_With_Compiler_Runtime
     (Self : in out Object;
      Comp : Compiler);
   --  Update the knowledge base with additional runtime specific chunks
   --  if a given compiler has any.

   function Create_Filter
     (Self  : Object;
      Descr : Project.Configuration.Description) return Compiler;
   --  Transform Description into Compiler object

   function Configuration_Node_Image
     (Config : Configuration_Type) return Unbounded_String;
   --  Returns partial image of <configuration> node that is used in verbose
   --  output to explain unsupported configuration.

   function GPR_Executable_Prefix_Path return String;
   --  Tries to find the installation location of gprtools.
   --  If current executable may be one of gprtools and is spawned with path
   --  prefix, returns corresponding prefix directory.
   --  Returns empty string if all approaches do not work.
   --  When a directory is returned, it is guaranteed to end with a directory
   --  separator.

   Default_Target_Parsed : Boolean := False;
   Default_Target_Val    : Unbounded_String;

   procedure Parse_Default_Target_Val;
   --  Tries to parse <gprtools directory>/share/gprconfig/default_target
   --  and sets Default_Target_Val.

   ---------
   -- Add --
   ---------

   procedure Add
     (Self     : in out Object;
      Flags    : Parsing_Flags;
      Location : GPR2.Path_Name.Object) is
   begin
      if Self.Parsed_Directories.Contains (Location) then
         --  Do not parse several times the same database directory
         return;
      end if;

      Self.Parsed_Directories.Append (Location);
      Parsing.Parse_Knowledge_Base (Self, Location, Flags);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Self    : in out Object;
      Flags   : Parsing_Flags;
      Content : Value_Not_Empty) is
   begin
      Parsing.Add (Self, Flags, Content);
   end Add;

   -------------------
   -- All_Compilers --
   -------------------

   function All_Compilers
     (Self     : in out Object;
      Settings : Project.Configuration.Description_Set;
      Target   : Name_Type;
      Messages : in out GPR2.Log.Object) return Compiler_Array
   is
      use Compiler_Lists;
      use Ada.Containers;
      use KB.Compiler_Iterator;
      use Project.Configuration;

      Compilers : Compiler_Lists.List;
      Filters   : Compiler_Lists.List;

      type Boolean_Array  is array (Count_Type range <>) of Boolean;

      type All_Iterator (Count : Count_Type) is new
        GPR2.KB.Compiler_Iterator.Object with record
         Filter_Matched : Boolean_Array (1 .. Count) := (others => False);
         Filters        : Compiler_Lists.List;
         Compilers      : Compiler_Lists.List;
      end record;

      overriding
      procedure Callback
        (Iterator          : in out All_Iterator;
         Base              : in out Object;
         Comp              : Compiler;
         Runtime_Specified : Boolean;
         From_Extra_Dir    : Boolean;
         Continue          : out Boolean);
      --  Search all compilers on path, preselecting the first one matching
      --  each of the filters.

      function Display_Before (Comp1, Comp2 : Compiler) return Boolean;
      --  Whether Comp1 should be displayed before Comp2 when displaying lists
      --  of compilers. This ensures that similar languages are grouped,
      --  among othe things.

      package Compiler_Sort is
        new Compiler_Lists.Generic_Sorting (Display_Before);

      --------------
      -- Callback --
      --------------

      overriding
      procedure Callback
        (Iterator          : in out All_Iterator;
         Base              : in out Object;
         Comp              : Compiler;
         Runtime_Specified : Boolean;
         From_Extra_Dir    : Boolean;
         Continue          : out Boolean)
      is
         New_Comp : Compiler := Comp;
         C        : Compiler_Lists.Cursor;
         Index    : Count_Type := 1;
      begin
         --  Do nothing if a runtime needs to be specified, as this is only for
         --  interactive use.

         if not Runtime_Specified then
            if Iterator.Filter_Matched /=
              (Iterator.Filter_Matched'Range => True)
            then
               C := First (Iterator.Filters);
               while Has_Element (C) loop
                  if not Iterator.Filter_Matched (Index)
                    and then Filter_Match
                      (Base, Comp => Comp, Filter => Element (C))
                  then
                     Set_Selection (New_Comp, True);
                     Iterator.Filter_Matched (Index) := True;
                     exit;
                  end if;

                  Index := Index + 1;
                  Next (C);
               end loop;
            end if;

            --  Ignore compilers from extra directories, unless they have been
            --  selected because of a --config argument

            if Is_Selected (New_Comp) or else not From_Extra_Dir then
               GNATCOLL.Traces.Trace
                 (Main_Trace,
                  "Adding compiler to interactive menu "
                  & To_String (Comp)
                  & " selected=" & Is_Selected (New_Comp)'Img);
               Append (Iterator.Compilers, New_Comp);
            end if;
         end if;

         Continue := True;
      end Callback;

      --------------------
      -- Display_Before --
      --------------------

      function Display_Before (Comp1, Comp2 : Compiler) return Boolean is

         type Compare_Type is (Before, Equal, After);
         function Compare
           (Name1, Name2 : Unbounded_String) return Compare_Type;
         --  Compare alphabetically two strings

         -------------
         -- Compare --
         -------------

         function Compare
           (Name1, Name2 : Unbounded_String) return Compare_Type is
         begin
            if Name1 = Null_Unbounded_String then
               if Name2 = Null_Unbounded_String then
                  return Equal;
               else
                  return Before;
               end if;

            elsif Name2 = Null_Unbounded_String then
               return After;
            end if;

            if Name1 < Name2 then
               return Before;
            elsif Name1 > Name2 then
               return After;
            else
               return Equal;
            end if;

         end Compare;
      begin
         if Comp1.Language /= Comp2.Language then
            return Name (Comp1.Language) < Name (Comp2.Language);

         else
            if Comp1.Path_Order < Comp2.Path_Order then
               return True;

            elsif Comp2.Path_Order < Comp1.Path_Order then
               return False;

            else
               --  If the "default" attribute was specified for <runtime>,
               --  this only impacts the batch mode. We still want to sort
               --  the runtimes alphabetically in the interactive display.

               case Compare (Comp1.Runtime, Comp2.Runtime) is
                  when Before =>
                     return True;
                  when After =>
                     return False;
                  when Equal =>
                     return Compare (Comp1.Version, Comp2.Version) = Before;
               end case;
            end if;
         end if;
      end Display_Before;

      Iter : All_Iterator (Settings'Length);
   begin
      for Setting of Settings loop
         if Is_Language_With_No_Compiler
           (Self, Language (Setting))
         then
            Compilers.Append (Create_Filter (Self, Setting));
         else
            Filters.Append (Create_Filter (Self, Setting));
         end if;
      end loop;

      Iter.Filters := Filters;
      Foreach_In_Path
        (Self       => Iter,
         Base       => Self,
         On_Target  => Target,
         Extra_Dirs => Extra_Dirs_From_Filters (Filters));

      Splice (Target => Compilers,
              Before => Compiler_Lists.No_Element,
              Source => Iter.Compilers);

      if Compilers.Is_Empty then
         return No_Compilers;
      end if;

      Compiler_Sort.Sort (Compilers);

      return Res : Compiler_Array (1 .. Integer (Compilers.Length)) do
         for Idx in Res'Range loop
            Res (Idx) := Compilers.First_Element;
            Compilers.Delete_First;
         end loop;
      end return;
   exception
      when Invalid_KB =>
         return No_Compilers;
   end All_Compilers;

   -------------------------------------
   -- Complete_Command_Line_Compilers --
   -------------------------------------

   procedure Complete_Command_Line_Compilers
     (Self             : in out Object;
      On_Target        : Name_Type;
      Filters          : Compiler_Lists.List;
      Compilers        : in out Compiler_Lists.List;
      Selected_Target  : in out Unbounded_String;
      Fallback         : Boolean;
      Errors           : out Log.Object)
   is
      use Compiler_Lists;
      use Ada.Containers;
      use GNATCOLL.Traces;

      type Cursor_Array
        is array (Count_Type range <>) of Compiler_Lists.Cursor;
      type Boolean_Array  is array (Count_Type range <>) of Boolean;

      type Batch_Iterator (Count : Count_Type) is new
        GPR2.KB.Compiler_Iterator.Object with record
         Found      : Count_Type := 0;
         Compilers  : Compiler_Lists.List;
         Matched    : Cursor_Array (1 .. Count) :=
                        (others => Compiler_Lists.No_Element);
         Filters    : Compiler_Lists.List;

         Found_One  : Boolean_Array (1 .. Count) := (others => False);
         --  Whether we found at least one matching compiler for each filter
      end record;

      overriding
      procedure Callback
        (Iterator          : in out Batch_Iterator;
         Base              : in out Object;
         Comp              : Compiler;
         Runtime_Specified : Boolean;
         From_Extra_Dir    : Boolean;
         Continue          : out Boolean);
      --  Search the first compiler matching each --config command line
      --  argument.
      --  It might be discovered either in a path added through a --config
      --  parameter (in which case From_Extra_Dir is True), or in a path
      --  specified in the environment variable $PATH (in which case it is
      --  False). If the directory is both in Extra_Dirs and in $PATH,
      --  From_Extra_Dir is set to False.
      --  If Runtime_Specified is True, only filters with a specified runtime
      --  are taken into account.
      --
      --  On exit, Continue should be set to False if there is no need
      --  to discover further compilers.

      Iterator : Batch_Iterator (Length (Filters));

      function Foreach_Nth_Compiler
        (Filter : Compiler_Lists.Cursor) return Boolean;
      --  For all possible compiler matching the filter, check whether we
      --  find a compatible set of compilers matching the next filters.
      --  Return True if one was found (in which case it is the current
      --  selection on exit).

      --------------
      -- Callback --
      --------------

      overriding
      procedure Callback
        (Iterator          : in out Batch_Iterator;
         Base              : in out Object;
         Comp              : Compiler;
         Runtime_Specified : Boolean;
         From_Extra_Dir    : Boolean;
         Continue          : out Boolean)
      is

         C     : Compiler_Lists.Cursor := First (Iterator.Filters);
         Index : Count_Type := 1;
         Ncomp : Compiler;
         El    : Compiler;

         use GPR2.Path_Name;
      begin
         while Has_Element (C) loop
            Ncomp := No_Compiler;
            El := Compiler_Lists.Element (C);

            --  A compiler in an "extra_dir" (ie specified on the command line)
            --  can only match if that directory was explicitly specified in
            --  --config. We do not want to find all compilers in /dir if that
            --  directory is not in $PATH

            if (not From_Extra_Dir or else El.Path = Comp.Path)
              and then Filter_Match (Base, Comp => Comp, Filter => El)
              and then (not Runtime_Specified
                         or else El.Runtime_Dir /= Null_Unbounded_String)
            then
               Ncomp := Comp;
               if El.Runtime_Dir /= Null_Unbounded_String then
                  Ncomp.Runtime_Dir := El.Runtime_Dir;
                  Ncomp.Runtime := El.Runtime;
               end if;

               if not Ncomp.Any_Runtime
                 and then Ncomp.Runtime = Null_Unbounded_String
                 and then El.Runtime /= Null_Unbounded_String
               then
                  Ncomp.Runtime := El.Runtime;
               end if;

               Append (Iterator.Compilers, Ncomp);

               Trace
                 (Match_Trace,
                  "Saving compiler for possible backtracking: "
                  & To_String (Ncomp)
                  & " (matches --config "
                  & To_String (El)
                  & ")");

               if Iterator.Matched (Index) = Compiler_Lists.No_Element then
                  Iterator.Found := Iterator.Found + 1;

                  Trace
                    (Match_Trace,
                     "Selecting it since this filter was not matched yet "
                     & Iterator.Found'Img & "/" & Iterator.Count'Img);

                  Iterator.Matched (Index) := Last (Iterator.Compilers);
                  Iterator.Found_One (Index) := True;
                  Set_Selection
                    (Iterator.Compilers, Iterator.Matched (Index),
                     True);

                  --  Only keep those compilers that are not incompatible
                  --  (according to the knowledge base). It might happen that
                  --  none is selected as a result, but appropriate action is
                  --  taken in Complete_Command_Line_Compilers. We ignore
                  --  incompatible sets as early as possible, in the hope to
                  --  limit the number of system calls if another set is found
                  --  before all directories are traversed.

                  if not Is_Supported_Config (Base, Iterator.Compilers) then
                     Set_Selection
                       (Iterator.Compilers, Iterator.Matched (Index), False);
                     Trace
                       (Match_Trace,
                        "Compilers are not compatible, cancelling last"
                        & " compiler found");
                     Iterator.Matched (Index) := Compiler_Lists.No_Element;
                     Iterator.Found := Iterator.Found - 1;
                  end if;

               end if;
            end if;
            Index := Index + 1;
            Next (C);
         end loop;

         --  Stop at first compiler
         Continue := Iterator.Found /= Iterator.Count;
      end Callback;

      --------------------------
      -- Foreach_Nth_Compiler --
      --------------------------

      function Foreach_Nth_Compiler
        (Filter : Compiler_Lists.Cursor) return Boolean
      is
         C           : Compiler_Lists.Cursor := First (Iterator.Compilers);
         Comp_Filter : constant Compiler := Compiler_Lists.Element (Filter);
      begin
         while Has_Element (C) loop
            if Filter_Match
              (Self, Compiler_Lists.Element (C), Filter => Comp_Filter)
            then
               Set_Selection (Iterator.Compilers, C, True);

               if Next (Filter) = Compiler_Lists.No_Element then
                  Increase_Indent
                    (Match_Trace, "Testing the following compiler set:");
                  Trace (Match_Trace, To_String (Iterator.Compilers));

                  if Is_Supported_Config (Self, Iterator.Compilers) then
                     Decrease_Indent (Match_Trace, "They are compatible");
                     return True;
                  else
                     Decrease_Indent (Match_Trace);
                  end if;

               else
                  if Foreach_Nth_Compiler (Next (Filter)) then
                     return True;
                  end if;
               end if;

               Set_Selection (Iterator.Compilers, C, False);
            end if;

            Next (C);
         end loop;

         return False;
      end Foreach_Nth_Compiler;

      C                  : Compiler_Lists.Cursor;
      Extra_Dirs         : constant String :=
        Extra_Dirs_From_Filters (Filters);
      Found_All          : Boolean := True;
      Found_All_Fallback : Boolean := True;

   begin
      Iterator.Filters := Filters;
      Selected_Target  := To_Unbounded_String (String (On_Target));

      Increase_Indent
        (Main_Trace,
         "Completing info for --config parameters, extra_dirs=" & Extra_Dirs);

      --  Find all the compilers in PATH and Extra_Dirs

      Compiler_Iterator.Foreach_In_Path
        (Self       => Iterator,
         Base       => Self,
         On_Target  => On_Target,
         Extra_Dirs => Extra_Dirs);

      Decrease_Indent (Main_Trace);

      --  Check that we could find at least one of each compiler

      if Fallback then
         --  Check to see if fallback targets are of interest
         C := First (Filters);
         for F in Iterator.Found_One'Range loop
            if not Iterator.Found_One (F) then
               if Self.Languages_Known.Contains
                 (Compiler_Lists.Element (C).Language)
               then
                  --  Fallback should not be triggered for unknown languages
                  Found_All_Fallback := False;
               end if;
               Found_All := False;
            end if;
            Next (C);
         end loop;

         if not Found_All_Fallback then
            --  Looking for corresponding fallback set
            declare
               FB_List  : GPR2.Containers.Name_List :=
                            Fallback_List (Self, On_Target);
               Cur      : GPR2.Containers.Name_Type_List.Cursor :=
                            FB_List.First;
               N_Target : constant Name_Type :=
                            Self.Normalized_Target (On_Target);
               use GPR2.Containers.Name_Type_List;
            begin
               while Cur /= GPR2.Containers.Name_Type_List.No_Element loop
                  if Element (Cur) = N_Target then
                     --  No point processing the same target again
                     FB_List.Delete (Cur);
                     exit;
                  end if;
                  Next (Cur);
               end loop;

               Cur := FB_List.First;
               while Cur /= GPR2.Containers.Name_Type_List.No_Element loop
                  Trace
                    (Match_Trace,
                     "Attempting to fall back to target "
                     & String (Element (Cur)));

                  declare
                     Local_Iter  : Batch_Iterator (Length (Filters));
                  begin
                     Local_Iter := Iterator;

                     Compiler_Iterator.Foreach_In_Path
                       (Self       => Local_Iter,
                        Base       => Self,
                        On_Target  => Element (Cur),
                        Extra_Dirs => Extra_Dirs);

                     Found_All := True;
                     C := First (Filters);
                     for F in Local_Iter.Found_One'Range loop
                        if not Local_Iter.Found_One (F)
                          and then Self.Languages_Known.Contains
                            (Compiler_Lists.Element (C).Language)
                        then
                           --  Not finding a compiler for an unknown language
                           --  should not invalidate fallback search.
                           Found_All := False;
                        end if;
                        Next (C);
                     end loop;

                     if Found_All then
                        Iterator := Local_Iter;
                        Selected_Target :=
                          To_Unbounded_String (String (Element (Cur)));
                        Trace
                          (Match_Trace,
                           String (Element (Cur))
                           & " fallback target selected");
                        exit;
                     end if;
                  end;

                  Next (Cur);
               end loop;
            end;
         end if;
      end if;

      C := First (Filters);
      for F in Iterator.Found_One'Range loop
         if not Iterator.Found_One (F) then
            declare
               Comp           : constant Compiler :=
                                  Compiler_Lists.Element (C);
               Language_Known : constant Boolean :=
                                  Self.Languages_Known.Contains
                                    (Comp.Language);
            begin

               if not Language_Known then
                  Errors.Append
                    (Message.Create
                       (Message.Information,
                        "unknown language '"
                        & Image (Comp.Language) & "'",
                        Source_Reference.Create ("embedded_kb/kb", 0, 0)));

               else

                  Errors.Append
                    (Message.Create
                       (Message.Warning,
                        "can't find a toolchain "
                        & "for the following configuration: language '"
                        & Image (Comp.Language) & "', target '"
                        & String (On_Target) & "'"
                        & (if Comp.Runtime = Null_Unbounded_String then
                              ", default runtime"
                           else ", runtime '" & To_String (Comp.Runtime) & "'")
                        & (if Comp.Version /= Null_Unbounded_String then
                               ", version '" & To_String (Comp.Version) & "'"
                          else "")
                        & (if Comp.Path.Is_Defined then
                               ", path '" & Comp.Path.Value & "'"
                           else "")
                        & (if Comp.Name /= Null_Unbounded_String then
                               ", name '" & To_String (Comp.Name) & "'"
                           else ""),
                        Source_Reference.Create ("embedded_kb/kb", 0, 0)));
               end if;
            end;

            Found_All := False;
         end if;
         Next (C);
      end loop;

      --  If we could find at least one of each compiler, but that our initial
      --  attempt returned incompatible sets of compiler, we do a more thorough
      --  attempt now

      if Found_All
        and then Iterator.Found /= Iterator.Count
      then
         --  If no compatible set was found, try all possible combinations, in
         --  the hope that we can finally find one. In the following algorithm,
         --  we end up checking again some set that were checked in Callback,
         --  but that would be hard to avoid since the compilers can be found
         --  in any order.

         Increase_Indent
           (Match_Trace,
            "Attempting to find a supported compiler set");

         --  Unselect all compilers

         C := First (Iterator.Compilers);
         while Has_Element (C) loop
            Set_Selection (Iterator.Compilers, C, False);
            Next (C);
         end loop;

         if not Foreach_Nth_Compiler (First (Iterator.Filters)) then
            Errors.Append
              (Message.Create
                 (Message.Error,
                  "no set of compatible compilers was found",
                  Source_Reference.Create ("embedded_kb/kb", 0, 0)));

            Decrease_Indent (Match_Trace);
            raise No_Compatible_Compilers;
         end if;
      end if;

      Splice (Target => Compilers,
              Before => Compiler_Lists.No_Element,
              Source => Iterator.Compilers);
   end Complete_Command_Line_Compilers;

   -------------------
   -- Configuration --
   -------------------

   function Configuration
     (Self     : in out Object;
      Settings : Project.Configuration.Description_Set;
      Target   : Name_Type;
      Messages : in out GPR2.Log.Object;
      Fallback : Boolean := False)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      use Project.Configuration;

      Filters              : Compiler_Lists.List;
      Compilers            : Compiler_Lists.List;
      Selected_Target      : Unbounded_String;
      Runtime_Specific_KB  : Object;

      Configuration_String : Unbounded_String;

   begin
      for Setting of Settings loop
         if Is_Language_With_No_Compiler (Self, Language (Setting)) then
            Compilers.Append (Create_Filter (Self, Setting));

         elsif not Self.Languages_Known.Contains (Language (Setting)) then
            Messages.Append
              (Message.Create
                 (Message.Information,
                  "unknown language '"
                  & Image (Language (Setting)) & "'",
                  Source_Reference.Create ("embedded_kb/kb", 0, 0)));

         else
            Filters.Append (Create_Filter (Self, Setting));
         end if;
      end loop;

      begin
         Complete_Command_Line_Compilers
           (Self             => Self,
            On_Target        => Target,
            Filters          => Filters,
            Compilers        => Compilers,
            Selected_Target  => Selected_Target,
            Fallback         => Fallback,
            Errors           => Messages);
      exception
         when No_Compatible_Compilers | Invalid_KB =>
            return Null_Unbounded_String;
      end;

      --  Runtime dir may have additional knowledge base chunks specific to
      --  given runtime. We do not want to include them in Self, otherwise
      --  it becomes unusable for other runtimes/compiletrs.
      Runtime_Specific_KB := Self;

      for Comp of Compilers loop
         Update_With_Compiler_Runtime (Runtime_Specific_KB, Comp);
      end loop;

      Configuration_String := Runtime_Specific_KB.Generate_Configuration
        (Compilers, To_String (Selected_Target), Messages);

      return Configuration_String;
   end Configuration;

   -------------------
   -- Configuration --
   -------------------

   function Configuration
     (Self      : in out Object;
      Selection : Compiler_Array;
      Target    : Name_Type;
      Messages  : in out GPR2.Log.Object)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      Configuration_String : Unbounded_String;
      Runtime_Specific_KB  : Object;
      Compilers            : Compiler_Lists.List;
   begin

      --  Runtime dir may have additional knowledge base chunks specific to
      --  given runtime. We do not want to include them in Self, otherwise
      --  it becomes unusable for other runtimes/compilers.
      Runtime_Specific_KB := Self;

      for Comp of Selection loop
         Update_With_Compiler_Runtime (Runtime_Specific_KB, Comp);
         Compilers.Append (Comp);
      end loop;

      Configuration_String := Runtime_Specific_KB.Generate_Configuration
        (Compilers, String (Target), Messages);

      return Configuration_String;

   end Configuration;

   ------------------------------
   -- Configuration_Node_Image --
   ------------------------------

   function Configuration_Node_Image
     (Config : Configuration_Type) return Unbounded_String
   is
      Result : Unbounded_String;
   begin
      for Comp_Filter of Config.Compilers_Filters loop
         Append
           (Result,
            "<compilers negate='" & Comp_Filter.Negate'Img & "'>" & ASCII.LF);

         for Filter of Comp_Filter.Compiler loop
            Append
              (Result,
               "  <compiler name='"
               & To_String (Filter.Name) & "' version='"
               & To_String (Filter.Version) & "' runtime='"
               & To_String (Filter.Runtime) & "' language='"
               & String (Name (Filter.Language)) & "' />"
               & ASCII.LF);
         end loop;

         Append (Result, "</compilers>" & ASCII.LF);
      end loop;

      Append (Result, "<config supported='" & Config.Supported'Img & "' />");

      return Result;
   end Configuration_Node_Image;

   ------------
   -- Create --
   ------------

   function Create
     (Flags      : Parsing_Flags := Targetset_Only_Flags;
      Default_KB : Boolean := True;
      Custom_KB  : GPR2.Path_Name.Set.Object := GPR2.Path_Name.Set.Empty_Set)
      return Object
   is
      Result : Object;
   begin
      if Default_KB then
         Result := Create_Default (Flags => Flags);
      else
         Result := Create_Empty;
      end if;

      for Location of Custom_KB loop
         Result.Add (Flags, Location);
      end loop;

      return Result;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Content : GPR2.Containers.Value_List;
      Flags   : Parsing_Flags) return Object
   is
      Result : Object := Create_Empty;
   begin
      for Cont of Content loop
         if Cont /= "" then
            Result.Add (Flags, Cont);
         end if;
      end loop;

      return Result;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Location : GPR2.Path_Name.Object;
      Flags   : Parsing_Flags) return Object
   is
      Result    : Object := Create_Empty;
   begin
      Result.Parsed_Directories.Append (Location);
      Parsing.Parse_Knowledge_Base (Result, Location, Flags);

      return Result;
   end Create;

   --------------------
   -- Create_Default --
   --------------------

   function Create_Default
     (Flags : Parsing_Flags) return Object
   is
      Ret : Object;
   begin
      Ret := Parsing.Parse_Default_Knowledge_Base (Flags);

      return Ret;
   end Create_Default;

   ------------------
   -- Create_Empty --
   ------------------

   function Create_Empty return Object
   is
      Result : Object;
   begin
      Result.Initialized := True;
      Result.Is_Default  := False;
      return Result;
   end Create_Empty;

   -------------------
   -- Create_Filter --
   -------------------

   function Create_Filter
     (Self  : Object;
      Descr : Project.Configuration.Description) return Compiler
   is
      use GNATCOLL.Traces;
      use Project.Configuration;

      Result : Compiler;

      Lang_Id       : Language_Id renames Language (Descr);
      Exec_Suffix   : OS_Lib.String_Access :=
                        OS_Lib.Get_Executable_Suffix;

      function Legacy_Name_Support
        (Name : String; Lang : Language_Id) return String;
      --  For Ada, gnatmake was previously used to detect a GNAT compiler.
      --  However, as gnatmake may not be present in all the GNAT
      --  distributions, gnatls is now used. For upward compatibility,
      --  replace gnatmake with gnatls, so that a GNAT compiler may
      --  be detected.

      -------------------------
      -- Legacy_Name_Support --
      -------------------------

      function Legacy_Name_Support
        (Name : String; Lang : Language_Id) return String
      is
         use Ada.Strings.Fixed;

         Idx : constant Natural := Index (Name, "gnatmake");
      begin
         if Lang = Ada_Language and then Idx >= Name'First then
            return Replace_Slice (Name, Idx, Idx + 7, "gnatls");
         else
            return Name;
         end if;
      end Legacy_Name_Support;

   begin
      Result.Language := Lang_Id;

      if Is_Language_With_No_Compiler (Self, Language (Descr)) then
         Trace (Main_Trace,
                "Language " & Image (Lang_Id) &
                  " requires no compiler");
         Result.Complete := True;
         Result.Selected := True;
         Result.Targets_Set := All_Target_Sets;
         return Result;
      end if;

      Result.Version := To_Unbounded_String (String (Version (Descr)));
      Result.Runtime := To_Unbounded_String (String (Runtime (Descr)));

      if Result.Runtime /= Null_Unbounded_String
        and then GNAT.OS_Lib.Is_Absolute_Path (To_String (Result.Runtime))
      then
         --  If the runtime is a full path, set Runtime and
         --  Runtime_Dir to the same value.
         Result.Runtime_Dir := Result.Runtime;
      end if;

      if Path (Descr) /= No_Filename then
         Result.Path := Path_Name.Create_Directory
                          (Path (Descr), Resolve_Links => True);
      end if;

      if Name (Descr) /= No_Name then
         Result.Name :=
           To_Unbounded_String
             (GNAT.Directory_Operations.Base_Name
                (Legacy_Name_Support
                   (String (Name (Descr)), Lang_Id),
                 Exec_Suffix.all));
      end if;

      GNAT.OS_Lib.Free (Exec_Suffix);

      Result.Complete := False;

      Trace (Main_Trace,
             "Language " & Image (Lang_Id) & " requires a compiler");

      return Result;
   end Create_Filter;

   --------------------------------------
   -- Default_Knowledge_Base_Directory --
   --------------------------------------

   function Default_Location return GPR2.Path_Name.Object is
      use GNATCOLL.VFS;
      use GNATCOLL.VFS_Utils;

      GPRbuild : Filesystem_String_Access :=
                    Locate_Exec_On_Path ("gprbuild");
      Dir       : Virtual_File;
   begin
      if GPRbuild = null then
         raise Default_Location_Error;
      end if;

      Dir := Get_Parent (Create (Dir_Name (GPRbuild.all)));

      Free (GPRbuild);

      if Dir = No_File then
         raise Default_Location_Error;
      end if;

      Dir := Dir.Join ("share").Join ("gprconfig");

      if not OS_Lib.Is_Directory (Dir.Display_Full_Name) then
         raise Default_Location_Error;
      end if;

      return GPR2.Path_Name.Create_Directory
               (Filename_Type (Dir.Display_Full_Name));
   end Default_Location;

   --------------------
   -- Default_Target --
   --------------------

   function Default_Target return Name_Type is
   begin
      if not Default_Target_Parsed then
         Parse_Default_Target_Val;
      end if;

      if Default_Target_Val = Null_Unbounded_String then
         return Name_Type (System.OS_Constants.Target_Name);
      else
         return Name_Type (To_String (Default_Target_Val));
      end if;
   end Default_Target;

   -----------------------------
   -- Extra_Dirs_From_Filters --
   -----------------------------

   function Extra_Dirs_From_Filters
     (Filters : Compiler_Lists.List) return String
   is
      use Compiler_Lists;
      use GPR2.Path_Name;
      C          : Compiler_Lists.Cursor  := First (Filters);
      Extra_Dirs : Unbounded_String;
      Elem       : Compiler;
   begin
      while Has_Element (C) loop
         Elem := Compiler_Lists.Element (C);
         if Elem.Path /= GPR2.Path_Name.Undefined then
            Append
              (Extra_Dirs, Elem.Path.Value & GNAT.OS_Lib.Path_Separator);
         end if;
         Next (C);
      end loop;
      return To_String (Extra_Dirs);
   end Extra_Dirs_From_Filters;

   -------------------
   -- Fallback_List --
   -------------------

   function Fallback_List
     (Self   : Object;
      Target : Name_Type) return GPR2.Containers.Name_List
   is
      use GPR2.Containers.Name_Type_List;

      N_Target : constant Name_Type := Self.Normalized_Target (Target);
      Result   : GPR2.Containers.Name_List;
      Cur      : GPR2.Containers.Name_Type_List.Cursor;
   begin
      for Fallback_List of Self.Fallback_Targets_Sets loop
         Result := Fallback_List;
         Cur := Result.First;
         while Cur /= No_Element loop
            if Element (Cur) = N_Target then
               return Result;
            end if;
            Next (Cur);
         end loop;
      end loop;

      Result.Clear;
      Result.Append (N_Target);
      return Result;
   end Fallback_List;

   ---------------------------
   -- Filter_Compilers_List --
   ---------------------------

   procedure Filter_Compilers_List
     (Self       : Object;
      Compilers  : in out Compiler_Array;
      For_Target : Name_Type)
   is
      use GNATCOLL.Traces;

      For_Target_Set : constant Targets_Set_Id :=
                         Self.Query_Targets_Set (For_Target);
      Check_List     : Compiler_Lists.List;
   begin
      Increase_Indent (Main_Trace, "Filtering the list of compilers");

      for Idx in Compilers'Range loop

         if not Compilers (Idx).Selected then

            if For_Target_Set /= All_Target_Sets
              and then Compilers (Idx).Targets_Set /= All_Target_Sets
              and then Compilers (Idx).Targets_Set /= For_Target_Set
            then
               Compilers (Idx).Selectable := False;
               Trace
                 (Main_Trace,
                  "Incompatible target for: " & To_String (Compilers (Idx)));
               goto Next_Compiler;
            end if;

            for Other_Compiler of Compilers loop
               if Other_Compiler.Language = Compilers (Idx).Language
                 and then Other_Compiler.Selected
               then
                  Compilers (Idx).Selectable := False;
                  Trace
                    (Main_Trace,
                     "Already selected language for: "
                     & To_String (Compilers (Idx)));
                  goto Next_Compiler;
               end if;
            end loop;

            --  We need to check if the resulting selection would
            --  lead to a supported configuration.

            Compilers (Idx).Selected := True;
            for Comp of Compilers loop
               Check_List.Append (Comp);
            end loop;
            Compilers (Idx).Selected := False;

            if not Is_Supported_Config (Self, Check_List) then
               Compilers (Idx).Selectable := False;
               Trace
                 (Main_Trace,
                  "Unsupported config for: "
                  & To_String (Compilers (Idx)));
               goto Next_Compiler;
            end if;

            Compilers (Idx).Selectable := True;

         end if;

         <<Next_Compiler>>
         Check_List.Clear;
      end loop;

      Decrease_Indent (Main_Trace);
   end Filter_Compilers_List;

   ------------------
   -- Filter_Match --
   ------------------

   function Filter_Match
     (Self   : Object;
      Comp   : Compiler;
      Filter : Compiler) return Boolean
   is
      use GNATCOLL.Traces;
      use GPR2.Path_Name;
   begin
      if Filter.Name /= Null_Unbounded_String
        and then Comp.Name /= Filter.Name
        and then Comp.Base_Name /= Filter.Name
      then
         Trace
           (Match_Trace,
            "Filter=" & To_String (Filter) & ": name does not match");
         return False;
      end if;

      if Filter.Path.Is_Defined and then Filter.Path /= Comp.Path then
         Trace
           (Match_Trace,
            "Filter=" & To_String (Filter) & ": path does not match");
         return False;
      end if;

      if Filter.Version /= Null_Unbounded_String
        and then Filter.Version /= Comp.Version
      then
         Trace
           (Match_Trace,
            "Filter=" & To_String (Filter) & ": version does not match");
         return False;
      end if;

      if Comp.Any_Runtime then
         --  If compiler has no runtime node all runtimes should be accepted,
         --  no need to apply filter.
         if Filter.Runtime /= Null_Unbounded_String then

            if not GNAT.OS_Lib.Is_Absolute_Path (To_String (Filter.Runtime))
              and then Filter.Runtime /= Comp.Runtime
              and then Filter.Runtime /= Comp.Alt_Runtime
            then
               Trace
                 (Match_Trace,
                  "Filter=" & To_String (Filter) & ": runtime does not match");
               return False;
            end if;

         elsif not Comp.Default_Runtime then

            Trace
              (Match_Trace,
               "Filter=" & To_String (Filter) & ": no default runtime");
            return False;

         end if;

      end if;

      if Filter.Language /= No_Language
        and then Filter.Language /= Comp.Language
      then
         Trace
           (Match_Trace,
            "Filter=" & To_String (Filter) & ": language does not match");
         return False;
      end if;

      return True;
   end Filter_Match;

   ----------------------------
   -- Generate_Configuration --
   ----------------------------

   function Generate_Configuration
     (Self      : Object;
      Compilers : Compiler_Lists.List;
      Target    : String;
      Errors    : in out Log.Object) return Unbounded_String
   is
      Project_Name      : constant String := "Default";
      Packages          : String_Maps.Map;
      Result            : Unbounded_String;

      procedure Add_Line (S : String);
      --  Appends to result the given string and a line terminator. There is nj
      --  actual need for the terminators but it makes it more readable in
      --  traces.

      procedure Gen (C : String_Maps.Cursor);
      --  C is a cursor of the map "Packages"
      --  Generate the chunk of the config file corresponding to the
      --  given package.

      procedure Gen_And_Remove (Name : String);
      --  Generate the chunk of the config file corresponding to the
      --  package name and remove it from the map.

      --------------
      -- Add_Line --
      --------------

      procedure Add_Line (S : String) is
      begin
         Append (Result, S & ASCII.LF);
      end Add_Line;

      ---------
      -- Gen --
      ---------

      procedure Gen (C : String_Maps.Cursor) is
         use String_Maps;
      begin
         if Key (C) /= "" then
            Add_Line ("");
            Add_Line ("   package " & Key (C) & " is");
         end if;
         Add_Line (String_Maps.Element (C));
         if Key (C) /= "" then
            Add_Line ("   end " & Key (C) & ";");
         end if;
      end Gen;

      --------------------
      -- Gen_And_Remove --
      --------------------

      procedure Gen_And_Remove (Name : String) is
         use String_Maps;
         C : String_Maps.Cursor := Find (Packages, Name);
      begin
         if Has_Element (C) then
            Gen (C);
            Delete (Packages, C);
         end if;
      end Gen_And_Remove;

   begin

      for Config of Self.Configurations loop

         if Match (Config.Compilers_Filters, Compilers)
           and then Match
             (Config.Targets_Filters, Config.Negate_Targets, Compilers)
         then
            if not Config.Supported then
               Errors.Append
                 (Message.Create
                    (Message.Error,
                     "Code generated by these compilers cannot be linked"
                     & " as far as we know.",
                     Sloc => Source_Reference.Create
                       ("embedded_kb/kb", 0, 0)));

               return Null_Unbounded_String;
            end if;

            Merge_Config
              (Self,
               Packages,
               Compilers,
               To_String (Config.Config),
               Config.Sloc,
               Errors);
         end if;

      end loop;

      if Packages.Is_Empty then
         Errors.Append
           (Message.Create
              (Message.Error,
               "No valid configuration found",
               Sloc => Source_Reference.Create
                 ("embedded_kb/kb", 0, 0)));

         return Null_Unbounded_String;
      end if;

      Add_Line ("configuration project " & Project_Name & " is");
      Add_Line ("   for Target use """ & Target & """;");
      Add_Line ("   for Canonical_Target use """
                & String (Self.Normalized_Target (Name_Type (Target)))
                & """;");

      --  Generate known packages in order.  This takes care of possible
      --  dependencies.

      Gen_And_Remove ("");
      Gen_And_Remove ("Builder");
      Gen_And_Remove ("Compiler");
      Gen_And_Remove ("Naming");
      Gen_And_Remove ("Binder");
      Gen_And_Remove ("Linker");

      --  Generate remaining packages

      Packages.Iterate (Gen'Access);

      Add_Line ("end " & Project_Name & ";");

      return Result;
   exception
      when Invalid_KB =>
         --  Errors have already been added to the log
         return Null_Unbounded_String;
   end Generate_Configuration;

   ------------------------
   -- Get_External_Value --
   ------------------------

   procedure Get_External_Value
     (Attribute        : String;
      Value            : External_Value;
      Comp             : Compiler;
      Split_Into_Words : Boolean := True;
      Merge_Same_Dirs  : Boolean := False;
      Calls_Cache      : in out GPR2.Containers.Name_Value_Map;
      Messages         : in out Log.Object;
      Processed_Value  : out External_Value_Lists.List;
      Ignore_Compiler  : out Boolean)
   is
      use External_Value_Nodes;
      use GNAT.Regpat;

      use GNATCOLL.Traces;

      Error_Sloc : constant Source_Reference.Object := Value.Sloc;

      function Get_Command_Output_Cache
        (Path    : String;
         Command : String) return Unbounded_String;
      --  Spawns given command and caches results. When the same command
      --  (same full path and arguments) should be spawned again,
      --  returns output from cache instead.

      ------------------------------
      -- Get_Command_Output_Cache --
      ------------------------------

      function Get_Command_Output_Cache
        (Path    : String;
         Command : String) return Unbounded_String
      is
         use GNAT.OS_Lib;
         use GPR2.Containers.Name_Value_Map_Package;

         Path_Dir : constant String :=
                      GPR2.Path_Name.Create_Directory
                        (Filename_Type (Path)).Dir_Name;
         Key      : constant Name_Type := Name_Type (Path_Dir & Command);
         Cur      : constant GPR2.Containers.Name_Value_Map_Package.Cursor :=
                      Calls_Cache.Find (Key);

         Tmp_Result : Unbounded_String;
      begin
         if Cur = GPR2.Containers.Name_Value_Map_Package.No_Element then
            declare
               Args        : Argument_List_Access :=
                               Argument_String_To_List (Command);
               Args_Vector : GNATCOLL.OS.Process.Argument_List;
               Dummy       : Integer;
            begin
               Args_Vector.Append (Path_Dir & Args (Args'First).all);
               for J in Args'First + 1 .. Args'Last loop
                  Args_Vector.Append (Args (J).all);
               end loop;
               OS_Lib.Free (Args);
               Tmp_Result := GNATCOLL.OS.Process.Run
                 (Args_Vector,
                  Stdin  => GNATCOLL.OS.Process.FS.Null_FD,
                  Stderr => GNATCOLL.OS.Process.FS.To_Stdout,
                  Status => Dummy);
               Args_Vector.Clear;
               Calls_Cache.Include (Key, To_String (Tmp_Result));
               return Tmp_Result;
            end;

         else
            return To_Unbounded_String (Calls_Cache.Element (Key));
         end if;
      end Get_Command_Output_Cache;

      Saved_Path     : constant String :=
                         Environment_Variables.Value ("PATH");
      Extracted_From : Unbounded_String := Null_Unbounded_String;
      Tmp_Result     : Unbounded_String;
      Node_Cursor    : External_Value_Nodes.Cursor := Value.EV.First;
      Node           : External_Value_Node;
      From_Static    : Boolean := False;

      Visited        : String_To_External_Value.Map;
   begin
      Processed_Value.Clear;
      Ignore_Compiler := False;

      while Has_Element (Node_Cursor) loop
         while Has_Element (Node_Cursor) loop
            Node := External_Value_Nodes.Element (Node_Cursor);

            case Node.Typ is
               when Value_Variable =>
                  Extracted_From := Node.Var_Name;

               when Value_Constant =>
                  if Node.Value = Null_Unbounded_String then
                     Tmp_Result := Null_Unbounded_String;
                  else
                     Tmp_Result := To_Unbounded_String
                       (Substitute_Variables_In_Compiler_Description
                          (To_String (Node.Value),
                           Comp,
                           Error_Sloc,
                           Messages));
                  end if;

                  From_Static := True;
                  Trace
                    (Main_Trace,
                     Attribute & ": constant := " & To_String (Tmp_Result));

               when Value_Shell =>
                  Ada.Environment_Variables.Set
                    ("PATH",
                     Comp.Path.Value
                     & OS_Lib.Path_Separator & Saved_Path);

                  declare
                     Command : constant String :=
                                 Substitute_Variables_In_Compiler_Description
                                   (To_String (Node.Command),
                                    Comp,
                                    Error_Sloc,
                                    Messages);
                  begin
                     Tmp_Result := Null_Unbounded_String;
                     Tmp_Result :=
                       Get_Command_Output_Cache (Comp.Path.Value, Command);
                     Ada.Environment_Variables.Set ("PATH", Saved_Path);

                     Trace (Main_Trace,
                            Attribute & ": executing """ & Command
                            & """ output="""
                            & To_String (Tmp_Result) & """");
                  exception
                     when GNATCOLL.OS.OS_Error =>
                        Trace (Main_Trace, "Spawn failed for " & Command);
                  end;

               when Value_Directory =>
                  declare
                     Search : constant String :=
                                Substitute_Variables_In_Compiler_Description
                                  (To_String (Node.Directory),
                                   Comp,
                                   Error_Sloc,
                                   Messages);
                  begin
                     if Search (Search'First) = '/' then
                        Increase_Indent
                          (Main_Trace,
                           Attribute & ": search directories matching "
                           & Search & ", starting from /");

                        Parse_All_Dirs
                          (Processed_Value => Processed_Value,
                           Visited         => Visited,
                           Current_Dir     => "",
                           Path_To_Check   => Search,
                           Contents        => Node.Contents,
                           Regexp          =>
                             Compile (Search (Search'First + 1
                                              .. Search'Last)),
                           Regexp_Str      => Search,
                           Value_If_Match  => To_String (Node.Dir_If_Match),
                           Merge_Same_Dirs => Merge_Same_Dirs,
                           Group           => Node.Directory_Group,
                           Error_Sloc      => Error_Sloc,
                           Messages        => Messages);
                     else
                        Increase_Indent
                          (Main_Trace,
                           Attribute & ": search directories matching "
                           & Search & ", starting from "
                           & Comp.Path.Value);

                        Parse_All_Dirs
                          (Processed_Value => Processed_Value,
                           Visited         => Visited,
                           Current_Dir     => Comp.Path.Value,
                           Path_To_Check   => Search,
                           Contents        => Node.Contents,
                           Regexp          => Compile (Search),
                           Regexp_Str      => Search,
                           Value_If_Match  => To_String (Node.Dir_If_Match),
                           Merge_Same_Dirs => Merge_Same_Dirs,
                           Group           => Node.Directory_Group,
                           Error_Sloc      => Error_Sloc,
                           Messages        => Messages);
                     end if;

                     Decrease_Indent
                       (Main_Trace, "Done search directories");
                  end;

               when Value_Grep =>
                  declare
                     Tmp_Str : constant String := To_String (Tmp_Result);
                     Matched : Match_Array (0 .. Node.Group);
                  begin
                     Match (Node.Regexp_Re.Element, Tmp_Str, Matched);

                     if Matched (0) /= No_Match then
                        Tmp_Result := To_Unbounded_String
                          (Tmp_Str (Matched (Node.Group).First ..
                                    Matched (Node.Group).Last));
                        Trace
                          (Main_Trace,
                           Attribute & ": grep matched="""
                           & To_String (Tmp_Result) & """");
                     else
                        Tmp_Result := Null_Unbounded_String;
                        Trace (Main_Trace, Attribute & ": grep no match");
                     end if;
                  end;

               when Value_Nogrep =>
                  declare
                     Tmp_Str : constant String := To_String (Tmp_Result);
                     Matched : Match_Array (0 .. 0);
                  begin
                     Match (Node.Regexp_No.Element, Tmp_Str, Matched);

                     if Matched (0) /= No_Match then
                        Trace
                          (Main_Trace,
                           Attribute & ": nogrep matched=""" & Tmp_Str & """");
                        Ignore_Compiler := True;
                        return;

                     else
                        Trace (Main_Trace, Attribute & ": nogrep no match");
                     end if;
                  end;

               when Value_Must_Match =>
                  if not Match
                    (Expression => To_String (Node.Must_Match),
                     Data       => To_String (Tmp_Result))
                  then
                     Trace
                       (Main_Trace,
                        "Ignore compiler since external value """
                        & To_String (Tmp_Result) & """ must match "
                        & To_String (Node.Must_Match));

                     Tmp_Result := Null_Unbounded_String;
                     Ignore_Compiler := True;
                     return;
                  end if;

                  exit;

               when Value_Done | Value_Filter =>
                  exit;
            end case;

            Next (Node_Cursor);
         end loop;

         case Node.Typ is
            when Value_Done | Value_Filter | Value_Must_Match =>
               if Tmp_Result = Null_Unbounded_String then
                  --  Value could not be computed
                  if Extracted_From /= Null_Unbounded_String then
                     Processed_Value.Append
                        (External_Value_Item'
                          (Value          => Null_Unbounded_String,
                           Alternate      => Null_Unbounded_String,
                           Extracted_From => Extracted_From));
                  end if;

               elsif Split_Into_Words then
                  declare
                     Filter : constant String :=
                                (if Node.Typ = Value_Filter
                                 then To_String (Node.Filter)
                                 else "");
                     Split  : Containers.Value_List;
                  begin
                     --  When an external value is defined as a static string,
                     --  the only valid separator is ','. When computed
                     --  however, we also allow space as a separator.

                     if From_Static then
                        Get_Words
                          (Words                => To_String (Tmp_Result),
                           Filter               => Filter,
                           Separator1           => ',',
                           Separator2           => ',',
                           Map                  => Split,
                           Allow_Empty_Elements => False);

                     else
                        Get_Words
                          (Words                => To_String (Tmp_Result),
                           Filter               => Filter,
                           Separator1           => ' ',
                           Separator2           => ',',
                           Map                  => Split,
                           Allow_Empty_Elements => False);
                     end if;

                     for Elem of Split loop
                        Processed_Value.Append
                           (External_Value_Item'
                              (Value          =>
                                   To_Unbounded_String (String (Elem)),
                              Alternate      => Null_Unbounded_String,
                              Extracted_From => Extracted_From));
                     end loop;
                  end;

               else
                  Processed_Value.Append
                    (External_Value_Item'
                       (Value          => Tmp_Result,
                        Alternate      => Null_Unbounded_String,
                        Extracted_From => Extracted_From));
               end if;

            when others =>
               null;
         end case;

         Extracted_From := Null_Unbounded_String;

         Next (Node_Cursor);
      end loop;

   end Get_External_Value;

   --------------------------
   -- Get_String_No_Adalib --
   --------------------------

   function Get_String_No_Adalib (Str : String) return String is
      use GNAT.OS_Lib;

      Name : constant String (1 .. Str'Length) := Str;
      Last : Natural := Name'Last;
   begin
      if Last > 7
        and then (Name (Last) in Directory_Separator | '/')
      then
         Last := Last - 1;
      end if;

      if Last > 6
        and then Name (Last - 5 .. Last) = "adalib"
        and then (Name (Last - 6) in Directory_Separator | '/')
      then
         Last := Last - 6;
      else
         Last := Name'Last;
      end if;

      return Name (1 .. Last);
   end Get_String_No_Adalib;

   ------------------------
   -- Get_Variable_Value --
   ------------------------

   function Get_Variable_Value
     (Comp : Compiler;
      Name : String) return String
   is
      N : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if Variables_Maps.Contains (Comp.Variables, N) then
         return To_String (Variables_Maps.Element (Comp.Variables, N));
      elsif Name = "HOST" then
         return String (Default_Target);
      elsif Name = "TARGET" then
         return To_String (Comp.Target);
      elsif Name = "RUNTIME_DIR" then
         return Name_As_Directory (To_String (Comp.Runtime_Dir));
      elsif Name = "EXEC" then
         return To_String (Comp.Executable);
      elsif Name = "VERSION" then
         return To_String (Comp.Version);
      elsif Name = "LANGUAGE" then
         return String (GPR2.Name (Comp.Language));
      elsif Name = "RUNTIME" then
         return To_String (Comp.Runtime);
      elsif Name = "PREFIX" then
         return To_String (Comp.Prefix);
      elsif Name = "PATH" then
         return GNAT.OS_Lib.Normalize_Pathname
           (Comp.Path.Value, Case_Sensitive => False)
           & GNAT.OS_Lib.Directory_Separator;
      elsif Name = "GPRCONFIG_PREFIX" then
         return GPR_Executable_Prefix_Path;
      end if;

      raise Invalid_KB
        with "variable '" & Name & "' is not defined";
   end Get_Variable_Value;

   ---------------
   -- Get_Words --
   ---------------

   procedure Get_Words
     (Words                : String;
      Filter               : String;
      Separator1           : Character;
      Separator2           : Character;
      Map                  : out Containers.Value_List;
      Allow_Empty_Elements : Boolean)
   is
      First      : Integer := Words'First;
      Last       : Integer;
      Filter_Set : GPR2.Containers.Value_List;
   begin
      if Filter /= "" then
         Get_Words (Filter, "", Separator1,
                    Separator2, Filter_Set,
                    Allow_Empty_Elements => True);
      end if;

      if not Allow_Empty_Elements then
         while First <= Words'Last
           and then (Words (First) = Separator1
                     or else Words (First) = Separator2)
         loop
            First := First + 1;
         end loop;
      end if;

      while First <= Words'Last loop
         if Words (First) /= Separator1
           and then Words (First) /= Separator2
         then
            Last := First + 1;
            while Last <= Words'Last
              and then Words (Last) /= Separator1
              and then Words (Last) /= Separator2
            loop
               Last := Last + 1;
            end loop;
         else
            Last := First;
         end if;

         if (Allow_Empty_Elements or else First <= Last - 1)
           and then
             (Filter_Set.Is_Empty
              or else Filter_Set.Contains
                (Value_Type (Words (First .. Last - 1))))
         then
            Map.Append (Value_Type (Words (First .. Last - 1)));
         end if;

         First := Last + 1;
      end loop;
   end Get_Words;

   --------------------------------
   -- GPR_Executable_Prefix_Path --
   --------------------------------

   function GPR_Executable_Prefix_Path return String
   is
      use Ada.Directories;
      use Ada.Strings.Fixed;
      use GNAT.OS_Lib;

      Tools_Dir : constant String := Get_Tools_Directory;
      Exec_Name : constant String :=
                    Normalize_Pathname (Ada.Command_Line.Command_Name,
                                        Resolve_Links => True);
   begin

      if Has_Directory_Separator (Exec_Name)
        and then Head (Base_Name (Exec_Name), 3) = "gpr"
        and then Base_Name (Containing_Directory (Exec_Name)) = "bin"
      then
         --  A gprtool has been called with path prefix, we need
         --  to return the prefix of corresponding gprtools installation,
         --  in case it is not the first one on the path.

         return
           Containing_Directory (Containing_Directory (Exec_Name))
           & GNAT.OS_Lib.Directory_Separator;
      end if;

      --  It's either a gprtool called by base name or another kind of tool,
      --  in both cases we need to find gprtools on PATH.

      if Tools_Dir = "" then
         return "";
      else
         return Tools_Dir & GNAT.OS_Lib.Directory_Separator;
      end if;

   end GPR_Executable_Prefix_Path;

   ----------------------------------
   -- Is_Language_With_No_Compiler --
   ----------------------------------

   function Is_Language_With_No_Compiler
     (Self     : Object;
      Language : Language_Id) return Boolean is
   begin
      return Self.No_Compilers.Contains (Language);
   end Is_Language_With_No_Compiler;

   -------------------------
   -- Is_Supported_Config --
   -------------------------

   function Is_Supported_Config
     (Self      : Object;
      Compilers : Compiler_Lists.List) return Boolean
   is
      use Configuration_Lists;

      Config            : Configuration_Lists.Cursor :=
                            First (Self.Configurations);
      M                 : Boolean;
   begin
      while Has_Element (Config) loop
         M := Match (Configuration_Lists.Element (Config).Compilers_Filters,
                     Compilers);
         if M and then Match
           (Configuration_Lists.Element (Config).Targets_Filters,
            Configuration_Lists.Element (Config).Negate_Targets,
            Compilers)
         then
            if not Configuration_Lists.Element (Config).Supported then
               GNATCOLL.Traces.Trace
                 (Match_Trace,
                  "Selected compilers are not compatible, because of:");
               GNATCOLL.Traces.Trace
                 (Match_Trace,
                  To_String
                    (Configuration_Node_Image
                         (Configuration_Lists.Element (Config))));
               return False;
            end if;
         end if;
         Next (Config);
      end loop;
      return True;
   end Is_Supported_Config;

   --------------------------
   -- Known_Compiler_Names --
   --------------------------

   function Known_Compiler_Names (Self : Object) return Unbounded_String is
      use Compiler_Description_Maps;

      Result : Unbounded_String;
      Cur    : Compiler_Description_Maps.Cursor := Self.Compilers.First;
   begin
      while Has_Element (Cur) loop
         if Result /= Null_Unbounded_String then
            Append (Result, ",");
         end if;
         Append (Result, String (Key (Cur)));

         Next (Cur);
      end loop;

      return Result;
   end Known_Compiler_Names;

   -----------
   -- Match --
   -----------

   function Match
     (Filter    : Compilers_Filter_Lists.List;
      Compilers : Compiler_Lists.List) return Boolean
   is
      use Compilers_Filter_Lists;
      C : Compilers_Filter_Lists.Cursor := First (Filter);
      M : Boolean;
   begin
      while Has_Element (C) loop
         M := Match (Compilers_Filter_Lists.Element (C), Compilers);
         if not M then
            return False;
         end if;

         Next (C);
      end loop;

      return True;
   end Match;

   -----------
   -- Match --
   -----------

   function Match
     (Filter            : Compilers_Filter;
      Compilers         : Compiler_Lists.List) return Boolean
   is
      use Compiler_Filter_Lists;
      C : Compiler_Filter_Lists.Cursor := First (Filter.Compiler);
      M : Boolean;
   begin
      while Has_Element (C) loop
         M := Match (Compiler_Filter_Lists.Element (C), Compilers);
         if M then
            return not Filter.Negate;
         end if;
         Next (C);
      end loop;
      return Filter.Negate;
   end Match;

   -----------
   -- Match --
   -----------

   function Match
     (Filter    : Compiler_Filter;
      Compilers : Compiler_Lists.List) return Boolean
   is
      use Compiler_Lists;
      use GNAT.Regpat;
      C    : Compiler_Lists.Cursor := First (Compilers);
      Comp : Compiler;

      function Runtime_Base_Name (Rt : Unbounded_String) return String is
        (GNAT.Directory_Operations.Base_Name (To_String (Rt)));
      --  Runtime filters should only apply to the base name of runtime when
      --  full path is given, otherwise we can potentially match some unrelated
      --  patterns from enclosing directory names.

      function Name_Matches return Boolean;
      function Version_Matches return Boolean;
      function Runtime_Matches return Boolean;
      function Language_Matches return Boolean;
      --  Predicates checking matching of corresponding attributes

      ----------------------
      -- Language_Matches --
      ----------------------

      function Language_Matches return Boolean is
      begin
         if Filter.Language = No_Language
           or else Filter.Language = Comp.Language
         then
            return True;
         end if;

         return False;
      end Language_Matches;

      ------------------
      -- Name_Matches --
      ------------------

      function Name_Matches return Boolean is
      begin
         if Filter.Name = Null_Unbounded_String then
            return True;
         end if;

         if Comp.Name /= Null_Unbounded_String
           and then Match (Filter.Name_Re.Element, To_String (Comp.Name))
         then
            return True;
         end if;

         if Comp.Base_Name = Filter.Name then
            return True;
         end if;

         return False;
      end Name_Matches;

      ---------------------
      -- Runtime_Matches --
      ---------------------

      function Runtime_Matches return Boolean is
      begin
         if Filter.Runtime_Re.Is_Empty then
            return True;
         end if;

         if Comp.Runtime /= Null_Unbounded_String
           and then Match (Filter.Runtime_Re.Element,
                           Runtime_Base_Name (Comp.Runtime))
         then
            return True;
         end if;

         return False;
      end Runtime_Matches;

      ----------------------
      --  Version_Matches --
      ----------------------

      function Version_Matches return Boolean is
      begin
         if Filter.Version_Re.Is_Empty then
            return True;
         end if;

         if Comp.Version /= Null_Unbounded_String
           and then Match (Filter.Version_Re.Element, To_String (Comp.Version))
         then
            return True;
         end if;

         return False;
      end Version_Matches;

   begin
      while Has_Element (C) loop
         Comp := Compiler_Lists.Element (C);

         if Comp.Selected
           and then Name_Matches
           and then Version_Matches
           and then Runtime_Matches
           and then Language_Matches
         then
            return True;
         end if;
         Next (C);
      end loop;

      return False;
   end Match;

   -----------
   -- Match --
   -----------

   function Match
     (Target_Filter : Double_String_Lists.List;
      Negate        : Boolean;
      Compilers     : Compiler_Lists.List) return Boolean
   is
      use Compiler_Lists;
      use Double_String_Lists;
      use GNAT.Regpat;
      Target : Double_String_Lists.Cursor := First (Target_Filter);
      Comp   : Compiler_Lists.Cursor;
   begin
      if Is_Empty (Target_Filter) then
         return True;

      else
         while Has_Element (Target) loop
            declare
               Positive_Pattern : constant Pattern_Matcher :=
                 Compile
                   (To_String
                      (Double_String_Lists.Element (Target).Positive_Regexp),
                    GNAT.Regpat.Case_Insensitive);
               Negative_Pattern : constant Pattern_Matcher :=
                 Compile
                   (To_String
                      (Double_String_Lists.Element (Target).Negative_Regexp),
                    GNAT.Regpat.Case_Insensitive);

               Ignore_Negative : constant Boolean :=
                 Double_String_Lists.Element (Target).Negative_Regexp = "";
            begin
               Comp := First (Compilers);
               while Has_Element (Comp) loop
                  if Compiler_Lists.Element (Comp).Selected then
                     if Compiler_Lists.Element (Comp).Target =
                       Null_Unbounded_String
                     then
                        if Match (Positive_Pattern, "") then
                           return not Negate;
                        end if;

                     elsif Match
                       (Positive_Pattern,
                        To_String (Compiler_Lists.Element (Comp).Target))
                       and then
                         (Ignore_Negative or else not Match
                            (Negative_Pattern,
                             To_String
                               (Compiler_Lists.Element (Comp).Target)))
                     then
                        return not Negate;
                     end if;
                  end if;
                  Next (Comp);
               end loop;
            end;

            Next (Target);
         end loop;
         return Negate;
      end if;
   end Match;

   ------------------
   -- Merge_Config --
   ------------------

   procedure Merge_Config
     (Self       : Object;
      Packages   : in out String_Maps.Map;
      Compilers  : Compiler_Lists.List;
      Config     : String;
      Error_Sloc : Source_Reference.Object;
      Errors     : in out Log.Object)
   is
      procedure Add_Package
        (Name : String; Chunk : String; Prefix : String := "      ");
      --  Add the chunk in the appropriate package

      procedure Skip_Spaces (Str : String; Index : in out Integer);
      --  Move Index from its current position to the next non-whitespace
      --  character in Str

      procedure Skip_Spaces_Backward (Str : String; Index : in out Integer);
      --  Same as Skip_Spaces, but goes backward

      -----------------
      -- Add_Package --
      -----------------

      procedure Add_Package
        (Name : String; Chunk : String; Prefix : String := "      ")
      is
         use String_Maps;
         C        : constant String_Maps.Cursor :=
                      Find (Packages, Name);
         Replaced : constant String :=
                      Substitute_Variables_In_Configuration
                        (Self, Chunk, Compilers, Error_Sloc, Errors);
      begin
         if Replaced /= "" then
            if Has_Element (C) then
               Replace_Element
                 (Packages,
                  C,
                  Element (C) & ASCII.LF & Prefix & Replaced);
            else
               Insert
                 (Packages,
                  Name, Prefix & Replaced);
            end if;
         end if;
      end Add_Package;

      -----------------
      -- Skip_Spaces --
      -----------------

      procedure Skip_Spaces (Str : String; Index : in out Integer) is
      begin
         while Index <= Str'Last
           and then (Str (Index) = ' ' or else Str (Index) = ASCII.LF)
         loop
            Index := Index + 1;
         end loop;
      end Skip_Spaces;

      --------------------------
      -- Skip_Spaces_Backward --
      --------------------------

      procedure Skip_Spaces_Backward (Str : String; Index : in out Integer) is
      begin
         while Index >= Str'First
           and then (Str (Index) = ' ' or else Str (Index) = ASCII.LF)
         loop
            Index := Index - 1;
         end loop;
      end Skip_Spaces_Backward;

      First                         : Integer := Config'First;
      Pkg_Name_First, Pkg_Name_Last : Integer;
      Pkg_Content_First             : Integer;
      Last                          : Integer;

      use Ada.Strings.Fixed;
   begin
      while First /= 0 and then First <= Config'Last loop
         --  Do we have a toplevel attribute ?
         Skip_Spaces (Config, First);
         Pkg_Name_First := Index (Config (First .. Config'Last), "package ");
         if Pkg_Name_First = 0 then
            Pkg_Name_First := Config'Last + 1;
         end if;

         Last := Pkg_Name_First - 1;
         Skip_Spaces_Backward (Config, Last);
         Add_Package
           (Name   => "",
            Chunk  => Config (First .. Last),
            Prefix => "   ");

         exit when Pkg_Name_First > Config'Last;

         --  Parse the current package

         Pkg_Name_First := Pkg_Name_First + 8;  --  skip "package "
         Skip_Spaces (Config, Pkg_Name_First);

         Pkg_Name_Last := Pkg_Name_First + 1;

         while Pkg_Name_Last <= Config'Last
           and then Config (Pkg_Name_Last) /= ' '
           and then Config (Pkg_Name_Last) /= ASCII.LF
         loop
            Pkg_Name_Last := Pkg_Name_Last + 1;
         end loop;

         Pkg_Content_First := Pkg_Name_Last + 1;
         Skip_Spaces (Config, Pkg_Content_First);
         Pkg_Content_First := Pkg_Content_First + 2; --  skip "is"
         Skip_Spaces (Config, Pkg_Content_First);

         Last := Index (Config (Pkg_Content_First .. Config'Last),
                        "end " & Config (Pkg_Name_First .. Pkg_Name_Last - 1));
         if Last /= 0 then
            First := Last - 1;
            Skip_Spaces_Backward (Config, First);
            Add_Package
              (Name  => Config (Pkg_Name_First .. Pkg_Name_Last - 1),
               Chunk => Config (Pkg_Content_First .. First));

            while Last <= Config'Last and then Config (Last) /= ';' loop
               Last := Last + 1;
            end loop;
            Last := Last + 1;
         end if;
         First := Last;
      end loop;
   end Merge_Config;

   -----------------------
   -- Name_As_Directory --
   -----------------------

   function Name_As_Directory (Dir : String) return String is
      use GNAT.OS_Lib;
   begin
      if Dir = ""
        or else Dir (Dir'Last) in Directory_Separator | '/'
      then
         return Dir;
      else
         return Dir & Directory_Separator;
      end if;
   end Name_As_Directory;

   -----------------------
   -- Normalized_Target --
   -----------------------

   function Normalized_Target
     (Self   : Object;
      Target : Name_Type) return Name_Type
   is
      Result : Target_Set_Description;
   begin
      Result := Targets_Set_Vectors.Element
        (Self.Targets_Sets, Self.Query_Targets_Set (Target));

      return Name_Type (To_String (Result.Name));
   exception
      when others =>
         return "unknown";
   end Normalized_Target;

   --------------------
   -- Parse_All_Dirs --
   --------------------

   procedure Parse_All_Dirs
     (Processed_Value : out External_Value_Lists.List;
      Visited         : in out String_To_External_Value.Map;
      Current_Dir     : String;
      Path_To_Check   : String;
      Regexp          : Regpat.Pattern_Matcher;
      Regexp_Str      : String;
      Value_If_Match  : String;
      Group           : Integer;
      Group_Match     : String := "";
      Group_Count     : Natural := 0;
      Contents        : Pattern_Matcher_Holder;
      Merge_Same_Dirs : Boolean;
      Error_Sloc      : Source_Reference.Object;
      Messages        : in out Log.Object)
   is
      use GNAT.Regpat;
      use GNAT.OS_Lib;
      use GNATCOLL.Traces;

      procedure Save_File (Current_Dir : String; Val : String);
      --  Mark the given directory as valid for the <directory> configuration.
      --  This takes care of removing duplicates if needed.

      function Is_Regexp (Str : String) return Boolean;
      --  Whether Str is a regular expression

      function Unquote_Regexp
        (Str : String; Remove_Quoted : Boolean := False) return String;
      --  Remove special '\' quoting characters from Str.
      --  As a special case, if Remove_Quoted is true, then '\'
      --  and the following char are simply omitted in the output.
      --  For instance:
      --      Str="A\." Remove_Quoted=False  => output is "A."
      --      Str="A\." Remove_Quoted=False  => output is "A"

      ---------------
      -- Is_Regexp --
      ---------------

      function Is_Regexp (Str : String) return Boolean is
         --  Take into account characters quoted by '\'. We just remove them
         --  for now, so that when we quote the regexp it won't see these
         --  potentially special characters.
         --  The goal is that for instance "\.\." is not considered
         --  as a regexp, but "\.." is.
         Str2 : constant String := Unquote_Regexp (Str, Remove_Quoted => True);
      begin
         return Regpat.Quote (Str2) /= Str2;
      end Is_Regexp;

      ---------------
      -- Save_File --
      ---------------

      procedure Save_File (Current_Dir : String; Val : String) is
      begin
         if not Merge_Same_Dirs then
            Trace (Main_Trace, "<dir>: SAVE " & Current_Dir);

            Processed_Value.Append
               ((Value          => To_Unbounded_String (Val),
                 Alternate      => Null_Unbounded_String,
                 Extracted_From =>
                   To_Unbounded_String (Get_String_No_Adalib (Current_Dir))));

         else
            declare
               use String_To_External_Value;

               Normalized : constant String := Normalize_Pathname
                              (Name           => Current_Dir,
                               Directory      => "",
                               Resolve_Links  => True,
                               Case_Sensitive => True);
               Prev       : External_Value_Lists.Cursor;
               Rec        : External_Value_Item;
            begin
               if Visited.Contains (Normalized) then
                  Trace
                    (Main_Trace,
                     "<dir>: ALREADY FOUND (" & Val & ") " & Current_Dir);

                  Prev          := Visited.Element (Normalized);
                  Rec           := External_Value_Lists.Element (Prev);
                  Rec.Alternate := To_Unbounded_String (Val);

                  External_Value_Lists.Replace_Element
                    (Container => Processed_Value,
                     Position  => Prev,
                     New_Item  => Rec);

               else
                  Trace
                    (Main_Trace, "<dir>: SAVE (" & Val & ") " & Current_Dir);

                  Processed_Value.Append
                    ((Value          => To_Unbounded_String (Val),
                      Alternate      => Null_Unbounded_String,
                      Extracted_From =>
                        To_Unbounded_String
                          (Get_String_No_Adalib (Current_Dir))));

                  Visited.Include
                    (Normalized, External_Value_Lists.Last (Processed_Value));
               end if;
            end;
         end if;
      end Save_File;

      --------------------
      -- Unquote_Regexp --
      --------------------

      function Unquote_Regexp
        (Str : String; Remove_Quoted : Boolean := False) return String
      is
         Str2  : String (Str'Range);
         S     : Integer := Str'First;
         Index : Integer := Str2'First;
      begin
         while S <= Str'Last loop
            if Str (S) = '\' then
               S := S + 1;

               if not Remove_Quoted then
                  Str2 (Index) := Str (S);
                  Index := Index + 1;
               end if;

            else
               Str2 (Index) := Str (S);
               Index := Index + 1;
            end if;

            S     := S + 1;
         end loop;

         return Str2 (Str2'First .. Index - 1);
      end Unquote_Regexp;

      First : constant Integer := Path_To_Check'First;
      Last  : Integer;
      Val   : Unbounded_String;

   begin
      if Path_To_Check'Length = 0
        or else Path_To_Check = "/"
        or else Path_To_Check = String'(1 => Directory_Separator)
      then
         if Group = -1 then
            Val := To_Unbounded_String (Value_If_Match);
         else
            Val := To_Unbounded_String (Group_Match);
         end if;

         if not Contents.Is_Empty
           and then Is_Regular_File (Current_Dir)
         then
            Trace (Main_Trace, "<dir>: Checking inside file " & Current_Dir);

            declare
               use Ada.Text_IO;
               use Directory_Operations;
               F : File_Type;
            begin
               Open (F, In_File, Current_Dir);

               while not End_Of_File (F) loop
                  declare
                     Line : constant String := Get_Line (F);
                  begin
                     Trace (Main_Trace, "<dir>: read line " & Line);

                     if Match (Contents.Element, Line) then
                        Save_File
                          (Normalize_Pathname
                             (Name => Line,
                              Directory => Dir_Name (Current_Dir),
                              Resolve_Links => True),
                           To_String (Val));
                        exit;
                     end if;
                  end;
               end loop;

               Close (F);
            end;

         else
            Save_File (Current_Dir, To_String (Val));
         end if;

      else
         --  Do not split on '\', since we document we only accept UNIX paths
         --  anyway. This leaves \ for regexp quotes.
         Last := First + 1;

         while Last <= Path_To_Check'Last
           and then Path_To_Check (Last) /= '/'
         loop
            Last := Last + 1;
         end loop;

         --  If we do not have a regexp

         if not Is_Regexp (Path_To_Check (First .. Last - 1)) then
            declare
               Dir     : constant String :=
                           Normalize_Pathname
                             (Current_Dir, Resolve_Links => False)
                           & Directory_Separator
                           & Unquote_Regexp
                              (Path_To_Check (First .. Last - 1));
               Remains : constant String :=
                           Path_To_Check (Last + 1 .. Path_To_Check'Last);
            begin
               if (Remains'Length = 0
                   or else Remains = "/"
                   or else Remains = String'(1 => Directory_Separator))
                 and then Is_Regular_File (Dir)
               then
                  Trace (Main_Trace, "<dir>: Found file " & Dir);
                  --  If there is such a subdir, keep checking

                  Parse_All_Dirs
                    (Processed_Value => Processed_Value,
                     Visited         => Visited,
                     Current_Dir     => Dir,
                     Path_To_Check   => Remains,
                     Regexp          => Regexp,
                     Regexp_Str      => Regexp_Str,
                     Value_If_Match  => Value_If_Match,
                     Group           => Group,
                     Group_Match     => Group_Match,
                     Group_Count     => Group_Count,
                     Contents        => Contents,
                     Merge_Same_Dirs => Merge_Same_Dirs,
                     Error_Sloc      => Error_Sloc,
                     Messages        => Messages);

               elsif Is_Directory (Dir) then
                  Trace (Main_Trace, "<dir>: Recurse into " & Dir);
                  --  If there is such a subdir, keep checking

                  Parse_All_Dirs
                    (Processed_Value => Processed_Value,
                     Visited         => Visited,
                     Current_Dir     => Dir & Directory_Separator,
                     Path_To_Check   => Remains,
                     Regexp          => Regexp,
                     Regexp_Str      => Regexp_Str,
                     Value_If_Match  => Value_If_Match,
                     Group           => Group,
                     Group_Match     => Group_Match,
                     Group_Count     => Group_Count,
                     Contents        => Contents,
                     Merge_Same_Dirs => Merge_Same_Dirs,
                     Error_Sloc      => Error_Sloc,
                     Messages        => Messages);
               else
                  Trace (Main_Trace, "<dir>: No such directory: " & Dir);
               end if;
            end;

         --  Else we have a regexp, check all files

         else
            declare
               use Ada.Directories;

               File_Re     : constant String :=
                               Path_To_Check (First .. Last - 1);
               File_Regexp : constant Pattern_Matcher := Compile (File_Re);
               Search      : Search_Type;
               File        : Directory_Entry_Type;
               Filter      : Ada.Directories.Filter_Type;
            begin
               if File_Re = ".." then
                  Trace
                    (Main_Trace,
                     "Potential error: .. is generally not meant as a regexp,"
                     & " and should be quoted in this case, as in \.\.");
               end if;

               if Path_To_Check (Last) = '/' then
                  Trace
                    (Main_Trace,
                     "<dir>: Check directories in " & Current_Dir
                     & " that match " & File_Re);
                  Filter := (Directory => True, others => False);

               else
                  Trace
                    (Main_Trace,
                     "<dir>: Check files in " & Current_Dir
                     & " that match " & File_Re);
                  Filter := (others => True);
               end if;

               Start_Search
                 (Search    => Search,
                  Directory => Current_Dir,
                  Filter    => Filter,
                  Pattern   => "");

               while More_Entries (Search) loop
                  begin
                     Get_Next_Entry (Search, File);
                  exception
                     when E : others =>
                        Trace
                          (Main_Trace,
                           "<dir>: ignoring entry, "
                           & Ada.Exceptions.Exception_Message (E));
                        goto Next_Entry;
                  end;

                  if Directories.Simple_Name (File) /= "."
                    and then Directories.Simple_Name (File) /= ".."
                  then
                     declare
                        Simple  : constant String :=
                                    Directories.Simple_Name (File);
                        Count   : constant Natural :=
                                    Paren_Count (File_Regexp);
                        Matched : Match_Array (0 .. Integer'Max (Group, 0));
                     begin
                        Match (File_Regexp, Simple, Matched);

                        if Matched (0) /= No_Match then
                           Trace
                             (Main_Trace,
                              "<dir>: Matched "
                              & Ada.Directories.Simple_Name (File));

                           if Group_Count < Group
                             and then Group_Count + Count >= Group
                           then
                              if Matched (Group - Group_Count) = No_Match then
                                 Trace
                                   (Main_Trace,
                                    "<dir>: Matched group is empty, skipping");
                                 return;
                              end if;
                              Trace
                                (Main_Trace,
                                 "<dir>: Found matched group: "
                                 & Simple (Matched (Group - Group_Count).First
                                   .. Matched (Group - Group_Count).Last));

                              Parse_All_Dirs
                                (Processed_Value => Processed_Value,
                                 Visited         => Visited,
                                 Current_Dir     =>
                                   Full_Name (File) & Directory_Separator,
                                 Path_To_Check   => Path_To_Check
                                   (Last + 1 .. Path_To_Check'Last),
                                 Regexp          => Regexp,
                                 Regexp_Str      => Regexp_Str,
                                 Value_If_Match  => Value_If_Match,
                                 Group           => Group,
                                 Group_Match     =>
                                   Simple (Matched (Group - Group_Count).First
                                       .. Matched (Group - Group_Count).Last),
                                 Group_Count     => Group_Count + Count,
                                 Contents        => Contents,
                                 Merge_Same_Dirs => Merge_Same_Dirs,
                                 Error_Sloc      => Error_Sloc,
                                 Messages        => Messages);

                           else
                              Parse_All_Dirs
                                (Processed_Value => Processed_Value,
                                 Visited         => Visited,
                                 Current_Dir     =>
                                   Full_Name (File) & Directory_Separator,
                                 Path_To_Check   => Path_To_Check
                                   (Last + 1 .. Path_To_Check'Last),
                                 Regexp          => Regexp,
                                 Regexp_Str      => Regexp_Str,
                                 Value_If_Match  => Value_If_Match,
                                 Group           => Group,
                                 Group_Match     => Group_Match,
                                 Group_Count     => Group_Count + Count,
                                 Contents        => Contents,
                                 Merge_Same_Dirs => Merge_Same_Dirs,
                                 Error_Sloc      => Error_Sloc,
                                 Messages        => Messages);
                           end if;
                        end if;
                     end;
                  end if;
                  << Next_Entry >>
               end loop;
            end;
         end if;
      end if;
   end Parse_All_Dirs;

   ------------------------------
   -- Parse_Default_Target_Val --
   ------------------------------

   procedure Parse_Default_Target_Val is
      use GNATCOLL.Traces;
      use GNAT.OS_Lib;

      Tgt_File_Base : constant String := "default_target";
      Tgt_File_Full : constant String :=
                        GPR_Executable_Prefix_Path
                        & "share" & Directory_Separator
                        & "gprconfig" & Directory_Separator & Tgt_File_Base;

      F : Ada.Text_IO.File_Type;
   begin
      Trace (Main_Trace, "Parsing default target");
      Default_Target_Parsed := True;

      if GPR_Executable_Prefix_Path = "" then
         Trace (Main_Trace, "Gprtools installation not found");
         return;
      end if;

      if not Is_Regular_File (Tgt_File_Full) then
         Trace (Main_Trace, Tgt_File_Full & " not found");
         return;
      end if;

      Ada.Text_IO.Open (F, Ada.Text_IO.In_File, Tgt_File_Full);
      Default_Target_Val := To_Unbounded_String (Ada.Text_IO.Get_Line (F));
      Ada.Text_IO.Close (F);
   exception
      when X : others =>
         Trace (Main_Trace, "Cannot parse " & Tgt_File_Full);
         Trace (Main_Trace, Ada.Exceptions.Exception_Information (X));
   end Parse_Default_Target_Val;

   -----------------------
   -- Query_Targets_Set --
   -----------------------

   function Query_Targets_Set
     (Self   : Object;
      Target : Name_Type) return Targets_Set_Id
   is
      use Targets_Set_Vectors;
      use Target_Lists;

      Tgt : constant String := String (Target);
   begin
      if Target = "all" then
         return All_Target_Sets;
      end if;

      for I in
        First_Index (Self.Targets_Sets) .. Last_Index (Self.Targets_Sets)
      loop
         declare
            Set : constant Target_Lists.List :=
                    Targets_Set_Vectors.Element
                      (Self.Targets_Sets, I).Patterns;
            C   : Target_Lists.Cursor := First (Set);
         begin
            while Has_Element (C) loop
               if GNAT.Regpat.Match
                 (Target_Lists.Element (C), Tgt) > Tgt'First - 1
               then
                  return I;
               end if;

               Next (C);
            end loop;
         end;
      end loop;

      return Unknown_Targets_Set;
   end Query_Targets_Set;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Object) is
   begin
      null;
   end Release;

   -------------
   -- Runtime --
   -------------

   function Runtime
     (Comp      : Compiler;
      Alternate : Boolean := False) return Optional_Name_Type is
   begin
      if Alternate then
         if Comp.Runtime /= Null_Unbounded_String then
            if Comp.Alt_Runtime /= Null_Unbounded_String then
               return Optional_Name_Type
                 (To_String (Comp.Runtime)
                  & " ["
                  & To_String (Comp.Alt_Runtime)
                  & "]");
            else
               return Optional_Name_Type (To_String (Comp.Runtime));
            end if;
         else
            return No_Name;
         end if;
      end if;

      if Comp.Alt_Runtime /= Null_Unbounded_String then
         return Optional_Name_Type (To_String (Comp.Alt_Runtime));
      elsif Comp.Runtime /= Null_Unbounded_String then
         return Optional_Name_Type (To_String (Comp.Runtime));
      else
         return No_Name;
      end if;
   end Runtime;

   -------------------
   -- Set_Selection --
   -------------------

   procedure Set_Selection
     (Compilers : in out Compiler_Lists.List;
      Cursor    : Compiler_Lists.Cursor;
      Selected  : Boolean)
   is
      procedure Internal (Comp : in out Compiler);

      --------------
      -- Internal --
      --------------

      procedure Internal (Comp : in out Compiler) is
      begin
         Set_Selection (Comp, Selected);
      end Internal;

   begin
      Compiler_Lists.Update_Element (Compilers, Cursor, Internal'Access);
   end Set_Selection;

   -------------------
   -- Set_Selection --
   -------------------

   procedure Set_Selection
     (Comp     : in out Compiler;
      Selected : Boolean) is
   begin
      Comp.Selected := Selected;
   end Set_Selection;

   --------------------------
   -- Substitute_Variables --
   --------------------------

   function Substitute_Variables
     (Str        : String;
      Error_Sloc : Source_Reference.Object;
      Messages   : in out Log.Object) return String
   is
      use Ada.Characters.Handling;

      Str_Len              : constant Natural := Str'Last;
      Pos                  : Natural := Str'First;
      Last                 : Natural := Pos;
      Result               : Unbounded_String;
      Word_Start, Word_End : Natural;
      Tmp                  : Natural;
      Has_Index            : Boolean;

   begin
      while Pos < Str_Len loop
         if Str (Pos) = '$' and then Str (Pos + 1) = '$' then
            Append (Result, Str (Last .. Pos - 1));
            Append (Result, "$");
            Last := Pos + 2;
            Pos  := Last;

         elsif Str (Pos) = '$' then
            if Str (Pos + 1)  = '{' then
               Word_Start := Pos + 2;
               Tmp := Pos + 2;

               while Tmp <= Str_Len and then Str (Tmp) /= '}' loop
                  Tmp := Tmp + 1;
               end loop;

               Tmp := Tmp + 1;
               Word_End := Tmp - 2;

            else
               Word_Start := Pos + 1;
               Tmp := Pos + 1;

               while Tmp <= Str_Len
                 and then (Is_Alphanumeric (Str (Tmp)) or else Str (Tmp) = '_')
               loop
                  Tmp := Tmp + 1;
               end loop;

               Word_End := Tmp - 1;
            end if;

            Append (Result, Str (Last ..  Pos - 1));

            Has_Index := False;

            for W in Word_Start .. Word_End loop
               if Str (W) = '(' then
                  Has_Index := True;

                  if Str (Word_End) /= ')' then
                     Messages.Append
                       (Message.Create
                          (Message.Error,
                           "Missing closing parenthesis in variable name: "
                           & Str (Word_Start .. Word_End),
                           Sloc => Error_Sloc));
                     raise Invalid_KB;

                  else
                     Append
                       (Result,
                        Callback
                          (Var_Name => Str (Word_Start .. W - 1),
                           Index    => Str (W + 1 .. Word_End - 1)));
                  end if;

                  exit;
               end if;
            end loop;

            if not Has_Index then
               Append (Result, Callback (Str (Word_Start .. Word_End), ""));
            end if;

            Last := Tmp;
            Pos  := Last;
         else
            Pos := Pos + 1;
         end if;
      end loop;

      Append (Result, Str (Last .. Str_Len));
      return To_String (Result);
   end Substitute_Variables;

   --------------------------------------------------
   -- Substitute_Variables_In_Compiler_Description --
   --------------------------------------------------

   function Substitute_Variables_In_Compiler_Description
     (Str        : String;
      Comp       : Compiler;
      Error_Sloc : Source_Reference.Object;
      Messages   : in out Log.Object) return String
   is

      function Callback (Var_Name, Index : String) return String;
      --  Wraps Get_Variable_Value for <compiler_description> nodes
      --  and aborts KB parsing in case of improper use of indexed
      --  variables in those nodes.

      --------------
      -- Callback --
      --------------

      function Callback (Var_Name, Index : String) return String is
      begin
         if Index /= "" then
            Messages.Append
              (Message.Create
                 (Message.Error,
                  "Indexed variables only allowed in <configuration> (in "
                  & Var_Name & "(" & Index & ")",
                  Sloc => Error_Sloc));
            raise Invalid_KB;
         end if;

         begin
            return Get_Variable_Value (Comp, Var_Name);
         exception
            when Ex : Invalid_KB =>
               Messages.Append
                 (Message.Create
                    (Message.Error,
                     Ada.Exceptions.Exception_Message (Ex),
                     Sloc => Error_Sloc));
               raise Invalid_KB;
         end;
      end Callback;

      function Do_Substitute is new Substitute_Variables (Callback);

   begin
      return Do_Substitute (Str, Error_Sloc, Messages);
   end Substitute_Variables_In_Compiler_Description;

   -------------------------------------------
   -- Substitute_Variables_In_Configuration --
   -------------------------------------------

   function Substitute_Variables_In_Configuration
     (Self       : Object;
      Str        : String;
      Comps      : Compiler_Lists.List;
      Error_Sloc : Source_Reference.Object;
      Messages   : in out Log.Object) return String
   is
      function Callback (Var_Name, Index : String) return String;
      --  Wraps Get_Variable_Value for configuration> nodes
      --  and aborts configuration creation in case of improper use of indexed
      --  variables in those nodes.

      --------------
      -- Callback --
      --------------

      function Callback (Var_Name, Index : String) return String is
      begin
         if Var_Name = "GPRCONFIG_PREFIX" then
            return GPR_Executable_Prefix_Path;

         elsif Index = "" then

            if Var_Name = "TARGET"
              and then not Comps.Is_Empty
            then
               --  Can have an optional language index.
               --  If there is no index, all compilers share the same target,
               --  so just take that of the first compiler in the list

               return
                 String
                   (Self.Normalized_Target
                      (Name_Type (To_String (Comps.First_Element.Target))));

            else
               Messages.Append
                 (Message.Create
                    (Message.Error,
                     "Ambiguous variable substitution, need to specify the"
                     & " language (in " & Var_Name & ")",
                     Sloc => Error_Sloc));
               raise Invalid_KB;
            end if;

         else
            declare
               Lang : constant Language_Id := +Name_Type (Index);
            begin
               for Comp of Comps loop
                  if Comp.Selected
                    and then Comp.Language = Lang
                  then
                     begin
                        return Get_Variable_Value (Comp, Var_Name);
                     exception
                        when Ex : Invalid_KB =>
                           Messages.Append
                             (Message.Create
                                (Message.Error,
                                 Ada.Exceptions.Exception_Message (Ex),
                                 Sloc => Error_Sloc));
                           raise Invalid_KB;
                     end;
                  end if;
               end loop;
            end;
         end if;

         return "";
      end Callback;

      function Do_Substitute is new Substitute_Variables (Callback);
   begin
      return Do_Substitute (Str, Error_Sloc, Messages);
   end Substitute_Variables_In_Configuration;
   ---------------
   -- To_String --
   ---------------

   function To_String (Comp : Compiler) return String is
      function Get_String_Or_Empty (S : Unbounded_String) return String is
        (if S = Null_Unbounded_String then "" else To_String (S));

   begin
      return String (Name (Comp.Language))
           & ',' & Get_String_Or_Empty (Comp.Version)
           & ',' & Get_String_Or_Empty (Comp.Runtime)
           & ',' & (if Comp.Path.Is_Defined then Comp.Path.Value else "")
           & ',' & Get_String_Or_Empty (Comp.Name);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Compilers : Compiler_Lists.List) return String
   is
      use Compiler_Lists;
      Comp   : Compiler_Lists.Cursor := First (Compilers);
      Result : Unbounded_String;
   begin
      while Has_Element (Comp) loop
         if Compiler_Lists.Element (Comp).Selected then
            Append (Result, To_String (Compiler_Lists.Element (Comp)));
            Append (Result, ASCII.LF);
         end if;

         Next (Comp);
      end loop;
      return To_String (Result);
   end To_String;

   ----------------------------------
   -- Update_With_Compiler_Runtime --
   ----------------------------------

   procedure Update_With_Compiler_Runtime
     (Self : in out Object; Comp : Compiler) is
   begin
      if Comp.Selected
        and then Comp.Runtime_Dir /= Null_Unbounded_String
      then
         declare
            RTS  : constant String := To_String (Comp.Runtime_Dir);
            Last : Natural := RTS'Last;
         begin
            if RTS (Last) = '/' or else
              RTS (Last) = GNAT.OS_Lib.Directory_Separator
            then
               Last := Last - 1;
            end if;

            if Last - RTS'First > 6 and then
              RTS (Last - 5 .. Last) = "adalib" and then
              (RTS (Last - 6) = GNAT.OS_Lib.Directory_Separator or else
                    (RTS (Last - 6) = '/'))
            then
               Last := Last - 6;
            else
               Last := RTS'Last;
            end if;

            if GNAT.OS_Lib.Is_Directory (RTS (RTS'First .. Last)) then
               GNATCOLL.Traces.Trace
                 (Main_Trace,
                  "Parsing runtime-specific KB chunks at "
                  & RTS (RTS'First .. Last));
               Self.Add
                 (Default_Flags,
                  GPR2.Path_Name.Create_Directory
                    (Filename_Type (RTS (RTS'First .. Last))));
            end if;

         end;
      end if;
   end Update_With_Compiler_Runtime;

end GPR2.KB;
