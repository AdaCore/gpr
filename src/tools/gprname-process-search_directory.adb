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

with Ada.Text_IO;

with GNAT.Directory_Operations;
with GNAT.Expect;
with GNAT.OS_Lib;
with GNAT.Regexp;
with GNAT.Regpat;

with GNATCOLL.Strings;

with GPRname.Unit;

separate (GPRname.Process)
procedure Search_Directory
  (Dir_Path         : Path_Name.Object;
   Sect             : Section.Object;
   Processed_Dirs   : in out Path_Name_Set.Set;
   Recursively      : Boolean;
   Compiler_Path    : GPR2.Path_Name.Object;
   Compiler_Args    : GNAT.OS_Lib.Argument_List_Access;
   Lang_Sources_Map : in out Language_Sources_Map.Map;
   Source_Names     : in out String_Set.Set)
is

   use GPRname.Unit;

   procedure Update_Lang_Sources_Map
     (Map : in out Language_Sources_Map.Map;
      Src : Source.Object);
   --  Add Src to Map, using Src.Lang as the key

   -----------------------------
   -- Update_Lang_Sources_Map --
   -----------------------------

   procedure Update_Lang_Sources_Map
     (Map : in out Language_Sources_Map.Map;
      Src : Source.Object)
   is
      Is_New_Key : Boolean;
      Position   : Language_Sources_Map.Cursor;
   begin
      Map.Insert (Src.Language, Source.Set.Empty_Set, Position, Is_New_Key);
      Map (Position).Insert (Src);
   end Update_Lang_Sources_Map;

   type Matched_Type is (Match, No_Match, Excluded);
   --  Type to represent whether a source file matches a regexp, doesn't, or
   --  matches an excluded pattern.

   Compiler_Output_Regexp : constant String :=
                              "^Unit (\S+) \((spec|body)\) line (\d+), "
                              & "file offset (\d+), file name (\S+)\r?$";
   --  Regexp used to parse the Ada compiler's output

   Matcher   : constant Regpat.Pattern_Matcher :=
                 Regpat.Compile (Compiler_Output_Regexp);

   Lang_Excl : Language_Set.Set;
   Matched   : Matched_Type := No_Match;
   Str       : String (1 .. 2_000);
   Last      : Natural;
   Dir       : Directory_Operations.Dir_Type;
   File      : GPR2.Path_Name.Object;
   Processed : Boolean := False;

begin
   if Processed_Dirs.Contains (Dir_Path) then
      Put_Line
        ("directory " & Dir_Path.Value & " already searched. skipping...",
         Low);

      if Recursively then
         Processed := True;
      else
         return;
      end if;

   else
      Processed_Dirs.Insert (Dir_Path);
   end if;

   Put_Line ("entering directory: " & Dir_Path.Value, Low);

   begin
      Directory_Operations.Open (Dir, Dir_Path.Value);
   exception
      when Directory_Operations.Directory_Error =>
         raise GPRname_Exception
           with "cannot open directory " & String (Dir_Path.Value);
   end;

   --  Loop over the directory's content

   loop
      Directory_Operations.Read (Dir, Str, Last);
      exit when Last = 0;

      --  Current entry

      File := Path_Name.Create_File
        (Name_Type (Str (1 .. Last)), Dir_Path.Name);

      Put_Line ("  checking file: " & String (File.Name), Low);

      if OS_Lib.Is_Regular_File (File.Value) and then not Processed then
         --  Regular file: check if it matches any naming schemes for Ada or
         --  other languages. Get additional info for Ada source files.

         Lang_Excl.Clear;

         --  Try every pattern of the section, starting from the rightmost one

         Patt_Loop : for Patt_Lang of reverse Sect.Patterns loop

            --  If we have already matched a file with the same basename,
            --  either the "ignore duplicate files" option is set and we skip
            --  the file, or we carry on and just emit a warning.

            if Source_Names.Contains (String (File.Simple_Name)) then
               if Opt.Ignore_Duplicate_Files then
                  exit Patt_Loop;

               else
                  --  ??? We should use a proper logging system.
                  --  This warning is not part of the verbose output and should
                  --  appear after all the directory processing.

                  Text_IO.Put_Line
                    ("warning: duplicate file " & String (File.Simple_Name));
               end if;
            end if;

            Matched := Match;

            declare
               use GNAT.Regexp;

               Lang   : constant Language_Type := Patt_Lang.Language;
               Regexp : constant GNAT.Regexp.Regexp := Patt_Lang.Regexp;

            begin
               Put_Line
                 ("    trying pattern '" & Patt_Lang.Pattern
                  & "' for language " & String (Lang), Low);

               --  Check if we match (or have matched in a previous
               --  iteration) with an excluded pattern for language Lang.
               --  In either cases we should skip this iteration.

               if Lang_Excl.Contains (Lang) then
                  Matched := Excluded;
                  Put_Line ("      -> already excluded", Low);

               elsif Sect.Excluded_Patterns.Contains (Lang) then
                  for Excl_Patt of Excluded_Patterns (Sect) (Lang) loop
                     Put_Line
                       ("      trying excluded pattern '"
                        & Excl_Patt.Pattern & "'", Low);

                     if Match
                       (String (File.Simple_Name), Excl_Patt.Regexp)
                     then
                        Put_Line ("      -> excluded", Low);
                        Lang_Excl.Insert (Lang);
                        Matched := Excluded;
                        exit;
                     end if;
                  end loop;
               end if;

               --  If not, try to match with the pattern

               if Matched = Match then
                  Matched := No_Match;

                  if Match (String (File.Simple_Name), Regexp) then
                     Matched := Match;
                     Put_Line ("      -> match", Low);
                     Source_Names.Include (String (File.Simple_Name));
                  end if;
               end if;

               --  If we got a match and the language is Ada, call the
               --  compiler to get info about the source. For other languages
               --  just register the source as non-unit-based.

               if Matched = Match then
                  if Lang = Ada_Lang then
                     Put_Line ("      calling gcc for " & File.Value, Low);

                     Compiler_Args (Compiler_Args'Last) :=
                       new String'(File.Value);

                     Put ("      " & Compiler_Path.Value, Low);

                     for A of Compiler_Args.all loop
                        Put (" " & A.all, Low);
                     end loop;

                     Put_Line ("", Low);

                     declare
                        use GNATCOLL.Strings;
                        use GNAT.Regpat;

                        Status : aliased Integer;

                        --  Call the compiler and store the output

                        Compiler_Output : constant String :=
                                            (Expect.Get_Command_Output
                                               (Compiler_Path.Value,
                                                Compiler_Args.all,
                                                Input      => "",
                                                Status     => Status'Access,
                                                Err_To_Out => True));

                        --  Use GNATCOLL XString for line splitting

                        Lines : constant XString_Array :=
                                  Split
                                    (To_XString (Compiler_Output), ASCII.LF);

                        Is_Multi_Unit : constant Boolean := Lines'Length > 1;

                        ------------
                        -- Substr --
                        ------------

                        function Substr
                          (Input : XString;
                           Loc   : Match_Location) return String
                        is
                          (To_String (Input) (Loc.First .. Loc.Last))
                            with Pre => Loc /= GNAT.Regpat.No_Match;
                        --  Return the substring of Input
                        --  from Loc.First to Loc.Last.

                        Matches : Match_Array (0 .. 5);

                        Src : Source.Object :=
                                Source.Create
                                  (File       => File,
                                   Language   => Ada_Lang,
                                   Unit_Based => True);

                        Unit_Count : Natural := 0;

                     begin
                        GNAT.OS_Lib.Free (Compiler_Args (Compiler_Args'Last));

                        if Status /= 0 then
                           raise GPRname_Exception with "compiler failed";
                        end if;

                        --  Parse every line output by the compiler
                        --  (one line per unit in the source)

                        for Line of Lines loop
                           Match (Matcher, To_String (Line), Matches);

                           if Matches (0) = GNAT.Regpat.No_Match then
                              raise GPRname_Exception
                                with "unexpected compiler output";
                           end if;

                           declare
                              Name : constant Name_Type :=
                                       Name_Type (Substr (Line, Matches (1)));

                              Kind : constant Unit_Kind :=
                                       (if Substr (Line, Matches (2)) = "spec"
                                        then K_Spec else K_Body);

                              Index_In_Source : constant Natural :=
                                                  (if Is_Multi_Unit
                                                   then Unit_Count else 0);
                           begin
                              Put_Line
                                ("      found unit: " & To_String (Line), Low);

                              --  Add the unit to the source, unless it is a
                              --  predefined Ada unit and the related "ignore"
                              --  option is set.

                              if not (Opt.Ignore_Predefined_Units
                                        and then
                                      Is_Ada_Predefined_Unit (String (Name)))
                              then
                                 Unit_Count := Unit_Count + 1;
                                 Src.Append_Unit
                                   (Create (Name, Kind, Index_In_Source));
                              else
                                 Put_Line
                                   ("        -> predefined unit:"
                                    & " ignored", Low);
                              end if;
                           end;
                        end loop;

                        --  Unit_Count could be zero here, if we got only
                        --  predefined units and skipped them.

                        if Unit_Count > 0 then
                           Update_Lang_Sources_Map (Lang_Sources_Map, Src);
                        end if;
                     end;

                  else
                     --  Add non-Ada source

                     Update_Lang_Sources_Map (Lang_Sources_Map,
                                              Create (File, Lang));
                  end if;

                  --  The file is associated to a language: we are done with it

                  exit Patt_Loop;

               end if;
            end;
         end loop Patt_Loop;

      elsif Recursively
        and then GNAT.OS_Lib.Is_Directory (File.Value)
        and then Str (1 .. Last) /= "."
        and then Str (1 .. Last) /= ".."
      then
         Search_Directory
           (Path_Name.Create_Directory (Name_Type (File.Value)),
            Sect,
            Processed_Dirs,
            True,
            Compiler_Path,
            Compiler_Args,
            Lang_Sources_Map,
            Source_Names);
      end if;
   end loop;

   Directory_Operations.Close (Dir);
end Search_Directory;
