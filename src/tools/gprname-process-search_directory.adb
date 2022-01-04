------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

with GPRname.Unit.Vector;

with GPRtools.Util;

with GPR2.Unit;

separate (GPRname.Process)
procedure Search_Directory
  (Dir_Path       : Path_Name.Object;
   Sect           : Section.Object;
   Processed_Dirs : in out Path_Name_Set.Set;
   Recursively    : Boolean;
   Compiler_Args  : GNAT.OS_Lib.Argument_List_Access)
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
      Src_Idx    : Source.Object :=
                     Source.Create
                       (Path_Name.Create_File
                          (Src.File.Simple_Name, Path_Name.No_Resolution),
                        Src.Language,
                        Src.Unit_Based);
      --  Use source based only on simple name to index in Source_Names
      Src_Pos    : constant Source.Set.Cursor := Source_Names.Find (Src_Idx);
   begin
      if Source.Set.Set.Has_Element (Src_Pos) then
         if Opt.Ignore_Duplicate_Files then
            return;

         else
            declare
               use type Unit.Vector.Object;
               use type Ada.Containers.Count_Type;

               function Less_Unit (Left, Right : Unit.Object) return Boolean is
                 (Left.Name < Right.Name);

               package Sort_Units is new Unit.Vector.Vector.Generic_Sorting
                 ("<" => Less_Unit);

               function Image
                 (List : Unit.Vector.Object;
                  From : Positive := 1) return String
               is
                 ((if From > 1 then ", "
                   elsif List.Length > 1 then "(" else "")
                  & (if not List.Is_Empty then String (List (From).Name)
                     else "")
                  & (if From < List.Last_Index
                     then Image (List, From + 1)
                     elsif List.Length > 1 then ")" else ""));

               Units1 : Unit.Vector.Object;
               Units2 : Unit.Vector.Object;
               Prefix : constant String :=
                          "warning: duplicate file "
                          & String (Src.File.Simple_Name);
            begin
               if Source_Names (Src_Pos).Unit_Based then
                  Units1 := Source_Names (Src_Pos).Units;
                  Sort_Units.Sort (Units1);
               end if;

               if Src.Unit_Based then
                  Units2 := Src.Units;
                  Sort_Units.Sort (Units2);
               end if;

               --  ??? We should use a proper logging system.
               --  This warning is not part of the verbose output and should
               --  appear after all the directory processing.

               if Units1 /= Units2 then
                  Text_IO.Put_Line
                    (Prefix & " for units " & Image (Units2) & " and "
                     & Image (Units1));
                  Text_IO.Put_Line
                    ("warning: generated Naming package needs to be reviewed"
                     & " manually");

               elsif Units1.Is_Empty then
                  Text_IO.Put_Line (Prefix);

               else
                  Text_IO.Put_Line
                    (Prefix & " for unit"
                     & (if Units1.Length > 1 then "s" else "") & ' '
                     & Image (Units1) & " will be ignored");
               end if;
            end;
         end if;

      else
         if Src.Unit_Based then
            for U of Src.Units loop
               Src_Idx.Append_Unit (U);
            end loop;
         end if;
         Source_Names.Insert (Src_Idx);
      end if;

      Map.Insert (Src.Language, Source.Set.Empty_Set, Position, Is_New_Key);
      Map (Position).Insert (Src);
   end Update_Lang_Sources_Map;

   type Matched_Type is (Match, No_Match, Excluded);
   --  Type to represent whether a source file matches a regexp, doesn't, or
   --  matches an excluded pattern.

   Compiler_Output_Regexp : constant String :=
                              "^Unit (\S+) \((spec|body)\) line (\d+), "
                              & "file offset (\d+),.* file name (\S+)\r?$";
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
        (Filename_Type (Str (1 .. Last)), Dir_Path.Name);

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
                  for Excl_Patt of Sect.Excluded_Patterns.Element (Lang) loop
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

                        Unit_Index : Natural := 0;

                     begin
                        GNAT.OS_Lib.Free (Compiler_Args (Compiler_Args'Last));

                        if Status /= 0 then
                           Put_Line (Compiler_Output, None);

                        else
                           --  Parse every line output by the compiler
                           --  (one line per unit in the source)

                           for Line of Lines loop
                              Match (Matcher, To_String (Line), Matches);

                              if Matches (0) /= GNAT.Regpat.No_Match then
                                 Unit_Index := Unit_Index + 1;

                                 declare
                                    Name : constant Name_Type :=
                                             Name_Type
                                               (Substr (Line, Matches (1)));

                                    Kind : constant Unit_Kind :=
                                             (if Substr (Line, Matches (2))
                                              = "spec"
                                              then K_Spec else K_Body);

                                    Index_In_Source : constant Natural :=
                                                        (if Is_Multi_Unit
                                                         then Unit_Index
                                                         else 0);
                                 begin
                                    Put_Line
                                      ("      found unit: " & To_String (Line),
                                       Low);

                                    --  Add the unit to the source, unless it
                                    --  is a predefined Ada unit and the
                                    --  related "ignore" option is set.

                                    if not GPR2.Unit.Valid_Unit_Name
                                      (Name,
                                       On_Error => Ada.Text_IO.Put_Line'Access)
                                    then
                                       --  Ignore whong unit name

                                       null;

                                    elsif Opt.Ignore_Predefined_Units
                                      and then
                                        GPRtools.Util.Is_Ada_Predefined_Unit
                                          (Name)
                                    then
                                       Put_Line
                                         ("        -> predefined unit """
                                          & String (Name) & """ ignored", Low);
                                    else
                                       Src.Append_Unit
                                         (Create
                                            (Name, Kind, Index_In_Source));
                                    end if;
                                 end;
                              end if;
                           end loop;

                           if Src.Has_Units then
                              Update_Lang_Sources_Map (Lang_Sources_Map, Src);
                           end if;
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
        and then Str (1 .. Last) not in "." | ".."
      then
         Search_Directory
           (Path_Name.Create_Directory (Filename_Type (File.Value)),
            Sect,
            Processed_Dirs,
            True,
            Compiler_Args);
      end if;
   end loop;

   Directory_Operations.Close (Dir);
end Search_Directory;
