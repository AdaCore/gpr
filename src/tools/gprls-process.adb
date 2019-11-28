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
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR.Err;
with GPR.Scans;
with GPR.Sinput;
with GPR.Snames;

with GPR2.ALI.Definition;
with GPR2.ALI.Dependency;
with GPR2.ALI.Unit;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Configuration;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Source;
with GPR2.Unit.Set;
with GPR2.Version;

with GPRtools.Util;

with GPRls.Common;
with GPRls.Options;

procedure GPRls.Process (Opt : GPRls.Options.Object) is

   use Ada;

   use GPR2;
   use GPR2.ALI;
   use GPR2.Project.Source.Set;

   use GPRls.Common;
   use GPRls.Options;

   use GPRtools;
   use GPRtools.Util;

   Tree : Project.Tree.Object renames Opt.Tree.all;

   procedure Display_Paths;

   function Is_Ada_Predefined_File_Name (Fname : String) return Boolean;

   procedure Put (Str : String; Lvl : Verbosity_Level);
   pragma Unreferenced (Put);
   --  Call Ada.Text_IO.Put (Str) if Opt.Verbosity is at least Lvl

   procedure Put_Line (Str : String; Lvl : Verbosity_Level);
   pragma Unreferenced (Put_Line);
   --  Call Ada.Text_IO.Put_Line (Str) if Opt.Verbosity is at least Lvl

   procedure Show_Tree_Load_Errors;
   --  Print errors/warnings following a project tree load

   -------------------
   -- Display_Paths --
   -------------------

   procedure Display_Paths is
   begin
      Text_IO.New_Line;
      Version.Display ("GPRLS", "2018", Version_String => Version.Long_Value);

      declare
         Src_Path : Path_Name.Set.Object;
         Obj_Path : Path_Name.Set.Object;

      begin
         --  Source search path

         for V of Tree loop
            if V.Kind not in K_Aggregate | K_Abstract then
               for D of V.Source_Directories.Values loop
                  Src_Path.Append
                    (Path_Name.Create_Directory (Optional_Name_Type (D.Text)));
               end loop;
            end if;
         end loop;

         for D of Tree.Runtime_Project.Source_Directories.Values loop
            Src_Path.Append
              (Path_Name.Create_Directory (Optional_Name_Type (D.Text)));
         end loop;

         Text_IO.New_Line;
         Text_IO.Put_Line ("Source Search Path:");

         for P of Src_Path loop
            Text_IO.Put_Line ("   " & P.Value);
         end loop;

         --  Object search path

         for V of Tree loop
            if V.Kind in K_Standard | K_Library | K_Aggregate_Library then
               Obj_Path.Append (V.Object_Directory);
            end if;
         end loop;

         Obj_Path.Append (Tree.Runtime_Project.Object_Directory);

         Text_IO.New_Line;
         Text_IO.Put_Line ("Object Search Path:");

         for P of Obj_Path loop
            Text_IO.Put_Line ("   " & P.Value);
         end loop;

         --  Project search path

         Text_IO.New_Line;
         Text_IO.Put_Line ("Project Search Path:");

         Text_IO.Put_Line ("   " & "<Current_Directory>");

         for P of Tree.Project_Search_Paths loop
            Text_IO.Put_Line ("   " & P.Value);
         end loop;

         Text_IO.New_Line;
      end;
   end Display_Paths;

   ---------------------------------
   -- Is_Ada_Predefined_File_Name --
   ---------------------------------

   function Is_Ada_Predefined_File_Name (Fname : String) return Boolean
   is
      subtype Str8 is String (1 .. 8);

      Predef_Names : constant array (1 .. 12) of Str8 :=
        ("ada     ",       -- Ada
         "interfac",       -- Interfaces
         "system  ",       -- System
         "gnat    ",       -- GNAT
         "calendar",       -- Calendar
         "machcode",       -- Machine_Code
         "unchconv",       -- Unchecked_Conversion
         "unchdeal",       -- Unchecked_Deallocation
         "directio",       -- Direct_IO
         "ioexcept",       -- IO_Exceptions
         "sequenio",       -- Sequential_IO
         "text_io ");      -- Text_IO

      Name_Len      : Integer := Fname'Length;
      Name_Buffer   : constant String (1 .. Name_Len) := Fname;
   begin

      --  Remove extension (.ads/.adb) if present

      if Name_Len > 4 and then Name_Buffer (Name_Len - 3) = '.' then
         Name_Len := Name_Len - 4;
      end if;

      --  Definitely predefined if prefix is a- i- or s- followed by letter

      if Name_Len >=  3
        and then Name_Buffer (2) = '-'
        and then (Name_Buffer (1) = 'a'
                    or else
                  Name_Buffer (1) = 'g'
                    or else
                  Name_Buffer (1) = 'i'
                    or else
                  Name_Buffer (1) = 's')
        and then (Name_Buffer (3) in 'a' .. 'z'
                    or else
                  Name_Buffer (3) in 'A' .. 'Z')
      then
         return True;

      --  Definitely false if longer than 12 characters (8.3)

      elsif Name_Len > 8 then
         return False;
      end if;

      --  Otherwise check against special list, first padding to 8 characters

      declare
         Name_Buffer_8 : constant Str8 := Strings.Fixed.Head
           (Name_Buffer (1 .. Name_Len), 8);
      begin
         Name_Len := 8;

         for J in Predef_Names'Range loop
            if Name_Buffer_8 (1 .. 8) = Predef_Names (J) then
               return True;
            end if;
         end loop;

         return False;
      end;
   end Is_Ada_Predefined_File_Name;

   ---------
   -- Put --
   ---------

   procedure Put (Str : String; Lvl : Verbosity_Level) is
   begin
      if Opt.Verbosity >= Lvl then
         Text_IO.Put (Str);
      end if;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Str : String; Lvl : Verbosity_Level) is
   begin
      if Opt.Verbosity >= Lvl then
         Text_IO.Put_Line (Str);
      end if;
   end Put_Line;

   ---------------------------
   -- Show_Tree_Load_Errors --
   ---------------------------

   procedure Show_Tree_Load_Errors is
   begin
      for C in Tree.Log_Messages.Iterate
        (False, False, True, True, True)
      loop
         Text_IO.Put_Line (Log.Element (C).Format);
      end loop;

      Finish_Program (E_Errors,
                      "unable to process project file " &
                        String (Opt.Project_File.Name));
   end Show_Tree_Load_Errors;

begin
   --  Load the project (if defined) and its configuration

   Tree.Load_Autoconf
     (Filename          => Opt.Project_File,
      Context           => Opt.Project_Context,
      Absent_Dir_Error  => True,
      Target            => Opt.Get_Target,
      Language_Runtimes => Opt.RTS_Map);

   --  The configuration step could fail because the language list has been
   --  set to empty ("for Languages use ()"), in this case just exit.
   --  In other cases this is an error.

   if not Tree.Has_Configuration then
      if Tree.Root_Project.Has_Languages then
         Show_Tree_Load_Errors;
      else
         Finish_Program (E_Success);
      end if;
   end if;

   if Opt.Only_Display_Paths then
      --  For the "gprls -v" usage

      Display_Paths;
      Finish_Program (E_Success);
   end if;

   --  Make sure the sources are up to date

   Tree.Update_Sources;

   --
   --  Main processing
   --

   declare
      use type Project.View.Object;

      --  Cache some data to speed up later processing.
      --  The maps should have Value_Path keys to support case-insensitive FS.

      package View_List_Package is new Ada.Containers.Indefinite_Vectors
        (Index_Type   => Ada.Containers.Count_Type,
         Element_Type => Project.View.Object);

      All_Views : View_List_Package.Vector;

      use type Project.Source.Object;

      package Source_Vector_Package is new Ada.Containers.Vectors
        (Positive, Project.Source.Object);

      package Src_Simple_Names_Package is new
        Ada.Containers.Indefinite_Ordered_Maps (String, Positive);

      package Dep_Simple_Names_Package is new
        Ada.Containers.Indefinite_Ordered_Maps (String, Positive);

      package Dep_Base_Names_Package is new
        Ada.Containers.Indefinite_Ordered_Maps (String, Positive);

      package Obj_Simple_Names_Package is new
        Ada.Containers.Indefinite_Ordered_Maps (String, Positive);

      All_Sources      : Source_Vector_Package.Vector;
      Src_Simple_Names : Src_Simple_Names_Package.Map;
      Dep_Simple_Names : Dep_Simple_Names_Package.Map;
      Dep_Base_Names   : Dep_Base_Names_Package.Map;
      Obj_Simple_Names : Obj_Simple_Names_Package.Map;

      type File_Status is
        (OK,                  --  matching timestamp
         Checksum_OK,         --  only matching checksum
         Not_Same);           --  neither checksum nor timestamp matching

      No_Obj : constant String := "<no_obj>";

      Sources : Project.Source.Set.Object;
      --  The sources that we will browse. This set may be:
      --     - All the project sources when not in closure mode, possibly from
      --       the full project tree if All_Projects is True
      --     - The sources associated with the files given on the CL
      --     - In closure mode and no file given on the CL, the root project's
      --       main sources.

      function Checksum (Source : Path_Name.Object) return Word;

      procedure Display_Closures;

      procedure Display_Normal;

      function Source_For
        (File                 : String;
         File_May_Be_Artifact : Boolean) return Project.Source.Object;
      --  Given a file basename and a project view, tries to find a
      --  compilable source from this view that is associated with the
      --  file.

      --------------
      -- Checksum --
      --------------

      function Checksum (Source : Path_Name.Object) return Word is
         use GPR;
         use type Scans.Token_Type;

         Source_Index : constant Source_File_Index :=
                          Sinput.Load_File (Source.Value);

      begin
         Err.Scanner.Initialize_Scanner (Source_Index, Err.Scanner.Ada);

         loop
            Err.Scanner.Scan;
            exit when Scans.Token = Scans.Tok_EOF;
         end loop;

         return GPR2.Word (Scans.Checksum);
      end Checksum;

      ----------------------
      -- Display_Closures --
      ----------------------

      procedure Display_Closures is

         type Closure_Status_Type is (Success, Incomplete_Closure);

         Processed_ALI : Path_Name.Set.Object;
         Closures      : Project.Source.Set.Object;
         Status        : Closure_Status_Type;

         procedure Compute_Closure (Source : Project.Source.Object);
         --  Equivalent to the legacy GPR.Util.Get_Closures.Process:
         --
         --  - Add Source to Result. Return if already present.
         --
         --  - Let A be the path to the ALI file for Source.
         --    Check that A is not in Processed_ALI, otherwise return.
         --
         --  - Try to read A. If it doesn't exist or the read fails, set status
         --    to Incomplete_Closure and return.
         --
         --  - Read the 'U' sections (2 at most).
         --    For each 'U' section related to <unit>:
         --
         --    - if it is the first one, then:
         --      - call Recurse_On_Sources_For_Unit on <unit>,
         --      - if it is a body, i.e. the line looks like "U <unit>%b [...]"
         --        then loop over the 'D' lines and add to Closures any subunit
         --        of <unit>.
         --
         --    - for every 'W' (also including 'Z') line of the section, call
         --      Recurse_On_Sources_For_Unit on the "withed" unit.

         ---------------------
         -- Compute_Closure --
         ---------------------

         procedure Compute_Closure (Source : Project.Source.Object) is

            procedure Recurse_On_Sources_For_Unit (Unit : Name_Type);
            --  Calls Compute_Closure on every source defining Unit.
            --  If All_Projects is False, nothing is done for sources that do
            --  not belong to the root project.

            procedure Recurse_On_Sources_For_Unit (Unit : Name_Type) is
               Unit_Obj : GPR2.Unit.Object;

            begin
               for V of All_Views loop
                  Unit_Obj := V.Unit (Unit, Need_Update => False);

                  if Unit_Obj.Is_Defined then
                     Unit_Obj := V.Units (Need_Update => False).Element (Unit);
                     exit;
                  end if;
               end loop;

               if not Unit_Obj.Is_Defined then
                  return;
               end if;

               if Unit_Obj.Has_Spec then
                  Compute_Closure (Unit_Obj.Spec);
               end if;

               if Unit_Obj.Has_Body then
                  Compute_Closure (Unit_Obj.Main_Body);
               end if;

               for B of Unit_Obj.Separates loop
                  Compute_Closure (B);
               end loop;
            end Recurse_On_Sources_For_Unit;

            ALI_File   : Path_Name.Object;
            ALI_Object : Definition.Object;

         begin
            if Closures.Contains (Source) then
               return;
            end if;

            Closures.Insert (Source);

            if Source.Source.Kind = GPR2.S_Separate then
               return;  --  No ALI file for a separate
            end if;

            ALI_File := Source.Artifacts.Dependency;

            if Processed_ALI.Contains (ALI_File) then
               return;
            end if;

            if not ALI_File.Exists then
               Status := Incomplete_Closure;
               return;
            end if;

            ALI_Object := Definition.Scan_ALI (ALI_File, Tree.Log_Messages);

            if not ALI_Object.Is_Defined then
               Output_Messages (Opt);
               Finish_Program (E_Errors, "unable to scan ALI file: "
                               & ALI_File.Value);
            end if;

            Processed_ALI.Append (ALI_File);

            declare
               First : Boolean := True;

               function Is_Subunit_Of
                 (U1 : Name_Type; U2 : Name_Type) return Boolean;
               --  Compares the unit names U1 and U2 to decide if U1 is a
               --  subunit of U2.

               function Is_Subunit_Of
                 (U1 : Name_Type; U2 : Name_Type) return Boolean is
                 (U1'Length >= U2'Length + 2 and then
                  U1 (U1'First .. U1'First + U2'Length - 1) = U2 and then
                  U1 (U1'First + U2'Length) = '.');

               use ALI.Unit;

            begin
               for U_Sec of ALI_Object.Units loop
                  if First then
                     First := False;
                     Recurse_On_Sources_For_Unit (U_Sec.Uname);

                     --  For a body, check if there are subunits and add the
                     --  corresponding sources to Closures.

                     if U_Sec.Utype in Is_Body | Is_Body_Only then
                        for Dep of ALI_Object.Sdeps loop
                           if not Dep.Is_Configuration
                             and then Is_Subunit_Of
                                        (Dep.Unit_Name, U_Sec.Uname)
                           then
                              Recurse_On_Sources_For_Unit (Dep.Unit_Name);
                           end if;
                        end loop;
                     end if;
                  end if;

                  for W of U_Sec.Withs loop
                     Recurse_On_Sources_For_Unit (W.Uname);
                  end loop;
               end loop;
            end;
         end Compute_Closure;

         use type Ada.Containers.Count_Type;

      begin
         if Sources.Is_Empty then
            Finish_Program (E_Errors, "no main specified for closure");
         end if;

         Status := Success;

         for S of Sources loop
            Compute_Closure (S);
         end loop;

         Text_IO.New_Line;

         if Status = Incomplete_Closure then
            if Sources.Length = 1 then
               Text_IO.Put_Line ("Incomplete closure:");
            else
               Text_IO.Put_Line ("Incomplete closures:");
            end if;

         elsif Status = Success then
            if Sources.Length = 1 then
               Text_IO.Put_Line ("Closure:");
            else
               Text_IO.Put_Line ("Closures:");
            end if;

         else
            Finish_Program (E_Errors, "unable to get closures: " & Status'Img);
         end if;

         Text_IO.New_Line;

         declare
            Output : String_Vector.Vector;
            package String_Sorting is new String_Vector.Generic_Sorting;

         begin
            for R of Closures loop
               Output.Append ("  " & R.Source.Path_Name.Value);
            end loop;

            String_Sorting.Sort (Output);

            for O of Output loop
               Text_IO.Put_Line (O);
            end loop;
         end;

         Text_IO.New_Line;
      end Display_Closures;

      --------------------
      -- Display_Normal --
      --------------------

      procedure Display_Normal is

         procedure Output_Source
           (S : Project.Source.Object; ALI_Obj : Definition.Object);

         -------------------
         -- Output_Source --
         -------------------

         procedure Output_Source
           (S : Project.Source.Object; ALI_Obj : Definition.Object)
         is
            use type Ada.Calendar.Time;

            D : constant Dependency.Object := ALI_Obj.Dep_For
              (S.Source.Path_Name.Simple_Name);

            Status : File_Status;

         begin
            pragma Assert (D.Is_Defined);

            --  For now we stick to the timestamp-based logic: if time stamps
            --  are equal, assume the file didn't change.

            if D.Stamp = S.Source.Time_Stamp then
               Status := OK;

            elsif D.Checksum = Checksum (S.Source.Path_Name) then
               Status := Checksum_OK;

            else
               Status := Not_Same;
            end if;

            if Opt.Verbose then
               Text_IO.Put ("     Source => ");
               Text_IO.Put (S.Source.Path_Name.Value);

               case Status is
                  when OK =>
                     Text_IO.Put (" unchanged");

                  when Checksum_OK =>
                     Text_IO.Put (" slightly modified");

                  when Not_Same =>
                     Text_IO.Put (" modified");
               end case;

            else
               if not Opt.Selective_Output then
                  Text_IO.Put ("    ");

                  case Status is
                  when OK =>
                     Text_IO.Put ("  OK ");

                  when Checksum_OK =>
                     Text_IO.Put (" MOK ");

                  when Not_Same =>
                     Text_IO.Put (" DIF ");
                  end case;
               end if;

               Text_IO.Put (S.Source.Path_Name.Value);
            end if;

            Text_IO.New_Line;
         end Output_Source;

      begin
         --  Unfortunately, initialization of the GPR hash-consing of strings
         --  is still needed for the GPR tokenizer that we're using for now to
         --  compute source checksums.

         GPR.Snames.Initialize;

         for S of Sources loop
            declare
               use GPR2.ALI.Unit;

               ALI_File     : Path_Name.Object;
               ALI_Object   : Definition.Object;
               Obj_File     : Path_Name.Object;
               U_Sec_Source : Project.Source.Object;
               Dep_Source   : Project.Source.Object;
               U_Flags      : Flag_Array;
               Artifacts    : constant Project.Source.Artifact.Object :=
                                S.Artifacts;

            begin
               ALI_File := Artifacts.Dependency;
               Obj_File := Artifacts.Object_Code;

               if ALI_File.Is_Defined and then ALI_File.Exists then
                  ALI_Object := Definition.Scan_ALI
                                  (ALI_File, Tree.Log_Messages);

                  if not ALI_Object.Is_Defined then
                     Output_Messages (Opt);
                     Finish_Program
                       (E_Errors,
                        "unable to scan ALI file: " & ALI_File.Value);
                  end if;

                  if Opt.Print_Object_Files then
                     if Obj_File.Is_Defined and then Obj_File.Exists then
                        Text_IO.Put_Line (Obj_File.Value);
                     else
                        Text_IO.Put_Line (No_Obj);
                     end if;
                  end if;

                  for U_Sec of ALI_Object.Units loop
                     if Opt.Print_Units then
                        if Opt.Verbose then
                           Text_IO.Put_Line ("   Unit =>");
                           Text_IO.Put ("     Name   => ");
                           Text_IO.Put (String (U_Sec.Uname));
                           Text_IO.New_Line;

                           Text_IO.Put_Line
                             ("     Kind   => "
                              & (case U_Sec.Kind is
                                   when Kind_Package => "package",
                                   when Kind_Subprogram => "subprogram")
                              & ' '
                              & (case U_Sec.Utype is
                                   when Is_Spec | Is_Spec_Only => "spec",
                                   when others                 => "body"));

                           U_Flags := U_Sec.Flags;
                           if U_Flags /= Flag_Array'(others => False) then
                              Text_IO.Put ("     Flags  =>");
                              for Flag in U_Flags'Range loop
                                 if U_Flags (Flag) then
                                    Text_IO.Put (' ' & Image (Flag));
                                 end if;
                              end loop;
                              Text_IO.New_Line;
                           end if;

                        else
                           Text_IO.Put_Line ("   " & String (U_Sec.Uname));
                        end if;
                     end if;

                     if Opt.Print_Sources and then not Opt.Dependency_Mode
                     then
                        U_Sec_Source := Source_For
                          (File                 => String (U_Sec.Sfile),
                           File_May_Be_Artifact => False);

                        if U_Sec_Source.Is_Defined then
                           Output_Source (S       => U_Sec_Source,
                                          ALI_Obj => ALI_Object);
                        end if;
                     end if;

                     --  In non-verbose mode, only print the first U section
                     --  (i.e. for a body, do not go on to show the
                     --  dependencies of the corresponding spec).

                     exit when not Opt.Verbose;
                  end loop;

                  if Opt.Dependency_Mode and then Opt.Print_Sources then
                     if Opt.Verbose then
                        Text_IO.Put_Line ("   depends upon");
                     end if;

                     for D of ALI_Object.Sdeps loop
                        if Opt.With_Predefined_Units and then
                          Is_Ada_Predefined_File_Name (String (D.Sfile))
                        then
                           Dep_Source := Source_For
                             (File                 => String (D.Sfile),
                              File_May_Be_Artifact => False);
                        else
                           Dep_Source := Source_For
                             (File                 => String (D.Sfile),
                              File_May_Be_Artifact => False);
                        end if;

                        if Dep_Source.Is_Defined then
                           Text_IO.Put ("   ");
                           Output_Source (S       => Dep_Source,
                                          ALI_Obj => ALI_Object);

                        end if;
                     end loop;
                  end if;
               end if;
            end;
         end loop;
      end Display_Normal;

      ----------------
      -- Source_For --
      ----------------

      function Source_For
        (File                 : String;
         File_May_Be_Artifact : Boolean) return Project.Source.Object
      is
      begin
         if Src_Simple_Names.Contains (File) then
            return All_Sources (Src_Simple_Names (File));

         elsif File_May_Be_Artifact then
            if Dep_Simple_Names.Contains (File) then
               return All_Sources (Dep_Simple_Names (File));
            elsif Dep_Base_Names.Contains (File) then
               return All_Sources (Dep_Base_Names (File));
            elsif Obj_Simple_Names.Contains (File) then
               return All_Sources (Obj_Simple_Names (File));
            end if;
         end if;

         return Project.Source.Undefined;
      end Source_For;

   begin
      if Opt.Verbose then
         Display_Paths;
      end if;

      --  Fill the various caches

      for V of Tree loop
         All_Views.Append (V);

         for S of V.Sources (Need_Update => False) loop
            All_Sources.Append (S);

            Src_Simple_Names.Include
              (String (S.Source.Path_Name.Simple_Name),
               Positive (All_Sources.Length));

            --  For artifact -> source caches, use the Include method as we
            --  will have duplicates (e.g. body and spec have both the same
            --  .ali file).

            declare
               Artifacts : constant Project.Source.Artifact.Object :=
                             S.Artifacts;
            begin
               if Artifacts.Has_Dependency then
                  Dep_Simple_Names.Include
                    (String (Artifacts.Dependency.Simple_Name),
                     Positive (All_Sources.Length));
                  Dep_Base_Names.Include
                    (String (Artifacts.Dependency.Base_Name),
                     Positive (All_Sources.Length));
               end if;

               if Artifacts.Has_Object_Code then
                  Obj_Simple_Names.Include
                    (String (Artifacts.Object_Code.Simple_Name),
                     Positive (All_Sources.Length));
               end if;
            end;
         end loop;
      end loop;

      --
      --  All along, we will exclude non-ada sources.
      --

      --  Fill the Sources set with the files given on the CL.
      --  Print "Can't find source for ..." if a file can't be matched with a
      --  compilable source from the root project (or from the project tree if
      --  All_Projects is set).

      for F of Opt.Files loop
         declare
            Source : constant Project.Source.Object :=
                       Source_For
                         (File                 => F,
                          File_May_Be_Artifact => True);
         begin
            if not Source.Is_Defined then
               Text_IO.Put_Line ("Can't find source for " & F);

            elsif Source.Source.Language = Name_Type (Ada_Lang) then
               Sources.Insert (Source);
            end if;
         end;
      end loop;

      --  If none was provided, then:
      --     - Either we're in closure mode, and we want to use the mains from
      --       the root project.
      --     - Or we're not, and we will use all the compilable sources (from
      --       the root project or the entire tree, depending on All_Sources).

      if Opt.Files.Is_Empty then
         if Opt.Closure_Mode then
            for S of Tree.Root_Project.Sources (Need_Update => False) loop
               if Tree.Root_Project.Has_Mains and then
                 S.Is_Main and then
                 S.Source.Language = Name_Type (Ada_Lang)
               then
                  Sources.Insert (S);
               end if;
            end loop;

         else
            if Opt.All_Projects then
               for V of All_Views loop
                  for S_Cur in V.Sources (Need_Update => False).Iterate
                    (Filter => S_Compilable)
                  loop
                     if Element (S_Cur).Source.Language = Name_Type (Ada_Lang)
                     then
                        Sources.Insert (Element (S_Cur));
                     end if;
                  end loop;
               end loop;

            else
               for S_Cur in Tree.Root_Project.Sources
                 (Need_Update => False).Iterate (Filter => S_Compilable)
               loop
                  if Element (S_Cur).Source.Language = Name_Type (Ada_Lang)
                  then
                     Sources.Insert (Element (S_Cur));
                  end if;
               end loop;
            end if;
         end if;
      end if;

      --  Do nothing if no source was found

      if Sources.Is_Empty then
         Finish_Program (E_Success);
      end if;

      --  Check all sources and notify when no ALI file is present

      for S of Sources loop
         if not S.Artifacts.Dependency.Exists then
            Text_IO.Put_Line ("Can't find ALI file for "
                              & S.Source.Path_Name.Value);
         end if;
      end loop;

      --  We gathered all the sources:
      --  Process them according to the chosen mode.

      if Opt.Closure_Mode then
         Display_Closures;

      else
         Display_Normal;
         --  List the project sources (or the subset given in the CL) that have
         --  compilation artifacts (.o/.ali) i.e. only the bodies.
         --
         --  The options -o, -u, -s are used to select specific information to
         --  print.
         --
         --  With -d, for every item listed (in non-closure mode) we also
         --  develop the dependencies (D lines of ALI) with their status.
      end if;
   end;

exception
   when Project_Error | Processing_Error =>
      Show_Tree_Load_Errors;
end GPRls.Process;
