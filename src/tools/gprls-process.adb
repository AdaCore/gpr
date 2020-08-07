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

with Ada.Calendar;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Text_IO;

with GPR.Err;
with GPR.Scans;
with GPR.Sinput;
with GPR.Snames;

with GPR2.Unit;
with GPR2.Containers;
with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Configuration;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.Unit_Info;
with GPR2.Project.View;
with GPR2.Source;
with GPR2.Source_Info;
with GPR2.Version;

with GPRtools.Util;

with GPRls.Common;
with GPRls.Options;

procedure GPRls.Process (Opt : GPRls.Options.Object) is

   use Ada;

   use GPR2;
   use GPR2.Project.Source.Set;
   use all type GPR2.Unit.Library_Unit_Type;

   use GPRls.Common;
   use GPRls.Options;

   use GPRtools;
   use GPRtools.Util;

   Tree : Project.Tree.Object renames Opt.Tree.all;

   function "&" (Left, Right : Optional_Name_Type) return Optional_Name_Type is
     (GPR2."&" (Left, Right));
   --  Workaround for GNAT visibility issue

   procedure Display_Paths;

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

         if Tree.Has_Runtime_Project then
            for D of Tree.Runtime_Project.Source_Directories.Values loop
               Src_Path.Append
                 (Path_Name.Create_Directory (Optional_Name_Type (D.Text)));
            end loop;
         end if;

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

         if Tree.Has_Runtime_Project then
            Obj_Path.Append (Tree.Runtime_Project.Object_Directory);
         end if;

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

      Finish_Program
        (E_Errors,
         "unable to process project file " & String (Opt.Project_File.Name));
   end Show_Tree_Load_Errors;

begin
   --  Load the project (if defined) and its configuration

   Tree.Load_Autoconf
     (Filename          => Opt.Project_File,
      Project_Dir       => Opt.Project_Base,
      Context           => Opt.Project_Context,
      Absent_Dir_Error  => True,
      Target            => Opt.Get_Target,
      Language_Runtimes => Opt.RTS_Map,
      Check_Shared_Lib  => not Opt.Unchecked_Shared_Lib);

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

      use type Project.Source.Object;

      function Path_Equal (Left, Right : Project.Source.Object) return Boolean
      is (Left = Right and then Left.Path_Name.Value = Right.Path_Name.Value);

      function Path_Less (Left, Right : Project.Source.Object) return Boolean
      is (Left < Right
          or else (Left = Right
                   and then Left.Path_Name.Value < Right.Path_Name.Value));

      package Sources_By_Path is new Ada.Containers.Indefinite_Ordered_Sets
        (Project.Source.Object, "<" => Path_Less, "=" => Path_Equal);

      type File_Status is
        (OK,          --  matching timestamp
         Checksum_OK, --  only matching checksum
         Not_Same);   --  neither checksum nor timestamp matching

      No_Obj : constant String := "<no_obj>";

      Position : Sources_By_Path.Cursor;
      Inserted : Boolean;

      Remains : GPR2.Containers.Value_Set := Opt.Files;
      Sources : Sources_By_Path.Set;
      --  The sources that we will browse. This set may be:
      --     - All the project sources when not in closure mode, possibly from
      --       the full project tree if All_Projects is True
      --     - The sources associated with the files given on the CL
      --     - In closure mode and no file given on the CL, the root project's
      --       main sources.

      Full_Closure : Boolean := Opt.Closure_Mode;
      --  Reset this flag to False if closure became incomplete

      function Checksum (Source : Path_Name.Object) return Word;

      procedure Display_Closures;

      procedure Display_Normal;

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

         Closures : Project.Source.Set.Object;

         use type Ada.Containers.Count_Type;

      begin
         if Sources.Is_Empty then
            Finish_Program (E_Errors, "no main specified for closure");
         end if;

         for S of Sources loop
            declare
               Deps : constant Project.Source.Set.Object :=
                        S.Dependencies (Closure => True);
            begin
               if Deps.Is_Empty then
                  --  If no dependencies, use only this one because without ALI
                  --  file we don't know dependency even on itself.
                  Closures.Include (S);
               else
                  Closures.Union (Deps);
               end if;
            end;
         end loop;

         Text_IO.New_Line;

         declare
            Output : String_Vector.Vector;
            package String_Sorting is new String_Vector.Generic_Sorting;

         begin
            for R of Closures loop
               if not R.Source.Is_Runtime then
                  if not R.Artifacts.Has_Dependency then
                     Full_Closure := False;
                  end if;

                  Output.Append ("  " & R.Source.Path_Name.Value);
               end if;
            end loop;

            String_Sorting.Sort (Output);

            Text_IO.Put_Line
              ((if Full_Closure then "C" else "Incomplete c") & "losure"
               & (if Sources.Length = 1 then "" else "s") & ":");
            Text_IO.New_Line;

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

         procedure Output_Source (S : Project.Source.Object);

         -------------------
         -- Output_Source --
         -------------------

         procedure Output_Source (S : Project.Source.Object) is
            use type Ada.Calendar.Time;

            Status : File_Status;

         begin
            --  For now we stick to the timestamp-based logic: if time stamps
            --  are equal, assume the file didn't change.

            if S.Source.Build_Timestamp = S.Source.Timestamp then
               Status := OK;

            elsif S.Source.Checksum = Checksum (S.Source.Path_Name) then
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
               View      : constant Project.View.Object := S.View;
               Obj_File  : Path_Name.Object;
               Unit_Info : Project.Unit_Info.Object;
               Artifacts : constant Project.Source.Artifact.Object :=
                             S.Artifacts;
               Main_Unit : Unit.Object;

               procedure Print_Unit_From (Src : Path_Name.Object);
               function  Print_Unit (U_Sec : Unit.Object) return Boolean;

               procedure Dependence_Output
                 (Dep_Source : Project.Source.Object);

               -----------------------
               -- Dependence_Output --
               -----------------------

               procedure Dependence_Output
                 (Dep_Source : Project.Source.Object) is
               begin
                  if Opt.With_Predefined_Units
                    or else not Dep_Source.Source.Is_Runtime
                  then
                     Text_IO.Put ("   ");
                     Output_Source (S => Dep_Source);
                  end if;
               end Dependence_Output;

               ----------------
               -- Print_Unit --
               ----------------

               function  Print_Unit (U_Sec : Unit.Object) return Boolean is
                  use type Unit.Object;
               begin
                  if not Main_Unit.Is_Defined then
                     Main_Unit := U_Sec;
                  elsif Main_Unit = U_Sec then
                     return False;
                  end if;

                  if Opt.Verbose then
                     Text_IO.Put_Line ("   Unit =>");
                     Text_IO.Put ("     Name   => ");
                     Text_IO.Put (String (U_Sec.Name));
                     Text_IO.New_Line;

                     Text_IO.Put_Line
                       ("     Kind   => "
                        & (case U_Sec.Library_Item_Kind is
                             when Unit.Is_Package    => "package",
                             when Unit.Is_Subprogram => "subprogram")
                        & ' '
                        & (case U_Sec.Kind is
                             when Unit.Spec_Kind  => "spec",
                             when Unit.Body_Kind  => "body",
                             when Unit.S_Separate => "separate"));

                     if U_Sec.Is_Any_Flag_Set then
                        Text_IO.Put ("     Flags  =>");
                        for Flag in Unit.Flag'Range loop
                           if U_Sec.Is_Flag_Set (Flag) then
                              Text_IO.Put (' ' & Unit.Image (Flag));
                           end if;
                        end loop;
                        Text_IO.New_Line;
                     end if;
                  else
                     Text_IO.Put_Line ("   " & String (U_Sec.Name));
                  end if;

                  return True;
               end Print_Unit;

               ---------------------
               -- Print_Unit_From --
               ---------------------

               procedure Print_Unit_From (Src : Path_Name.Object) is
                  U_Src : constant Project.Source.Object := View.Source (Src);
               begin
                  if not Opt.Print_Units
                    or else
                      (Print_Unit (U_Src.Source.Units.First_Element)
                       and then not Opt.Dependency_Mode
                       and then Opt.Print_Sources)
                  then
                     Output_Source (U_Src);
                  end if;
               end Print_Unit_From;

            begin
               for U_Sec of S.Source.Units loop
                  if Artifacts.Has_Dependency (U_Sec.Index)
                    and then Artifacts.Dependency (U_Sec.Index).Exists
                  then
                     if Opt.Print_Object_Files
                       and then not S.Is_Aggregated
                     then
                        Obj_File := Artifacts.Object_Code (U_Sec.Index);

                        if Obj_File.Exists then
                           Text_IO.Put_Line (Obj_File.Value);
                        else
                           Text_IO.Put_Line (No_Obj);
                        end if;
                     end if;

                     if Opt.Print_Units and then Print_Unit (U_Sec) then
                        null;
                     end if;

                     if Opt.Print_Sources and then not Opt.Dependency_Mode then
                        Output_Source (S);
                     end if;

                     exit when not Opt.Verbose;

                     Unit_Info := S.View.Unit (U_Sec.Name);

                     if Unit_Info.Has_Spec then
                        Print_Unit_From (Unit_Info.Spec);
                     end if;

                     if Unit_Info.Has_Body then
                        Print_Unit_From (Unit_Info.Main_Body);
                     end if;

                     for S of Unit_Info.Separates loop
                        Print_Unit_From (S);
                     end loop;
                  end if;
               end loop;

               if Opt.Dependency_Mode and then Opt.Print_Sources then
                  if Opt.Verbose then
                     Text_IO.Put_Line ("   depends upon");
                  end if;

                  S.Dependencies (Dependence_Output'Access);
               end if;

            end;
         end loop;
      end Display_Normal;

   begin
      if Opt.Verbose then
         Display_Paths;
      end if;

      if not Opt.Files.Is_Empty then
         --  Fill the various caches to get the sources from simple filenames
         --  and artefacts

         for CV in Tree.Iterate ((Project.I_Extended => False, others => True))
         loop
            for S of Project.Tree.Element (CV).Sources (Need_Update => False)
            loop
               declare
                  Artifacts : Project.Source.Artifact.Object;

                  function Insert_Prefer_Body
                    (Key  : Name_Type;
                     Kind : GPR2.Unit.Library_Unit_Type) return Boolean;

                  ------------------------
                  -- Insert_Prefer_Body --
                  ------------------------

                  function Insert_Prefer_Body
                    (Key  : Name_Type;
                     Kind : GPR2.Unit.Library_Unit_Type) return Boolean
                  is
                     Position : Sources_By_Path.Cursor;
                     Inserted : Boolean;
                  begin
                     if Kind /= GPR2.Unit.S_Spec
                       and then Opt.Files.Contains (String (Key))
                     then
                        Remains.Exclude (String (Key));

                        Sources.Insert (S, Position, Inserted);

                        return True;
                     end if;

                     return False;
                  end Insert_Prefer_Body;

               begin
                  if not Insert_Prefer_Body
                           (S.Source.Path_Name.Simple_Name, GPR2.Unit.S_Body)
                    and then S.Source.Has_Units
                  then
                     Artifacts := S.Artifacts;

                     for CU of S.Source.Units loop
                        exit when Artifacts.Has_Dependency (CU.Index)
                          and then
                            (Insert_Prefer_Body
                               (Artifacts.Dependency (CU.Index).Simple_Name,
                                CU.Kind)
                             or else
                             Insert_Prefer_Body
                               (Artifacts.Dependency (CU.Index).Base_Name,
                                CU.Kind));

                        exit when Artifacts.Has_Object_Code (CU.Index)
                          and then
                           Insert_Prefer_Body
                             (Artifacts.Object_Code (CU.Index).Simple_Name,
                              CU.Kind);
                     end loop;
                  end if;
               end;
            end loop;
         end loop;

         --
         --  All along, we will exclude non-ada sources.
         --

         --  Fill the Sources set with the files given on the CL.
         --  Print "Can't find source for ..." if a file can't be matched with
         --  a compilable source from the root project (or from the project
         --   tree if All_Projects is set).

         for F of Remains loop
            Text_IO.Put_Line ("Can't find source for " & F);
         end loop;

      elsif Opt.Closure_Mode then
         --  If none was provided, then:
         --     - Either we're in closure mode, and we want to use the mains
         --       from the root project.

         for S of Tree.Root_Project.Sources (Need_Update => False) loop
            if Tree.Root_Project.Has_Mains
              and then S.Is_Main
              and then S.Source.Language = Name_Type (Ada_Lang)
            then
               Sources.Insert (S);
            end if;
         end loop;

      elsif Opt.All_Projects then
         --  - Or we're not, and we will use all the compilable sources (from
         --    the root project or the entire tree, depending on All_Sources).

         for CV in Tree.Iterate ((Project.I_Extended => False, others => True))
         loop
            for S_Cur in Project.Tree.Element (CV).Sources
              (Need_Update => False).Iterate (Filter => S_Compilable)
            loop
               if Element (S_Cur).Source.Language = Name_Type (Ada_Lang)
               then
                  Sources.Insert (Element (S_Cur), Position, Inserted);

                  --  Source could be already in the set because we
                  --  can have the same project in the All_Views
                  --  twice, one time for aggregated project another
                  --  time for the imported project. Besides that we
                  --  can have the same source in the aggregated
                  --  project and in the aggregating library project.

                  if not Inserted
                    and then Element (S_Cur).Is_Aggregated
                    < Sources_By_Path.Element (Position).Is_Aggregated
                  then
                     --  We prefer Is_Aggregated = False because it
                     --  has object files.

                     Sources.Replace (Element (S_Cur));
                  end if;
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

      --  Do nothing if no source was found

      if Sources.Is_Empty then
         Finish_Program (E_Success);
      end if;

      --  Check all sources and notify when no ALI file is present

      for S of Sources loop
         for CU of S.Source.Units loop
            if S.Artifacts.Has_Dependency (CU.Index)
              and then not S.Artifacts.Dependency (CU.Index).Exists
            then
               Full_Closure := False;
               Text_IO.Put_Line
                 ("Can't find ALI "
                  & String
                    (if CU.Index > 1
                     then S.Artifacts.Dependency (CU.Index).Simple_Name & " "
                     else "") & "file for " & S.Source.Path_Name.Value);
            end if;
         end loop;
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
