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

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Text_IO;

with GPR2.Unit;
with GPR2.Containers;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Part_Set;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.Unit_Info;
with GPR2.Project.View;
with GPR2.Source_Info.Parser.Registry;
with GPR2.Version;

with GPRtools.Options;
with GPRtools.Util;

with GPRls.Common;
with GPRls.Gnatdist;
with GPRls.Options;

function GPRls.Process
  (Opt : in out GPRls.Options.Object) return Ada.Command_Line.Exit_Status
is

   use Ada;

   use GPR2;
   use GPR2.Project.Source.Set;
   use all type GPR2.Unit.Library_Unit_Type;

   use GPRls.Common;
   use GPRls.Options;

   use GPRtools;
   use GPRtools.Util;

   Tree : Project.Tree.Object renames Opt.Tree.all;

   procedure Display_Paths;

   procedure Put (Str : String; Lvl : Verbosity_Level);
   pragma Unreferenced (Put);
   --  Call Ada.Text_IO.Put (Str) if Opt.Verbosity is at least Lvl

   procedure Put_Line (Str : String; Lvl : Verbosity_Level);
   --  Call Ada.Text_IO.Put_Line (Str) if Opt.Verbosity is at least Lvl

   procedure Show_Tree_Load_Errors;
   --  Print errors/warnings following a project tree load

   -------------------
   -- Display_Paths --
   -------------------

   procedure Display_Paths is
      Curr_Dir : constant String := Directories.Current_Directory;
      Obj_Path : Path_Name.Set.Object;

      function Mask_Current (Dir : String) return String is
        (if Dir (Dir'First .. Dir'Last - 1) = Curr_Dir
         then "<Current_Directory>" else Dir);

   begin
      Text_IO.New_Line;
      Version.Display ("GPRLS", "2018", Version_String => Version.Long_Value);

      --  Source search path

      Text_IO.New_Line;
      Text_IO.Put_Line ("Source Search Path:");

      for V of Tree loop
         if V.Kind in With_Source_Dirs_Kind then
            for Src of V.Source_Directories loop
               Text_IO.Put_Line ("   " & String (Src.Dir_Name));
            end loop;
         end if;
      end loop;

      if Tree.Has_Runtime_Project then
         for Src of Tree.Runtime_Project.Source_Directories loop
            Text_IO.Put_Line ("   " & String (Src.Dir_Name));
         end loop;
      end if;

      --  Object search path

      for V of Tree loop
         case V.Kind is
            when K_Standard =>
               Obj_Path.Append (V.Object_Directory);
            when K_Library | K_Aggregate_Library =>
               Obj_Path.Append (V.Library_Ali_Directory);
            when others =>
               null;
         end case;
      end loop;

      if Tree.Has_Runtime_Project then
         Obj_Path.Append (Tree.Runtime_Project.Object_Directory);
      end if;

      Text_IO.New_Line;
      Text_IO.Put_Line ("Object Search Path:");

      for P of Obj_Path loop
         Text_IO.Put_Line ("   " & P.Dir_Name);
      end loop;

      --  Project search path

      Text_IO.New_Line;
      Text_IO.Put_Line ("Project Search Path:");

      for P of Tree.Project_Search_Paths loop
         Text_IO.Put_Line ("   " & Mask_Current (P.Dir_Name));
      end loop;

      Text_IO.New_Line;
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
      if Tree.Log_Messages.Has_Error then
         --  In case both warnings and errors are present, only displpay the
         --  errors as they are probably responsible for the warnings.

         for C in Tree.Log_Messages.Iterate
           (Information => False,
            Warning     => False,
            Error       => True,
            Read        => False,
            Unread      => True)
         loop
            Put_Line (Log.Element (C).Format, Quiet);
         end loop;
      else
         for C in Tree.Log_Messages.Iterate
           (Information => Opt.Verbose,
            Warning     => True,
            Error       => False,
            Read        => False,
            Unread      => True)
         loop
            Put_Line (Log.Element (C).Format, Regular);
         end loop;
      end if;
   end Show_Tree_Load_Errors;

begin
   --  Load the project tree

   if not GPRtools.Options.Load_Project
     (Opt,
      Absent_Dir_Error   => False,
      Handle_Information => Opt.Verbose,
      Handle_Lint        => Opt.Verbose)
   then
      if Opt.Project_File.Is_Defined then
         Text_IO.Put_Line
           ("gprls: unable to process project file "
            & String (Opt.Project_File.Name));
      else
         Text_IO.Put_Line
           ("gprls: unable to process default project file in "
            & String (Opt.Project_Base.Name));
      end if;

      return Command_Line.Failure;
   end if;

   if Opt.Only_Display_Paths then
      --  For the "gprls -v" usage

      Display_Paths;
      return Command_Line.Success;
   end if;

   Show_Tree_Load_Errors;

   pragma Assert
     (not Opt.Source_Parser
      or else GPR2.Source_Info.Parser.Registry.Exists
        (Ada_Language, Source_Info.Source), "Source parser is not registered");

   pragma Assert
     (GPR2.Source_Info.Parser.Registry.Exists
        (Ada_Language, Source_Info.LI), "ALI parser is not registered");

   --  Make sure the sources are up to date

   Tree.Update_Sources
     (Backends     => (Source_Info.Source => Opt.Source_Parser,
                       Source_Info.LI     => True),
      With_Runtime => (Opt.Gnatdist or else Opt.With_Predefined_Units));

   --
   --  Main processing
   --

   declare
      --  Cache some data to speed up later processing.
      --  The maps should have Value_Path keys to support case-insensitive FS.

      use type Project.Source.Object;
      use type Project.View.Object;
      use all type Project.Source.Naming_Exception_Kind;

      function Path_Equal
        (Left, Right : Project.Source.Source_Part) return Boolean
      is (Left.Source = Right.Source
          and then Left.Source.View.Namespace_Root =
                   Right.Source.View.Namespace_Root
          and then Left.Index = Right.Index);

      type One_Type is range -1 .. 1;

      function Compare
        (Left, Right : Project.View.Object) return One_Type
      is (if Left < Right then -1 elsif Left = Right then 0 else 1);

      --  function Compare (Left, Right : Path_Name.Full_Name) return One_Type
      --  is (if Left < Right then -1 elsif Left = Right then 0 else 1);

      function Compare
        (Left, Right : Project.Source.Object) return One_Type
      is (if Left < Right then -1 elsif Left = Right then 0 else 1);

      function Compare
        (Left, Right : Unit_Index) return One_Type
      is (if Left < Right then -1 elsif Left = Right then 0 else 1);

      function Path_Less
        (Left, Right : Project.Source.Source_Part) return Boolean
      is (case Compare
            (Left.Source.View.Namespace_Root, Right.Source.View.Namespace_Root)
          is
             when -1 => True,
             when  1 => False,
             when  0 =>
            (case Compare (Left.Source, Right.Source) is
                when -1 => True,
                when  1 => False,
                when  0 => Compare (Left.Index, Right.Index) = -1));

      package Sources_By_Path is new Ada.Containers.Indefinite_Ordered_Sets
        (Project.Source.Source_Part, "<" => Path_Less, "=" => Path_Equal);

      type File_Status is
        (OK,        -- matching timestamp
         Not_Same); -- non matching timestamp

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

      procedure Display_Closures;

      procedure Display_Gnatdist;

      procedure Display_Normal;

      ----------------------
      -- Display_Closures --
      ----------------------

      procedure Display_Closures is
         use type Ada.Containers.Count_Type;

         Closures : Project.Source.Part_Set.Object (Sorted => True);
      begin
         if Sources.Is_Empty then
            Finish_Program (E_Errors, "no main specified for closure");
         end if;

         for S of Sources loop
            declare
               Deps : constant Project.Source.Part_Set.Object :=
                        S.Source.Dependencies (Closure => True,
                                               Sorted  => False);
            begin
               if Deps.Is_Empty then
                  --  If no dependencies, use only this one because without ALI
                  --  file we don't know dependency even on itself.

                  Closures.Insert (S);
               else
                  Closures.Union (Deps);
               end if;
            end;
         end loop;

         Text_IO.New_Line;

         declare
            package String_Sorting is new String_Vector.Generic_Sorting;

            Output : String_Vector.Vector;
         begin
            for R of Closures loop
               if not R.Source.Is_Runtime then
                  if not GPR2.Project.Source.Artifact.Dependency
                           (R.Source, R.Index).Is_Defined
                  then
                     Full_Closure := False;
                  end if;

                  if R.Index not in Multi_Unit_Index then
                     Output.Append ("  " & R.Source.Path_Name.Value);
                  else
                     Output.Append
                       ("  " & R.Source.Path_Name.Value
                        & " @" & R.Index'Image);
                  end if;
               end if;
            end loop;

            String_Sorting.Sort (Output);

            Text_IO.Put_Line
              ((if Full_Closure
               then "C"
               else "Incomplete c") & "losure"
               & (if Sources.Length = 1
                 then ""
                 else "s") & ":");

            Text_IO.New_Line;

            for O of Output loop
               Text_IO.Put_Line (O);
            end loop;
         end;

         Text_IO.New_Line;
      end Display_Closures;

      ----------------------
      -- Display_Gnatdist --
      ----------------------

      procedure Display_Gnatdist is

         function Has_Dependency
           (S : Project.Source.Source_Part) return Boolean;

         --------------------
         -- Has_Dependency --
         --------------------

         function Has_Dependency
           (S : Project.Source.Source_Part) return Boolean is
         begin
            return GPR2.Project.Source.Artifact.Dependency
              (S.Source, S.Index).Is_Defined;
         end Has_Dependency;

         No_ALI : Boolean := True;

      begin
         for S of Sources loop
            if S.Index = 0 then
               for CU of S.Source.Units loop
                  if Has_Dependency ((S.Source, Index => CU.Index)) then
                     No_ALI := False;
                     Gnatdist.Output_ALI (S.Source, CU.Index);
                  end if;
               end loop;

            elsif Has_Dependency (S) then
               No_ALI := False;
               Gnatdist.Output_ALI (S.Source, S.Index);
            end if;

            if No_ALI then
               Gnatdist.Output_No_ALI (S.Source, S.Index);
            end if;
         end loop;
      end Display_Gnatdist;

      --------------------
      -- Display_Normal --
      --------------------

      procedure Display_Normal is
         use type Source_Info.Backend;

         procedure Output_Source
           (S          : Project.Source.Object;
            Idx        : Unit_Index;
            Build_Time : Ada.Calendar.Time;
            A          : Project.Source.Artifact.Object :=
                           Project.Source.Artifact.Undefined);

         -------------------
         -- Output_Source --
         -------------------

         procedure Output_Source
           (S          : Project.Source.Object;
            Idx        : Unit_Index;
            Build_Time : Ada.Calendar.Time;
            A          : Project.Source.Artifact.Object :=
                           Project.Source.Artifact.Undefined)
         is
            use type Calendar.Time;

            package SI renames GPR2.Source_Info;

            Status    : File_Status;
            Artifacts : Project.Source.Artifact.Object;

            function Check_Object_Code return Boolean;
            --  Returns true if source has object code and set Artifacts

            function No_Trail_Zero (Item : String) return String;
            --  Remove trailing zeroes with possible dot and leading space

            -----------------------
            -- Check_Object_Code --
            -----------------------

            function Check_Object_Code return Boolean is
               package PSA renames Project.Source.Artifact;
            begin
               if A.Is_Defined then
                  Artifacts := A;
               else
                  Artifacts := PSA.Create
                    (S,
                     Filter => (PSA.Object_File => True,
                                others          => False));
               end if;

               return Artifacts.Has_Object_Code;
            end Check_Object_Code;

            -------------------
            -- No_Trail_Zero --
            -------------------

            function No_Trail_Zero (Item : String) return String is
            begin
               for J in reverse Item'Range loop
                  if Item (J) /= '0' then
                     return Item
                       (Item'First +
                          (if Item (Item'First) = ' ' then 1 else 0) ..
                            J - (if Item (J) = '.' then 1 else 0));
                  end if;
               end loop;

               return Item;
            end No_Trail_Zero;

         begin
            --  For now we stick to the timestamp-based logic: if time stamps
            --  are equal, assume the file didn't change.

            if Build_Time = S.Timestamp (ALI => True)
              or else
                (not SI.Parser.Registry.Exists (S.Language, SI.None)
                 and then Check_Object_Code
                 and then Artifacts.Object_Code (Index => Idx).Exists
                 and then S.Timestamp (ALI => False) <
                        Artifacts.Object_Code (Index => Idx).Modification_Time)
            then
               Status := OK;

            else
               Status := Not_Same;
            end if;

            if Opt.Verbose then
               Text_IO.Put ("     Source => ");
               Text_IO.Put (S.Path_Name.Value);
               if S.Has_Index then
                  Text_IO.Put (" @");
                  Text_IO.Put (Idx'Image);
               end if;

               case Status is
                  when OK =>
                     Text_IO.Put (" unchanged");

                  when Not_Same =>
                     Text_IO.Put (" modified");
               end case;

            else
               if not Opt.Selective_Output then
                  Text_IO.Put ("    ");

                  case Status is
                     when OK =>
                        Text_IO.Put ("  OK ");

                     when Not_Same =>
                        Text_IO.Put (" DIF ");

                        if GPR2.Is_Debug ('F') then
                           if S.Is_Parsed (Idx) then
                              Text_IO.Put (S.Used_Backend (Idx)'Img);
                              Text_IO.Put (' ');

                              if S.Build_Timestamp (Idx) /=
                                S.Timestamp (ALI => True)
                              then
                                 Text_IO.Put
                                   (No_Trail_Zero
                                      (Duration'Image
                                           (S.Timestamp (ALI => True) -
                                                S.Build_Timestamp (Idx))));
                                 Text_IO.Put (' ');
                              end if;

                           else
                              Text_IO.Put ("not parsed ");
                           end if;
                        end if;
                  end case;
               end if;

               Text_IO.Put
                 (if S.Is_Runtime and then Opt.Hide_Runtime_Directory
                  then String (S.Path_Name.Simple_Name)
                  else S.Path_Name.Value);

               if Idx /= No_Index then
                  Text_IO.Put (" at index" & Idx'Image);
               end if;
            end if;

            Text_IO.New_Line;
         end Output_Source;

      begin
         for S of Sources loop
            declare
               use Project.Source;
               View      : constant Project.View.Object := S.Source.View;
               Artifacts : constant Project.Source.Artifact.Object :=
                             Project.Source.Artifact.Create
                               (S.Source,
                                Filter => (Artifact.Dependency_File => True,
                                           Artifact.Object_File     => True,
                                           others                   => False));
               Main_Unit : GPR2.Unit.Object;

               procedure Print_Unit_From
                 (Src : GPR2.Unit.Source_Unit_Identifier);

               function  Print_Unit (U_Sec : GPR2.Unit.Object) return Boolean;

               procedure Print_Object (Index : Unit_Index);

               procedure Print_Object (U_Sec : GPR2.Unit.Object);

               procedure Dependency_Output
                 (Dep_Source : Project.Source.Object;
                  Index      : Unit_Index;
                  Timestamp  : Ada.Calendar.Time);

               function Has_Dependency (Index : Unit_Index) return Boolean is
                 (Artifacts.Has_Dependency (Index)
                  and then
                    (Artifacts.Dependency (Index).Exists
                     or else Opt.Source_Parser));

               -----------------------
               -- Dependency_Output --
               -----------------------

               procedure Dependency_Output
                 (Dep_Source : Project.Source.Object;
                  Index      : Unit_Index;
                  Timestamp  : Ada.Calendar.Time) is
               begin
                  if Opt.With_Predefined_Units
                    or else not Dep_Source.Is_Runtime
                  then
                     Text_IO.Put ("   ");
                     Output_Source (S          => Dep_Source,
                                    Idx        => Index,
                                    Build_Time => Timestamp);
                  end if;
               end Dependency_Output;

               ------------------
               -- Print_Object --
               ------------------

               procedure Print_Object (Index : GPR2.Unit_Index) is
                  Obj_File : GPR2.Path_Name.Object;
               begin
                  if Opt.Print_Object_Files
                    and then not S.Source.Is_Aggregated
                  then
                     Obj_File := Artifacts.Object_Code (Index);

                     if Obj_File.Exists then
                        Text_IO.Put_Line (Obj_File.Value);
                     else
                        Text_IO.Put_Line (No_Obj);
                     end if;
                  end if;
               end Print_Object;

               ------------------
               -- Print_Object --
               ------------------

               procedure Print_Object (U_Sec : GPR2.Unit.Object) is
                  Unit_Info : Project.Unit_Info.Object;
               begin
                  Print_Object (U_Sec.Index);

                  if Opt.Print_Units and then Print_Unit (U_Sec) then
                     null;
                  end if;

                  if Opt.Print_Sources and then not Opt.Dependency_Mode then
                     Output_Source
                       (S.Source, S.Index, S.Source.Build_Timestamp (S.Index),
                        Artifacts);
                  end if;

                  if Opt.Verbose then
                     Unit_Info := S.Source.View.Unit (U_Sec.Name);

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
               end Print_Object;

               ----------------
               -- Print_Unit --
               ----------------

               function  Print_Unit
                 (U_Sec : GPR2.Unit.Object) return Boolean
               is
                  use type GPR2.Unit.Object;
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
                             when GPR2.Unit.Is_Package    => "package",
                             when GPR2.Unit.Is_Subprogram => "subprogram")
                        & ' '
                        & (case U_Sec.Kind is
                             when GPR2.Unit.Spec_Kind  => "spec",
                             when GPR2.Unit.Body_Kind  => "body",
                             when GPR2.Unit.S_Separate => "separate"));

                     if U_Sec.Is_Any_Flag_Set then
                        Text_IO.Put ("     Flags  =>");

                        for Flag in GPR2.Unit.Flag'Range loop
                           if U_Sec.Is_Flag_Set (Flag) then
                              Text_IO.Put (' ' & GPR2.Unit.Image (Flag));
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

               procedure Print_Unit_From
                 (Src : GPR2.Unit.Source_Unit_Identifier)
               is
                  U_Src : constant Project.Source.Object :=
                            View.Source (Src.Source);
               begin
                  if not Opt.Print_Units
                    or else
                      (Print_Unit (U_Src.Unit (Src.Index))
                       and then not Opt.Dependency_Mode
                       and then Opt.Print_Sources)
                  then
                     Output_Source (U_Src, Src.Index,
                                    U_Src.Build_Timestamp (Src.Index));
                  end if;
               end Print_Unit_From;

            begin
               if not S.Source.Has_Units then
                  Print_Object (No_Index);

                  if Opt.Print_Sources and then not Opt.Dependency_Mode then
                     Output_Source
                       (S.Source, No_Index,
                        S.Source.Build_Timestamp (No_Index),
                        Artifacts);
                  end if;

               elsif S.Index = No_Index then
                  for U_Sec of S.Source.Units loop
                     if Has_Dependency (U_Sec.Index) then
                        Print_Object (U_Sec);
                        exit when not Opt.Verbose;
                     end if;
                  end loop;

               elsif Has_Dependency (S.Index) then
                  Print_Object (S.Source.Unit (S.Index));
               end if;

               if Opt.Dependency_Mode and then Opt.Print_Sources then
                  if Opt.Verbose then
                     Text_IO.Put_Line ("   depends upon");
                  end if;

                  S.Source.Dependencies
                    (S.Index, Dependency_Output'Access);
               end if;
            end;
         end loop;
      end Display_Normal;

      View   : GPR2.Project.View.Object;
      Filter : GPR2.Project.Iterator_Control :=
                 GPR2.Project.Default_Iterator;

   begin
      if Opt.Verbose then
         Display_Paths;
      end if;

      if not Opt.Files.Is_Empty then
         --  Fill the various caches to get the sources from simple filenames
         --  and artefacts.

         for CV in
           Tree.Iterate ((Project.I_Extended => False, others => True))
         loop
            for S of Project.Tree.Element (CV).Sources loop
               declare
                  use Project.Source.Artifact;

                  Artifacts : Project.Source.Artifact.Object;
                  Dismiss   : Boolean with Unreferenced;

                  function Insert_Prefer_Body
                    (Key   : Filename_Type;
                     Kind  : GPR2.Unit.Library_Unit_Type;
                     Index : Unit_Index) return Boolean;

                  ------------------------
                  -- Insert_Prefer_Body --
                  ------------------------

                  function Insert_Prefer_Body
                    (Key   : Filename_Type;
                     Kind  : GPR2.Unit.Library_Unit_Type;
                     Index : Unit_Index) return Boolean
                  is
                     procedure Do_Insert (Index : Unit_Index);

                     ---------------
                     -- Do_Insert --
                     ---------------

                     procedure Do_Insert (Index : Unit_Index)
                     is
                        Position : Sources_By_Path.Cursor;
                        Inserted : Boolean;

                     begin
                        Sources.Insert ((S, Index), Position, Inserted);

                        if not Inserted
                          and then S.Is_Aggregated
                                   < Sources (Position).Source.Is_Aggregated
                        then
                           --  Prefer none aggregated, more information there

                           Sources.Replace_Element (Position, (S, Index));
                        end if;
                     end Do_Insert;

                  begin
                     if Kind /= GPR2.Unit.S_Spec
                       and then Opt.Files.Contains (String (Key))
                     then
                        Remains.Exclude (String (Key));

                        if S.Has_Units and then Index = No_Index then
                           for CU of S.Units loop
                              if CU.Kind not in
                                GPR2.Unit.S_Spec | GPR2.Unit.S_Separate
                              then
                                 Do_Insert (CU.Index);
                              end if;
                           end loop;

                        else
                           Do_Insert (Index);
                        end if;

                        return True;
                     end if;

                     return False;
                  end Insert_Prefer_Body;

                  function Insert_Prefer_Body
                    (Kind  : GPR2.Unit.Library_Unit_Type;
                     Index : Unit_Index) return Boolean
                  is
                    ((Artifacts.Has_Dependency (Index)
                     and then
                       (Insert_Prefer_Body
                          (Artifacts.Dependency (Index).Simple_Name,
                           Kind, Index)
                        or else
                        Insert_Prefer_Body
                          (Artifacts.Dependency (Index).Base_Filename,
                           Kind, Index)))
                     or else
                       (Artifacts.Has_Object_Code (Index)
                        and then
                        Insert_Prefer_Body
                          (Artifacts.Object_Code (Index).Simple_Name,
                           Kind, Index)));

                  use GPR2.Project.Source;

               begin
                  if not Insert_Prefer_Body
                    (S.Path_Name.Simple_Name, GPR2.Unit.S_Body, No_Index)
                  then
                     Artifacts := GPR2.Project.Source.Artifact.Create
                       (S, Filter => (Artifact.Dependency_File => True,
                                      Artifact.Object_File     => True,
                                      others                   => False));

                     if S.Has_Units then
                        for CU of S.Units loop
                           exit when Insert_Prefer_Body (CU.Kind, CU.Index);
                        end loop;
                     else
                        Dismiss := Insert_Prefer_Body (S.Kind, No_Index);
                     end if;
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

         for S of Tree.Root_Project.Sources loop
            if Tree.Root_Project.Has_Mains
              and then S.Is_Main
              and then (not GPR2.Is_Debug ('1')
                        or else S.Language = Ada_Language)
            then
               Sources.Insert ((S, No_Index));
            end if;
         end loop;

      elsif Opt.All_Projects then
         --  - Or we're not, and we will use all the compilable sources (from
         --    the root project or the entire tree, depending on All_Sources).

         Filter (GPR2.Project.I_Runtime) := Opt.With_Predefined_Units;

         for C in Tree.Iterate (Kind => Filter) loop
            View := GPR2.Project.Tree.Element (C);

            if not View.Is_Extended then
               for Src of View.Sources (Compilable_Only => True) loop
                  if not GPR2.Is_Debug ('1')
                    or else Src.Language = Ada_Language
                  then
                     if Src.Has_Units then
                        for CU of Src.Units loop
                           if Src.Is_Compilable (CU.Index) then
                              Sources.Insert ((Src, CU.Index),
                                              Position, Inserted);
                           end if;
                        end loop;
                     else
                        Sources.Insert ((Src, No_Index), Position, Inserted);
                     end if;

                     --  Source could be already in the set because we
                     --  can have the same project in the All_Views
                     --  twice, one time for aggregated project another
                     --  time for the imported project. Besides that we
                     --  can have the same source in the aggregated
                     --  project and in the aggregating library project.

                     if not Inserted
                       and then Src.Is_Aggregated
                         < Sources_By_Path.Element
                             (Position).Source.Is_Aggregated
                     then
                        --  We prefer Is_Aggregated = False because it
                        --  has object files.
                        if Src.Has_Units then
                           for CU of Src.Units loop
                              Sources.Replace_Element
                                (Position, (Src, CU.Index));
                           end loop;
                        else
                           Sources.Replace_Element (Position, (Src, No_Index));
                        end if;
                     end if;
                  end if;
               end loop;
            end if;
         end loop;

      else
         for Src of Tree.Root_Project.Sources (Compilable_Only => True) loop
            if not GPR2.Is_Debug ('1')
              or else Src.Language = Ada_Language
            then
               if Src.Has_Units then
                  for CU of Src.Units loop
                     if Src.Is_Compilable (CU.Index) then
                        Sources.Insert ((Src, CU.Index));
                     end if;
                  end loop;
               else
                  Sources.Insert ((Src, No_Index));
               end if;
            end if;
         end loop;
      end if;

      --  Do nothing if no source was found

      if Sources.Is_Empty then
         return Command_Line.Success;
      end if;

      --  Check all sources and notify when no ALI file is present

      if not Opt.Source_Parser and then not Opt.Gnatdist then
         for S of Sources loop
            if S.Source.Has_Units then
               declare
                  Other : constant GPR2.Project.Source.Source_Part :=
                            S.Source.Other_Part_Unchecked (S.Index);
               begin
                  if S.Source.Kind (S.Index) /= Unit.S_Separate
                    and then not S.Source.Is_Parsed (S.Index)
                    and then
                      (not Other.Source.Is_Defined
                       or else not Other.Source.Is_Parsed (Other.Index))
                  then
                     Full_Closure := False;

                     if S.Source.Has_Naming_Exception
                       and then S.Source.Naming_Exception
                         = Project.Source.Multi_Unit
                     then
                        --  In case of multi-unit we have no information
                        --  until the unit is compiled. There is no need to
                        --  report that there is missing ALI in this case.
                        --  But we report that the status for this file is
                        --  unknown.

                        Text_IO.Put_Line
                          ("UNKNOWN status for unit " &
                             String (S.Source.Unit_Name (S.Index)) & " in " &
                             S.Source.Path_Name.Value & " at index" &
                             S.Index'Image);

                     else
                        Text_IO.Put_Line
                          ("Can't find ALI file for " &
                             S.Source.Path_Name.Value);
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end if;

      --  We gathered all the sources:
      --  Process them according to the chosen mode.

      if Opt.Closure_Mode then
         Display_Closures;

      elsif Opt.Gnatdist then
         Display_Gnatdist;

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

   return Command_Line.Success;

exception
   when Project_Error | Processing_Error =>
      Show_Tree_Load_Errors;

      Finish_Program
        (E_Errors,
         "unable to process project file " & String (Opt.Project_File.Name));

      return Command_Line.Failure;
end GPRls.Process;
