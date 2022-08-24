--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Path_Name;
with GPR2.Project.Source.Artifact;
with GPR2.Source_Info.Parser.Registry;

with GNAT.Case_Util;
with GNAT.OS_Lib;

package body GPR2.Source_Info.Parser.D is

   C_Handle   : Object (C_Language);
   CPP_Handle : Object (CPP_Language);

   function Unescape (Path : String) return String;
   --  Remove the character '\' if it is before ' ', '#', ':', or '\'.
   --  Remove the character '$' if it is before '$'.

   -------------
   -- Compute --
   -------------

   overriding procedure Compute
     (Self   : not null access Object;
      Data   : in out Source_Info.Object'Class;
      Source : Project.Source.Object)
   is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
      use GNAT;

      Artifacts : constant Project.Source.Artifact.Object := Source.Artifacts;
      Dep_Name  : constant Path_Name.Object := Artifacts.Dependency (0);
      Obj_Dir   : constant String := Source.View.Object_Directory.Value;
      Dep_File  : File_Type;

      Last_Obj  : Natural;
      Start     : Natural;
      Finish    : Natural;
      Looping   : Boolean := False;
      Buffer    : String (1 .. 1024);
      Last      : Natural;
      C_Dep     : Dependency_Vectors.Cursor;

      OK        : Boolean;

      function Is_Time_Stamp (S : String) return Boolean;
      --  Return True iff S has the format of a Time_Stamp_Type

      function Dependencies_Reference
        return Dependency_Vectors_Ref.Reference_Type;
      --  Create dependencies reference

      procedure Wrong_Format;
      --  Print message that file has wrong format

      function Is_Time_Stamp (S : String) return Boolean is
        (S'Length = Time_String'Length
         and then (for all C of S => C in '0' .. '9'));

      ----------------------------
      -- Dependencies_Reference --
      ----------------------------

      function Dependencies_Reference
        return Dependency_Vectors_Ref.Reference_Type
      is
         CU1      : Unit_Dependencies.Cursor;
         Inserted : Boolean;
      begin
         if Data.Dependencies.Is_Empty then
            declare
               Ref : Dependency_Vectors_Ref.Ref;
            begin
               Ref.Set (Dependency_Vectors.Empty_Vector);
               Data.Dependencies.Insert
                 (No_Index, Ref, CU1, Inserted);
            end;
            pragma Assert (Inserted);

         else
            CU1 := Data.Dependencies.First;
         end if;

         return Result : constant Dependency_Vectors_Ref.Reference_Type :=
                           Data.Dependencies (CU1).Get
         do
            if not Result.Is_Empty then
               Result.Clear;
            end if;
         end return;
      end Dependencies_Reference;

      ------------------
      -- Wrong_Format --
      ------------------

      procedure Wrong_Format is
      begin
         Put  ("      -> dependency file ");
         Put  (Dep_Name.Value);
         Put_Line (" has wrong format");
      end Wrong_Format;

      Dependencies : constant Dependency_Vectors_Ref.Reference_Type :=
                       Dependencies_Reference;

   begin
      if not Dep_Name.Is_Defined
        or else not Dep_Name.Exists
      then
         return;
      end if;

      begin
         Open (Dep_File, In_File, Dep_Name.Value);
      exception
         when E : others =>
            if Debug ('D') then
               Put  ("      -> could not open dependency file ");
               Put_Line (Dep_Name.Value);
               Put_Line (Ada.Exceptions.Exception_Message (E));
            end if;

            return;
      end;

      --  Loop Big_Loop is executed several times only when the dependency file
      --  contains several times
      --     <object file>: <source1> ...
      --  When there is only one of such occurence, Big_Loop is exited
      --  successfully at the beginning of the second loop.

      Big_Loop : loop
         declare
            End_Of_File_Reached : Boolean := False;
            Object_Found        : Boolean := False;
         begin
            loop
               if End_Of_File (Dep_File) then
                  End_Of_File_Reached := True;
                  exit;
               end if;

               Get_Line (Dep_File, Buffer, Last);

               if Last > 0 and then Buffer (1) /= '#' then
                  --  Skip a first line that is an empty continuation line

                  for J in 1 .. Last - 1 loop
                     if Buffer (J) /= ' ' then
                        Object_Found := True;
                        exit;
                     end if;
                  end loop;

                  exit when Object_Found or else Buffer (Last) /= '\';
               end if;
            end loop;

               --  If dependency file contains only empty lines or comments,
               --  then dependencies are unknown, and the source needs to be
               --  recompiled.

            if End_Of_File_Reached then
               --  If we have reached the end of file after the first
               --  loop, there is nothing else to do.

               exit Big_Loop when Looping;

               if Debug ('D') then
                  Put  ("      -> dependency file ");
                  Put  (Dep_Name.Value);
                  Put_Line (" is empty");
               end if;

               Close (Dep_File);
               return;
            end if;
         end;

         Start  := 1;
         Finish := Index (Buffer (1 .. Last), ":");

         if Finish > 0
           and then (Finish = Last
                     or else Buffer (Finish + 1) not in ' ' | ASCII.HT)
         then
            Finish := 0;
         end if;

         if Finish = 0 then
            OK := False;

         else
            Last_Obj := Finish;

            loop
               Last_Obj := Last_Obj - 1;
               exit when Last_Obj = Start or else Buffer (Last_Obj) /= ' ';
            end loop;

            while Start < Last_Obj and then Buffer (Start) = ' ' loop
               Start := Start + 1;
            end loop;

            if not File_Names_Case_Sensitive then
               GNAT.Case_Util.To_Lower (Buffer (Start .. Last_Obj));
            end if;

            --  First line must start with simple name or path name of object
            --  file, followed by colon.

            declare
               Obj : constant Path_Name.Object := Artifacts.Object_Code (0);
               Try : String renames Buffer (Start .. Last_Obj);
            begin
               OK := Filename_Type (Try) = Obj.Simple_Name
                 or else Try = Path_Name.To_OS_Case (Obj.Value);
            end;
         end if;

         if not OK then
            if Debug ('D') then
               Wrong_Format;

               if Finish = 0 then
                  Put_Line ("         no colon");

               else
                  Put  ("         expected object file name ");
                  Put  (Artifacts.Object_Code (0).Value);
                  Put  (", got ");
                  Put_Line (Buffer (Start .. Last_Obj));
               end if;
            end if;

            Close (Dep_File);
            return;
         end if;

         Start := Finish + 2;

         --  Process each line

         Line_Loop : loop
            declare
               Line : String  := Buffer (1 .. Last);
               Last : Natural := Line'Last;
            begin
               Name_Loop : loop
                  --  Find the beginning of the next source path name

                  while Start <= Last and then Line (Start) = ' ' loop
                     Start := Start + 1;
                  end loop;

                  exit Line_Loop when Start > Last;

                  --  Go to next line when there is a continuation
                  --  character \ at the end of the line.

                  exit Name_Loop when Start = Last
                    and then Line (Start) = '\';

                  --  We should not be at the end of the line, without
                  --  a continuation character \.

                  if Start = Last then
                     if Debug ('D') then
                        Wrong_Format;
                     end if;

                     Close (Dep_File);
                     return;
                  end if;

                  --  Look for the end of the source path name

                  Finish := Start;

                  while Finish < Last loop
                     if Line (Finish) = '\' then
                        --  On Windows, a '\' is part of the path
                        --  name, except when it is not the first
                        --  character followed by another '\' or by a
                        --  space. On other platforms, when we are
                        --  getting a '\' that is not the last
                        --  character of the line, the next character
                        --  is part of the path name, even if it is a
                        --  space.

                        pragma Warnings
                          (Off, "this code can never be executed");

                        if On_Windows
                          and then Finish = Start
                          and then Line (Finish + 1) = '\'
                        then
                           Finish := Finish + 2;

                           if Finish > Last then
                              if Debug ('D') then
                                 Wrong_Format;
                              end if;

                              Close (Dep_File);
                              return;
                           end if;

                        elsif On_Windows
                          and then Line (Finish + 1) not in '\' | ' '
                        then
                           Finish := Finish + 1;

                        else
                           Line (Finish .. Last - 1) :=
                             Line (Finish + 1 .. Last);
                           Last := Last - 1;
                        end if;

                        pragma Warnings (On);

                     else
                        --  A space that is not preceded by '\'
                        --  indicates the end of the path name.

                        exit when Line (Finish + 1) = ' ';
                        Finish := Finish + 1;
                     end if;
                  end loop;

                  if Dependency_Vectors.Has_Element (C_Dep)
                    and then Is_Time_Stamp (Line (Start .. Finish))
                  then
                     declare
                        Ref : constant Dependency_Vectors.Reference_Type :=
                                Dependencies.Reference (C_Dep);
                     begin
                        Ref.Stamp := To_Time (Line (Start .. Finish));

                        if Filename_Type (Ref.Sfile)
                          = Filename_Type (Source.Path_Name.Simple_Name)
                        then
                           Data.LI_Timestamp := Ref.Stamp;
                        end if;
                     end;

                     C_Dep := Dependency_Vectors.No_Element;

                  else
                     declare
                        Src_Name : constant String :=
                                     OS_Lib.Normalize_Pathname
                                       (Unescape (Line (Start .. Finish)),
                                        Directory      => Obj_Dir,
                                        Resolve_Links  => False,
                                        Case_Sensitive => False);
                        Src_Simple : constant String :=
                                       Directories.Simple_Name (Src_Name);
                     begin
                        Dependencies.Insert
                          (Before   => Dependency_Vectors.No_Element,
                           New_Item => (Name_Length  => Src_Simple'Length,
                                        Unit_Name    => Src_Simple,
                                        Unit_Kind    => GPR2.Unit.S_Spec,
                                        SFile_Length => Src_Simple'Length,
                                        Stamp        => No_Time,
                                        Checksum     => 0,
                                        Sfile        => Src_Simple),
                           Position => C_Dep);
                     end;
                  end if;

                  --  If the source path name ends the line, we are
                  --  done.

                  exit Line_Loop when Finish = Last;

                  --  Go get the next source on the line

                  Start := Finish + 1;
               end loop Name_Loop;
            end;

            --  If we are here, we had a continuation character \ at
            --  the end of the line, so we continue with the next
            --  line.

            Get_Line (Dep_File, Buffer, Compute.Last);
            Start  := 1;
            Finish := 1;
         end loop Line_Loop;

         --  Set Looping at the end of the first loop

         Looping := True;
      end loop Big_Loop;

      Data.Parsed := LI;
      Close (Dep_File);
   end Compute;

   --------------
   -- Unescape --
   --------------

   function Unescape (Path : String) return String is
      Result : String (Path'Range);
      Source : Natural := Path'First;
      Target : Integer := Path'First - 1;
   begin
      while Source <= Path'Last loop
         if Source < Path'Last
              and then
            Path (Source .. Source + 1) in "\\" | "\#" | "\ " | "\:" | "$$"
         then
            Source := Source + 1;
         end if;

         Target := Target + 1;
         Result (Target) := Path (Source);
         Source := Source + 1;
      end loop;

      return Result (Path'First .. Target);
   end Unescape;

begin
   GPR2.Source_Info.Parser.Registry.Register (C_Handle);
   GPR2.Source_Info.Parser.Registry.Register (CPP_Handle);
end GPR2.Source_Info.Parser.D;
