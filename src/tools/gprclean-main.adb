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

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.OS_Lib;

with GNATCOLL.Traces;
with GNATCOLL.Tribooleans;
with GNATCOLL.Utils;

with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Configuration;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Source;
with GPR2.Source_Reference;
with GPRclean.Options;
with GPRtools.Util;

procedure GPRclean.Main is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   use GNATCOLL.Tribooleans;

   use GPR2;
   use GPRtools;
   use GPR2.Path_Name;

   procedure Sources (View : Project.View.Object);
   --  Display view sources

   procedure Exclude_File (Name : String);
   --  Remove file if exists

   Project_Tree : Project.Tree.Object;
   Config       : Project.Configuration.Object;
   Options      : GPRclean.Options.Object;

   ------------------
   -- Exclude_File --
   ------------------

   procedure Exclude_File (Name : String) is
      use GNAT.OS_Lib;
      Success : Boolean := False;
   begin
      if Options.Dry_Run then
         if Is_Regular_File (Name) then
            Text_IO.Put_Line (Name);

         elsif Options.Verbose then
            Text_IO.Put_Line ("absent: " & Name);
         end if;

      else
         Delete_File (Name, Success);

         if not Options.Quiet and then Success then
            Text_IO.Put_Line ('"' & Name & """ has been deleted");

         elsif Options.Verbose and then not Success then
            Text_IO.Put_Line ('"' & Name & """ absent");
         end if;
      end if;
   end Exclude_File;

   -------------
   -- Sources --
   -------------

   procedure Sources (View : Project.View.Object) is
      Obj_Dir : constant Path_Name.Object := View.Object_Directory;
      Tree    : constant access Project.Tree.Object := View.Tree;

      pragma Warnings (Off);

      function "&" (Left, Right : Name_Type) return Name_Type renames GPR2."&";
      --  ??? work around a strange visibility issue

      procedure Binder_Artifacts
        (Name     : Name_Type;
         Language : Optional_Name_Type := No_Name);
      --  Add binder artefacts for the name

      ----------------------
      -- Binder_Artifacts --
      ----------------------

      procedure Binder_Artifacts
        (Name     : Name_Type;
         Language : Optional_Name_Type := No_Name)
      is
         use Ada.Text_IO;

         BF        : constant Path_Name.Full_Name :=
                       Obj_Dir.Compose
                         ((if Language = No_Name
                           then No_Name
                           else View.Binder_Prefix (Language)) & Name).Value;
         File      : File_Type;
         Generated : Boolean := False;

      begin
         if GNAT.OS_Lib.Is_Regular_File (BF) then
            Open (File, Mode => In_File, Name => BF);

            while not End_Of_File (File) loop
               declare
                  use GNATCOLL.Utils;
                  Line : constant String := Get_Line (File);
               begin
                  if Line (Line'First) = '[' then
                     Generated := Starts_With (Line, "[GENERATED ");
                  elsif Generated then
                     Exclude_File (Obj_Dir.Compose (Name_Type (Line)).Value);
                  end if;
               end;
            end loop;

            Exclude_File (BF);
            Close (File);
         end if;
      end Binder_Artifacts;

      Has_Mains : constant Boolean := View.Has_Mains;

   begin
      if Options.Verbose then
         Text_IO.Put_Line ("Cleaning project: """ & String (View.Name) & '"');
      end if;

      for C in View.Sources.Iterate (Project.Source.Set.S_Compilable) loop
         declare
            S : constant Project.Source.Object :=
                  Project.Source.Set.Element (C);
            Cleanup : Boolean := True;
            --  To disable cleanup if main files list exists and the main file
            --  is not from list.
            In_Mains : Boolean := False;
            Is_Main  : constant Boolean := Has_Mains and then S.Is_Main;
         begin
            if Options.Verbose then
               Text_IO.Put_Line ("source: " & S.Source.Path_Name.Value);
            end if;

            if Options.Mains.Contains
                 (String (S.Source.Path_Name.Simple_Name))
            then
               In_Mains := True;
               Options.Mains.Delete (String (S.Source.Path_Name.Simple_Name));
            end if;

            if Is_Main or else In_Mains then
               if Is_Main and then Options.Arg_Mains and then not In_Mains then
                  Cleanup := False;
               else
                  Binder_Artifacts
                    (S.Source.Path_Name.Base_Name & ".bexch",
                     Language => S.Source.Language);
               end if;
            end if;

            if Cleanup then
               for F of S.Artifacts.List loop
                  Exclude_File (F.Value);
               end loop;

               if not Options.Remain_Useful and then Options.Arg_Mains
                 and then In_Mains
               then
                  --  When we took main procedure filename from Main project
                  --  attributes, the executable file name included into
                  --  atrifacts list above. This case is when main procedure
                  --  filename defined in command line and we have to remove
                  --  the executable file separetely.

                  Exclude_File
                    (String (S.Source.Path_Name.Base_Name)
                     & View.Executable_Suffix);
               end if;
            end if;
         end;
      end loop;

      if Options.Arg_Mains and then not Options.Mains.Is_Empty then
         GPRtools.Util.Fail_Program
           ('"' & Options.Mains.First_Element
            & """ was not found in the sources of any project");
      end if;

      if not Options.Remain_Useful and then View.Has_Mains
        and then not Options.Arg_Mains
      then
         for M of View.Mains loop
            Exclude_File (M.Value);
         end loop;
      end if;

      declare
         Lexch : constant Name_Type := ".lexch";
         GI_DB : constant Name_Type := "gnatinspect.db";
      begin
         Exclude_File (Obj_Dir.Compose (GI_DB).Value);
         Exclude_File (Obj_Dir.Compose (GI_DB & "-shm").Value);
         Exclude_File (Obj_Dir.Compose (GI_DB & "-wal").Value);

         if Has_Mains or else Options.Arg_Mains then
            declare
               Main_Lib : constant Value_Type :=
                            Obj_Dir.Compose
                              ("lib" & View.Path_Name.Base_Name).Value;
            begin
               Exclude_File (Main_Lib & String (Tree.Archive_Suffix));
               Exclude_File (Main_Lib & ".deps");
            end;
         end if;

         if View.Is_Library then
            if View.Is_Aggregated_In_Library then
               Binder_Artifacts (View.Aggregate.Library_Name & Lexch);
            else
               if not Options.Remain_Useful then
                  Exclude_File (View.Library_Filename.Value);
               end if;

               Binder_Artifacts (View.Library_Name & Lexch);
            end if;
         end if;
      end;

      if Options.Remove_Empty_Dirs then
         declare
            use Ada.Directories;

            Attr : Project.Attribute.Object;

            procedure Delete_Dir (Dir : String);

            procedure Delete_If_Not_Project (Dir : Path_Name.Object);
            --  Delete the directory if it is not the directory where the
            --  project is resided.

            procedure Delete_Dir (Dir : String) is
               Search : Search_Type;
            begin
               if Kind (Dir) = Directory then
                  Delete_Directory (Dir);
               end if;
            exception
               when Use_Error =>
                  if not Options.Quiet then
                     Start_Search (Search, Dir, "");
                     Text_IO.Put_Line
                       ("warning: Directory """ & Dir
                        & """ could not be removed"
                        & (if More_Entries (Search)
                           then " because it is not empty"
                           else "") & '.');
                  end if;
            end Delete_Dir;

            procedure Delete_If_Not_Project (Dir : Path_Name.Object) is
            begin
               if Dir.Value /= View.Dir_Name.Value and then Dir.Exists then
                  declare
                     Dir_Name : constant String := Dir.Value;
                     DS       : Character renames
                                  GNAT.OS_Lib.Directory_Separator;
                  begin
                     Delete_Dir (Dir_Name);

                     if Options.Subdirs /= Null_Unbounded_String
                       and then String (Dir.Simple_Name) = Options.Subdirs
                     then
                        --  If subdirs is defined try to remove the parent one

                        pragma Assert
                          (Dir_Name
                             (Dir_Name'Last - Length (Options.Subdirs)
                              - Boolean'Pos (Dir_Name (Dir_Name'Last) = DS))
                           = DS,
                           Dir_Name);

                        Delete_Dir
                          (Dir_Name (Dir_Name'First
                           .. Dir_Name'Last - Length (Options.Subdirs) - 1));
                     end if;
                  end;
               end if;
            end Delete_If_Not_Project;

         begin
            if View.Kind in K_Standard | K_Library | K_Aggregate_Library then
               Delete_If_Not_Project (View.Object_Directory);

               if View.Executable_Directory /= View.Object_Directory then
                  Delete_If_Not_Project (View.Executable_Directory);
               end if;
            end if;

            if View.Is_Library then
               Delete_If_Not_Project (View.Library_Directory);

               if View.Library_Directory /= View.Library_Ali_Directory then
                  Delete_If_Not_Project (View.Library_Ali_Directory);
               end if;

               if View.Check_Attribute
                 (Project.Registry.Attribute.Library_Src_Dir, Result => Attr)
               then
                  Delete_If_Not_Project
                    (Path_Name.Create_Directory (Name_Type (Attr.Value.Text)));
               end if;
            end if;
         end;
      end if;
   end Sources;

begin
   GNATCOLL.Traces.Parse_Config_File;
   GPRtools.Util.Set_Program_Name ("gprclean");

   Options.Parse_Command_Line (Project_Tree);

   if Options.Version then
      return;
   end if;

   if Options.Config_File.Is_Defined then
      Config := Project.Configuration.Load
        (Options.Config_File, Name_Type (To_String (Options.Target)));

      Project_Tree.Load
        (Options.Project_Path, Options.Context, Config, Options.Build_Path,
         Optional_Name_Type (To_String (Options.Subdirs)),
         Check_Shared_Lib => not Options.Unchecked_Shared_Lib_Import,
         Implicit_Project => Options.Implicit_Proj);

   else
      Project_Tree.Load_Autoconf
        (Options.Project_Path, Options.Context, Options.Build_Path,
         Optional_Name_Type (To_String (Options.Subdirs)),
         Check_Shared_Lib => not Options.Unchecked_Shared_Lib_Import,
         Implicit_Project => Options.Implicit_Proj);
   end if;

   if Project_Tree.Root_Project.Is_Library and then Options.Arg_Mains then
      Project_Tree.Log_Messages.Append
        (GPR2.Message.Create
           (GPR2.Message.Error,
            "main cannot be a source of a library project: """
            & Options.Mains.First_Element & '"',
            GPR2.Source_Reference.Create (Options.Project_Path.Value, 0, 0)));

      Util.Output_Messages
        (Project_Tree.Log_Messages.all, Options.Verbosity,
         Options.Full_Path_Name_For_Brief);

      GPRtools.Util.Fail_Program ("problems with main sources");
   end if;

   for V in Project_Tree.Iterate
     (Kind   => (Project.I_Recursive  => Options.All_Projects,
                 Project.I_Imported   => Options.All_Projects,
                 Project.I_Aggregated => Options.All_Projects, others => True),
      Status => (Project.S_Externally_Built => False),
      Filter => (Project.F_Abstract | Project.F_Aggregate => False,
                 others => True))
   loop
      Sources (Project.Tree.Element (V));
   end loop;

   if Options.Remove_Config then
      Exclude_File (Options.Config_File.Value);
   end if;

   Util.Output_Messages
     (Project_Tree.Log_Messages.all, Options.Verbosity,
      Options.Full_Path_Name_For_Brief);

exception
   when E : GNAT.Command_Line.Exit_From_Command_Line
      | GNAT.Command_Line.Invalid_Switch
      | GNAT.Command_Line.Invalid_Parameter
      | GPRtools.Usage_Error =>
      GPRtools.Util.Fail_Program (Exception_Message (E));

   when Project_Error =>
      GPRtools.Util.Project_Processing_Failed
        (Project_Tree, Options.Verbosity, Options.Full_Path_Name_For_Brief);

   when E : others =>
      GPRtools.Util.Fail_Program
        ("cannot parse project: " & Exception_Information (E));
end GPRclean.Main;
