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

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.OS_Lib;

with GNATCOLL.Traces;
with GNATCOLL.Tribooleans;
with GNATCOLL.Utils;

with GPR2.Compilation.Registry;
with GPR2.Containers;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Configuration;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Source.Artifact;

pragma Warnings (Off, "unit ""GPR2.Project.Source.Set"" is not referenced");
--  This pragma need to workaround GNAT bug U622-047 when unit is necessary,
--  but when set, issues warning that it is not referenced.
--
with GPR2.Project.Source.Set;
--  Without this import
--  gprclean-main.adb:386:20: error: cannot call function that returns limited
--             view of type "Object" defined at gpr2-project-source-set.ads:32
--  gprclean-main.adb:386:20: error: there must be a regular with_clause for
--            package "Set" in the current unit, or in some unit in its context
pragma Warnings (On);

with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Source_Reference;
with GPR2.Unit;

with GPRtools.Command_Line;
with GPRtools.Options;
with GPRtools.Util;

with GPRclean.Options;

procedure GPRclean.Main is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   use GNATCOLL.Tribooleans;

   use GPR2;
   use GPRtools;
   use GPR2.Path_Name;
   use all type Unit.Library_Unit_Type;

   package PRA renames GPR2.Project.Registry.Attribute;

   procedure Clean (View : Project.View.Object);
   --  Clean given View

   Project_Tree : Project.Tree.Object;
   Options      : GPRclean.Options.Object;
   Parser       : GPRtools.Options.Command_Line_Parser;

   procedure Delete_File
     (Name : Path_Name.Full_Name; Opts : GPRclean.Options.Object);
   --  Remove file if exists.
   --  Opts parameter need because the command line options is used inside of
   --  this routine and could be different for different projects because of
   --  Switches attribute in project package Clean.

   -----------
   -- Clean --
   -----------

   procedure Clean (View : Project.View.Object) is
      use GNATCOLL.Utils;

      Obj_Dir     : constant Path_Name.Object := View.Object_Directory;
      Tree        : constant access Project.Tree.Object := View.Tree;
      Lib_Dir     : constant GPR2.Path_Name.Object :=
                      (if View.Is_Library
                       then View.Library_Directory
                       else GPR2.Path_Name.Undefined);
      Lib_Ali_Dir : constant GPR2.Path_Name.Object :=
                      (if Lib_Dir.Is_Defined
                       and then View.Library_Ali_Directory /= Lib_Dir
                       then View.Library_Ali_Directory
                       else GPR2.Path_Name.Undefined);
      Lib_Src_Dir : constant GPR2.Path_Name.Object :=
                      (if Lib_Dir.Is_Defined
                       and then View.Library_Src_Directory /= Lib_Dir
                       and then View.Library_Src_Directory /= Lib_Ali_Dir
                       then View.Library_Src_Directory
                       else GPR2.Path_Name.Undefined);
      pragma Warnings (Off);

      function "&" (Left, Right : Name_Type) return Name_Type renames GPR2."&";
      --  ??? work around a strange visibility issue

      pragma Warnings (On);

      procedure Binder_Artifacts
        (Name     : GPR2.Simple_Name;
         Language : Language_Id := No_Language);
      --  Add binder artefacts for the name

      procedure Delete_File (Name : Path_Name.Full_Name);
      --  Delete file with specific for this project options

      function Partial_Path
        (View         : Project.View.Object;
         Library_Name : GPR2.Simple_Name;
         Number       : Natural) return Path_Name.Object;
      --  return partially linked object

      function Linker_Options
        (View         : Project.View.Object;
         Library_Name : GPR2.Simple_Name) return Path_Name.Object;
      --  return linker options path

      function In_Library_Directories
        (File : Path_Name.Object) return Boolean;
      --  return true if view is a library and File is in library_dir,
      --  library_ali_dir or library_src_dir directories

      ----------------------
      -- Binder_Artifacts --
      ----------------------

      procedure Binder_Artifacts
        (Name     : GPR2.Simple_Name;
         Language : Language_Id := No_Language) is
      begin
         for F of View.Binder_Artifacts (Name, Language) loop
            Delete_File (F.Value);
         end loop;
      end Binder_Artifacts;

      -----------------
      -- Delete_File --
      -----------------

      procedure Delete_File (Name : Path_Name.Full_Name) is
      begin
         Main.Delete_File (Name, Options);
      end Delete_File;

      ----------------------------
      -- In_Library_Directories --
      ----------------------------

      function In_Library_Directories
        (File : Path_Name.Object) return Boolean
      is
      begin
         if Lib_Dir.Is_Defined then
            declare
               Parent : constant Path_Name.Object :=
                          Create_Directory (Filename_Type (Dir_Name (File)));
            begin
               return Parent = Lib_Dir
                 or else Parent = Lib_Ali_Dir
                 or else Parent = Lib_Src_Dir;
            end;
         else
            return False;
         end if;
      end In_Library_Directories;

      Has_Mains : constant Boolean := View.Has_Mains;

      --------------------
      -- Linker_Options --
      --------------------

      function Linker_Options
        (View         : Project.View.Object;
         Library_Name : GPR2.Simple_Name) return Path_Name.Object is
      begin
         return View.Object_Directory.Compose
           (Library_Name & ".linker_options");
      end Linker_Options;

      ------------------
      -- Partial_Path --
      ------------------

      function Partial_Path
        (View         : Project.View.Object;
         Library_Name : GPR2.Simple_Name;
         Number       : Natural) return Path_Name.Object
      is
      begin
         return View.Object_Directory.Compose
           (GPRtools.Util.Partial_Name
              (Library_Name, Number, View.Tree.Object_Suffix));
      end Partial_Path;

      Mains_In_View : GPR2.Containers.Filename_Set;
      --  mains in cmd line found in this view

   begin
      if Options.Verbose then
         Text_IO.Put_Line ("Cleaning project: """ & String (View.Name) & '"');
      end if;

      if View.Object_Directory.Is_Defined
        and then View.Object_Directory.Is_Directory
      then
         if not View.Is_Library then
            if View.Has_Imports then
               for Import of View.Imports (True) loop
                  if Import.Is_Library
                    and then Import.Is_Library_Standalone
                  then
                     declare
                        Link_Opt : constant Path_Name.Object :=
                                     Linker_Options (View,
                                                     Import.Library_Name);
                        Partial  : constant Path_Name.Object :=
                                     Partial_Path (View,
                                                   Import.Library_Name, 0);
                     begin
                        if Link_Opt.Exists then
                           Delete_File (Link_Opt.Value);
                        end if;

                        if Partial.Exists then
                           Delete_File (Partial.Value);
                        end if;
                     end;
                  end if;
               end loop;
            end if;

         else
            --  For a library project, clean the partially link objects, if
            --  there are some.
            declare
               use GPR2.Project;

               Partial_Number : Natural := 0;
            begin
               loop
                  declare
                     Partial : constant Path_Name.Object :=
                                 Partial_Path (View,
                                               View.Library_Name,
                                               Partial_Number);
                  begin
                     if Partial.Exists then
                        Delete_File (Partial.Value);
                        Partial_Number := Partial_Number + 1;
                     else
                        exit;
                     end if;
                  end;
               end loop;

               --  For a static SAL, clean the .linker_options file which
               --  exists if the latest build was done in "keep temp files"
               --  mode.

               if View.Library_Standalone /= No
                 and then View.Is_Static_Library
               then
                  declare
                     Link_Opt : constant Path_Name.Object :=
                                  Linker_Options (View, View.Library_Name);
                  begin
                     if Link_Opt.Exists then
                        Delete_File (Link_Opt.Value);
                     end if;
                  end;
               end if;
            end;
         end if;
      end if;

      --  Handle mains without spec/body extension

      declare
         Removed_Mains : GPR2.Containers.Filename_Set;
      begin
         --  Add found sources to Found_Mains

         for M of Options.Mains loop
            declare
               Main : constant GPR2.Path_Name.Object :=
                        View.Source_Path
                          (Name            => M,
                           Allow_Spec_File => True,
                           Allow_Unit_Name => False);
            begin
               if Main.Is_Defined then
                  declare
                     Position : GPR2.Containers.Filename_Type_Set.Cursor;
                     Inserted : Boolean;
                  begin
                     Mains_In_View.Insert
                       (New_Item => Filename_Type (Main.Simple_Name),
                        Position => Position,
                        Inserted => Inserted);
                     if Inserted then
                        Removed_Mains.Insert (M);
                     end if;
                  end;
               end if;
            end;
         end loop;

         --- Remove found mains from Mains & Mains_In_Cmd

         for Arg of Removed_Mains loop
            Options.Mains.Delete (Arg);
         end loop;

      end;

      for S of View.Sources loop
         declare
            Cleanup : Boolean := True;
            --  To disable cleanup if main files list exists and the main file
            --  is not from list.
            In_Mains : Boolean := False;
            Is_Main  : constant Boolean := Has_Mains and then S.Is_Main;
            C_Main   : Containers.Filename_Type_Set.Cursor :=
                         Mains_In_View.Find (S.Path_Name.Simple_Name);
         begin
            --  Remove source simple name from Options.Mains as all Mains found
            --  is handled at Tree level not View level.

            if Containers.Filename_Type_Set.Has_Element (C_Main) then
               In_Mains := True;
               Mains_In_View.Delete (C_Main);
            end if;

            if Is_Main or else In_Mains then
               if Is_Main and then Options.Arg_Mains and then not In_Mains then
                  Cleanup := False;

               elsif S.Language = Ada_Language
                 and then not S.Has_Single_Unit
               then
                  for CU of S.Units loop
                     if CU.Kind in Unit.Body_Kind then
                        Binder_Artifacts
                          (S.Path_Name.Base_Filename
                           & Filename_Type
                             ('~' & Image (Positive (CU.Index), 1)),
                           Language => S.Language);
                     end if;
                  end loop;

               elsif not View.Is_Library then
                  Binder_Artifacts
                    (S.Path_Name.Base_Filename,
                     Language => S.Language);
               end if;
            end if;

            if Cleanup then
               for F of S.Artifacts.List_To_Clean loop
                  --  As S.Artifacts contains also files generated in library
                  --  directories, check if delete file is allowed

                  if not Options.Remain_Useful
                    or else not In_Library_Directories (F)
                  then
                     Delete_File (F.Value);
                  end if;
               end loop;

               if not Options.Remain_Useful and then Options.Arg_Mains
                 and then In_Mains
               then
                  --  When we took main procedure filename from Main project
                  --  attributes, the executable file name included into
                  --  View.Mains below. This case is when main procedure
                  --  filename defined in command line and we have to remove
                  --  the executable file separately.

                  for U of S.Units loop
                     Delete_File
                       (View.Executable
                          (S.Path_Name.Simple_Name,
                           U.Index).Value);
                  end loop;
               end if;
            end if;
         end;
      end loop;

      if not Options.Remain_Useful
        and then View.Has_Mains
        and then not Options.Arg_Mains
      then
         for M of View.Executables loop
            Delete_File (M.Value);
         end loop;
      end if;

      for A of View.Artifacts loop
         Delete_File (A.Value);
      end loop;

      if Has_Mains or else Options.Arg_Mains then
         declare
            Main_Lib : constant Value_Type :=
                         Obj_Dir.Compose
                           ("lib" &
                             Filename_Type (To_Lower (View.Name))).Value;
         begin
            Delete_File (Main_Lib & String (Tree.Archive_Suffix));
            Delete_File (Main_Lib & ".deps");
         end;
      end if;

      if View.Is_Library and then not Options.Remain_Useful then
         --  All library generated files should be deleted

         if View.Is_Aggregated_In_Library then
            for Lib of View.Aggregate_Libraries loop
               Binder_Artifacts (Lib.Library_Name);
            end loop;
         else
            Delete_File (View.Library_Filename.Value);

            --  Delete if any library version & library major version

            if not View.Is_Static_Library and then View.Has_Library_Version
            then
               if View.Library_Version_Filename
                 /= View.Library_Major_Version_Filename
               then
                  --  When library version attribute is provided, to keep all
                  --  links seen as regular file, link target should be deleted
                  --  after the link. Library binary files should be deleted
                  --  starting with Library_Filename and ending with
                  --  Library_Version_Filename

                  if View.Library_Major_Version_Filename
                    /= View.Library_Filename
                  then
                     Delete_File (View.Library_Major_Version_Filename.Value);
                  end if;

                  if View.Library_Version_Filename
                    /= View.Library_Major_Version_Filename
                  then
                     Delete_File (View.Library_Version_Filename.Value);
                  end if;
               end if;
            end if;

            Binder_Artifacts (View.Library_Name);
         end if;
      end if;

      if Options.Src_Subdirs /= Null_Unbounded_String
        and then View.Kind in K_Standard | K_Library | K_Aggregate_Library
      then
         declare
            use Ada.Directories;
            Search : Search_Type;
            File   : Directory_Entry_Type;
         begin
            Start_Search
              (Search, View.Source_Subdirectory.Value, "",
               Filter => (Ordinary_File => True, others => False));

            while More_Entries (Search) loop
               Get_Next_Entry (Search, File);
               Delete_File (Ada.Directories.Full_Name (File));
            end loop;
         end;
      end if;

      --  Delete source files found in library source directory

      if View.Is_Library and then View.Library_Src_Directory.Is_Defined
        and then not Options.Remain_Useful
      then
         declare
            Lib_Src_Dir : constant GPR2.Path_Name.Object :=
                            View.Library_Src_Directory;
         begin
            for Source of View.Sources loop
               declare
                  F : constant GPR2.Path_Name.Object :=
                        Lib_Src_Dir.Compose (Source.Path_Name.Simple_Name);
               begin
                  if F.Exists then
                     Delete_File (F.Value);
                  end if;
               end;
            end loop;
         end;
      end if;

      --  Removes empty directories

      if Options.Remove_Empty_Dirs then
         declare
            use Ada.Directories;

            procedure Delete_Dir (Dir : Value_Not_Empty);
            --  Delete directory if a directory

            procedure Delete_If_Not_Project (Dir : Path_Name.Object);
            --  Delete the directory if it is not the directory where the
            --  project is resided.

            ----------------
            -- Delete_Dir --
            ----------------

            procedure Delete_Dir (Dir : Value_Not_Empty) is
            begin
               if Kind (Dir) = Directory then
                  Delete_Directory (Dir);
               end if;
            exception
               when Use_Error =>
                  declare
                     Search : Search_Type;
                  begin
                     if Options.Warnings then
                        Start_Search (Search, Dir, "");
                        Text_IO.Put_Line
                          ("warning: Directory """ & Dir
                           & """ could not be removed"
                           & (if More_Entries (Search)
                             then " because it is not empty"
                             else "") & '.');
                     end if;
                  end;
            end Delete_Dir;

            ---------------------------
            -- Delete_If_Not_Project --
            ---------------------------

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
                           "invalid parent directory " & Dir_Name);

                        Delete_If_Not_Project (Dir.Containing_Directory);
                     end if;
                  end;
               end if;
            end Delete_If_Not_Project;

         begin
            if View.Kind in K_Standard | K_Library | K_Aggregate_Library then
               if Options.Src_Subdirs /= Null_Unbounded_String then
                  Delete_If_Not_Project (View.Source_Subdirectory);
               end if;

               Delete_If_Not_Project (Obj_Dir);

               if View.Executable_Directory /= Obj_Dir then
                  Delete_If_Not_Project (View.Executable_Directory);
               end if;
            end if;

            if View.Is_Library then
               Delete_If_Not_Project (View.Library_Directory);

               if View.Library_Directory /= View.Library_Ali_Directory then
                  Delete_If_Not_Project (View.Library_Ali_Directory);
               end if;

               if View.Library_Directory /= View.Library_Src_Directory
                 and then View.Library_Ali_Directory
                   /= View.Library_Src_Directory
               then
                  Delete_If_Not_Project (View.Library_Src_Directory);
               end if;
            end if;
         end;
      end if;
   end Clean;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File
     (Name : Path_Name.Full_Name; Opts : GPRclean.Options.Object)
   is
      use GNAT.OS_Lib;
      Success : Boolean := False;
   begin
      if Is_Regular_File (Name) then
         if Opts.Dry_Run then
            Text_IO.Put_Line (Name);

         else
            if GNAT.OS_Lib.Is_Owner_Writable_File (Name) then
               Delete_File (Name, Success);

            elsif Opts.Force_Deletions then
               GNAT.OS_Lib.Set_Writable (Name);
               Delete_File (Name, Success);
            end if;

            if Success then
               if Opts.Verbosity > Regular then
                  Text_IO.Put_Line ('"' & Name & """ has been deleted");
               end if;

            elsif Opts.Verbosity > Quiet then
               Text_IO.Put_Line
                 ("Warning: """ & Name & """ could not be deleted");
            end if;
         end if;
      end if;
   end Delete_File;

begin
   GNATCOLL.Traces.Parse_Config_File;
   GPRtools.Util.Set_Program_Name ("gprclean");
   GPRclean.Options.Setup (Parser);

   Options.Tree := Project_Tree.Reference;

   GPRclean.Options.Parse_Command_Line (Parser, Options);

   if not Options.Load_Project
     (Absent_Dir_Error   => False,
      Handle_Information => Options.Verbose,
      Handle_Lint        => Options.Verbose)
   then
      GPRtools.Util.Fail_Program
        ('"'
         & (if Options.Project_File.Is_Defined
           then String (Options.Project_File.Simple_Name)
           else Options.Project_Base.Value)
         & """ processing failed");
   end if;

   --  Check gprclean's Switch attribute from loaded project

   declare
      Attr : constant GPR2.Project.Attribute.Object :=
               Project_Tree.Root_Project.Attribute (PRA.Clean.Switches);
   begin
      if Attr.Is_Defined then
         Options := (GPRtools.Command_Line.Empty_Result
                     with others => <>);
         GPRclean.Options.Parse_Attribute_Switches
           (Parser, Options, Attr.Values);

         --  re-parse the command line to allow it to overwrite project
         --  defined Switches attribute.

         GPRclean.Options.Parse_Command_Line (Parser, Options);

         --  Note that we never need to reload the tree, as we ensured that
         --  no switch modifying the configuration of the project or the
         --  way we load the project tree is allowed in the Switches
         --  attribute.
      end if;
   exception
      when E : Usage_Error =>
         GPRtools.Util.Finish_Program
           (GPRtools.Util.E_Fatal, Exception_Message (E));
   end;

   if Project_Tree.Has_Configuration
     and then Project_Tree.Configuration.Log_Messages.Has_Element
       (Warning     => True,
        Information => False,
        Error       => False)
   then
      Project_Tree.Log_Messages.Append
        (GPR2.Message.Create
           (GPR2.Message.Warning,
            "Cleaning may be incomplete, as there were problems during"
            & " auto-configuration",
            Source_Reference.Create
              (Project_Tree.Root_Project.Path_Name.Value, 0, 0),
            Raw => True));
   end if;

   if Project_Tree.Root_Project.Is_Library and then Options.Arg_Mains then
      Project_Tree.Log_Messages.Append
        (GPR2.Message.Create
           (GPR2.Message.Error,
            "main cannot be a source of a library project: """
            & String (Options.Mains.First_Element) & '"',
            Source_Reference.Create
              (Project_Tree.Root_Project.Path_Name.Value, 0, 0)));

      Util.Output_Messages (Options);

      GPRtools.Util.Fail_Program ("problems with main sources");
   end if;

   Project_Tree.Update_Sources;

   --  Iterate on all view, and clean them

   for V in Project_Tree.Iterate
     (Kind   => (Project.I_Recursive     => Options.All_Projects,
                 Project.I_Imported      => Options.All_Projects,
                 Project.I_Aggregated    => Options.All_Projects
                                         or else Project_Tree.Root_Project.Kind
                                                 /= K_Aggregate_Library,
                 Project.I_Runtime       => False,
                 Project.I_Configuration => False,
                 others => True),
      Status => (Project.S_Externally_Built => False),
      Filter => (Project.F_Abstract | Project.F_Aggregate => False,
                 others => True))
   loop
      Clean (Project.Tree.Element (V));

      if Options.Distributed_Mode then
         GPR2.Compilation.Registry.Clean_Up_Remote_Slaves
           (Project.Tree.Element (V),
            Options);
      end if;
   end loop;

   if Options.Arg_Mains and then not Options.Mains.Is_Empty then
      GPRtools.Util.Fail_Program
        ('"' & String (Options.Mains.First_Element)
         & """ was not found in the sources of any project");
   end if;

   if Options.Remove_Config then
      Delete_File (Options.Config_Project.Value, Options);
   end if;

   Util.Output_Messages (Options);

exception
   when E : GPRtools.Usage_Error =>
      Text_IO.Put_Line
        (Text_IO.Standard_Error,
         "gprclean: " & Exception_Message (E));
      GPRtools.Command_Line.Try_Help;
      GPRtools.Util.Exit_Program (GPRtools.Util.E_Fatal);

   when Project_Error | Processing_Error =>
      GPRtools.Util.Project_Processing_Failed (Options);

   when E : others =>
      GPRtools.Util.Fail_Program
        ("Fatal error: " & Exception_Information (E));
end GPRclean.Main;
