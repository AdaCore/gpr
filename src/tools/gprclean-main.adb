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

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
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
with GPR2.Project.Registry.Pack;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Source;
with GPR2.Source_Reference;
with GPR2.Unit;

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
   package PRP renames GPR2.Project.Registry.Pack;

   procedure Clean (View : Project.View.Object);
   --  Clean given View

   function To_Optional_Name
     (Item : Unbounded_String) return Optional_Name_Type
   is
     (Optional_Name_Type (To_String (Item)));
   --  Convert Unboounded_String to Optional_Name_Type

   Project_Tree : Project.Tree.Object;
   Config       : Project.Configuration.Object;
   Options      : GPRclean.Options.Object;

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

      Obj_Dir : constant Path_Name.Object := View.Object_Directory;
      Tree    : constant access Project.Tree.Object := View.Tree;
      Opts    : GPRclean.Options.Object;

      pragma Warnings (Off);

      function "&" (Left, Right : Name_Type) return Name_Type renames GPR2."&";
      --  ??? work around a strange visibility issue

      pragma Warnings (On);

      procedure Binder_Artifacts
        (Name     : Name_Type;
         Language : Optional_Name_Type := No_Name);
      --  Add binder artefacts for the name

      procedure Delete_File (Name : Path_Name.Full_Name);
      --  Delete file with specific for this project options

      function Partial_Path
        (View         : Project.View.Object;
         Library_Name : Name_Type;
         Number       : Natural) return Path_Name.Object;
      --  return partially linked object

      function Linker_Options
        (View        : Project.View.Object;
         Library_Name : Name_Type) return Path_Name.Object;
      --  return linker options path

      ----------------------
      -- Binder_Artifacts --
      ----------------------

      procedure Binder_Artifacts
        (Name     : Name_Type;
         Language : Optional_Name_Type := No_Name) is
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
         Main.Delete_File (Name, Opts);
      end Delete_File;

      Has_Mains : constant Boolean := View.Has_Mains;
      Attr      : Project.Attribute.Object;

      --------------------
      -- Linker_Options --
      --------------------

      function Linker_Options
        (View         : Project.View.Object;
         Library_Name : Name_Type) return Path_Name.Object is
      begin
         return View.Object_Directory.Compose
           (Library_Name & ".linker_options");
      end Linker_Options;

      ------------------
      -- Partial_Path --
      ------------------

      function Partial_Path
        (View         : Project.View.Object;
         Library_Name : Name_Type;
         Number       : Natural) return Path_Name.Object
      is
      begin
         return View.Object_Directory.Compose
           (GPRtools.Util.Partial_Name
              (Library_Name, Number, View.Tree.Object_Suffix));
      end Partial_Path;

   begin
      --  Check for additional switches in Clean package

      if View.Has_Packages (PRP.Clean)
        and then View.Pack (PRP.Clean).Check_Attribute
                   (PRA.Switches, Result => Attr)
      then
         declare
            use GNAT.Command_Line;

            List : constant GPR2.Containers.Source_Value_List :=  Attr.Values;
            Args : aliased GNAT.OS_Lib.Argument_List :=
                     (List.First_Index .. List.Last_Index => null);
            OP   : Opt_Parser;
         begin
            for J in Args'Range loop
               Args (J) := new String'(List (J).Text);
            end loop;

            Initialize_Option_Scan (OP, Args'Unchecked_Access);

            GPRclean.Options.Parse_Command_Line (Opts, Project_Tree, OP);

            for J in Args'Range loop
               GNAT.OS_Lib.Free (Args (J));
            end loop;

         exception
            when E : GNAT.Command_Line.Exit_From_Command_Line
               | GNAT.Command_Line.Invalid_Switch
               | GNAT.Command_Line.Invalid_Parameter
               | GPRtools.Usage_Error
               =>
               Text_IO.Put_Line
                 (Text_IO.Current_Error,
                  "gprclean: " & Exception_Message (E) & " in package Clean");
         end;

         Opts.Append (Options);

      else
         Opts := Options;
      end if;

      if Opts.Verbose then
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

      for C in View.Sources (Need_Update => False).Iterate loop
         declare
            S       : constant Project.Source.Object :=
                        Project.Source.Set.Element (C);
            Cleanup : Boolean := True;
            --  To disable cleanup if main files list exists and the main file
            --  is not from list.
            In_Mains : Boolean := False;
            Is_Main  : constant Boolean := Has_Mains and then S.Is_Main;
         begin
            if Opts.Verbose then
               Text_IO.Put_Line ("source: " & S.Source.Path_Name.Value & ' '
                                 & S.Is_Aggregated'Img);
            end if;

            if Opts.Mains.Contains
                 (String (S.Source.Path_Name.Simple_Name))
            then
               In_Mains := True;
               Opts.Args.Delete (String (S.Source.Path_Name.Simple_Name));
            end if;

            if Is_Main or else In_Mains then
               if Is_Main and then Opts.Arg_Mains and then not In_Mains then
                  Cleanup := False;

               elsif S.Source.Language = "Ada"
                 and then not S.Source.Has_Single_Unit
               then
                  for CU of S.Source.Units loop
                     if CU.Kind in Unit.Body_Kind then
                        Binder_Artifacts
                          (S.Source.Path_Name.Base_Name
                           & Name_Type ('~' & Image (CU.Index, 1)),
                           Language => S.Source.Language);
                     end if;
                  end loop;

               elsif not View.Is_Library then
                  Binder_Artifacts
                    (S.Source.Path_Name.Base_Name,
                     Language => S.Source.Language);
               end if;
            end if;

            if Cleanup then
               for F of S.Artifacts.List loop
                  Delete_File (F.Value);
               end loop;

               if not Opts.Remain_Useful and then Opts.Arg_Mains
                 and then In_Mains
               then
                  --  When we took main procedure filename from Main project
                  --  attributes, the executable file name included into
                  --  View.Mains below. This case is when main procedure
                  --  filename defined in command line and we have to remove
                  --  the executable file separetely.

                  Delete_File
                    (Path_Name.Create_File
                       (S.Source.Path_Name.Base_Name
                        & View.Executable_Suffix,
                        Name_Type (View.Executable_Directory.Value)).Value);
               end if;
            end if;
         end;
      end loop;

      if Opts.Arg_Mains and then not Opts.Mains.Is_Empty then
         GPRtools.Util.Fail_Program
           ('"' & Opts.Mains.First_Element
            & """ was not found in the sources of any project");
      end if;

      if not Opts.Remain_Useful
        and then View.Has_Mains
        and then not Opts.Arg_Mains
      then
         for M of View.Mains loop
            Delete_File (M.Value);
         end loop;
      end if;

      for A of View.Artifacts loop
         Delete_File (A.Value);
      end loop;

      if Has_Mains or else Opts.Arg_Mains then
         declare
            Main_Lib : constant Value_Type :=
                         Obj_Dir.Compose
                           ("lib" & View.Path_Name.Base_Name).Value;
         begin
            Delete_File (Main_Lib & String (Tree.Archive_Suffix));
            Delete_File (Main_Lib & ".deps");
         end;
      end if;

      if View.Is_Library then
         if View.Is_Aggregated_In_Library then
            Binder_Artifacts (View.Aggregate.Library_Name);
         else
            if not Opts.Remain_Useful then
               Delete_File (View.Library_Filename.Value);
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

      --  Removes empty directories

      if Opts.Remove_Empty_Dirs then
         declare
            use Ada.Directories;

            Attr : Project.Attribute.Object;

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
                     if Opts.Warnings then
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

                     if Opts.Subdirs /= Null_Unbounded_String
                       and then String (Dir.Simple_Name) = Opts.Subdirs
                     then
                        --  If subdirs is defined try to remove the parent one

                        pragma Assert
                          (Dir_Name
                             (Dir_Name'Last - Length (Opts.Subdirs)
                              - Boolean'Pos (Dir_Name (Dir_Name'Last) = DS))
                           = DS,
                           Dir_Name);

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

               if View.Check_Attribute
                 (Project.Registry.Attribute.Library_Src_Dir, Result => Attr)
               then
                  Delete_If_Not_Project
                    (Path_Name.Create_Directory (Name_Type (Attr.Value.Text)));
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

            if Opts.Verbosity > Quiet then
               if Success then
                  Text_IO.Put_Line ('"' & Name & """ has been deleted");
               else
                  Text_IO.Put_Line
                    ("Warning: """ & Name & """ could not be deleted");
               end if;
            end if;
         end if;
      elsif Opts.Verbosity >= Verbose then
         Text_IO.Put_Line ('"' & Name & """ absent");
      end if;
   end Delete_File;

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

      if Config.Has_Error then
         Util.Output_Messages (Options, Config.Log_Messages);
         GPRtools.Util.Fail_Program
           ('"' & String (Options.Config_File.Name) & """ processing failed");
      end if;

      Project_Tree.Load
        (Options.Project_File, Options.Context, Config,
         Project_Dir      => Options.Project_Base,
         Build_Path       => Options.Build_Path,
         Subdirs          => To_Optional_Name (Options.Subdirs),
         Src_Subdirs      => To_Optional_Name (Options.Src_Subdirs),
         Check_Shared_Lib => not Options.Unchecked_Shared_Lib,
         Implicit_With    => Options.Implicit_With);

   else
      Project_Tree.Load_Autoconf
        (Options.Project_File, Options.Context,
         Project_Dir       => Options.Project_Base,
         Build_Path        => Options.Build_Path,
         Subdirs           => To_Optional_Name (Options.Subdirs),
         Src_Subdirs       => To_Optional_Name (Options.Src_Subdirs),
         Check_Shared_Lib  => not Options.Unchecked_Shared_Lib,
         Target            => Name_Type (To_String (Options.Target)),
         Language_Runtimes => Options.RTS_Map,
         Implicit_With     => Options.Implicit_With,
         Default_KB        => not Options.Skip_Default_KB,
         Custom_KB         => Options.KB_Locations);

      if Project_Tree.Configuration.Log_Messages.Has_Element
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
   end if;

   if Project_Tree.Root_Project.Is_Library and then Options.Arg_Mains then
      Project_Tree.Log_Messages.Append
        (GPR2.Message.Create
           (GPR2.Message.Error,
            "main cannot be a source of a library project: """
            & Options.Mains.First_Element & '"',
            Source_Reference.Create
              (Project_Tree.Root_Project.Path_Name.Value, 0, 0)));

      Util.Output_Messages (Options);

      GPRtools.Util.Fail_Program ("problems with main sources");
   end if;

   Project_Tree.Update_Sources;

   --  Iterate on all view, and clean them

   for V in Project_Tree.Iterate
     (Kind   => (Project.I_Recursive  => Options.All_Projects,
                 Project.I_Imported   => Options.All_Projects,
                 Project.I_Aggregated => Options.All_Projects
                                         or else Project_Tree.Root_Project.Kind
                                                 /= K_Aggregate_Library,
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

   if Options.Remove_Config then
      Delete_File (Options.Config_File.Value, Options);
   end if;

   Util.Output_Messages (Options);

exception
   when GNAT.Command_Line.Exit_From_Command_Line =>
      null;

   when GNAT.Command_Line.Invalid_Switch =>
      GPRtools.Util.Fail_Program ("");

   when E : GNAT.Command_Line.Invalid_Parameter
      | GPRtools.Usage_Error =>
      GPRtools.Util.Fail_Program (Exception_Message (E));

   when Project_Error | Processing_Error =>
      GPRtools.Util.Project_Processing_Failed (Options);

   when E : others =>
      GPRtools.Util.Fail_Program
        ("cannot parse project: " & Exception_Information (E));
end GPRclean.Main;
