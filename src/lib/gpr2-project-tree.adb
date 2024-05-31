--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Directories;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;
with GNATCOLL.Atomic;

with GPR2.Message.Reporter;
with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;

package body GPR2.Project.Tree is

   procedure Release is new Ada.Unchecked_Deallocation
     (Tree_Internal.Object, Tree_Internal_Access);

   function Get (Self : Object) return Tree_Internal.Object_Access is
     (Tree_Internal.Object_Access (Self.Tree));

   function Set (Tree : Tree_Internal.Object_Access) return Object;

   function Check_For_Default_Project
     (Directory : String := "") return GPR2.Path_Name.Object;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Object) is
      R : access GNATCOLL.Refcount.Counters;
   begin
      if Self.Tree = null then
         return;
      end if;

      R := Pools.Header_Of (Self.Tree);

      GNATCOLL.Atomic.Increment (R.Refcount);
   end Adjust;

   -------------------------------
   -- Check_For_Default_Project --
   -------------------------------

   function Check_For_Default_Project
     (Directory : String := "") return GPR2.Path_Name.Object
   is
      use Directories;
      Default_Name : constant String :=
                       (if Directory = ""
                        then "default.gpr"
                       else Directory
                        & GNAT.OS_Lib.Directory_Separator
                        & "default.gpr");
      Search       : Search_Type;
      Item         : Directory_Entry_Type;

   begin
      if Exists (Default_Name)
        and then Kind (Default_Name) = Ordinary_File
      then
         return Path_Name.Create_File (Filename_Type (Default_Name));
      end if;

      Start_Search
        (Search,
         (if Directory = "" then "." else Directory),
         "*.gpr",
         (Ordinary_File => True, others => False));

      if More_Entries (Search) then
         Get_Next_Entry (Search, Item);

         if not More_Entries (Search) then
            --  Only one project in current directory can be default one

            return Path_Name.Create_File (Filename_Type (Full_Name (Item)));
         end if;
      end if;

      return Path_Name.Undefined;
   end Check_For_Default_Project;

   -------------------
   -- Clear_Sources --
   -------------------

   procedure Clear_Sources
     (Self : Object; View : Project.View.Object := Project.View.Undefined)
   is
   begin
      Self.Tree.Clear_Sources (View);
   end Clear_Sources;

   ------------
   -- Create --
   ------------

   procedure Create (Self : in out Object) is
      R : access GNATCOLL.Refcount.Counters;
   begin
      if Self.Tree = null then
         Self.Tree := new Tree_Internal.Object;
         R := Pools.Header_Of (Self.Tree);
         R.Refcount  := 1;
         R.Weak_Data := null;
      end if;
   end Create;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Object) is
      R    : GNATCOLL.Refcount.Counters_Access;
      Data : Tree_Internal_Access := Self.Tree;
   begin
      if Self.Tree /= null then
         Self.Tree := null;

         R := Pools.Header_Of (Data);

         if GNATCOLL.Atomic.Decrement (R.Refcount) then
            Data.Unload;
            Release (Data);
         end if;
      end if;
   end Finalize;

   ----------
   -- Load --
   ----------

   function Load
     (Self                   : in out Object;
      Options                : in out GPR2.Options.Object'Class;
      With_Runtime           : Boolean := False;
      Absent_Dir_Error       : GPR2.Error_Level := GPR2.Warning;
      Allow_Implicit_Project : Boolean := True;
      Environment            : GPR2.Environment.Object :=
                                 GPR2.Environment.Process_Environment;
      File_Reader            : GPR2.File_Readers.File_Reader_Reference :=
                                 GPR2.File_Readers.No_File_Reader_Reference;
      Verbosity              : Verbosity_Level := Warnings_And_Errors)
      return Boolean
   is
      Conf         : GPR2.Project.Configuration.Object;
      Create_Cgpr  : Boolean := False;
      Project_File : GPR2.Path_Name.Object := Options.Project_File;

   begin
      if not Self.Is_Defined then
         Self.Create;
      else
         Self.Tree.Unload (Full => False);
      end if;

      if Project_File.Is_Defined
        and then not Project_File.Has_Dir_Name
        and then Options.Root_Path.Is_Defined
      then
         --  We have to resolve the project directory without target specific
         --  directories in search path because --root-dir exists in command
         --  line parameters.

         declare
            Search_Paths : Path_Name.Set.Object :=
                             GPR2.Project.Default_Search_Paths
                               (True, Environment);
         begin
            for P of Options.User_Specified_Project_Search_Path loop
               Search_Paths.Prepend (P);
            end loop;

            Project_File := GPR2.Project.Create
              (Project_File.Name, Options.Resolve_Links, Search_Paths);
         end;
      end if;

      if not Project_File.Is_Defined then
         if Options.No_Project then
            --  Specifying a directory as project file will create the default
            --  project in there, so expecting all sources and artifacts to
            --  share the same folder.

            Project_File := Path_Name.Create_Directory
              (Filename_Type (Ada.Directories.Current_Directory));

         elsif Allow_Implicit_Project then
            Project_File := Check_For_Default_Project;

            if Project_File.Is_Defined then
               Message.Reporter.Active_Reporter.Report
                 ("using project file " & Project_File.String_Value);
            else
               --  See comment in No_Project case as to how we handle projects
               --  as project directories.

               Project_File := Path_Name.Create_Directory
                 (Filename_Type (Ada.Directories.Current_Directory));
               Message.Reporter.Active_Reporter.Report
                 ("use implicit project in " & Directories.Current_Directory);
            end if;

         else
            raise GPR2.Options.Usage_Error with
              "no project file specified";
         end if;

      elsif Options.No_Project then
         raise GPR2.Options.Usage_Error with
           "cannot specify --no-project with a project file";
      end if;

      if not Options.Build_Path.Is_Defined
        and then Options.Root_Path.Is_Defined
      then
         raise GPR2.Options.Usage_Error with
           "cannot use --root-dir without --relocate-build-tree option";
      end if;

      for Path of Options.User_Specified_Project_Search_Path loop
         Self.Register_Project_Search_Path (Path);
      end loop;

      if Options.Config_Project.Is_Defined
        and then (not Options.Create_Config_Project
                  or else Options.Config_Project.Exists)
      then
         Conf := GPR2.Project.Configuration.Load (Options.Config_Project);

         if Conf.Has_Error then
            if Verbosity >= Warnings_And_Errors then
               Conf.Log_Messages.Output_Messages
                 (Information => False,
                  Warning     => False);
            end if;

            return False;
         end if;

         case Verbosity is
            when Quiet | Minimal =>
               null;

            when Warnings_And_Errors =>
               Conf.Log_Messages.Output_Messages (Information => False);

            when Info | Linter =>
               Conf.Log_Messages.Output_Messages;
         end case;

         Self.Tree.Load
           (Filename         => Project_File,
            Context          => Options.Context,
            With_Runtime     => With_Runtime,
            Config           => Conf,
            Build_Path       => Options.Build_Path,
            Root_Path        => Options.Root_Path,
            Subdirs          => Options.Subdirs,
            Src_Subdirs      => Options.Src_Subdirs,
            Check_Shared_Lib => Options.Check_Shared_Lib,
            Absent_Dir_Error => Absent_Dir_Error,
            Implicit_With    => Options.Implicit_With,
            Resolve_Links    => Options.Resolve_Links,
            File_Reader      => File_Reader,
            Environment      => Environment);

         if Options.Target /= "all" then
            --  if target is defined on the command line, and a config
            --  file is specified, issue an error if the target of the config
            --  is different from the command line.

            declare
               package PRA renames GPR2.Project.Registry.Attribute;

               Target_Attr : constant GPR2.Project.Attribute.Object :=
                               Self.Tree.Configuration.Corresponding_View.
                                 Attribute (PRA.Target);
               Conf_Target : constant Value_Type := Target_Attr.Value.Text;
               Base        : constant GPR2.KB.Object :=
                               (if Self.Tree.Get_KB.Is_Defined
                                then Self.Tree.Get_KB
                                else GPR2.KB.Create_Default
                                  (GPR2.KB.Targetset_Only_Flags,
                                   Environment));
               Conf_Norm   : constant Name_Type :=
                               Base.Normalized_Target
                                 (Name_Type (Conf_Target));
               Self_Norm   : constant Name_Type :=
                               Base.Normalized_Target (Options.Target);
            begin
               if Conf_Norm /= Self_Norm then
                  Self.Tree.Log_Messages.Append
                    (GPR2.Message.Create
                       (Level   =>  GPR2.Message.Error,
                        Message =>  "--target: '" &
                          String (Options.Target) &
                          "' is different from the target value in the" &
                          " configuration project '" &
                          String (Conf_Norm) & "'",
                        Sloc    => Target_Attr.Value));
               else
                  Self.Tree.Log_Messages.Append
                    (GPR2.Message.Create
                       (Level   =>  GPR2.Message.Warning,
                        Message =>  "--target is not used when a " &
                          "configuration project is specified.",
                        Sloc    => Target_Attr.Value));
               end if;
            end;
         end if;

         case Verbosity is
            when Quiet | Minimal =>
               null;

            when Warnings_And_Errors =>
               Self.Tree.Log_Messages.Output_Messages (Information => False);

            when Info =>
               Self.Tree.Log_Messages.Output_Messages;

            when Linter =>
               Self.Tree.Log_Messages.Output_Messages (Lint => True);
         end case;

      else
         if Options.Config_Project.Is_Defined then
            if Verbosity > Quiet then
               Ada.Text_IO.Put_Line
                 ("creating configuration project " &
                    String (Options.Config_Project.Name));
            end if;
            Create_Cgpr := True;
         end if;

         Self.Tree.Load_Autoconf
           (Filename          => Project_File,
            Context           => Options.Context,
            With_Runtime      => With_Runtime,
            Build_Path        => Options.Build_Path,
            Root_Path         => Options.Root_Path,
            Subdirs           => Options.Subdirs,
            Src_Subdirs       => Options.Src_Subdirs,
            Check_Shared_Lib  => Options.Check_Shared_Lib,
            Absent_Dir_Error  => Absent_Dir_Error,
            Implicit_With     => Options.Implicit_With,
            Resolve_Links     => Options.Resolve_Links,
            Target            => Options.Target,
            Language_Runtimes => Options.RTS_Map,
            Base              => Options.Base (Environment),
            Config_Project    => (if Create_Cgpr
                                  then Options.Config_Project
                                  else GPR2.Path_Name.Undefined),
            File_Reader       => File_Reader,
            Environment       => Environment);

         case Verbosity is
            when Quiet | Minimal =>
               null;

            when Warnings_And_Errors =>
               Self.Tree.Log_Messages.Output_Messages (Information => False);

            when Info =>
               Self.Tree.Log_Messages.Output_Messages;

            when Linter =>
               Self.Tree.Log_Messages.Output_Messages (Lint => True);
         end case;
      end if;

      return True;
   exception
      when GPR2.Project_Error =>
         if Verbosity >= Warnings_And_Errors then
            if Self.Tree.Has_Configuration
              and then Self.Tree.Configuration.Has_Error
            then
               Self.Tree.Configuration.Log_Messages.Output_Messages
                 (Information => False,
                  Warning     => False);
            else
               Self.Tree.Log_Messages.Output_Messages
                 (Information => False,
                  Warning     => False);
            end if;
         end if;

         return False;
   end Load;

   ----------------------------------
   -- Register_Project_Search_Path --
   ----------------------------------

   procedure Register_Project_Search_Path
     (Self : in out Object; Dir : Path_Name.Object)
   is
   begin
      if not Self.Is_Defined then
         Self.Create;
      end if;

      Self.Tree.Register_Project_Search_Path (Dir => Dir);
   end Register_Project_Search_Path;

   ------------------------------------
   -- Restrict_Autoconf_To_Languages --
   ------------------------------------

   procedure Restrict_Autoconf_To_Languages
     (Self  : in out Object;
      Langs : Containers.Language_Set) is
   begin
      if not Self.Is_Defined then
         Self.Create;
      end if;

      Self.Tree.Restrict_Autoconf_To_Languages (Langs);
   end Restrict_Autoconf_To_Languages;

   ---------
   -- Set --
   ---------

   function Set (Tree : Tree_Internal.Object_Access) return Object is
      R   : access GNATCOLL.Refcount.Counters;
      Res : Object;
   begin
      Res.Tree := Tree_Internal_Access (Tree);

      if Res.Tree /= null then
         R := Pools.Header_Of (Res.Tree);
         GNATCOLL.Atomic.Increment (R.Refcount);
      end if;

      return Res;
   end Set;

   -----------------
   -- Set_Context --
   -----------------

   function Set_Context
     (Self    : in out Object;
      Context : GPR2.Context.Object;
      Changed : access procedure (Project : View.Object) := null)
      return Boolean
   is
   begin
      Self.Tree.Set_Context (Context, Changed);
      Self.Log_Messages.Output_Messages (Information => False);

      return True;

   exception
      when Project_Error =>
         Self.Log_Messages.Output_Messages
           (Information => False,
            Warning     => False);
         return False;
   end Set_Context;

   ------------
   -- Unload --
   ------------

   procedure Unload (Self : in out Object) is
   begin
      if Self.Tree /= null then
         Self.Tree.Unload;
         Self.Finalize;
      end if;
   end Unload;

   --------------------
   -- Update_Sources --
   --------------------

   procedure Update_Sources
     (Self     :     Object; Option : Source_Info_Option := Sources_Units)
   is
      Log : GPR2.Log.Object;
   begin
      Self.Tree.Update_Sources (Option => Option, Messages => Log);
      Log.Output_Messages;
   end Update_Sources;

begin

   Tree_Internal.Get := Get'Access;
   Tree_Internal.Set := Set'Access;

end GPR2.Project.Tree;
