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

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Message.Reporter;
with GPR2.Project_Parser;
with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;
with GPR2.Tree_Internal.View_Builder;

package body GPR2.Project.Tree is

   procedure Release is new Ada.Unchecked_Deallocation
     (Tree_Internal.Object, Tree_Internal_Access);

   function Get (Self : Object) return Tree_Internal.Object_Access is
     (Tree_Internal.Object_Access (Self.Tree));

   function Set (Tree : Tree_Internal.Object_Access) return Object;

   function Check_For_Default_Project
     (Directory : String := "") return GPR2.Path_Name.Object;

   procedure Report_Messages (Log : GPR2.Log.Object);

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

   ----------------------
   -- For_Each_Closure --
   ----------------------

   procedure For_Each_Ada_Closure
     (Self              : Object;
      Action            : access procedure
                            (Unit : Build.Compilation_Unit.Object);
      Mains             : Containers.Filename_Set :=
                            Containers.Empty_Filename_Set;
      All_Sources       : Boolean := False;
      Root_Project_Only : Boolean := False;
      Externally_Built  : Boolean := False)
   is
     --  ??? This closure computation uses only our fast ada parser to compute
     --  the list of units to process. This means that implicit withs are not
     --  processed (so we miss some runtime units), and multi-unit sources are
     --  not properly processed either.

      use type Project.View.Object;

      procedure Process
        (Root : Project.View.Object;
         Name : Name_Type;
         Unit : Build.Compilation_Unit.Object);

      Processed_Units   : Containers.Name_Set;
      Stack             : Containers.Name_Set;
      Processed_Views   : Project.View.Set.Object;
      Subtree_Views     : Project.View.Set.Object;

      -------------
      -- Process --
      -------------

      procedure Process
        (Root : Project.View.Object;
         Name : Name_Type;
         Unit : Build.Compilation_Unit.Object) is
      begin
         if Processed_Units.Contains (Name) then
            return;
         end if;

         Processed_Units.Include (Name);

         if not Unit.Is_Defined then
            return;
         end if;

         --  Mark current unit as processed in the subtree
         Subtree_Views.Include (Unit.Owning_View);

         --  Check the Externally_Built status

         if not Externally_Built
           and then Unit.Owning_View.Is_Externally_Built
         then
            return;
         end if;

         --  Prune:
         --  - Units not belonging to root if --root-project is specified
         --  - Units belonging to a View already processed as part of an
         --    aggregated subtree
         if (Root_Project_Only and then Unit.Owning_View /= Root)
           or else Processed_Views.Contains (Unit.Owning_View)
         then
            return;
         end if;


         if Unit.Is_Defined
           and then (not Root_Project_Only
                     or else Unit.Owning_View = Root)
         then
            Action (Unit);

            --  Adjust list of remaining units to process: remove from
            --  the known dependencies the processed units (so that only
            --  unprocessed units remain, and make a union with current
            --  stack to add new unprocessed items.

            Stack := Stack.Union
              (Unit.Known_Dependencies.Difference (Processed_Units));
         end if;
      end Process;

      Source : Build.Source.Object;
      Unit   : Build.Compilation_Unit.Object;

   begin
      if Root_Project_Only
        and then Self.Root_Project.Kind in Aggregate_Kind
      then
         return;
      end if;

      for Root of Self.Namespace_Root_Projects loop
         --  In an aggregate, units are independant from each others so we
         --  need to reset the list of processed units. However views are
         --  consistent by construction (you can't have the same view with
         --  different units) by construction, so in Process we prune the
         --  already processed views and keep the list across namespace roots.

         Processed_Units := Containers.Name_Type_Set.Empty_Set;

         --  First phase: we check the initial list of entry points: so Mains
         --  for an application and Interface for a library.

         if not Mains.Is_Empty then
            --  Try to find all mains specified on the command line

            for Main of Mains loop
               Source := Root.View_Db.Visible_Source (Main);

               if not Source.Is_Defined then
                  Unit := Root.Unit (Name_Type (Main));

                  if Unit.Is_Defined then
                     Source := Unit.Main_Part.View.Source
                       (Unit.Main_Part.Source.Simple_Name);
                  end if;
               end if;

               if not Source.Is_Defined then
                  raise GPR2.Options.Usage_Error with
                    "cannot find """ & String (Main) & '"';
               end if;

               if Source.Has_Units then
                  for U of Source.Units loop
                     Stack.Include (U.Name);
                  end loop;
               end if;
            end loop;

         elsif not All_Sources
           and then Root.Has_Mains
         then
            --  no -U switch case, root project defines mains
            for Main of Root.Mains loop
               Source := Main.View.Source (Main.Source.Simple_Name);

               if Source.Has_Units then
                  for U of Source.Units loop
                     Stack.Include (U.Name);
                  end loop;
               end if;
            end loop;

         elsif not All_Sources
           and then Root.Is_Library
           and then Root.Has_Any_Interfaces
         then
            --  no -U switch case, standalone library case
            for C in Root.Interface_Units.Iterate loop
               --  ??? should handle also case where ada sources are defined
               --  in Root.Interface_Sources.

               declare
                  Unit_Name : constant Name_Type :=
                                Containers.Unit_Name_To_Sloc.Key (C);
               begin
                  Stack.Include (Unit_Name);
               end;
            end loop;

         else
            --  No mains and no library interface is defined, or -U is used
            --  so we use all units of the root project as a starting point.

            for Unit of Root.Units loop
               if not Root_Project_Only
                 or else Unit.Owning_View = Root
               then
                  Stack.Include (Unit.Name);
               end if;
            end loop;
         end if;

         --  Second phase, we process each entry point, and amend the list of
         --  units to analyze with their dependencies. Processing ends when
         --  all dependencies are processed.

         while not Stack.Is_Empty loop
            declare
               U_Name : constant Name_Type := Stack.First_Element;
            begin
               Stack.Delete_First;
               Process (Root, U_Name, Root.Unit (U_Name));
            end;
         end loop;

         --  Update the list of processed views
         Processed_Views := Subtree_Views;
      end loop;
   end For_Each_Ada_Closure;

   ----------
   -- Load --
   ----------

   function Load
     (Self                   : in out Object;
      Options                : GPR2.Options.Object'Class;
      With_Runtime           : Boolean := False;
      Absent_Dir_Error       : GPR2.Error_Level := GPR2.Warning;
      Allow_Implicit_Project : Boolean := True;
      Environment            : GPR2.Environment.Object :=
                                 GPR2.Environment.Process_Environment;
      Config                 : GPR2.Project.Configuration.Object :=
                                 GPR2.Project.Configuration.Undefined;
      File_Reader            : GPR2.File_Readers.File_Reader_Reference :=
                                 GPR2.File_Readers.No_File_Reader_Reference)
      return Boolean
   is
      use Tree_Internal;

      Conf         : GPR2.Project.Configuration.Object;
      Prj_Kind     : Project_Descriptor_Kind := Project_Path;
      Project_File : GPR2.Path_Name.Object := Options.Project_File;
      Root_Data    : GPR2.View_Internal.Data;

      function Prj_Descriptor return Tree_Internal.Project_Descriptor is
        (case Prj_Kind is
            when Project_Path => (Project_Path, Project_File),
            when Project_Definition => (Project_Definition, Root_Data));

   begin
      GPR2.Project_Parser.Clear_Cache;

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

               Message.Reporter.Active_Reporter.Report
                 ("use implicit project in " & Directories.Current_Directory);
               Root_Data := Tree_Internal.View_Builder.Create
                 (Project_Dir => Path_Name.Create_Directory
                    (Filename_Type (Ada.Directories.Current_Directory)),
                  Name        => "Default").Data;
               Prj_Kind := Project_Definition;
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

      if Config.Is_Defined
        or else
          (Options.Config_Project.Is_Defined
           and then (not Options.Create_Config_Project
                     or else Options.Config_Project.Exists))
      then
         if Config.Is_Defined then
            Conf := Config;
         else
            Conf := GPR2.Project.Configuration.Load (Options.Config_Project);
         end if;

         Self.Tree.Load
           (Root_Project     => Prj_Descriptor,
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

         Report_Messages (Self.Tree.Log_Messages.all);

      else
         if Options.Config_Project.Is_Defined and then Verbosity > Quiet then
            Ada.Text_IO.Put_Line
              ("creating configuration project " &
                 String (Options.Config_Project.Name));
         end if;

         Self.Tree.Load_Autoconf
           (Root_Project      => Prj_Descriptor,
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
            Config_Project    => Options.Config_Project,
            File_Reader       => File_Reader,
            Environment       => Environment);

         Report_Messages (Self.Tree.Log_Messages.all);
      end if;

      GPR2.Project_Parser.Clear_Cache;

      return True;
   exception
      when GPR2.Project_Error =>
         if Verbosity >= Errors then
            Self.Tree.Log_Messages.Output_Messages
              (Information => False,
               Warning     => False);
         end if;

         GPR2.Project_Parser.Clear_Cache;

         return False;
   end Load;

   -----------------------
   -- Load_Virtual_View --
   -----------------------

   function Load_Virtual_View
     (Self             : in out Object;
      Root_Project     : View_Builder.Object;
      Options          : GPR2.Options.Object'Class;
      With_Runtime     : Boolean := False;
      Absent_Dir_Error : GPR2.Error_Level := GPR2.Warning;
      Environment      : GPR2.Environment.Object :=
                           GPR2.Environment.Process_Environment;
      Config           : GPR2.Project.Configuration.Object :=
                           GPR2.Project.Configuration.Undefined;
      File_Reader      : GPR2.File_Readers.File_Reader_Reference :=
                           GPR2.File_Readers.No_File_Reader_Reference)
      return Boolean
   is
      Conf         : GPR2.Project.Configuration.Object;

   begin
      if not Self.Is_Defined then
         Self.Create;
      else
         Self.Tree.Unload (Full => False);
      end if;

      for Path of Options.User_Specified_Project_Search_Path loop
         Self.Register_Project_Search_Path (Path);
      end loop;

      if Config.Is_Defined
        or else
          (Options.Config_Project.Is_Defined
           and then (not Options.Create_Config_Project
                     or else Options.Config_Project.Exists))
      then
         if Config.Is_Defined then
            Conf := Config;
         else
            Conf := GPR2.Project.Configuration.Load (Options.Config_Project);
         end if;

         Self.Tree.Load
           (Root_Project     => (Kind => Tree_Internal.Project_Definition,
                                 Data => Get_View_Data (Root_Project)),
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

         Report_Messages (Self.Tree.Log_Messages.all);

      else
         if Options.Config_Project.Is_Defined and then Verbosity > Quiet then
            Ada.Text_IO.Put_Line
              ("creating configuration project " &
                 String (Options.Config_Project.Name));
         end if;

         Self.Tree.Load_Autoconf
           (Root_Project      => (Kind => Tree_Internal.Project_Definition,
                                  Data => Get_View_Data (Root_Project)),
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
            Config_Project    => Options.Config_Project,
            File_Reader       => File_Reader,
            Environment       => Environment);

         Report_Messages (Self.Tree.Log_Messages.all);
      end if;

      return True;
   exception
      when GPR2.Project_Error =>
         if Verbosity >= Errors then
            Self.Tree.Log_Messages.Output_Messages
              (Information => False,
               Warning     => False);
         end if;

         return False;
   end Load_Virtual_View;

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

   ---------------------
   -- Report_Messages --
   ---------------------

   procedure Report_Messages (Log : GPR2.Log.Object) is
   begin
      case Verbosity is
         when Quiet | Minimal =>
            null;

         when Errors =>
            Log.Output_Messages (Information => False,
                                 Warning     => False);

         when Warnings_And_Errors =>
            Log.Output_Messages (Information => False);

         when Info =>
            Log.Output_Messages;

         when Linter =>
            Log.Output_Messages (Lint => True);
      end case;
   end Report_Messages;

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
      Self.Tree.Log_Messages.Clear;
      Self.Tree.Set_Context (Context, Changed);
      Report_Messages (Self.Tree.Log_Messages.all);

      return True;

   exception
      when Project_Error =>
         if Verbosity >= Errors then
            Self.Log_Messages.Output_Messages
              (Information => False,
               Warning     => False);
         end if;

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
     (Self     : Object;
      Messages : out GPR2.Log.Object;
      Option   : Source_Info_Option := Sources_Units)
   is
   begin
      Self.Tree.Update_Sources (Option => Option, Messages => Messages);
      Report_Messages (Messages);
   end Update_Sources;

   procedure Update_Sources
     (Self     : Object;
      Option   : Source_Info_Option := Sources_Units)
   is
      Log : GPR2.Log.Object;
   begin
      Self.Update_Sources (Log, Option);
   end Update_Sources;

   function Update_Sources
     (Self     : Object;
      Option   : Source_Info_Option := Sources_Units) return Boolean
   is
      Log : GPR2.Log.Object;
   begin
      Self.Update_Sources (Log, Option);

      return not Log.Has_Element
        (Information => False,
         Warning     => False,
         Error       => True,
         Lint        => False,
         Read        => True,
         Unread      => True);
   end Update_Sources;

begin

   Tree_Internal.Get := Get'Access;
   Tree_Internal.Set := Set'Access;

end GPR2.Project.Tree;
