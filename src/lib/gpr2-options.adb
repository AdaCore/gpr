--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.OS_Lib;

with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Configuration;
with GPR2.Project.Registry;
with GPR2.Project.Registry.Attribute;

package body GPR2.Options is

   ----------------
   -- Add_Switch --
   ----------------

   procedure Add_Switch
     (Self   : in out Object;
      Switch : Option;
      Param  : String := "";
      Index  : String := "") is

   begin
      case Switch is
         when AP =>
            Self.Search_Paths.Append
              ((GPR2.Path_Name.Create_Directory (GPR2.Filename_Type (Param))));

         when Autoconf =>
            Self.Config_Project :=
              GPR2.Path_Name.Create_File
                (GPR2.Filename_Type (Param));
            Self.Create_Missing_Config := True;

         when Config =>
            Self.Config_Project :=
              GPR2.Path_Name.Create_File
                (GPR2.Filename_Type (Param));
            Self.Create_Missing_Config := False;

         when Db =>
            declare
               KB_Norm : constant String :=
                           GNAT.OS_Lib.Normalize_Pathname (Param);
               KB_Path : GPR2.Path_Name.Object;
            begin
               if GNAT.OS_Lib.Is_Directory (KB_Norm) then
                  KB_Path :=
                    GPR2.Path_Name.Create_Directory
                      (GPR2.Filename_Type (KB_Norm));

               elsif GNAT.OS_Lib.Is_Regular_File (KB_Norm) then
                  KB_Path :=
                    GPR2.Path_Name.Create_File (GPR2.Filename_Type (KB_Norm));

               else
                  raise Usage_Error with
                    KB_Norm & " is not a file or directory";
               end if;

               Self.KB_Locations.Append (KB_Path);
            end;

         when Db_Minus =>
            Self.Skip_Default_KB := True;

         when Implicit_With =>
            Self.Implicit_With.Append
              (GPR2.Path_Name.Create_File
                 (GPR2.Project.Ensure_Extension (GPR2.Filename_Type (Param))));

         when No_Project =>
            Self.No_Project := True;

         when P =>
            if not Self.Project_File.Is_Defined then
               Self.Project_File :=
                 GPR2.Path_Name.Create_File
                   (GPR2.Project.Ensure_Extension (GPR2.Filename_Type (Param)),
                    GPR2.Path_Name.No_Resolution);
            else
               if Self.Prj_Got_On_Extra_Arg then
                  raise GPR2.Options.Usage_Error with
                    "cannot have -P<prj> and <prj> on the same command line";

               else
                  raise GPR2.Options.Usage_Error with
                    """-P"", project already """
                    & (if Self.Project_File.Has_Dir_Name
                       then Self.Project_File.Value
                       else String (Self.Project_File.Name)) & '"';
               end if;
            end if;

         when Relocate_Build_Tree =>
            Self.Build_Path :=
              GPR2.Path_Name.Create_Directory (GPR2.Filename_Type (Param));

         when Root_Dir =>
            Self.Root_Path :=
              GPR2.Path_Name.Create_Directory (GPR2.Filename_Type (Param));

         when RTS =>
            declare
               Lang_Idx : constant GPR2.Language_Id :=
                            (if Index'Length > 0
                             then GPR2."+" (GPR2.Name_Type (Index))
                             else GPR2.No_Language);
            begin
               if Lang_Idx = GPR2.No_Language then
                  Self.RTS_Map.Include (GPR2.Ada_Language, Param);
               else
                  Self.RTS_Map.Include (Lang_Idx, Param);
               end if;
            end;

         when Src_Subdirs =>
            Self.Src_Subdirs := To_Unbounded_String (Param);

         when Subdirs =>
            Self.Subdirs := To_Unbounded_String (Param);

         when Target =>
            Self.Target := To_Unbounded_String (Param);

         when Unchecked_Shared_Lib_Imports =>
            Self.Unchecked_Shared_Lib := True;

         when X =>
            declare
               Idx : constant Natural := Ada.Strings.Fixed.Index (Param, "=");
            begin
               if Idx = 0 then
                  raise Usage_Error with
                    "Can't split '" & Param & "' to name and value";
               end if;

               Self.Context.Include
                 (GPR2.Name_Type (Param (Param'First .. Idx - 1)),
                  Param (Idx + 1 .. Param'Last));
            end;
      end case;
   end Add_Switch;

   -------------------------------
   -- Check_For_Default_Project --
   -------------------------------

   function Check_For_Default_Project
     (Directory : String := "") return GPR2.Path_Name.Object is
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

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (Self                   : in out Object;
      Allow_Implicit_Project : Boolean := True;
      Quiet                  : Boolean := False;
      Environment            : GPR2.Environment.Object :=
                                 GPR2.Environment.Process_Environment) is
   begin
      Self.Environment := Environment;

      if Self.Project_File.Is_Defined
        and then not Self.Project_File.Has_Dir_Name
        and then Self.Root_Path.Is_Defined
      then
         --  We have to resolve the project directory without target specific
         --  directories in search path because --root-dir exists in command
         --  line parameters.

         declare
            Search_Paths : Path_Name.Set.Object :=
                             GPR2.Project.Default_Search_Paths
                               (True, Self.Environment);
         begin
            for P of Self.Search_Paths loop
               Search_Paths.Prepend (P);
            end loop;

            Self.Project_File := GPR2.Project.Create
              (Self.Project_File.Name, Search_Paths);
         end;
      end if;

      Self.Project_Is_Defined := Self.Project_File.Is_Defined;

      if not Self.Project_File.Is_Defined then
         if Self.No_Project then
            Self.Project_Base := GPR2.Path_Name.Create_Directory
              (GPR2.Filename_Type (Ada.Directories.Current_Directory));

         elsif Allow_Implicit_Project then
            Self.Project_File := Check_For_Default_Project;

            if not Self.Project_File.Is_Defined then
               Self.Project_Base :=
                 GPR2.Path_Name.Create_Directory
                   (GPR2.Filename_Type (Ada.Directories.Current_Directory));

               if not Quiet then
                  Ada.Text_IO.Put_Line
                    ("use implicit project in " & Self.Project_Base.Value);
               end if;

            elsif not Quiet then
               Ada.Text_IO.Put_Line
                 ("using project file " & Self.Project_File.Value);
            end if;
         end if;

      elsif Self.No_Project then
         raise Usage_Error with
           "cannot specify --no-project with a project file";
      end if;

      if not Self.Build_Path.Is_Defined
        and then Self.Root_Path.Is_Defined
      then
         raise Usage_Error with
           "cannot use --root-dir without --relocate-build-tree option";
      end if;

      declare
         Project_Dir : constant GPR2.Path_Name.Object :=
                         (if Self.Project_Base.Is_Defined
                          then Self.Project_Base
                          elsif Self.Project_File.Is_Defined
                            and then Self.Project_File.Has_Dir_Name
                          then GPR2.Path_Name.Create_Directory
                            (Filename_Type (Self.Project_File.Dir_Name))
                          else
                             GPR2.Path_Name.Undefined);

      begin
         if Project_Dir.Is_Defined then
            if not Self.Build_Path.Is_Defined then
               Self.Build_Path := Project_Dir;

            elsif Self.Root_Path.Is_Defined then
               Self.Build_Path := GPR2.Path_Name.Create_Directory
                 (Project_Dir.Relative_Path (Self.Root_Path).Name,
                  Filename_Type (Self.Build_Path.Value));
            end if;
         end if;
      end;

      Self.Finalized := True;
   end Finalize;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project
     (Self             : in out Object;
      Tree             : in out GPR2.Project.Tree.Object;
      With_Runtime     : Boolean;
      Absent_Dir_Error : GPR2.Project.Tree.Error_Level :=
                           GPR2.Project.Tree.Warning;
      File_Reader      : GPR2.File_Readers.File_Reader_Reference :=
                           GPR2.File_Readers.No_File_Reader_Reference;
      Quiet            : Boolean := False) return Boolean is

      Conf        : GPR2.Project.Configuration.Object;
      Create_Cgpr : Boolean := False;

   begin
      if Tree.Is_Defined then
         Tree.Unload;
      end if;

      Self.Register_Project_Search_Paths (Tree);

      Self.Config_Project_Has_Error := False;
      Self.Config_Project_Log.Clear;

      if Self.Config_Project.Is_Defined
        and then
          (not Self.Create_Missing_Config
           or else Self.Config_Project.Exists)
      then
         Conf := GPR2.Project.Configuration.Load (Self.Config_Project);

         Self.Config_Project_Log := Conf.Log_Messages;

         if Conf.Has_Error then
            Self.Config_Project_Has_Error := True;
            return False;
         end if;

         Tree.Load
           (Filename         => Self.Filename,
            Context          => Self.Context,
            With_Runtime     => With_Runtime,
            Config           => Conf,
            Build_Path       => Self.Build_Path,
            Subdirs          => Subdirs (Self),
            Src_Subdirs      => Src_Subdirs (Self),
            Check_Shared_Lib => Self.Check_Shared_Lib,
            Absent_Dir_Error => Absent_Dir_Error,
            Implicit_With    => Self.Implicit_With,
            File_Reader      => File_Reader,
            Environment      => Self.Environment);


         if To_String (Self.Target) /= "all" then
            --  if target is defined on the command line, and a config
            --  file is specified, issue an error if the target of the config
            --  is different from the command line.

            declare
               package PRA renames GPR2.Project.Registry.Attribute;

               Target_Attr : constant GPR2.Project.Attribute.Object :=
                               Tree.Configuration.Corresponding_View.
                                 Attribute (PRA.Target);
               Conf_Target : constant Value_Type := Target_Attr.Value.Text;
               Base        : constant GPR2.KB.Object :=
                               (if Tree.Get_KB.Is_Defined
                                then Tree.Get_KB
                                else GPR2.KB.Create_Default
                                  (GPR2.KB.Targetset_Only_Flags,
                                   Self.Environment));
               Conf_Norm   : constant Name_Type :=
                               Base.Normalized_Target
                                 (Name_Type (Conf_Target));
               Self_Norm   : constant Name_Type :=
                               Base.Normalized_Target
                                  (Name_Type (To_String (Self.Target)));
            begin
               if Conf_Norm /= Self_Norm then
                  Tree.Log_Messages.Append
                    (GPR2.Message.Create
                       (Level   =>  GPR2.Message.Error,
                        Message =>  "--target: '" &
                          To_String (Self.Target) &
                          "' is different from the target value in the" &
                          " configuration project '" &
                          String (Conf_Norm) & "'",
                        Sloc    => Target_Attr.Value));
               else
                  Tree.Log_Messages.Append
                    (GPR2.Message.Create
                       (Level   =>  GPR2.Message.Warning,
                        Message =>  "--target is not used when a " &
                          "configuration project is specified.",
                        Sloc    => Target_Attr.Value));
               end if;
            end;
         end if;

      else
         if Self.Create_Missing_Config
           and then not Self.Config_Project.Exists
         then
            if not Quiet then
               Ada.Text_IO.Put_Line
                 ("creating configuration project " &
                    String (Self.Config_Project.Name));
            end if;
            Create_Cgpr := True;
         end if;

         Tree.Load_Autoconf
           (Filename          => Self.Filename,
            Context           => Self.Context,
            With_Runtime      => With_Runtime,
            Build_Path        => Self.Build_Path,
            Subdirs           => Subdirs (Self),
            Src_Subdirs       => Src_Subdirs (Self),
            Check_Shared_Lib  => Self.Check_Shared_Lib,
            Absent_Dir_Error  => Absent_Dir_Error,
            Implicit_With     => Self.Implicit_With,
            Target            => Target (Self),
            Language_Runtimes => Self.RTS_Map,
            Base              => Self.Base,
            Config_Project    => (if Create_Cgpr
                                  then Self.Config_Project
                                  else GPR2.Path_Name.Undefined),
            File_Reader       => File_Reader,
            Environment       => Self.Environment);

      end if;

      return True;

   exception
      when GPR2.Project_Error | GPR2.Processing_Error =>
         return False;
   end Load_Project;

   ------------------
   -- On_Extra_Arg --
   ------------------

   function On_Extra_Arg (Self : in out Object; Arg : String) return Boolean is
   begin
      if GNATCOLL.Utils.Ends_With
        (GPR2.Path_Name.To_OS_Case (Arg),
         String (GPR2.Project.Project_File_Extension))
      then
         if not Self.Project_File.Is_Defined then
            Self.Add_Switch
              (Switch => GPR2.Options.P,
               Param  => Arg,
               Index  => "");
            Self.Prj_Got_On_Extra_Arg := True;

            return True;

         elsif not Self.Prj_Got_On_Extra_Arg then
            raise GPR2.Options.Usage_Error with
              "cannot have -P<prj> and <prj> on the same command line";

         else
            raise GPR2.Options.Usage_Error with
              "cannot have multiple <proj> on the same command line";
         end if;

      else
         return False;
      end if;
   end On_Extra_Arg;

   -----------------------------------
   -- Register_Project_Search_Paths --
   -----------------------------------

   procedure Register_Project_Search_Paths
     (Self : Object;
      Tree : in out GPR2.Project.Tree.Object) is
   begin
      for Path of Self.Search_Paths loop
         Tree.Register_Project_Search_Path (Path);
      end loop;
   end Register_Project_Search_Paths;


end GPR2.Options;
