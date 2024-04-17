--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

separate (GPR2.Project.Tree)
procedure Load_Autoconf
  (Self              : in out Object;
   Root_Project      : Project_Descriptor;
   Context           : GPR2.Context.Object;
   Build_Path        : Path_Name.Object        := Path_Name.Undefined;
   Root_Path         : Path_Name.Object        := Path_Name.Undefined;
   Subdirs           : Optional_Name_Type      := No_Name;
   Src_Subdirs       : Optional_Name_Type      := No_Name;
   Check_Shared_Lib  : Boolean                 := True;
   Absent_Dir_Error  : Error_Level             := Warning;
   Implicit_With     : GPR2.Path_Name.Set.Object :=
                         GPR2.Path_Name.Set.Empty_Set;
   Target            : Optional_Name_Type      := No_Name;
   Language_Runtimes : Containers.Lang_Value_Map :=
                         Containers.Lang_Value_Maps.Empty_Map;
   Base              : GPR2.KB.Object          := GPR2.KB.Undefined;
   Config_Project    : GPR2.Path_Name.Object   := GPR2.Path_Name.Undefined;
   File_Reader       : GPR2.File_Readers.File_Reader_Reference :=
                         GPR2.File_Readers.No_File_Reader_Reference;
   Environment       : GPR2.Environment.Object :=
                         GPR2.Environment.Process_Environment)
is
   Languages   : Containers.Language_Set;
   Conf        : Project.Configuration.Object;
   Default_Cfg : Path_Name.Object;
   Lang_Sloc   : Attribute.Object;
   --  Keep languages attribute for Sloc parameter in error message

   function Get_Updated_Search_Paths return All_Search_Paths;
   --  Let Tree use Environment & return updates search paths.

   Old_Messages : Log.Object := Self.Messages;
   --  Likewise, Self may already have some messages and we don't want
   --  to loose them when we unload the tree for conf/reconf.

   package PRA renames Project.Registry.Attribute;

   function Actual_Target return Name_Type;
   --  Returns the target, depending on the parsing stage

   procedure Add_Languages (View : Project.View.Object);
   --  Adds project languages into the Languages container to configure.
   --  Warns about project has no languages.

   function Conf_Descriptions return Project.Configuration.Description_Set;
   --  Returns set of descriptions for configuration creation

   function Default_Config_File
     (Environment : GPR2.Environment.Object) return Filename_Type;
   --  Returns default config filename

   function Runtime
     (Language : Language_Id) return Optional_Name_Type;
   --  Returns the runtime to use during configuration for the specified
   --  language.

   function Toolchain_Name
     (Language : Language_Id) return Optional_Name_Type;
   --  Returns toolchain name specified by Toolchain_Name attribute

   function Toolchain_Version
     (Language : Language_Id) return Optional_Name_Type;
   --  Returns toolchain version specified by Required_Toolchain_Version
   --  attribute.

   function Toolchain_Path
     (Language : Language_Id) return Filename_Optional;
   --  Returns toolchain search path specified by Toolchain_Path attribute

   type Reconfiguration_Status is (Unchanged, Extended, Incompatible);

   Reconf_Status : Reconfiguration_Status;

   procedure Compare_Configurations
     (Before : Project.Configuration.Description_Set;
      After  : Project.Configuration.Description_Set;
      Result : out Reconfiguration_Status);
   --  Compares two sets of description sets to check for possible changes
   --  that happened after auto-configuration. For example, new languages
   --  may be added after finding missing imports. Also, previously
   --  unresolved constructs may cause changes in already established
   --  descriptions (toolchain-related attribute under a case statement
   --  depending on a variable from a missing import).
   --  Returns Unchanged when Before and After are identical.
   --  Returns Extended when all descriptions for languages present in
   --  Before are identical to those in After, but there are new extra
   --  languages in after.
   --  Otherwise returns Incompatible and adds corresponding error messages.

   -------------------
   -- Actual_Target --
   -------------------

   function Actual_Target return Name_Type is
      Tmp_Attr : GPR2.Project.Attribute.Object;
   begin
      if Target /= No_Name and then Target /= "all" then
         --  If Target is specified as parameter, this always takes
         --  precedence
         return Target;
      end if;

      if Self.Root.Is_Defined then
         Tmp_Attr := Self.Root.Attribute (PRA.Target);
      end if;

      if Tmp_Attr.Is_Defined then
         --  Check if the project explicitly defines the attribute or if
         --  this comes from a default value.

         if not Tmp_Attr.Is_Default
           and then not Tmp_Attr.Value.Is_From_Default
           and then Tmp_Attr.Value.Text'Length > 0
         then
            return Name_Type (Tmp_Attr.Value.Text);
         end if;
      end if;

      --  No explicit target as parameter or in project: return "all"
      return "all";
   end Actual_Target;

   -------------------
   -- Add_Languages --
   -------------------

   procedure Add_Languages (View : Project.View.Object) is
   begin
      if not View.Is_Abstract
        and then View.Has_Languages
        and then View.Languages.Length = 0
      then
         Self.Append_Message
           (Message.Create
              (Level   => Message.Warning,
               Message => "no language for the project "
               & String (View.Name),
               Sloc    => View.Attribute (PRA.Languages)));
      end if;

      if View.Has_Languages then
         for L of View.Languages loop
            Languages.Include (+Name_Type (L.Text));
         end loop;

         --  Keep languages attribute for possible error message Sloc
         --  parameter.

         Lang_Sloc := View.Attribute (PRA.Languages);
      end if;
   end Add_Languages;

   ----------------------------
   -- Compare_Configurations --
   ----------------------------

   procedure Compare_Configurations
     (Before : Project.Configuration.Description_Set;
      After  : Project.Configuration.Description_Set;
      Result : out Reconfiguration_Status)
   is
      use Project.Configuration;

      Found_In_After : Boolean;

      function Error (Before, After : Description) return String;
      --  Returns string with incompatible parts of descriptions

      -----------
      -- Error --
      -----------

      function Error (Before, After : Description) return String is
         Result : Unbounded_String;

         procedure Append_Result
           (Param   : String;
            Old_Val : Optional_Name_Type;
            New_Val : Optional_Name_Type);
         --  Adds new parts of error message

         -------------------
         -- Append_Result --
         -------------------

         procedure Append_Result
           (Param   : String;
            Old_Val : Optional_Name_Type;
            New_Val : Optional_Name_Type)
         is
            Msg : constant String :=
                    Param & " """ & String (Old_Val) & """ changed to """
                  & String (New_Val) & """";
         begin
            if Result = Null_Unbounded_String then
               Result := To_Unbounded_String (Msg);
            else
               Append (Result, "; " & Msg);
            end if;
         end Append_Result;

      begin
         if Version (Before) /= Version (After) then
            Append_Result ("version", Version (Before), Version (After));
         end if;

         if Runtime (Before) /= Runtime (After) then
            Append_Result ("runtime", Runtime (Before), Runtime (After));
         end if;

         if Path (Before) /= Path (After) then
            Append_Result
              ("path",
               Optional_Name_Type (Path (Before)),
               Optional_Name_Type (Path (After)));
         end if;

         if Name (Before) /= Name (After) then
            Append_Result ("name", Name (Before), Name (After));
         end if;

         return  To_String (Result);
      end Error;

   begin
      for Descr_B of Before loop
         Found_In_After := False;

         for Descr_A of After loop
            if Language (Descr_B) = Language (Descr_A) then
               if Descr_B = Descr_A then
                  Found_In_After := True;
                  exit;

               else
                  Self.Append_Message
                    (Message.Create
                       (Level   => Message.Error,
                        Message => "incompatible change for language "
                        & Image (Language (Descr_B))
                        & " during reconfiguration",
                        Sloc    => Source_Reference.Create
                          (Self.Root.Path_Name.Value, 0, 0)));

                  Self.Append_Message
                    (Message.Create
                       (Level   => Message.Error,
                        Message => Error (Descr_B, Descr_A),
                        Sloc    => Source_Reference.Create
                          (Self.Root.Path_Name.Value, 0, 0)));

                  Result := Incompatible;

                  return;
               end if;
            end if;
         end loop;

         if not Found_In_After then
            Self.Append_Message
              (Message.Create
                 (Level   => Message.Error,
                  Message => "language " & Image (Language (Descr_B))
                  & " missing for reconfiguration",
                  Sloc    => Source_Reference.Create
                    (Self.Root.Path_Name.Value, 0, 0)));
            Result := Incompatible;
            return;
         end if;

      end loop;

      if Before'Length = After'Length then
         Result := Unchanged;
      else
         Result := Extended;
      end if;
   end Compare_Configurations;

   -----------------------
   -- Conf_Descriptions --
   -----------------------

   function Conf_Descriptions
     return Project.Configuration.Description_Set
   is
      Descr_Index : Natural := 0;
      Restricted  : constant Boolean := not Self.Langs_Of_Interest.Is_Empty;
      Length      : constant Natural :=
                      Natural
                        (if Restricted then
                           (Languages.Intersection
                              (Self.Langs_Of_Interest).Length)
                         else Languages.Length);
      Result      : Project.Configuration.Description_Set (1 .. Length);
   begin
      if Restricted and then
        Languages.Intersection (Self.Langs_Of_Interest).Is_Empty
      then
         Self.Append_Message
           (Message.Create
              (Level   => Message.Error,
               Message => "no language for the projects tree "
               & "due to language restriction",
               Sloc    => Source_Reference.Create
                 (Self.Root.Path_Name.Value, 0, 0)));

         --  Generate a default config
         return (1 => Project.Configuration.Create (Language => Ada_Language));

      end if;

      for L of Languages loop
         if Self.Langs_Of_Interest.Contains (L) or else not Restricted then
            Descr_Index := Descr_Index + 1;

            Result (Descr_Index) :=
              Project.Configuration.Create
                (Language => L,
                 Version  => Toolchain_Version (L),
                 Runtime  => Runtime (L),
                 Path     => Toolchain_Path (L),
                 Name     => Toolchain_Name (L));
         end if;
      end loop;

      return Result;
   end Conf_Descriptions;

   -------------------------
   -- Default_Config_File --
   -------------------------

   function Default_Config_File
     (Environment : GPR2.Environment.Object) return Filename_Type
   is
      Ada_RTS_Val  :  constant Value_Type :=
                       Containers.Value_Or_Default
                         (Language_Runtimes, Ada_Language);
      Ada_RTS      : constant Filename_Optional :=
                       (if Ada_RTS_Val = No_Value then No_Filename
                        else Filename_Optional
                          (Directories.Simple_Name (Ada_RTS_Val)));

      Target_Val   : constant Filename_Optional :=
                       (if Target not in No_Name | "all" then
                                          Filename_Type (Target)
                        else No_Filename);

      Platform     : constant Filename_Type :=
                       (if Target_Val /= No_Filename then
                          (if Ada_RTS = No_Filename then Target_Val
                           else Target_Val & "-" & Ada_RTS)
                        & Config_File_Extension
                        elsif Ada_RTS /= No_Filename then
                           Ada_RTS & Config_File_Extension
                        else Default_Config_Name);

      GPR_Config_V : constant String := "GPR_CONFIG";

   begin
      if Environment.Exists (GPR_Config_V) then
         declare
            GPR_CONFIG : constant String :=
                           Environment.Value (GPR_Config_V, "");
         begin
            if GPR_CONFIG = "" then
               return Platform;
            else
               if GNAT.OS_Lib.Is_Directory (GPR_CONFIG) then
                  return Filename_Type
                    (GPR_CONFIG & GNAT.OS_Lib.Directory_Separator) & Platform;
               else
                  return Filename_Type (GPR_CONFIG);
               end if;
            end if;
         end;
      else
         return Platform;
      end if;
   end Default_Config_File;

   ------------------------------
   -- Get_Updated_Search_Paths --
   ------------------------------

   function Get_Updated_Search_Paths return All_Search_Paths is
   begin
      Self.Set_Environment (Environment);
      return Self.Search_Paths;
   end Get_Updated_Search_Paths;

   -------------
   -- Runtime --
   -------------

   function Runtime
     (Language : Language_Id) return Optional_Name_Type
   is
      function Attr_As_Abs_Path
        (Attr : Attribute.Object;
         View : GPR2.Project.View.Object) return Optional_Name_Type;

      ----------------------
      -- Attr_As_Abs_Path --
      ----------------------

      function Attr_As_Abs_Path
        (Attr : Attribute.Object;
         View : GPR2.Project.View.Object) return Optional_Name_Type
      is
         Value              : constant String := String (Attr.Value.Text);
         Has_Dir_Indication : Boolean := False;
      begin
         for C of Value loop
            if C = '/' or else C = '\' then
               Has_Dir_Indication := True;
               exit;
            end if;
         end loop;

         if Has_Dir_Indication then
            if GNAT.OS_Lib.Is_Absolute_Path (Value) then
               return Name_Type (Value);
            else
               return Name_Type
                 (GNAT.OS_Lib.Normalize_Pathname (Value, View.Dir_Name.Value));
            end if;
         else
            return Optional_Name_Type (Value);
         end if;
      end Attr_As_Abs_Path;

      Tmp_Attr : Attribute.Object;
      LRT      : constant Value_Type :=
                   Containers.Value_Or_Default
                     (Language_Runtimes, Language);

   begin
      if LRT /= No_Value then
         --  Return the value given as parameter
         return Name_Type (LRT);
      end if;

      if Self.Root.Is_Defined then
         Tmp_Attr := Self.Root.Attribute
           (PRA.Runtime, Index => Attribute_Index.Create (Language));
      end if;

      if Tmp_Attr.Is_Defined then
         return Attr_As_Abs_Path (Tmp_Attr, Self.Root);
      end if;

      return No_Name;
   end Runtime;

   --------------------
   -- Toolchain_Name --
   --------------------

   function Toolchain_Name
     (Language : Language_Id) return Optional_Name_Type
   is
      Tmp_Attr : GPR2.Project.Attribute.Object;
   begin
      if Self.Root.Is_Defined then
         Tmp_Attr := Self.Root.Attribute
           (PRA.Toolchain_Name, Index => Attribute_Index.Create (Language));
      end if;

      if Tmp_Attr.Is_Defined then
         return Optional_Name_Type (Tmp_Attr.Value.Text);
      end if;

      return No_Name;
   end Toolchain_Name;

   --------------------
   -- Toolchain_Path --
   --------------------

   function Toolchain_Path
     (Language : Language_Id) return Filename_Optional
   is
      Tmp_Attr : GPR2.Project.Attribute.Object;
   begin
      if Self.Root.Is_Defined then
         Tmp_Attr := Self.Root.Attribute
           (PRA.Toolchain_Path, Index => Attribute_Index.Create (Language));
      end if;

      if Tmp_Attr.Is_Defined and then Tmp_Attr.Value.Text /= "" then
         return Filename_Type
           (GNAT.OS_Lib.Normalize_Pathname
              (Tmp_Attr.Value.Text, Self.Root.Dir_Name.Value));
      end if;

      return No_Filename;
   end Toolchain_Path;

   -----------------------
   -- Toolchain_Version --
   -----------------------

   function Toolchain_Version
     (Language : Language_Id) return Optional_Name_Type
   is
      Tmp_Attr : GPR2.Project.Attribute.Object;
   begin
      if Self.Root.Is_Defined then
         Tmp_Attr := Self.Root.Attribute
                        (PRA.Required_Toolchain_Version,
                         Index => Attribute_Index.Create (Language));
      end if;

      if Tmp_Attr.Is_Defined then
         return Optional_Name_Type (Tmp_Attr.Value.Text);
      end if;

      return No_Name;
   end Toolchain_Version;

   package Description_Set_Holders is new Ada.Containers.Indefinite_Holders
     (Project.Configuration.Description_Set,
      Project.Configuration."=");

   Pre_Conf_Description   : Description_Set_Holders.Holder;
   Post_Conf_Description  : Description_Set_Holders.Holder;
   Has_Errors             : Boolean;
   use Description_Set_Holders;

   Old_Paths : constant All_Search_Paths := Get_Updated_Search_Paths;
   --  Search paths may be affected by -aP options passed by gprtools,
   --  so we need to keep the original search paths for the reconfiguration
   --  stage.

begin
   GPR2.Project.Parser.Clear_Cache;

   if Base.Is_Defined then
      Self.Base := Base;
   end if;

   Self.Explicit_Target   :=
     +((if Target = No_Name then "all" else String (Target)));
   Self.Explicit_Runtimes := Language_Runtimes;

   Self.Load
     (Root_Project,
      Context,
      File_Reader      => File_Reader,
      Build_Path       => Build_Path,
      Root_Path        => Root_Path,
      Subdirs          => Subdirs,
      Src_Subdirs      => Src_Subdirs,
      Check_Shared_Lib => Check_Shared_Lib,
      Absent_Dir_Error => No_Error,
      Implicit_With    => Implicit_With,
      Pre_Conf_Mode    => True,
      Environment      => Environment);
   --  Ignore possible missing dirs and imported projects since they can
   --  depend on the result of auto-configuration.

   Has_Errors := Self.Messages.Has_Error;

   --  Ignore messages issued with this initial load: as we don't have
   --  a valid configuration here, we can't really know whether they
   --  are meaningful or not.

   Self.Messages.Clear;

   if not Has_Errors then
      --  First, check for Config_Prj_File declaration

      if not Default_Cfg.Is_Defined then
         declare
            CPF_Attr : Project.Attribute.Object;
         begin
            if Self.Root_Project.Check_Attribute
              (PRA.Config_Prj_File, Result => CPF_Attr)
            then
               Default_Cfg := Path_Name.Create_File
                 (Filename_Type (CPF_Attr.Value.Text));

               if not Default_Cfg.Exists then
                  Self.Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Message =>
                          "could not locate main configuration project "
                        & Default_Cfg.Value,
                        Sloc    => CPF_Attr));
                  GPR2.Project.Parser.Clear_Cache;
                  raise Project_Error with "cannot locate configuration";
               end if;
            end if;
         end;
      end if;

      --  Then check for legacy way of specifying --config among
      --  Builder.Switches or Builder.Default_Switches.

      if not Default_Cfg.Is_Defined then
         declare
            Attr_Sloc             : Project.Attribute.Object;
            Report_Obsolete_Usage : Boolean := False;
         begin
            for Attr of Self.Root_Project.Attributes (PRA.Builder.Switches)
            loop
               for Val of Attr.Values loop
                  declare
                     S : constant String := Val.Text;
                  begin
                     if S'Length > 9 and then S (1 .. 9) = "--config=" then
                        Default_Cfg := Path_Name.Create_File
                          (Filename_Type (S (10 .. S'Last)));
                        Attr_Sloc := Attr;
                        Report_Obsolete_Usage := True;
                     end if;
                  end;
               end loop;
            end loop;

            for Attr of Self.Root_Project.Attributes
              (PRA.Builder.Default_Switches)
            loop
               for Val of Attr.Values loop
                  declare
                     S : constant String := Val.Text;
                  begin
                     if S'Length > 9 and then S (1 .. 9) = "--config=" then
                        Default_Cfg := Path_Name.Create_File
                          (Filename_Type (S (10 .. S'Last)));
                        Attr_Sloc := Attr;
                        Report_Obsolete_Usage := True;
                     end if;
                  end;
               end loop;
            end loop;

            if Default_Cfg.Is_Defined and then not Default_Cfg.Exists then
               Self.Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Message =>
                       "could not locate main configuration project "
                     & Default_Cfg.Value,
                     Sloc    => Attr_Sloc));
               GPR2.Project.Parser.Clear_Cache;
               raise Project_Error with "cannot locate configuration";
            end if;

            if Report_Obsolete_Usage then
               --  The warning should go into Old_Messages because
               --  Self.Messages will be reset after applying configuration.

               Old_Messages.Append
                 (Message.Create
                    (Level   => Message.Warning,
                     Message =>
                       "--config in Builder switches is obsolescent, "
                     & "use Config_Prj_File instead",
                     Sloc    => Attr_Sloc));
            end if;
         end;
      end if;

      --  Finally, check if default config file is present

      if not Default_Cfg.Is_Defined then
         Default_Cfg :=
           Path_Name.Create_File (Default_Config_File (Environment));
      end if;

      if Default_Cfg.Exists then
         if not Language_Runtimes.Is_Empty then
            Old_Messages.Append
              (Message.Create
                 (Level   => Message.Warning,
                  Message => "runtimes are taken into account "
                  & "only in auto-configuration",
                  Sloc    => Source_Reference.Create
                    (Default_Cfg.Value, 0, 0)));
         end if;

         Conf := Project.Configuration.Load (Default_Cfg);
      end if;

   end if;

   if not Conf.Is_Defined then
      if not Has_Errors then
         --  No configuration file specified, but project load was sucessfull.
         --  Generate configuration automatically.

         --  This involves some delicate bootstrap:
         --  1- we load the project without configuration
         --  2- using the loaded project, we determine
         --     * the Target: if explicitly given to us, this one is used,
         --       else if the project defines it, this one is used, else the
         --       host's value is used.
         --     * the list of languages
         --     and we load a configuration for the above.
         --  3- we then reload the project with the configuration

         for C in Self.Iterate
           (Filter =>
              (F_Aggregate | F_Aggregate_Library => False, others => True))
         loop
            Add_Languages (Element (C));
         end loop;

         if Languages.Length = 0 then
            if not Self.Root_Project.Is_Abstract then
               Self.Append_Message
                 (Message.Create
                    (Level   => Message.Warning,
                     Message => "no language for the projects tree: "
                     & "configuration skipped",
                     Sloc    => (if Lang_Sloc.Is_Defined
                                 then Lang_Sloc
                                 else Source_Reference.Create
                                   (Self.Root.Path_Name.Value, 0, 0))));
            end if;

            GPR2.Project.Parser.Clear_Cache;
            return;
         end if;

      else
         --  Generate a default config, since a critical failure occurred:
         --  this will reload the project in normal mode and print the
         --  relevant error messages.
         --  Additionally, this is used when attempting to load a predefined
         --  project. First loading attempt results in a failure since we don't
         --  have the toolchain yet and we cannot find the project, so we use
         --  default Ada config to get the toolchain and then retry loading
         --  the project.

         Languages.Include (Ada_Language);
      end if;

      Pre_Conf_Description := To_Holder (Conf_Descriptions);

      if not Self.Base.Is_Defined then
         Self.Base := GPR2.KB.Create
           (GPR2.KB.Default_Flags, Environment => Environment);
      end if;

      Conf := Project.Configuration.Create
        (Pre_Conf_Description.Element,
         Actual_Target,
         (if Self.Root.Is_Defined then Self.Root.Path_Name
          elsif Root_Project.Kind = Project_Definition
          then Root_Project.Data.Trees.Project.Path_Name
          else Root_Project.Path),
         Self.Base,
         Save_Name   => Config_Project,
         Environment => Environment);
   end if;

   if Conf.Has_Error then
      for M of Conf.Log_Messages loop
         Self.Append_Message (M);
      end loop;

      GPR2.Project.Parser.Clear_Cache;
      raise Project_Error with "cannot create configuration";
   end if;

   --  Unload the project that was loaded without configuration.
   --  We need to backup the messages and default search path:
   --  messages issued during configuration are relevant, together with
   --  already computed search paths

   Self.Unload (False);
   Self.Messages := Old_Messages;
   Self.Search_Paths := Old_Paths;

   Self.Load
     (Root_Project,
      Context, Conf,
      File_Reader      => File_Reader,
      Build_Path       => Build_Path,
      Root_Path        => Root_Path,
      Subdirs          => Subdirs,
      Src_Subdirs      => Src_Subdirs,
      Check_Shared_Lib => Check_Shared_Lib,
      Absent_Dir_Error => Absent_Dir_Error,
      Implicit_With    => Implicit_With,
      Environment      => Environment);

   if Default_Cfg.Is_Defined and then Default_Cfg.Exists then
      --  No need for reconfiguration if explicit default configuration
      --  project has been specified.
      GPR2.Project.Parser.Clear_Cache;
      return;
   end if;

   --  Configuration parameters might have changed, i.e. new languages
   --  may be added from missing imported projects that have been found
   --  after search path update from configuration data. We need to check
   --  for that and perform a reconfiguration if necessary.

   Languages.Clear;

   for C in Self.Iterate
     (Filter => (F_Aggregate | F_Aggregate_Library => False,
                 others                            => True))
   loop
      Add_Languages (Element (C));
   end loop;

   Post_Conf_Description := To_Holder (Conf_Descriptions);

   if not Pre_Conf_Description.Is_Empty then
      Compare_Configurations
        (Pre_Conf_Description.Element,
         Post_Conf_Description.Element,
         Reconf_Status);

      if Reconf_Status = Unchanged then
         --  Nothing changed, no need for reconfiguration
         GPR2.Project.Parser.Clear_Cache;
         return;
      end if;

      if Reconf_Status = Incompatible then
         GPR2.Project.Parser.Clear_Cache;
         raise Project_Error with "reconfiguration error";
      end if;
   end if;

   --  We need to reconfigure in order to account for new languages

   Conf := Project.Configuration.Create
     (Post_Conf_Description.Element,
      Actual_Target,
      Self.Root.Path_Name,
      Self.Base,
      Save_Name   => Config_Project,
      Environment => Environment);

   Self.Unload (False);
   Self.Messages := Old_Messages;
   Self.Search_Paths := Old_Paths;

   Self.Load
     (Root_Project,
      Context, Conf,
      File_Reader      => File_Reader,
      Build_Path       => Build_Path,
      Root_Path        => Root_Path,
      Subdirs          => Subdirs,
      Src_Subdirs      => Src_Subdirs,
      Check_Shared_Lib => Check_Shared_Lib,
      Absent_Dir_Error => Absent_Dir_Error,
      Implicit_With    => Implicit_With,
      Environment      => Environment);

   GPR2.Project.Parser.Clear_Cache;
end Load_Autoconf;
