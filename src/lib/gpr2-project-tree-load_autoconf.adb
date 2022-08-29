--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

separate (GPR2.Project.Tree)
procedure Load_Autoconf
  (Self              : in out Object;
   Root_Project      : Project_Descriptor;
   Context           : GPR2.Context.Object;
   Build_Path        : Path_Name.Object        := Path_Name.Undefined;
   Subdirs           : Optional_Name_Type      := No_Name;
   Src_Subdirs       : Optional_Name_Type      := No_Name;
   Check_Shared_Lib  : Boolean                 := True;
   Absent_Dir_Error  : Boolean                 := False;
   Implicit_With     : GPR2.Path_Name.Set.Object :=
                         GPR2.Path_Name.Set.Empty_Set;
   Target            : Optional_Name_Type      := No_Name;
   Language_Runtimes : Containers.Lang_Value_Map :=
                         Containers.Lang_Value_Maps.Empty_Map;
   Base              : GPR2.KB.Object          := GPR2.KB.Undefined;
   Config_Project    : GPR2.Path_Name.Object   := GPR2.Path_Name.Undefined;
   File_Reader       : GPR2.File_Readers.File_Reader_Reference :=
                         GPR2.File_Readers.No_File_Reader_Reference)
is
   Languages   : Containers.Language_Set;
   Conf        : Project.Configuration.Object;
   GNAT_Prefix : constant String := Get_Tools_Directory;
   Default_Cfg : Path_Name.Object;
   Lang_Sloc   : Attribute.Object;
   --  Keep languages attribute for Sloc parameter in error message

   Old_Paths    : constant Path_Name.Set.Object := Self.Search_Paths;
   --  Search paths may be affected by -aP options passed by gprtools,
   --  so we need to keep the original search paths for the reconfiguration
   --  stage.
   Old_Messages : constant Log.Object := Self.Messages;
   --  Likewise, Self may already have some messages and we don't want
   --  to loose them when we unload the tree for conf/reconf.

   function Actual_Target return Name_Type;
   --  Returns the target, depending on the parsing stage

   procedure Add_Languages (View : Project.View.Object);
   --  Adds project languages into the Languages container to configure.
   --  Warns about project has no languages.

   function Conf_Descriptions return Project.Configuration.Description_Set;
   --  Returns set of descriptions for configuration creation

   function Default_Config_File return Filename_Type;
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
   --  that happened after autoconfiguration. For example, new languages
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
                          (Self.Root.Path_Name.Value, 0, 0),
                        Raw     => True));

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
      Result      : Project.Configuration.Description_Set
        (1 .. Positive (Languages.Length));
   begin
      for L of Languages loop
         Descr_Index := Descr_Index + 1;

         Result (Descr_Index) :=
           Project.Configuration.Create
             (Language => L,
              Version  => Toolchain_Version (L),
              Runtime  => Runtime (L),
              Path     => Toolchain_Path (L),
              Name     => Toolchain_Name (L));
      end loop;

      return Result;
   end Conf_Descriptions;

   -------------------------
   -- Default_Config_File --
   -------------------------

   function Default_Config_File return Filename_Type is
      Ada_RTS_Val : constant Value_Type :=
                      Containers.Value_Or_Default
                        (Language_Runtimes, Ada_Language);
      Ada_RTS     : constant Filename_Optional :=
                      (if Ada_RTS_Val = No_Value then No_Filename
                       else Filename_Optional
                         (Directories.Simple_Name (Ada_RTS_Val)));
   begin
      if Target not in No_Name | "all" then
         return Filename_Type (Target)
           & (if Ada_RTS = No_Filename then "" else "-" & Ada_RTS)
           & Config_File_Extension;

      elsif Ada_RTS /= No_Filename then
         return Ada_RTS & Config_File_Extension;

      else
         declare
            GPR_Config : constant String := "GPR_CONFIG";
            Filename   : constant String :=
                           Environment_Variables.Value (GPR_Config, "");
         begin
            if Filename = "" then
               return Default_Config_Name;
            else
               return Filename_Type (Filename);
            end if;
         end;
      end if;
   end Default_Config_File;

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
               return Name_Type (GNAT.OS_Lib.Normalize_Pathname
                                 (Value, View.Dir_Name.Value));
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

begin
   if GNAT_Prefix = "" then
      --  No GNAT, use default config only in current directory

      Default_Cfg := Path_Name.Create_File (Default_Config_File);

   else
      --  GNAT found, look for the default config first in the current
      --  directory and then in the GNAT/share/gpr

      Default_Cfg :=
        Create
          (Default_Config_File,
           Path_Name.Set.To_Set
             (Path_Name.Create_Directory
                ("share", Filename_Type (GNAT_Prefix)).Compose
              ("gpr", Directory => True)));
   end if;

   if Default_Cfg.Exists then
      Conf := Project.Configuration.Load (Default_Cfg);
   end if;

   if Base.Is_Defined then
      Self.Base := Base;
   end if;

   Self.Explicit_Target   :=
     +((if Target = No_Name then "all" else String (Target)));
   Self.Explicit_Runtimes := Language_Runtimes;

   if not Conf.Is_Defined then
      --  Default configuration file does not exists. Generate configuration
      --  automatically.

      --  This involves some delicate bootstrap:
      --  1- we load the project without configuration
      --  2- using the loaded project, we determine
      --     * the Target: if explicitely given to us, this one is used,
      --       else if the project defines it, this one is used, else the
      --       host's value is used.
      --     * the list of languages
      --     and we load a configuration for the above.
      --  3- we then reload the project with the configuration

      Self.Load
        (Root_Project,
         Context,
         File_Reader      => File_Reader,
         Build_Path       => Build_Path,
         Subdirs          => Subdirs,
         Src_Subdirs      => Src_Subdirs,
         Check_Shared_Lib => Check_Shared_Lib,
         Absent_Dir_Error => False,
         Implicit_With    => Implicit_With,
         Pre_Conf_Mode    => True);
      --  Ignore possible missing dirs and imported projects since they can
      --  depend on the result of autoconfiguration.

      Has_Errors := Self.Messages.Has_Error;

      --  Ignore messages issued with this initial load: as we don't have
      --  a valid configuration here, we can't really know whether they
      --  are meaningful or not.

      Self.Messages.Clear;

      if not Has_Errors then
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

            return;
         end if;

      else
         --  Generate a default config, since a critical failure occurred:
         --  this will reload the project in normal mode and print the
         --  relevant error messages.

         Languages.Include (Ada_Language);
      end if;

      Pre_Conf_Description := To_Holder (Conf_Descriptions);

      if not Self.Base.Is_Defined then
         Self.Base := GPR2.KB.Create (GPR2.KB.Default_Flags);
      end if;

      Conf := Project.Configuration.Create
        (Pre_Conf_Description.Element,
         Actual_Target,
         (if Self.Root.Is_Defined then Self.Root.Path_Name
          elsif Root_Project.Kind = Project_Definition
          then Root_Project.Data.Trees.Project.Path_Name
          else Root_Project.Path),
         Self.Base,
         Save_Name => Config_Project);

      if Conf.Has_Error then
         for M of Conf.Log_Messages loop
            Self.Append_Message (M);
         end loop;

         raise Project_Error with "cannot create configuration";
      end if;

      --  Unload the project that was loaded without configuration.
      --  We need to backup the messages and default search path:
      --  messages issued during configuration are relevant, together with
      --  already computed search paths

      Self.Unload (False);
      Self.Messages := Old_Messages;
      Self.Search_Paths := Old_Paths;
   end if;

   Self.Load
     (Root_Project,
      Context, Conf,
      File_Reader      => File_Reader,
      Build_Path       => Build_Path,
      Subdirs          => Subdirs,
      Src_Subdirs      => Src_Subdirs,
      Check_Shared_Lib => Check_Shared_Lib,
      Absent_Dir_Error => Absent_Dir_Error,
      Implicit_With    => Implicit_With);

   if Default_Cfg.Exists then
      --  No need for reconfiguration if explicit default configuration
      --  project has been specified.
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
         return;
      end if;

      if Reconf_Status = Incompatible then
         raise Project_Error with "reconfiguration error";
      end if;
   end if;

   --  We need to reconfigure in order to account for new languages

   Conf := Project.Configuration.Create
     (Post_Conf_Description.Element,
      Actual_Target,
      Self.Root.Path_Name,
      Self.Base,
      Save_Name => Config_Project);

   Self.Unload (False);
   Self.Messages := Old_Messages;
   Self.Search_Paths := Old_Paths;

   Self.Load
     (Root_Project,
      Context, Conf,
      File_Reader      => File_Reader,
      Build_Path       => Build_Path,
      Subdirs          => Subdirs,
      Src_Subdirs      => Src_Subdirs,
      Check_Shared_Lib => Check_Shared_Lib,
      Absent_Dir_Error => Absent_Dir_Error,
      Implicit_With    => Implicit_With);
end Load_Autoconf;
