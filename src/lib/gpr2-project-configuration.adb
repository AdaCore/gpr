--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Environment_Variables;
with Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.String_Split;

with GPR2.KB;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Definition;
with GPR2.Project.Registry.Attribute;
with GPR2.Source_Reference.Value;
with GPR2.View_Ids;

package body GPR2.Project.Configuration is

   package PRA renames Project.Registry.Attribute;

   --------------------
   -- Archive_Suffix --
   --------------------

   function Archive_Suffix (Self : Object) return Filename_Type is
   begin
      return Filename_Type
        (-Self.Conf.Attribute (PRA.Archive_Suffix).Value.Unchecked_Text);
   end Archive_Suffix;

   ------------------
   -- Bind_To_Tree --
   ------------------

   procedure Bind_To_Tree
     (Self : in out Object;
      Tree : not null access Project.Tree.Object)
   is
      Data : Definition.Data;
   begin
      Data.Trees.Project := Self.Project;
      Data.Kind          := K_Configuration;
      Data.Tree          := Tree;
      Data.Path          := Path_Name.Create_Directory
                              (Filename_Type
                                 (Self.Project.Path_Name.Dir_Name));
      Data.Unique_Id     := GPR2.View_Ids.Config_View_Id;
      Self.Conf          := Definition.Register (Data);
   end Bind_To_Tree;

   ------------------------
   -- Corresponding_View --
   ------------------------

   function Corresponding_View (Self : Object) return Project.View.Object is
   begin
      return Self.Conf;
   end Corresponding_View;

   ------------
   -- Create --
   ------------

   function Create
     (Language : Language_Id;
      Version  : Optional_Name_Type := No_Name;
      Runtime  : Optional_Name_Type := No_Name;
      Path     : Filename_Optional  := No_Filename;
      Name     : Optional_Name_Type := No_Name) return Description
   is
      function "+" (Str : Optional_Name_Type) return Unbounded_String
        is (To_Unbounded_String (String (Str)));
   begin
      return Description'
        (Language => Language,
         Version  => +Version,
         Runtime  => +Runtime,
         Path     => +String (Path),
         Name     => +Name);
   end Create;

   function Create
     (Settings   : Description_Set;
      Target     : Name_Type;
      Project    : GPR2.Path_Name.Object;
      Base       : in out GPR2.KB.Object;
      Save_Name  : GPR2.Path_Name.Object := GPR2.Path_Name.Undefined)
      return Object
   is

      Settings_Local : Description_Set := Settings;

      Native_Target : constant Boolean := Target = "all";

      Result    : Object;

      Configuration_String : Unbounded_String;
      Parsing_Messages     : Log.Object;

      Project_Path : constant Path_Name.Object :=
                       (if Project.Has_Dir_Name then Project
                        else Create (Project.Name));
      --  Project may be not even found at this stage, but since we ignore
      --  all errors at first parsing we don't know it yet. We need to resolve
      --  the Project path name for proper error reporting.

      function Check_Runtime_Dir (Dir : Path_Name.Object) return Boolean;
      --  Checks if Dir can be a runtime directory

      function Locate_Runtime
        (Dir : Filename_Optional; Path : String) return Path_Name.Object;
      --  Returns runtime DIR directory resolved against Path or Undefined
      --  if nothing is found.

      -----------------------
      -- Check_Runtime_Dir --
      -----------------------

      function Check_Runtime_Dir (Dir : Path_Name.Object) return Boolean is
         Adalib_Dir     : constant Path_Name.Object :=
                            Path_Name.Create_Directory
                              ("adalib", Filename_Type (Dir.Value));
         Adainclude_Dir : constant Path_Name.Object :=
                            Path_Name.Create_Directory
                              ("adainclude", Filename_Type (Dir.Value));
         AOP_File       : constant Path_Name.Object :=
                            Path_Name.Create_File
                              ("ada_object_path", Filename_Type (Dir.Value));
         ASP_File       : constant Path_Name.Object :=
                            Path_Name.Create_File
                              ("ada_source_path", Filename_Type (Dir.Value));
      begin
         return (Adalib_Dir.Exists or else AOP_File.Exists)
           and then (Adainclude_Dir.Exists or else ASP_File.Exists);
      end Check_Runtime_Dir;

      --------------------
      -- Locate_Runtime --
      --------------------

      function Locate_Runtime
        (Dir : Filename_Optional; Path : String) return Path_Name.Object is
         use GNAT;

         Paths       : GNAT.String_Split.Slice_Set;
         Runtime_Dir : Path_Name.Object;
      begin
         String_Split.Create (Paths, Path, (1 => OS_Lib.Path_Separator));
         for J in 1 .. String_Split.Slice_Count (Paths) loop
            Runtime_Dir := Path_Name.Create_Directory
              (Dir,
               Filename_Optional (String_Split.Slice (Paths, J)));

            if Runtime_Dir.Exists then
               return Runtime_Dir;
            end if;
         end loop;

         return Path_Name.Undefined;
      end Locate_Runtime;

   begin

      --  Ada runtime has a special 3 step lookup:
      --  1) check <runtime> subdir relatively to root project location
      --  2) check <runtime> subdir relatively to GPR_RUNTIME_PATH value
      --  3) pass it as is to configuration creation.
      --
      --  If step 1 or 2 results in a valid runtime dir, pass full path
      --  to it to cofiguration creation.
      --  If on step 2 corresponding directory is found, but it does not
      --  have runtime features, configuration is abandoned.

      for Descr in Settings_Local'Range loop
         if Settings_Local (Descr).Language = Ada_Language
           and then Runtime (Settings_Local (Descr)) /= No_Name
           and then not GNAT.OS_Lib.Is_Absolute_Path
             (To_String (Settings_Local (Descr).Runtime))
         then
            declare
               Runtime_Dir   : Path_Name.Object :=
                 Path_Name.Create_Directory
                   (Filename_Optional (Runtime (Settings_Local (Descr))),
                    Filename_Optional (Project_Path.Dir_Name));
            begin
               if Runtime_Dir.Exists and then Check_Runtime_Dir (Runtime_Dir)
               then
                  Settings_Local (Descr).Runtime :=
                    To_Unbounded_String (Runtime_Dir.Value);
                  exit;
               end if;

               if Ada.Environment_Variables.Exists ("GPR_RUNTIME_PATH") then

                  Runtime_Dir := Locate_Runtime
                    (Filename_Optional (Runtime (Settings_Local (Descr))),
                     Ada.Environment_Variables.Value ("GPR_RUNTIME_PATH"));

                  if Runtime_Dir.Is_Defined and then Runtime_Dir.Exists then
                     if Check_Runtime_Dir (Runtime_Dir) then

                        Settings_Local (Descr).Runtime :=
                          To_Unbounded_String (Runtime_Dir.Value);
                        exit;
                     else

                        Result.Messages.Append
                          (Message.Create
                             (Message.Error,
                              "invalid runtime directory " & Runtime_Dir.Value,
                              Sloc => Source_Reference.Create
                                (Project_Path.Value, 0, 0)));

                        return Result;
                     end if;
                  end if;

               end if;
            end;

            exit;
         end if;
      end loop;

      if Native_Target then
         --  Normalize implicit target
         declare
            Normalized : constant Name_Type :=
                           Base.Normalized_Target (GPR2.KB.Default_Target);
         begin
            if Normalized = "unknown" then
               Configuration_String :=
                 Base.Configuration
                   (Settings => Settings_Local,
                    Target   => GPR2.KB.Default_Target,
                    Messages => Result.Messages,
                    Fallback => True);
            else
               Configuration_String :=
                 Base.Configuration
                   (Settings => Settings_Local,
                    Target   => Normalized,
                    Messages => Result.Messages,
                    Fallback => True);
            end if;
         end;

      else
         Configuration_String :=
           Base.Configuration
             (Settings => Settings_Local,
              Target   => Target,
              Messages => Result.Messages,
              Fallback => False);
      end if;

      if Configuration_String /= Null_Unbounded_String then
         if Save_Name.Is_Defined then
            declare
               Output : Text_IO.File_Type;
            begin
               Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File,
                                   String (Save_Name.Value));
               Ada.Text_IO.Put_Line
                 (Output, To_String (Configuration_String));
               Ada.Text_IO.Close (Output);
            end;
         end if;

         if Path_Name.Temporary_Directory.Is_Defined then
            Result.Project :=
              GPR2.Project.Parser.Parse
                (Contents        => Configuration_String,
                 Messages        => Parsing_Messages,
                 Pseudo_Filename => Path_Name.Create_File
                   ("autoconf.cgpr",
                    Filename_Type (Path_Name.Temporary_Directory.Value)));
         else
            Result.Project :=
              GPR2.Project.Parser.Parse
                (Contents        => Configuration_String,
                 Messages        => Parsing_Messages,
                 Pseudo_Filename => Path_Name.Create_File
                   ("autoconf.cgpr",
                    Filename_Type (Project_Path.Dir_Name)));
         end if;

         for M of Parsing_Messages loop
            Result.Messages.Append (M);
         end loop;

         for S of Settings loop
            Result.Descriptions.Append (S);
         end loop;

      else
         for Msg of Base.Log_Messages loop
            Result.Messages.Append (Msg);
         end loop;

         Result.Messages.Append
           (Message.Create
              (Message.Error,
               "cannot create configuration file",
               Sloc => Source_Reference.Create (Project_Path.Value, 0, 0)));
      end if;

      Result.Cache.Set (Config_Cache_Object'(others => <>));

      return Result;
   end Create;

   ----------------------------
   -- Dependency_File_Suffix --
   ----------------------------

   function Dependency_File_Suffix
     (Self     : Object;
      Language : Language_Id) return Filename_Type
   is
      pragma Unreferenced (Self);
   begin
      --  ??? there is no attribute in the configuration file for this, so we
      --  end up having hard coded value for Ada and all other languages.
      if Language = Ada_Language then
         return ".ali";
      else
         return ".d";
      end if;
   end Dependency_File_Suffix;

   ---------------
   -- Externals --
   ---------------

   function Externals (Self : Object) return Containers.Name_List is
   begin
      return Self.Project.Externals;
   end Externals;

   -------------------
   -- Has_Externals --
   -------------------

   function Has_Externals (Self : Object) return Boolean is
   begin
      return Self.Project.Is_Defined and then Self.Project.Has_Externals;
   end Has_Externals;

   ------------------
   -- Has_Messages --
   ------------------

   function Has_Messages (Self : Object) return Boolean is
   begin
      return not Self.Messages.Is_Empty;
   end Has_Messages;

   ----------
   -- Load --
   ----------

   function Load
     (Filename : Path_Name.Object) return Object
   is
      Result : Object;
   begin
      Result.Project :=
        Project.Parser.Parse
          (Filename, GPR2.Path_Name.Set.Empty_Set, Result.Messages);

      --  Continue only if there is no parsing error on the configuration
      --  project.

      Result.Cache.Set (Config_Cache_Object'(others => <>));

      return Result;
   end Load;

   ------------------
   -- Log_Messages --
   ------------------

   function Log_Messages (Self : Object) return Log.Object is
   begin
      return Self.Messages;
   end Log_Messages;

   ------------------------
   -- Object_File_Suffix --
   ------------------------

   function Object_File_Suffix
     (Self     : Object;
      Language : Language_Id) return Filename_Type
   is
      Attr  : constant Project.Attribute.Object :=
                Self.Conf.Attribute
                  (Name  => PRA.Compiler.Object_File_Suffix,
                   Index => Attribute_Index.Create (Language));
   begin
      if Attr.Is_Defined then
         return Filename_Type (Attr.Value.Text);
      else
         return ".o";
      end if;
   end Object_File_Suffix;

   -------------
   -- Runtime --
   -------------

   function Runtime
     (Self : Object; Language : Language_Id) return Optional_Name_Type is
   begin
      for Description of Self.Descriptions loop
         if Description.Language = Language then
            return Optional_Name_Type (To_String (Description.Runtime));
         end if;
      end loop;

      return "";
   end Runtime;

begin
   Definition.Bind_Configuration_To_Tree := Bind_To_Tree'Access;
end GPR2.Project.Configuration;
