------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2021, AdaCore                      --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Environment_Variables;

with GNAT.OS_Lib;
with GNAT.String_Split;

with GPR2.KB;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Definition;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Source_Reference.Value;
with GPR2.View_Ids;

package body GPR2.Project.Configuration is

   package PRA renames Project.Registry.Attribute;
   package PRP renames Project.Registry.Pack;

   --------------------
   -- Archive_Suffix --
   --------------------

   function Archive_Suffix (Self : Object) return Filename_Type is
      Cache : constant Cache_Refcount.Reference_Type := Self.Cache.Get;
   begin
      if Cache.Archive_Suffix = "" then
         Cache.Archive_Suffix :=
           Self.Conf.Attribute (PRA.Archive_Suffix).Value.Unchecked_Text;
      end if;

      return Filename_Type (-Cache.Archive_Suffix);
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
      Base       : in out GPR2.KB.Object)
      return Object
   is

      Settings_Local : Description_Set := Settings;

      Native_Target : constant Boolean := Target = "all";

      Result    : Object;
      Host      : constant Name_Type :=
                    Name_Type (System.OS_Constants.Target_Name);

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
            Normalized : constant Name_Type := Base.Normalized_Target (Host);
         begin
            if Normalized = "unknown" then
               Configuration_String :=
                 Base.Configuration
                   (Settings => Settings_Local,
                    Target   => Name_Type (System.OS_Constants.Target_Name),
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

         if Path_Name.Temporary_Directory.Is_Defined then
            Result.Project :=
              Parser.Project.Parse
                (Contents        => Configuration_String,
                 Messages        => Parsing_Messages,
                 Pseudo_Filename => Path_Name.Create_File
                   ("autoconf.cgpr",
                    Filename_Type (Path_Name.Temporary_Directory.Value)));
         else
            Result.Project :=
              Parser.Project.Parse
                (Contents        => Configuration_String,
                 Messages        => Parsing_Messages,
                 Pseudo_Filename => Path_Name.Create_File
                   ("autoconf.cgpr",
                    Filename_Type (Project_Path.Dir_Name)));
         end if;

         --  Continue only if there is no parsing error on the configuration
         --  project.

         if Result.Project.Is_Defined then
            Result.Target :=
              (if Target = "all"
               then Null_Unbounded_String
               else To_Unbounded_String (String (Target)));
         end if;

         for S of Settings loop
            Result.Descriptions.Append (S);
         end loop;

      else

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
     (Filename : Path_Name.Object;
      Target   : Name_Type := "all") return Object
   is
      Result : Object;
   begin
      Result.Project :=
        Parser.Project.Parse
          (Filename, GPR2.Path_Name.Set.Empty_Set, Result.Messages);

      --  Continue only if there is no parsing error on the configuration
      --  project.

      if Result.Project.Is_Defined then
         Result.Target :=
           (if Target = "all"
            then Null_Unbounded_String
            else To_Unbounded_String (String (Target)));
      end if;

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
      Cache : constant Cache_Refcount.Reference_Type := Self.Cache.Get;
      A     : Project.Attribute.Object;
   begin
      if not Cache.Object_File_Suffix.Contains (Language) then
         if Self.Conf.Has_Packages (PRP.Compiler)
           and then Self.Conf.Pack (PRP.Compiler).Check_Attribute
                      (PRA.Object_File_Suffix,
                       Attribute_Index.Create (Language),
                       Result => A)
         then
            Cache.Object_File_Suffix.Include (Language, A.Value.Text);
         else
            Cache.Object_File_Suffix.Include (Language, ".o");
         end if;
      end if;

      return Filename_Type (Cache.Object_File_Suffix.Element (Language));
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

   ------------
   -- Target --
   ------------

   function Target (Self : Object) return Optional_Name_Type is
   begin
      return Optional_Name_Type (To_String (Self.Target));
   end Target;

begin
   Definition.Bind_Configuration_To_Tree := Bind_To_Tree'Access;
end GPR2.Project.Configuration;
