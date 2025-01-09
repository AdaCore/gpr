--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Text_IO;

with GNATCOLL.OS.FSUtil;

with GPR2.KB;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Source_Reference.Value;
with GPR2.View_Ids;
with GPR2.View_Internal;

package body GPR2.Project.Configuration is

   package PRA renames Project.Registry.Attribute;

   function Externals
     (Self : Project.Configuration.Object)
      return GPR2.Project_Parser.Externals_Map
   is (Self.Project.Externals);

   --------------------
   -- Archive_Suffix --
   --------------------

   function Archive_Suffix (Self : Object) return Filename_Type is
   begin
      return -Self.Conf.Attribute (PRA.Archive_Suffix).Value.Unchecked_Text;
   end Archive_Suffix;

   ------------------
   -- Bind_To_Tree --
   ------------------

   procedure Bind_To_Tree
     (Self : in out Object;
      Tree : not null access Tree_Internal.Object)
   is
      Data : View_Internal.Data;
   begin
      Data.Trees.Project := Self.Project;
      Data.Kind          := K_Configuration;
      Data.Tree          := Tree;
      Data.Path          := Path_Name.Create_Directory
                              (Filename_Type
                                 (Self.Project.Path_Name.Dir_Name));
      Data.Unique_Id     := GPR2.View_Ids.Config_View_Id;
      Self.Conf          := View_Internal.Register (Data);
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
     (Settings    : Description_Set;
      Target      : Name_Type;
      Project     : GPR2.Path_Name.Object;
      Base        : in out GPR2.KB.Object;
      Save_Name   : GPR2.Path_Name.Object := GPR2.Path_Name.Undefined;
      Environment : GPR2.Environment.Object :=
                      GPR2.Environment.Process_Environment)
      return Object
   is
      procedure Ensure_Directory (Path : Path_Name.Object);
      --  If Path is a directory, this ensures this exist, and if it is a file
      --  this ensures its enclosing directory exists, recursively.

      ----------------------
      -- Ensure_Directory --
      ----------------------

      procedure Ensure_Directory (Path : Path_Name.Object) is
      begin
         if not Path.Is_Directory
           and then not Path.Containing_Directory.Exists
         then
            Ensure_Directory (Path.Containing_Directory);
         elsif Path.Is_Directory
           and then not Path.Exists
         then
            Ensure_Directory (Path.Containing_Directory);
            if not GNATCOLL.OS.FSUtil.Create_Directory (Path.String_Value) then
               raise GPR2.Project_Error with
                 "cannot create directory " & Path.String_Value;
            end if;
         end if;
      end Ensure_Directory;

      Settings_Local : constant Description_Set := Settings;

      Native_Target : constant Boolean := Target = "all";

      Result    : Object;

      Configuration_String : Unbounded_String;
      Parsing_Messages     : Log.Object;

      Project_Path : constant Path_Name.Object :=
                       (if Project.Has_Dir_Name then Project
                        else Create (Project.Name, False));
      --  Project may be not even found at this stage, but since we ignore
      --  all errors at first parsing we don't know it yet. We need to resolve
      --  the Project path name for proper error reporting.

   begin
      if Native_Target then
         --  Normalize implicit target
         declare
            Normalized : constant Name_Type :=
                           Base.Normalized_Target (GPR2.KB.Default_Target);
         begin
            if Normalized = "unknown" then
               Configuration_String :=
                 Base.Configuration
                   (Settings    => Settings_Local,
                    Target      => GPR2.KB.Default_Target,
                    Messages    => Result.Messages,
                    Fallback    => True,
                    Environment => Environment);
            else
               Configuration_String :=
                 Base.Configuration
                   (Settings    => Settings_Local,
                    Target      => Normalized,
                    Messages    => Result.Messages,
                    Fallback    => True,
                    Environment => Environment);
            end if;
         end;

      else
         Configuration_String :=
           Base.Configuration
             (Settings    => Settings_Local,
              Target      => Target,
              Messages    => Result.Messages,
              Fallback    => False,
              Environment => Environment);
      end if;

      if Configuration_String /= Null_Unbounded_String then
         if Save_Name.Is_Defined then
            declare
               Output : Text_IO.File_Type;
            begin
               Ensure_Directory (Save_Name);
               Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File,
                                   Save_Name.String_Value);
               Ada.Text_IO.Put_Line
                 (Output, To_String (Configuration_String));
               Ada.Text_IO.Close (Output);
            end;
         end if;

         Result.Project :=
           GPR2.Project_Parser.Parse
             (Contents        => Configuration_String,
              Messages        => Parsing_Messages,
              Pseudo_Filename =>
                Path_Name.Create_Pseudo_File ("autoconf.cgpr"));

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
        Project_Parser.Parse
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
   View_Internal.Bind_Configuration_To_Tree := Bind_To_Tree'Access;
   View_Internal.Configuration_Externals := Externals'Access;
end GPR2.Project.Configuration;
