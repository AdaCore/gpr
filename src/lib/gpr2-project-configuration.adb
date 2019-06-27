------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Strings.Fixed;

with Ada.Directories;
with GNAT.OS_Lib;

with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Definition;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Value;

package body GPR2.Project.Configuration is

   package PRA renames Project.Registry.Attribute;
   package PRP renames Project.Registry.Pack;

   --------------------
   -- Archive_Suffix --
   --------------------

   function Archive_Suffix (Self : Object) return Name_Type is
   begin
      return Name_Type (Self.Conf.Attribute (PRA.Archive_Suffix).Value.Text);
   end Archive_Suffix;

   ------------------
   -- Bind_To_Tree --
   ------------------

   procedure Bind_To_Tree
     (Self : in out Object;
      Tree : not null access Project.Tree.Object)
   is
      Data : Definition.Data (Has_Context => False);
   begin
      Data.Trees.Project := Self.Project;
      Data.Status        := Root;
      Data.Kind          := K_Configuration;
      Data.Tree          := Tree;
      Data.Path          := Path_Name.Create_Directory
                              (Name_Type (Self.Project.Path_Name.Dir_Name));
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
     (Language : Name_Type;
      Version  : Optional_Name_Type := No_Name;
      Runtime  : Optional_Name_Type := No_Name;
      Path     : Optional_Name_Type := No_Name;
      Name     : Optional_Name_Type := No_Name) return Description
   is
      function "+" (Str : Optional_Name_Type) return Unbounded_String
        is (To_Unbounded_String (String (Str)));
   begin
      return Description'
        (Language => +Language,
         Version  => +Version,
         Runtime  => +Runtime,
         Path     => +Path,
         Name     => +Name);
   end Create;

   function Create
     (Settings : Description_Set;
      Target   : Name_Type;
      Project  : GPR2.Path_Name.Object) return Object
   is
      --  Note that this is a temporary implementation to bring a solution
      --  for the configuration support in LibGPR2. The long term and proper
      --  solution will be implemented later based on the knowledge base API
      --  which could be reviewed from scratch.

      use Ada;
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use GNAT;

      function Process_Id return String is
        (Strings.Fixed.Trim
           (Integer'Image (OS_Lib.Pid_To_Integer (OS_Lib.Current_Process_Id)),
            Strings.Both));

      --  Note that Temporary_Directory_Path could be the empty string and in
      --  this case we just write the temporary file into the current working
      --  directory.

      Key : constant String := Config_File_Key'Img;

      Out_Filename  : constant String :=
                        (if not Path_Name.Temporary_Directory.Is_Defined
                         then ""
                         else Path_Name.Compose
                           (Path_Name.Temporary_Directory,
                            Name_Type
                              (Process_Id & "-gpr2_tmp_out.tmp")).Value);

      Conf_Filename : constant String :=
                        (if not Path_Name.Temporary_Directory.Is_Defined
                         then ""
                         else Path_Name.Compose
                           (Path_Name.Temporary_Directory,
                            Name_Type
                              (Process_Id & "-gpr2_tmp_conf_"
                               & Trim (Key, Left) & ".cgpr")).Value);

      GPRconfig : constant OS_Lib.String_Access :=
                    OS_Lib.Locate_Exec_On_Path ("gprconfig");
      Args      : OS_Lib.Argument_List (1 .. Settings'Length +
                                        (if Debug then 6 else 5));
      Success   : Boolean := False;
      Ret_Code  : Integer := 0;

      Result    : Object;
   begin
      --  Build parameters

      Args (1) := GPRconfig;
      Args (2) := new String'("-o");
      Args (3) := new String'(Conf_Filename);
      Args (4) := new String'("--batch");
      Args (5) := new String'("--target=" & String (Target));

      for K in Settings'Range loop
         Args (6 + K - Settings'First) :=
           new String'("--config="
                       & To_String (Settings (K).Language)
                       & "," & To_String (Settings (K).Version)
                       & "," & To_String (Settings (K).Runtime)
                       & "," & To_String (Settings (K).Path)
                       & "," & To_String (Settings (K).Name));
      end loop;

      if Debug then
         Args (Args'Last) := new String'("-v");
      end if;

      --  Execute external GPRconfig tool

      OS_Lib.Spawn (GPRconfig.all, Args, Out_Filename, Success, Ret_Code);

      --  Free arguments

      for K in Args'Range loop
         OS_Lib.Free (Args (K));
      end loop;

      --  Increment the key to make sure we have distinct files if Debug mode
      --  is set.

      Config_File_Key := Config_File_Key + 1;

      --  Load the configuration object generated if execution was succeful

      Result.Conf := View.Undefined;

      if Success then
         Result := Load (Create (Name_Type (Conf_Filename)), Target);

         for S of Settings loop
            Result.Descriptions.Append (S);
         end loop;

      else
         Result.Messages.Append
           (Message.Create
              (Message.Error,
               "cannot create configuration file, fail to execute gprconfig",
               Sloc => Source_Reference.Create (Project.Value, 1, 1)));
      end if;

      if not Debug then
         if Directories.Exists (Conf_Filename) then
            Directories.Delete_File (Conf_Filename);
         end if;

         if Directories.Exists (Out_Filename) then
            Directories.Delete_File (Out_Filename);
         end if;
      end if;

      return Result;
   end Create;

   ----------------------------
   -- Dependency_File_Suffix --
   ----------------------------

   function Dependency_File_Suffix
     (Self     : Object;
      Language : Name_Type) return Name_Type
   is
      pragma Unreferenced (Self);
   begin
      --  ??? there is no attribute in the configuration file for this, so we
      --  end up having hard coded value for Ada and all other languages.
      if Language = "Ada" then
         return ".ali";
      else
         return ".d";
      end if;
   end Dependency_File_Suffix;

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
      Result.Project := Parser.Project.Parse (Filename, Result.Messages);

      --  Continue only if there is no parsing error on the configuration
      --  project.

      if Result.Project.Is_Defined then
         Result.Target :=
           (if Target = "all"
            then Null_Unbounded_String
            else To_Unbounded_String (String (Target)));
      end if;

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
      Language : Name_Type) return Name_Type is
   begin
      if Self.Conf.Has_Packages (PRP.Compiler) then
         return Name_Type
                  (Self.Conf.Pack (PRP.Compiler).Attribute
                     (PRA.Object_File_Suffix, String (Language)).Value.Text);
      else
         return ".o";
      end if;
   end Object_File_Suffix;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Object) is
   begin
      Self.Conf.Release;
      Self := Undefined;
   end Release;

   -------------
   -- Runtime --
   -------------

   function Runtime
     (Self : Object; Language : Name_Type) return Optional_Name_Type is
   begin
      for Description of Self.Descriptions loop
         if Optional_Name_Type (To_String (Description.Language))
           = Language
         then
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
