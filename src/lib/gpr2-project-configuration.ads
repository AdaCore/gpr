--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Handle the configuration object for a project tree

limited with GPR2.KB;
with GPR2.Log;
with GPR2.Project.Parser;
with GPR2.Project.View;
with GNATCOLL.Refcount;

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Vectors;

limited with GPR2.Project.Tree;

package GPR2.Project.Configuration is

   Config_File_Key : Integer := 1;

   type Object is tagged private;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Has_Messages (Self : Object) return Boolean;
   --  Returns whether some messages are present for this configuration

   function Has_Error (Self : Object) return Boolean;
   --  Returns whether error messages are present for this configuration

   function Log_Messages (Self : Object) return Log.Object
     with Post => not Self.Has_Messages or else Log_Messages'Result.Count > 0;
   --  Returns the Logs, this contains information, warning and error messages
   --  found while handling the project.

   type Description is private;
   --  A configuration description, for a language, a version, specific
   --  runtime, etc.

   overriding function "=" (Left, Right : Description) return Boolean;
   --  Returns True when the Left and Right configuration descriptions are
   --  equal.

   type Description_Set is array (Positive range <>) of Description;
   --  A set of description

   Default_Description : constant Description_Set;

   function Create
     (Language : Language_Id;
      Version  : Optional_Name_Type := No_Name;
      Runtime  : Optional_Name_Type := No_Name;
      Path     : Filename_Optional  := No_Filename;
      Name     : Optional_Name_Type := No_Name) return Description;
   --  Returns a description for a configuration. This description object is
   --  used to retrieve the available and corresponding compilers. That is,
   --  the compilers for the given language, the runtime if specified, etc.

   function Language (Descr : Description) return Language_Id;
   --  Returns language specified by Descr

   function Version (Descr : Description) return Optional_Name_Type;
   --  Returns version specified by Descr

   function Runtime (Descr : Description) return Optional_Name_Type;
   --  Returns runtime specified by Descr

   function Path (Descr : Description) return Filename_Optional;
   --  Returns path specified by Descr

   function Name (Descr : Description) return Optional_Name_Type;
   --  Returns name specified by Descr

   function Create
     (Settings   : Description_Set;
      Target     : Name_Type;
      Project    : GPR2.Path_Name.Object;
      Base       : in out GPR2.KB.Object;
      Save_Name  : GPR2.Path_Name.Object := GPR2.Path_Name.Undefined)
      return Object
   with Pre => Settings'Length > 0;
   --  Creates a configuration based on the settings requested.
   --  Project parameter is used as source reference for the potential
   --  error messages reported.
   --  If Save_Name is defined, the configuration project is saved there.

   function Load
     (Filename : Path_Name.Object) return Object;
   --  Creates a configuration object for the given configuration file

   function Corresponding_View (Self : Object) return Project.View.Object
     with Pre  => Self.Is_Defined,
          Post => Corresponding_View'Result.Is_Defined;
   --  Gets project for the given configuration object

   function Runtime
     (Self : Object; Language : Language_Id) return Optional_Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the runtime specified for Language or the empty string if no
   --  specific runtime has been specified for this language.

   function Archive_Suffix (Self : Object) return Filename_Type
     with Inline,
          Pre  => Self.Is_Defined,
          Post => Archive_Suffix'Result (Archive_Suffix'Result'First) = '.';
   --  Returns the archive file suffix (with the leading dot)

   function Object_File_Suffix
     (Self     : Object;
      Language : Language_Id) return Filename_Type
     with Inline,
          Pre  => Self.Is_Defined,
          Post => Object_File_Suffix'Result
                    (Object_File_Suffix'Result'First) = '.';
   --  Returns the object file suffix (with the leading dot)

   function Dependency_File_Suffix
     (Self     : Object;
      Language : Language_Id) return Filename_Type
     with Pre  => Self.Is_Defined,
          Post => Dependency_File_Suffix'Result
                    (Dependency_File_Suffix'Result'First) = '.';
   --  Returns the dependency file suffix (with the leading dot)

   function Has_Externals (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns whether Externals where found during configuration project load

   function Externals (Self : Object) return Containers.Name_List
     with Pre  => Self.Is_Defined and then Self.Has_Externals,
          Post => not Externals'Result.Is_Empty;
   --  Returns externals found in parsed configuration project

private

   type Description is record
      Language : Language_Id;
      Version  : Unbounded_String;
      Runtime  : Unbounded_String;
      Path     : Unbounded_String;
      Name     : Unbounded_String;
   end record;

   function Language (Descr : Description) return Language_Id is
     (Descr.Language);

   function Version (Descr : Description) return Optional_Name_Type is
     (Optional_Name_Type (To_String (Descr.Version)));

   function Runtime (Descr : Description) return Optional_Name_Type is
     (Optional_Name_Type (To_String (Descr.Runtime)));

   function Path (Descr : Description) return Filename_Optional is
     (Filename_Optional (To_String (Descr.Path)));

   function Name (Descr : Description) return Optional_Name_Type is
     (Optional_Name_Type (To_String (Descr.Name)));

   overriding function "=" (Left, Right : Description) return Boolean
   is
     (Language (Left) = Language (Right)
      and then Version (Left) = Version (Right)
      and then Runtime (Left) = Runtime (Right)
      and then Path (Left) = Path (Right)
      and then Name (Left) = Name (Right));

   package Descriptions is new Ada.Containers.Vectors (Positive, Description);
   package Language_Dict is new Ada.Containers.Indefinite_Hashed_Maps
     (Language_Id, String, GPR2.Hash, GPR2."=");

   type Config_Cache_Object;
   type Config_Cache_Access is access all Config_Cache_Object;

   type Config_Cache_Object is record
      Archive_Suffix     : Unbounded_String;
      Object_File_Suffix : Language_Dict.Map;
   end record;

   package Cache_Refcount is new GNATCOLL.Refcount.Shared_Pointers
     (Element_Type => Config_Cache_Object);

   type Object is tagged record
      Messages           : Log.Object;
      Project            : GPR2.Project.Parser.Object;
      Conf               : GPR2.Project.View.Object;
      Descriptions       : Configuration.Descriptions.Vector;

      --  Cache
      Cache              : Cache_Refcount.Ref;
   end record;

   procedure Bind_To_Tree
     (Self : in out Object;
      Tree : not null access Project.Tree.Object);
   --  Bind configuration to Tree

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Has_Error (Self : Object) return Boolean is
      (Self.Messages.Has_Error);

   Default_Description : constant Description_Set (1 .. 1) :=
                           (1 => (Language => Ada_Language,
                                  others   => <>));

end GPR2.Project.Configuration;
