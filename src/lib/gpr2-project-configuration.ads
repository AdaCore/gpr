------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2020, AdaCore                      --
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

--  Handle the configuration object for a project tree

limited with GPR2.KB;
with GPR2.Log;
with GPR2.Parser.Project;
with GPR2.Project.View;

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

   type Description_Set is array (Positive range <>) of Description
     with Dynamic_Predicate => Description_Set'Length > 0;
   --  A set of description

   Default_Description : constant Description_Set;

   function Create
     (Language : Name_Type;
      Version  : Optional_Name_Type := No_Name;
      Runtime  : Optional_Name_Type := No_Name;
      Path     : Optional_Name_Type := No_Name;
      Name     : Optional_Name_Type := No_Name) return Description;
   --  Returns a description for a configuration. This description object is
   --  used to retrieve the available and corresponding compilers. That is,
   --  the compilers for the given language, the runtime if specified, etc.

   function Language (Descr : Description) return Name_Type;
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
      Base       : in out GPR2.KB.Object)
      return Object;
   --  Creates a configuration based on the settings requested.
   --  Project parameter need to log error if happen.

   function Load
     (Filename : Path_Name.Object;
      Target   : Name_Type := "all") return Object;
   --  Creates a configuration object for the given configuration file

   function Corresponding_View (Self : Object) return Project.View.Object
     with Pre  => Self.Is_Defined,
          Post => Corresponding_View'Result.Is_Defined;
   --  Gets project for the given configuration object

   function Target (Self : Object) return Optional_Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the target used for the configuration

   function Runtime
     (Self : Object; Language : Name_Type) return Optional_Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the runtime specified for Language or the empty string if no
   --  specific runtime has been specified for this language.

   function Archive_Suffix (Self : Object) return Filename_Type
     with Pre  => Self.Is_Defined,
          Post => Archive_Suffix'Result (Archive_Suffix'Result'First) = '.';
   --  Returns the archive file suffix (with the leading dot)

   function Object_File_Suffix
     (Self     : Object;
      Language : Name_Type) return Filename_Type
     with Pre  => Self.Is_Defined,
          Post => Object_File_Suffix'Result
                    (Object_File_Suffix'Result'First) = '.';
   --  Returns the object file suffix (with the leading dot)

   function Dependency_File_Suffix
     (Self     : Object;
      Language : Name_Type) return Filename_Type
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
      Language : Unbounded_String;
      Version  : Unbounded_String;
      Runtime  : Unbounded_String;
      Path     : Unbounded_String;
      Name     : Unbounded_String;
   end record;

   function Language (Descr : Description) return Name_Type is
     (Name_Type (To_String (Descr.Language)));

   function Version (Descr : Description) return Optional_Name_Type is
     (Optional_Name_Type (To_String (Descr.Version)));

   function Runtime (Descr : Description) return Optional_Name_Type is
     (Optional_Name_Type (To_String (Descr.Runtime)));

   function Path (Descr : Description) return Filename_Optional is
     (Filename_Optional (To_String (Descr.Path)));

   function Name (Descr : Description) return Optional_Name_Type is
     (Optional_Name_Type (To_String (Descr.Name)));

   package Descriptions is new Ada.Containers.Vectors (Positive, Description);

   type Object is tagged record
      Messages     : Log.Object;
      Target       : Unbounded_String;
      Project      : Parser.Project.Object;
      Conf         : GPR2.Project.View.Object;
      Descriptions : Configuration.Descriptions.Vector;
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
                           (1 => (Language => To_Unbounded_String ("ada"),
                                  others   => <>));

end GPR2.Project.Configuration;
