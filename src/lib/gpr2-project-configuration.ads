------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2017-2018, Free Software Foundation, Inc.          --
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

with GPR2.Containers;
with GPR2.Log;
with GPR2.Project.View;

private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

package GPR2.Project.Configuration is

   use type Containers.Count_Type;
   use type Project.View.Object;

   type Object is tagged private;

   Undefined : constant Object;

   function Has_Messages (Self : Object) return Boolean;
   --  Returns whether some messages are present for this configuration

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

   function Create
     (Settings : Description_Set;
      Target   : Name_Type := "all") return Object;
   --  Creates a configuration based on the settings requested

   function Load
     (Filename : Path_Name.Object;
      Target   : Name_Type := "all") return Object;
   --  Creates a configuration object for the given configuration file

   procedure Release (Self : in out Object)
     with Pre => Self /= Undefined;
   --  Releases memory associated with the configuration object

   function Corresponding_View (Self : Object) return Project.View.Object
     with Pre  => Self /= Undefined,
          Post => Corresponding_View'Result /= Project.View.Undefined;
   --  Gets project for the given configuration object

   function Target (Self : Object) return Optional_Name_Type
     with Pre => Self /= Undefined;
   --  Returns the target used for the configuration

   function Runtime
     (Self : Object; Language : Name_Type) return Optional_Name_Type
     with Pre => Self /= Undefined;
   --  Returns the runtime specified for Language or the empty string if no
   --  specific runtime has been specified for this language.

   function Object_File_Suffix
     (Self     : Object;
      Language : Name_Type) return Name_Type
     with Pre  => Self /= Undefined,
          Post => Object_File_Suffix'Result
                    (Object_File_Suffix'Result'First) = '.';
   --  Returns the object file suffix (with the leading dot)

   function Dependency_File_Suffix
     (Self     : Object;
      Language : Name_Type) return Name_Type
     with Pre  => Self /= Undefined,
          Post => Dependency_File_Suffix'Result
                    (Dependency_File_Suffix'Result'First) = '.';
   --  Returns the dependency file suffix (with the leading dot)

private

   use Ada.Strings.Unbounded;

   type Description is record
      Language : Unbounded_String;
      Version  : Unbounded_String;
      Runtime  : Unbounded_String;
      Path     : Unbounded_String;
      Name     : Unbounded_String;
   end record;

   package Descriptions is new Ada.Containers.Vectors (Positive, Description);

   type Object is tagged record
      Messages     : Log.Object;
      Target       : Unbounded_String;
      Conf         : Project.View.Object;
      Descriptions : Configuration.Descriptions.Vector;
   end record;

   Undefined : constant Object := (others => <>);

   Default_Description : constant Description_Set (1 .. 1) :=
                           (1 => (Language => To_Unbounded_String ("ada"),
                                  others   => <>));

end GPR2.Project.Configuration;
