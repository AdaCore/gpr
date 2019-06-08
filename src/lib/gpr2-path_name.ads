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

--  This package provides API to deal with pathname. For example, it ensures
--  that a directory is well-formed, and keeps internal information about
--  the base name and directory of each file. Most importantly, it abstracts
--  out the pathname normalization. That is, the canonical name is kept
--  internally and the redefined "=" and "<" operators do the expected
--  comparison.
--
--  From a path-name object it is always possible to get the full pathname
--  of the file and its containing directory.

with GNAT.MD5;

private with Ada.Directories;
private with Ada.Strings.Unbounded;

package GPR2.Path_Name is

   type Object is tagged private;
   --  A project path-name, will always be normalized according to the running
   --  target.

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   overriding function "=" (Left, Right : Object) return Boolean;
   --  Returns True if Left and Right are referencing the same normalized path

   function "<" (Left, Right : Object) return Boolean;
   --  Returns True based on the normalized names

   function Is_Directory (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self represent a directory

   function Create_File
     (Name      : Name_Type;
      Directory : Optional_Name_Type := "") return Object
     with Post => Create_File'Result.Is_Defined
                  and then not Create_File'Result.Is_Directory;
   --  Creates a Path_Name_Type for a file

   function Create_Directory
     (Name      : Name_Type;
      Directory : Optional_Name_Type := "") return Object
     with Post => Create_Directory'Result.Is_Defined
                  and then Create_Directory'Result.Is_Directory;
   --  Creates a Path_Name_Type for a directory

   function Create (Name, Path_Name : Name_Type) return Object
     with Post => Create'Result.Is_Defined;
   --  Creates a path-name object

   subtype Full_Name is String
     with Dynamic_Predicate => (for some C of Full_Name => C in '/' | '\');

   function Name (Self : Object; Extension : Boolean := True) return Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the original, untouched name used to create the object. If
   --  Extension is set to False then the final extension is removed. Note that
   --  this is not the base-name as the leading directory information is not
   --  removed.

   function Value (Self : Object) return Full_Name
     with Pre => Self.Is_Defined;
   --  Returns the full pathname for Self

   function Base_Name (Self : Object) return Simple_Name
     with Pre => Self.Is_Defined;
   --  Returns the base name for Self (no extension)

   function Simple_Name (Self : Object) return GPR2.Simple_Name
     with Pre => Self.Is_Defined;
   --  Returns the base name for Self (with extension)

   function Dir_Name (Self : Object) return Full_Name
     with Pre  => Self.Is_Defined,
          Post => Dir_Name'Result (Dir_Name'Result'Last) in '/' | '\';
   --  Returns the directory part for Self

   function Temporary_Directory return Object;
   --  Returns the current temporary directory

   function Compose
     (Self      : Object;
      Name      : Name_Type;
      Directory : Boolean := False) return Object
     with Pre  => Self.Is_Defined,
          Post => Compose'Result.Is_Defined;
   --  Returns Name as sub-directory of Self : Self & '/' & Name

   function Exists (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self is an existing and readable file or directory

   function Content_MD5 (Self : Object) return GNAT.MD5.Message_Digest
     with Pre => Self.Is_Defined and then Self.Exists;
   --  Returns the MD5 signature for the given file

   procedure Create_Sym_Link (Self, To : Object)
     with Pre => Self.Is_Defined and then To.Is_Defined;
   --  Creates a sym-link for Self as To

   function Relative_Path (Self, To : Object) return Object
     with Pre  => Self.Is_Defined and then To.Is_Defined,
          Post => Relative_Path'Result.Is_Defined;
   --  Returns the relative pathname which corresponds to Self when
   --  starting from directory To. Note that the relative pathname is actually
   --  given by Relative_Path'Result.Name.

   function Common_Prefix (Self, Path : Object) return Object
     with Pre  => Self.Is_Defined and then Path.Is_Defined,
          Post => not Common_Prefix'Result.Is_Defined
     or else
       (Self.Value (1 .. Common_Prefix'Result.Value'Length)
         = Common_Prefix'Result.Value
        and then
          Path.Value (1 .. Common_Prefix'Result.Value'Length)
         = Common_Prefix'Result.Value);
   --  Returns the longest common prefix for Self and Path

private

   use Ada.Strings.Unbounded;

   type Object is tagged record
      Is_Dir    : Boolean := False;
      As_Is     : Unbounded_String;
      Value     : Unbounded_String; -- the normalized path-name
      Comparing : Unbounded_String; -- normalized path-name for comparision
      Base_Name : Unbounded_String;
      Dir_Name  : Unbounded_String;
   end record;
   --  Comparing is equal to Value for case sensitive OS and lowercased Value
   --  for case insensitive OS.

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   overriding function "=" (Left, Right : Object) return Boolean is
     (Left.Comparing = Right.Comparing);

   function "<" (Left, Right : Object) return Boolean is
     (Left.Comparing < Right.Comparing);

   function Base_Name (Self : Object) return GPR2.Simple_Name is
     (GPR2.Simple_Name (To_String (Self.Base_Name)));

   function Simple_Name (Self : Object) return GPR2.Simple_Name is
     (GPR2.Simple_Name (Ada.Directories.Simple_Name (To_String (Self.As_Is))));

   function Dir_Name (Self : Object) return Full_Name is
     (Full_Name
        (To_String (if Self.Is_Dir then Self.Value else Self.Dir_Name)));

   function Is_Directory (Self : Object) return Boolean is (Self.Is_Dir);

end GPR2.Path_Name;
