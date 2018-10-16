------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2018, Free Software Foundation, Inc.            --
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

   overriding function "=" (Left, Right : Object) return Boolean;
   --  Returns True if Left and Right are referencing the same normalized path

   function "<" (Left, Right : Object) return Boolean;
   --  Returns True based on the normalized names

   function Is_Directory (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if Self represent a directory

   function Create_File
     (Name      : Name_Type;
      Directory : Optional_Name_Type := "") return Object
     with Post => Create_File'Result /= Undefined
                  and then not Create_File'Result.Is_Directory;
   --  Creates a Path_Name_Type for a file

   function Create_Directory
     (Name      : Name_Type;
      Directory : Optional_Name_Type := "") return Object
     with Post => Create_Directory'Result /= Undefined
                  and then Create_Directory'Result.Is_Directory;
   --  Creates a Path_Name_Type for a directory

   function Create (Name, Path_Name : Name_Type) return Object
     with Post => Create'Result /= Undefined;
   --  Creates a path-name object

   subtype Full_Name is String
     with Dynamic_Predicate => (for some C of Full_Name => C in '/' | '\');

   function Name (Self : Object) return Name_Type
     with Pre => Self /= Undefined;
   --  Returns the original, untouched name used to create the object

   function Value (Self : Object) return Full_Name
     with Pre => Self /= Undefined;
   --  Returns the full pathname for Self

   function Base_Name (Self : Object) return Simple_Name
     with Pre => Self /= Undefined;
   --  Returns the base name for Self (no extension)

   function Simple_Name (Self : Object) return GPR2.Simple_Name
     with Pre => Self /= Undefined;
   --  Returns the base name for Self (with extension)

   function Dir_Name (Self : Object) return Full_Name
     with Pre  => Self /= Undefined,
          Post => Dir_Name'Result (Dir_Name'Result'Last) in '/' | '\';
   --  Returns the directory part for Self

   function Temporary_Directory return Object;
   --  Returns the current temporary directory

   function Compose (Self : Object; Name : Name_Type) return Object
     with Pre  => Self /= Undefined,
          Post => Compose'Result /= Undefined;
   --  Returns Name as sub-directory of Self : Self & '/' & Name

   function Exists (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if Self is an existing and readable file or directory

   function Content_MD5 (Self : Object) return GNAT.MD5.Message_Digest
     with Pre => Self /= Undefined and then Self.Exists;
   --  Returns the MD5 signature for the given file

   procedure Create_Sym_Link (Self, To : Object)
     with Pre => Self /= Undefined and then To /= Undefined;
   --  Creates a sym-link for Self as To

   function Relative_Path (Self, To : Object) return Object
     with Pre  => Self /= Undefined and then To /= Undefined,
          Post => Relative_Path'Result /= Undefined;
   --  Returns the relative pathname which corresponds to Self when
   --  starting from directory To. Note that the relative pathname is actually
   --  given by Relative_Path'Result.Name.

   function Common_Prefix (Self, Path : Object) return Object
     with Pre  => Self /= Undefined and then Path /= Undefined,
          Post => Common_Prefix'Result = Undefined
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
      Is_Dir    : Boolean;
      As_Is     : Unbounded_String;
      Value     : Unbounded_String; -- the normalized path-name
      Base_Name : Unbounded_String;
      Dir_Name  : Unbounded_String;
   end record;

   Undefined : constant Object :=
                 (False,
                  Null_Unbounded_String,
                  Null_Unbounded_String,
                  Null_Unbounded_String,
                  Null_Unbounded_String);

   overriding function "=" (Left, Right : Object) return Boolean is
     (Left.Value = Right.Value);

   function "<" (Left, Right : Object) return Boolean
     is (Left.Value < Right.Value);

   function Base_Name (Self : Object) return GPR2.Simple_Name is
     (GPR2.Simple_Name (To_String (Self.Base_Name)));

   function Simple_Name (Self : Object) return GPR2.Simple_Name is
     (GPR2.Simple_Name (Ada.Directories.Simple_Name (To_String (Self.As_Is))));

   function Name (Self : Object) return Name_Type is
     (Name_Type (To_String (Self.As_Is)));

   function Dir_Name (Self : Object) return Full_Name is
     (Full_Name
        (To_String (if Self.Is_Dir then Self.Value else Self.Dir_Name)));

   function Is_Directory (Self : Object) return Boolean is (Self.Is_Dir);

end GPR2.Path_Name;
