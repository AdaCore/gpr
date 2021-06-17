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

--  This package provides API to deal with pathname. For example, it ensures
--  that a directory is well-formed, and keeps internal information about
--  the base name and directory of each file. Most importantly, it abstracts
--  out the pathname normalization. That is, the canonical name is kept
--  internally and the redefined "=" and "<" operators do the expected
--  comparison.
--
--  From a path-name object it is always possible to get the full pathname
--  of the file and its containing directory.

with Ada.Calendar;
with GNAT.MD5;

private with Ada.Directories;

package GPR2.Path_Name is

   type Object is tagged private;
   --  A project path-name, will always be normalized according to the running
   --  target.

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   Implicit_Project : constant Object;
   --  Means that an empty project has to be generated instead of parsed from
   --  file.

   Resolve_On_Current : constant Filename_Type := "./";
   --  Resolves relative path from current directory
   No_Resolution      : constant Filename_Optional := "";
   --  Skip full path-name resolution, delay for later

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Is_Implicit_Project (Self : Object) return Boolean;
   --  Returns True if Self is implicit project

   overriding function "=" (Left, Right : Object) return Boolean;
   --  Returns True if Left and Right are referencing the same normalized path

   function "<" (Left, Right : Object) return Boolean;
   --  Returns True based on the normalized names

   function Is_Directory (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self represent a directory

   function Create_File
     (Name      : Filename_Type;
      Directory : Filename_Optional := Resolve_On_Current) return Object
     with Post => Create_File'Result.Is_Defined
                  and then not Create_File'Result.Is_Directory;
   --  Creates a Path_Name.Object for a file.
   --  See Resolve_On_Current and No_Resolution above for Directory options.
   --  If Directory parameter is No_Resolution and Name is not absolute
   --  filename then Object is created without directory information. To create
   --  the file relatively to the current directory, use the Resolve_On_Current
   --  in the Directory parameter. In case of absolute pathname in Name
   --  parameter the Directory parameter has no meaning.

   function Create_Directory
     (Name          : Filename_Type;
      Directory     : Filename_Optional := "";
      Resolve_Links : Boolean := False) return Object
     with Post => Create_Directory'Result.Is_Defined
                  and then Create_Directory'Result.Is_Directory;
   --  Creates a Path_Name_Type for a directory

   function Create (Name, Path_Name : Filename_Type) return Object
     with Post => Create'Result.Is_Defined;
   --  Creates a path-name object

   subtype Full_Name is String
     with Dynamic_Predicate => (for some C of Full_Name => C in '/' | '\');

   function Name
     (Self : Object; Extension : Boolean := True) return Filename_Type
     with Pre => Self.Is_Defined;
   --  Returns the original, untouched name used to create the object. If
   --  Extension is set to False then the final extension is removed. Note that
   --  this is not the base-name as the leading directory information is not
   --  removed.

   function Value (Self : Object) return Full_Name
     with Pre => Self.Is_Defined;
   --  Returns the full pathname for Self

   function Base_Name (Self : Object) return Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the base name for Self (no extension).
   --  This routine should be used when base filename used to get unit name
   --  from filename.

   function Base_Filename (Self : Object) return GPR2.Simple_Name
     with Pre => Self.Is_Defined;
   --  Returns the base name for Self (no extension).
   --  This routine should be used when base filename used as part of another
   --  source related filenames.

   function Simple_Name (Self : Object) return GPR2.Simple_Name
     with Pre => Self.Is_Defined;
   --  Returns the base name for Self (with extension)

   function Has_Dir_Name (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self has directory information

   function Dir_Name (Self : Object) return Full_Name
     with Pre  => Self.Is_Defined and then Self.Has_Dir_Name,
          Post => Dir_Name'Result (Dir_Name'Result'Last) in '/' | '\';
   --  Returns the directory part for Self

   function Temporary_Directory return Object;
   --  Returns the current temporary directory

   function Compose
     (Self      : Object;
      Name      : Filename_Type;
      Directory : Boolean := False) return Object
     with Pre  => Self.Is_Defined,
          Post => Compose'Result.Is_Defined;
   --  If Directory = True then returns Name as sub-directory of Self :
   --  Self & '/' & Name
   --  If Directory = False use Name as simple filename in directory of Self.

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

   function Containing_Directory (Self : Object) return Object
     with Pre  => Self.Is_Defined,
          Post => Containing_Directory'Result.Is_Defined;
   --  Returns the containing directory of the directory information of Self

   function To_OS_Case (Name : String) return String with Inline;
   --  If filenames is case insensitive converts path name to lowercase,
   --  returns the same value otherwise.

   function Change_Extension
     (Self : Object; Extension : Value_Type) return Object
     with Pre => Self.Is_Defined and then not Self.Is_Directory;
   --  Return file object with another extension (possibly empty, which means
   --  removing current extension if any).
   --  First dot in the Extension is ignored.

   function Modification_Time (Self : Object) return Ada.Calendar.Time
     with Pre => Self.Is_Defined;
   --  Returns Self's modification time

private

   type Object is tagged record
      Is_Dir    : Boolean := False;
      As_Is     : Unbounded_String;
      Value     : Unbounded_String; -- the normalized path-name
      Comparing : Unbounded_String; -- normalized path-name for comparison
      Base_Name : Unbounded_String;
      Dir_Name  : Unbounded_String;
   end record;
   --  Comparing is equal to Value for case sensitive OS and lowercased Value
   --  for case insensitive OS.

   Undefined : constant Object := (others => <>);

   Implicit_Project : constant Object :=
                        (Comparing => To_Unbounded_String ("^"), others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Is_Implicit_Project (Self : Object) return Boolean is
     (Self = Implicit_Project);

   overriding function "=" (Left, Right : Object) return Boolean is
     (Left.Comparing = Right.Comparing);

   function "<" (Left, Right : Object) return Boolean is
     (Left.Comparing < Right.Comparing);

   function Base_Name (Self : Object) return Name_Type is
     (Name_Type (To_String (Self.Base_Name)));

   function Base_Filename (Self : Object) return GPR2.Simple_Name is
     (GPR2.Simple_Name (To_String (Self.Base_Name)));

   function Dir_Name (Self : Object) return Full_Name is
     (Full_Name
        (To_String (if Self.Is_Dir then Self.Value else Self.Dir_Name)));

   function Has_Dir_Name (Self : Object) return Boolean is
     (Self.Dir_Name /= Null_Unbounded_String);

   function Is_Directory (Self : Object) return Boolean is (Self.Is_Dir);

   function Modification_Time (Self : Object) return Ada.Calendar.Time is
     (Ada.Directories.Modification_Time (To_String (Self.Value)));

end GPR2.Path_Name;
