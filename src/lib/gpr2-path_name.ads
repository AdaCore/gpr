--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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

with GNATCOLL;
with GNATCOLL.Utils;
with GNATCOLL.VFS;

private with Ada.Directories;
private with Ada.Strings.Unbounded.Hash;

package GPR2.Path_Name is

   use GNATCOLL;

   type Object is tagged private;
   --  A project path-name, will always be normalized according to the running
   --  target.

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   Resolve_On_Current : constant Filename_Type := "./";
   --  Resolves relative path from current directory
   No_Resolution      : constant Filename_Optional := "";
   --  Skip full path-name resolution, delay for later

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   overriding function "=" (Left, Right : Object) return Boolean;
   --  Returns True if Left and Right are referencing the same normalized path

   function "<" (Left, Right : Object) return Boolean;
   --  Returns True based on the normalized names

   function Is_Directory (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self represent a directory

   function Is_Root_Dir (Self : Object) return Boolean
     with Pre => Self.Is_Defined;

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

   function Has_Value (Self : Object) return Boolean;
   --  Whether Self is defined and has a full pathname.

   function Value (Self : Object) return Full_Name
     with Pre => Self.Is_Defined and then Self.Has_Value;
   --  Returns the full pathname for Self if defined, or the empty string if
   --  the full path has not been resolved.

   function Base_Name (Self : Object) return Name_Type
     with Pre => Self.Is_Defined
                   and then not Self.Is_Directory;
   --  Returns the base name for Self (no extension).
   --  This routine should be used when base filename used to get unit name
   --  from filename.

   function Base_Filename (Self : Object) return GPR2.Simple_Name
     with Pre => Self.Is_Defined
                   and then not Self.Is_Directory;
   --  Returns the base name for Self (no extension).
   --  This routine should be used when base filename used as part of another
   --  source related filenames.

   function Simple_Name (Self : Object) return GPR2.Simple_Name
     with Pre => Self.Is_Defined
                   and then not Self.Is_Root_Dir;
   --  Returns the base name for Self (with extension)

   function Simple_Name (Path : String) return String;
   --  Returns the simple name portion of the file name specified by Name.
   --  This is Ada.Directories.Simple_Name implementation with
   --  valid path name check removed to allow '*' chars.

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

   function Relative_Path (Self, From : Object) return Object
     with Pre  => Self.Is_Defined and then From.Is_Defined,
          Post => Relative_Path'Result.Is_Defined;
   --  Returns the relative pathname which corresponds to Self when
   --  starting from directory From. Note that the relative pathname is
   --  actually given by Relative_Path'Result.Name.

   function Common_Prefix (Self, Path : Object) return Object
     with Pre  => Self.Is_Defined and then Path.Is_Defined,
          Post => not Common_Prefix'Result.Is_Defined
     or else
       (Self.Value (1 .. Common_Prefix'Result.Value'Length)
         = Common_Prefix'Result.Value
        and then
          Path.Value (1 .. Common_Prefix'Result.Value'Length)
         = Common_Prefix'Result.Value
        and then Common_Prefix'Result.Is_Directory);
   --  Returns the longest common path for Self and Path

   function Containing_Directory (Self : Object) return Object
     with Pre  => Self.Is_Defined
                    and then not Self.Is_Root_Dir,
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
     with Pre => Self.Exists;
   --  Returns Self's modification time

   function Filesystem_String (Self : Object) return VFS.Filesystem_String;
   --  GPR2.Path_Name.Object to GNATCOLL.VFS.Filesystem_String conversion

   function Virtual_File (Self : Object) return VFS.Virtual_File;
   --  GPR2.Path_Name.Object to GNATCOLL.VFS.Virtual_File conversion

   function Create (Filename : VFS.Filesystem_String) return Object;
   --  GNATCOLL.VFS.Filesystem_String to GPR2.Path_Name.Object conversion

   function Create (File : VFS.Virtual_File) return Object;
   --  GNATCOLL.VFS.Virtual_File to GPR2.Path_Name.Object to conversion

   function Hash
     (Self : Object) return Ada.Containers.Hash_Type;
   --  For use with Hashed containers

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

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

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

   function Has_Value (Self : Object) return Boolean is
     (Self.Value /= Null_Unbounded_String);

   function Is_Directory (Self : Object) return Boolean is (Self.Is_Dir);

   function Modification_Time (Self : Object) return Ada.Calendar.Time is
     (Ada.Directories.Modification_Time (To_String (Self.Value)));

   function Create (Filename : VFS.Filesystem_String) return Object
   is
     (if Filename'Length > 1
      then
        (if GNATCOLL.Utils.Is_Directory_Separator (Filename (Filename'Last))
         then Create_Directory (Filename_Type (Filename))
         else Create (Filename_Type (Filename), Filename_Type (Filename)))
      else Undefined);

   function Create (File : VFS.Virtual_File) return Object
   is
     (if VFS."/=" (File, VFS.No_File)
      then
        (if VFS.Is_Directory (File)
         or else GNATCOLL.Utils.Is_Directory_Separator
                   (File.Full_Name (File.Full_Name.all'Last))
         then Create_Directory
                (Filename_Type (File.Display_Full_Name))
         else Create
                (Filename_Type (File.Display_Full_Name),
                 Filename_Type (File.Display_Full_Name)))
      else Undefined);

   function Virtual_File (Self : Object) return VFS.Virtual_File is
     (if Self.Is_Defined
      then VFS.Create (Self.Filesystem_String)
      else VFS.No_File);

   function Hash (Self : Object) return Ada.Containers.Hash_Type is
     (Ada.Strings.Unbounded.Hash (Self.Comparing));

end GPR2.Path_Name;
