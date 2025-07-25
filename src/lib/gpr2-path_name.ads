--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
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

with GNATCOLL;
with GNATCOLL.Utils;
with GNATCOLL.VFS;

private with Ada.Strings.Hash;
private with GNATCOLL.Refcount;

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

   function Is_Pseudo_File  (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self is a virtual file

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

   function Create_Pseudo_File (Name : Filename_Type) return Object
     with Post => Create_Pseudo_File'Result.Is_Defined
                  and then Create_Pseudo_File'Result.Is_Pseudo_File;
   --  Creates a Path_Name.Object for a pseudo file. Pseudo file does not
   --  exist anywhere on the file system, instead it is located in memory.
   --  This is used in particular for configuration project files created in
   --  memory during autoconfiguration step.
   --  All possible path information is ignored, and resulting object gets
   --  a pseudo-path /<ram>/Base_Name (Name).

   function Create
     (Name          : Filename_Type;
      Path_Name     : Filename_Type;
      Resolve_Links : Boolean := False) return Object
     with Post => Create'Result.Is_Defined;
   --  Creates a path-name object. If Resolve_Links is set the returned
   --  path-name object will be fully resolved to point to the actual file.

   subtype Full_Name is Filename_Type
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
     with Pre => Self.Is_Defined and then Self.Has_Value,
          Inline;
   --  Returns the full pathname for Self if defined, or the empty string if
   --  the full path has not been resolved.

   function String_Value (Self : Object) return String
     with Pre => Self.Is_Defined and then Self.Has_Value,
          Inline;
   --  Same as Value, but returning a simple String

   function Base_Name (Self : Object) return Name_Type
     with Pre => Self.Is_Defined
                   and then not Self.Is_Directory;
   --  Returns the base name for Self (no extension).
   --  This routine should be used when base filename used to get unit name
   --  from filename.

   function Base_Name (Path : Filename_Type) return GPR2.Simple_Name;
   --  Return Path basename (so simple name without extension).
   --  Assumes Path is a file.

   function Base_Name (Path : String) return String;
   --  Return Path basename (so simple name without extension).
   --  Assumes Path is a file.

   function Base_Filename (Self : Object) return GPR2.Simple_Name
     with Pre => Self.Is_Defined
                   and then not Self.Is_Directory;
   --  Returns the base name for Self (no extension).
   --  This routine should be used when base filename used as part of another
   --  source related filenames.

   function Extension (Self : Object) return Filename_Optional
     with Pre => Self.Is_Defined
                   and then not Self.Is_Directory;
   --  Returns the extension of the file, with the dot.
   --  If the file doesn't have an extension, the empty string is returned.
   --  Self.Base_Filename & Self.Extension give the same result as
   --   Self.Simple_Name.

   function Simple_Name (Self : Object) return GPR2.Simple_Name
     with Pre => Self.Is_Defined
                   and then not Self.Is_Root_Dir;
   --  Returns the base name for Self (with extension)

   function Simple_Name (Path : Filename_Optional) return GPR2.Simple_Name;
   --  Returns the simple name portion of the file name specified by Name.
   --  This is Ada.Directories.Simple_Name implementation with
   --  valid path name check removed to allow '*' chars.

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

   function Relative_Path (Self, From : Object) return Filename_Type
     with Pre  => Self.Is_Defined and then From.Is_Defined,
          Post => Relative_Path'Result'Length > 0;
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

   function Contains (Self, Path : Object) return Boolean
     with Pre => Self.Is_Defined
                   and then Self.Is_Directory and then Path.Is_Defined;
   --  Tell whether Path is contained in Self (is a subdir or a file in a
   --  subdir)

   function Containing_Directory (Self : Object) return Object
     with Pre  => Self.Is_Defined
                    and then not Self.Is_Root_Dir,
          Post => Containing_Directory'Result.Is_Defined;
   --  Returns the containing directory of the directory information of Self

   function To_OS_Case (Name : Filename_Optional) return String with Inline;
   --  If filenames is case insensitive converts path name to lowercase,
   --  returns the same value otherwise.

   function Change_Extension
     (Self : Object; Extension : Filename_Optional) return Object
     with Pre => Self.Is_Defined and then not Self.Is_Directory;
   --  Return file object with another extension (possibly empty, which means
   --  removing current extension if any).
   --  First dot in the Extension is ignored.

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

   type Object_Internal
     (As_Is_Len     : Natural;
      Value_Len     : Natural;
      Comparing_Len : Natural;
      Base_Name_Len : Natural;
      Dir_Name_Len  : Natural)
   is record
      Is_Dir    : Boolean := False;
      In_Memory : Boolean := False;
      As_Is     : Filename_Optional (1 .. As_Is_Len);
      Value     : Filename_Optional (1 .. Value_Len);
      --  the normalized path-name
      Comparing : String (1 .. Comparing_Len);
      --  normalized path-name for comparison
      Base_Name : Filename_Optional (1 .. Base_Name_Len);
      Dir_Name  : Filename_Optional (1 .. Dir_Name_Len);
   end record;
   --  Comparing is equal to Value for case sensitive OS and lowercased Value
   --  for case insensitive OS.

   package Refcnt is new GNATCOLL.Refcount.Shared_Pointers (Object_Internal);
   use Refcnt;

   type Object is new Refcnt.Ref with null record;

   Undefined : constant Object := (Refcnt.Null_Ref with null record);

   function Is_Defined (Self : Object) return Boolean is
     (not Self.Is_Null);

   overriding function "=" (Left, Right : Object) return Boolean is
     (if Left.Is_Defined and then Right.Is_Defined
      then Get (Left).Comparing = Get (Right).Comparing
      else Left.Is_Defined = Right.Is_Defined);

   function "<" (Left, Right : Object) return Boolean is
     (if Left.Is_Defined and then Right.Is_Defined
      then Get (Left).Comparing < Get (Right).Comparing
      elsif not Left.Is_Defined and then not Right.Is_Defined
      then False
      elsif Left.Is_Defined
      then False
      else True);

   function Base_Name (Path : String) return String is
     (if Path'Length = 0 then ""
      else String (Base_Name (Filename_Type (Path))));

   function Base_Name (Self : Object) return Name_Type is
     (Name_Type (Get (Self).Base_Name));

   function Base_Filename (Self : Object) return GPR2.Simple_Name is
     (GPR2.Simple_Name (Get (Self).Base_Name));

   function Dir_Name (Self : Object_Internal) return Full_Name is
      (Full_Name (if Self.Is_Dir then Self.Value else Self.Dir_Name));

   function Dir_Name (Self : Object) return Full_Name is
     (Dir_Name (Get (Self)));

   function Has_Dir_Name (Self : Object) return Boolean is
     (Get (Self).Dir_Name'Length > 0);

   function Simple_Name (Path : String) return String is
     (String (Simple_Name (Filename_Type (Path))));

   function Has_Value (Self : Object) return Boolean is
     (Get (Self).Value'Length > 0);

   function Value (Self : Object) return Full_Name is
     (Full_Name (String_Value (Self)));

   function Is_Directory (Self : Object) return Boolean is (Get (Self).Is_Dir);

   function Is_Pseudo_File (Self : Object) return Boolean is
     (Get (Self).In_Memory);

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
     (Ada.Strings.Hash (Get (Self).Comparing));

end GPR2.Path_Name;
