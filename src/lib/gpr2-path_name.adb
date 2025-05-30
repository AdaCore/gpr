--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Handling;
with Ada.Directories.Hierarchical_File_Names;
with Ada.Environment_Variables;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;

with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with GNAT.OS_Lib;
with GNAT.Regexp;

with System;

package body GPR2.Path_Name is

   use GNAT;
   use GNAT.Regexp;

   function To_OS_Case (Name : Filename_Optional) return String is
     (if File_Names_Case_Sensitive
      then String (Name)
      else Characters.Handling.To_Lower (String (Name)));

   function To_OS_Case (Name : Unbounded_String) return Unbounded_String is
     (if File_Names_Case_Sensitive
      then Name
      else +Characters.Handling.To_Lower (To_String (Name)));
   --  If filenames is case insensitive converts path name to lowercase,
   --  returns the same value otherwise.

   function To_OS_Case (C : Character) return Character is
     (if File_Names_Case_Sensitive
      then C
      else Characters.Handling.To_Lower (C));
   --  If filenames is case insensitive converts char to lowercase,
   --  returns the same value otherwise.

   Root_Path : constant GNAT.Regexp.Regexp :=
                 Compile ("/+|[A-Z]:\\+", Case_Sensitive => False);

   Temp_Directory : Object;
   --  The name of the temporary directory, computed once at elaboration time

   Dir_Seps : constant Ada.Strings.Maps.Character_Set :=
                Strings.Maps.To_Set ("/\");
   --  UNIX and DOS style directory separators

   --  From old GPR

   procedure Determine_Temporary_Directory;
   --  Determine temporary directory

   function Ensure_Directory (Path : Filename_Optional) return Filename_Type is
     (if Path (Path'Last) in OS_Lib.Directory_Separator | '/'
      then Path
      else Path & OS_Lib.Directory_Separator);

   function Remove_Last_DS (Path : Filename_Optional) return Filename_Optional
   is (if Path'Length > 0
         and then Path (Path'Last) in OS_Lib.Directory_Separator | '/' | '\'
       then Path (Path'First .. Path'Last - 1)
       else Path);

   function Containing_Directory (Path : Filename_Type) return Filename_Type;
   --  Containing directory for / is '/'
   --  This is Ada.Directories.Containing_Directory implementation with
   --  valid path name check removed to allow '*' chars.

   function Unchecked_Value (Self : Object) return String;
   --  Value function allowing call returning a value with no path separator
   --  For path objects created using a Path_Name with no path separator.

   -------------------
   -- Make_Absolute --
   -------------------

   function Make_Absolute
     (Name          : Filename_Type;
      Directory     : Filename_Optional := "";
      Resolve_Links : Boolean := False) return Filename_Type
   is
     (Filename_Type
        (OS_Lib.Normalize_Pathname
           ((if OS_Lib.Is_Absolute_Path (String (Name)) or else Directory = ""
              then ""
              else String (Ensure_Directory (Directory))) & String (Name),
              Resolve_Links => Resolve_Links)));

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (Path : Filename_Type) return GPR2.Simple_Name is
      Simple : constant GPR2.Simple_Name := Simple_Name (Path);
      --  Simple'First is guaranteed to be 1
   begin
      --  Ada.Directories.Base_Name cannot be used here as
      --  Path can contain '*' character that will be rejected on windows
      --  by Ada.Directories.Validity.Is_Valid_Path_Name check.
      --  The following code is Ada.Directories.Base_Name implementation
      --  calling a Simple_Name version allowing '*' chars in Path.

      --  Look for the last dot in the file name and
      --  return the part of the file name preceding this last dot.
      --  If the first dot is the first character of the file name,
      --  the base name is the empty string.
      --  ".filename" case is treated as a base name without extension.

      for Pos in reverse Simple'Range loop
         if Simple (Pos) = '.' and then Pos /= Simple'First then
            return Simple (Simple'First .. Pos - 1);
         end if;
      end loop;

      --  If there is no dot, return the complete file name

      return Simple;
   end Base_Name;

   ----------------------
   -- Change_Extension --
   ----------------------

   function Change_Extension
     (Self : Object; Extension : Filename_Optional) return Object
   is
      Result  : Object;
      Old_Ext : constant Filename_Optional :=
                  Filename_Optional (Directories.Extension (-Self.Value));
      New_Ext : constant Filename_Optional :=
                  (if Extension /= ""
                     and then Extension (Extension'First) = '.'
                   then Extension (Extension'First + 1 .. Extension'Last)
                   else Extension);

      procedure Replace_Extension
        (Path : in out Unbounded_String; New_Ext : Filename_Optional);
      --  Replaces the file extension in Path to the New_Ext

      -----------------------
      -- Replace_Extension --
      -----------------------

      procedure Replace_Extension
        (Path : in out Unbounded_String; New_Ext : Filename_Optional)
      is
         Low  :  constant Positive := Length (Path) - Old_Ext'Length +
                   (if Old_Ext = "" then 1 else 0);
         Suff : constant Filename_Optional :=
                   (if New_Ext = "" then "" else '.' & New_Ext);
      begin
         Replace_Slice (Path, Low, Length (Path), String (Suff));
      end Replace_Extension;

   begin
      if New_Ext = Old_Ext then
         return Self;
      end if;

      Result := Self;

      pragma Assert
        (Directories.Extension (-Self.As_Is) = String (Old_Ext));
      pragma Assert
        (Directories.Extension (-Self.Comparing) = To_OS_Case (Old_Ext));

      Replace_Extension (Result.Value,     New_Ext);
      Replace_Extension (Result.As_Is,     New_Ext);
      Replace_Extension (Result.Comparing,
                         Filename_Optional (To_OS_Case (New_Ext)));

      pragma Assert
        (Directories.Extension (-Result.As_Is) = String (New_Ext));
      pragma Assert
        (Directories.Extension (-Result.Value) = String (New_Ext));
      pragma Assert
        (Directories.Extension (-Result.Comparing)
         = To_OS_Case (New_Ext));

      return Result;
   end Change_Extension;

   -------------------
   -- Common_Prefix --
   -------------------

   function Common_Prefix (Self, Path : Object) return Object is

      use Ada.Directories.Hierarchical_File_Names;

      P1 : constant Filename_Type := Filename_Type (To_String (Self.Dir_Name));
      P2 : constant Filename_Type := Filename_Type (To_String (Path.Dir_Name));
      I1 : Positive := P1'First;
      I2 : Positive := P2'First;
      N1 : Natural;
      N2 : Natural;

   begin
      loop
         --  Object.Dir_Name always end with a dir separator, so no need to
         --  check for the case where P1 or P2 end with a subdir name.
         N1 := Strings.Fixed.Index (String (P1), Dir_Seps, I1);
         N2 := Strings.Fixed.Index (String (P2), Dir_Seps, I2);

         declare
            Sub1 : constant Filename_Optional := P1 (I1 .. N1 - 1);
            Sub2 : constant Filename_Optional := P2 (I2 .. N2 - 1);
         begin
            exit when Sub1 /= Sub2;

            I1 := N1 + 1;
            I2 := N2 + 1;
         end;

         exit when I1 not in P1'Range or else I2 not in P2'Range;
      end loop;

      if I1 = P1'First then
         --  No common directory at all: happens on windows when the
         --  paths are on two different drives
         return Undefined;

      elsif Is_Root_Directory_Name (String (P1 (P1'First .. I1 - 1))) then
         --  root dir is the only thing remaining: keep the final /
         return Create_Directory (P1 (P1'First .. I1 - 1));

      else
         --  remove last dir separator
         return Create_Directory (P1 (P1'First .. I1 - 2));
      end if;
   end Common_Prefix;

   -------------
   -- Compose --
   -------------

   function Compose
     (Self      : Object;
      Name      : Filename_Type;
      Directory : Boolean := False) return Object
   is
      Filename : constant Filename_Type :=
                   Filename_Type (Dir_Name (Self)) & Name;
   begin
      if Directory then
         return Create_Directory (Filename);
      else
         return Create_File (Filename, No_Resolution);
      end if;
   end Compose;

   --------------------------
   -- Containing_Directory --
   --------------------------

   function Containing_Directory (Path : Filename_Type) return Filename_Type is

      --  Ada.Directories.Containing_Directory cannot be used here as
      --  Path can contain '*' character that will be rejected on windows
      --  by Ada.Directories.Validity.Is_Valid_Path_Name check.
      use Ada.Directories.Hierarchical_File_Names;

      Last_DS : constant Natural :=
                  Strings.Fixed.Index
                    (String (Path), Dir_Seps, Going => Strings.Backward);

   begin
      if Last_DS = 0 then
         --  There is no directory separator, so return ".", representing
         --  the current working directory.

         return ".";

      else
         declare
            Result : constant Filename_Type := Path (Path'First .. Last_DS);

         begin
            --  Remove any trailing directory separator, except as the
            --  first character or the first character following a drive
            --  number on Windows.

            if Is_Root_Directory_Name (String (Result)) then
               return Result;
            else
               return Result (Result'First .. Result'Last - 1);
            end if;
         end;
      end if;
   end Containing_Directory;

   function Containing_Directory (Self : Object) return Object is
   begin
      if Self.Is_Directory then
         return Create_Directory
           (Containing_Directory (Remove_Last_DS (Dir_Name (Self))));

      elsif Self.Has_Dir_Name then
         return Create_Directory (Dir_Name (Self));

      else
         return Create_Directory
           (Containing_Directory (Name (Self)));
      end if;
   end Containing_Directory;

   --------------
   -- Contains --
   --------------

   function Contains (Self, Path : Object) return Boolean is
      Root   : constant Filename_Optional := Dir_Name (Self);
      Target : constant Filename_Optional := Dir_Name (Path);

   begin
      if Target'Length < Root'Length then
         return False;
      end if;

      return To_OS_Case (Root) = To_OS_Case (Target (Root'First .. Root'Last));
   end Contains;

   -----------------
   -- Content_MD5 --
   -----------------

   function Content_MD5 (Self : Object) return GNAT.MD5.Message_Digest is
      use Ada.Streams;
      use GNAT.MD5;

      C : Context;
      S : Stream_IO.File_Type;
      B : Stream_Element_Array (1 .. 100 * 1024);
      --  Buffer to read chunk of data
      L : Stream_Element_Offset;
   begin
      Stream_IO.Open (S, Stream_IO.In_File, To_String (Self.Value));

      while not Stream_IO.End_Of_File (S) loop
         Stream_IO.Read (S, B, L);
         Update (C, B (1 .. L));
      end loop;

      Stream_IO.Close (S);

      return Digest (C);
   end Content_MD5;

   ------------
   -- Create --
   ------------

   function Create
     (Name          : Filename_Type;
      Path_Name     : Filename_Type;
      Resolve_Links : Boolean := False) return Object
   is
      NN : constant Filename_Type :=
               (if Resolve_Links
                then Make_Absolute (Path_Name, Resolve_Links => Resolve_Links)
                else Path_Name);
      VN : constant Unbounded_String := +NN;
   begin
      return Object'
        (Is_Dir    => False,
         In_Memory => False,
         As_Is     => +String (Name),
         Value     => VN,
         Comparing => To_OS_Case (VN),
         Base_Name => +String (Base_Name (NN)),
         Dir_Name  => +Ensure_Directory (Containing_Directory (NN)));
   end Create;

   ----------------------
   -- Create_Directory --
   ----------------------

   function Create_Directory
     (Name          : Filename_Type;
      Directory     : Filename_Optional := "";
      Resolve_Links : Boolean := False) return Object
   is
      NN : constant Filename_Type :=
             Ensure_Directory (Make_Absolute (Name, Directory, Resolve_Links));
      VN : constant Unbounded_String := +NN;
   begin
      return Object'
        (Is_Dir    => True,
         In_Memory => False,
         As_Is     => +String (Name),
         Value     => VN,
         Comparing => To_OS_Case (VN),
         Base_Name => Null_Unbounded_String,
         Dir_Name  => VN);
   end Create_Directory;

   -----------------
   -- Create_File --
   -----------------

   function Create_File
     (Name      : Filename_Type;
      Directory : Filename_Optional := Resolve_On_Current) return Object is
   begin
      if Directory = No_Resolution
        and then not OS_Lib.Is_Absolute_Path (String (Name))
      then
         return Object'
           (As_Is     => +String (Name),
            Comparing => +To_OS_Case (Name),
            Base_Name => +Base_Name (Name),
            others    => <>);

      else
         declare
            NN : constant Filename_Type := Make_Absolute (Name, Directory);
            VN : constant Unbounded_String := +NN;
         begin
            return Object'
              (Is_Dir    => False,
               In_Memory => False,
               As_Is     => +String (Name),
               Value     => VN,
               Comparing => To_OS_Case (VN),
               Base_Name => +Base_Name (NN),
               Dir_Name  => +Ensure_Directory (Containing_Directory (NN)));
         end;
      end if;
   end Create_File;

   ------------------------
   -- Create_Pseudo_File --
   ------------------------

   function Create_Pseudo_File (Name : Filename_Type) return Object is
      Pseudo_Dir  : constant Filename_Type :=
                      OS_Lib.Directory_Separator & "<ram>";
      Pseudo_Full : constant Filename_Type :=
                      Pseudo_Dir
                      & OS_Lib.Directory_Separator
                      & Simple_Name (Name);
   begin
      return Object'
              (Is_Dir    => False,
               In_Memory => True,
               As_Is     => +String (Name),
               Value     => +Pseudo_Full,
               Comparing => To_OS_Case (+Pseudo_Full),
               Base_Name => +Base_Name (Name),
               Dir_Name  => +Ensure_Directory (Pseudo_Dir));
   end Create_Pseudo_File;

   ---------------------
   -- Create_Sym_Link --
   ---------------------

   procedure Create_Sym_Link (Self, To : Object) is

      function Symlink
        (Oldpath : System.Address;
         Newpath : System.Address) return Integer;
      pragma Import (C, Symlink, "__gnat_symlink");

      C_From  : constant String := To_String (Self.Value) & ASCII.NUL;
      pragma Warnings (Off, "*actuals for this call may be in wrong order*");
      C_To    : constant String :=
                  String (Relative_Path (To, Self)) & ASCII.NUL;
      Result  : Integer;
      Success : Boolean;
      pragma Unreferenced (Result);

   begin
      OS_Lib.Delete_File (To_String (Self.Value), Success);
      Result := Symlink (C_To'Address, C_From'Address);
   end Create_Sym_Link;

   -----------------------------------
   -- Determine_Temporary_Directory --
   -----------------------------------

   procedure Determine_Temporary_Directory is

      function Check_Directory (Name : String) return Boolean;
      --  Returns True if directory exists

      function Check_Environment (Name : String) return Boolean;
      --  Returns True if directory from environment variable exists

      ---------------------
      -- Check_Directory --
      ---------------------

      function Check_Directory (Name : String) return Boolean is
      begin
         if OS_Lib.Is_Directory (Name) then
            Temp_Directory :=
              Create_Directory
                (Filename_Type (OS_Lib.Normalize_Pathname (Name)));
            return True;
         end if;

         return False;
      end Check_Directory;

      -----------------------
      -- Check_Environment --
      -----------------------

      function Check_Environment (Name : String) return Boolean is
      begin
         if Environment_Variables.Exists (Name) then
            return Check_Directory (Environment_Variables.Value (Name));
         end if;

         return False;
      end Check_Environment;

   begin
      if Check_Environment ("TMPDIR")
        or else Check_Environment ("TEMP")
        or else Check_Environment ("TMP")
      then
         return;
      end if;

      case OS_Lib.Directory_Separator is
         when '\' =>
            if Check_Directory ("C:\TEMP")
              or else Check_Directory ("C:\TMP")
              or else Check_Directory ("\TEMP")
              or else Check_Directory ("\TMP")
            then
               return;
            end if;

         when '/' =>
            if Check_Directory ("/tmp")
              or else Check_Directory ("/var/tmp")
              or else Check_Directory ("/usr/tmp")
            then
               return;
            end if;

         when others =>
            pragma Annotate (Xcov, Exempt_On, "unreachable code");
            raise Internal_Error with
              "Unsupported directory separator " & OS_Lib.Directory_Separator;
            pragma Annotate (Xcov, Exempt_Off);

      end case;
   end Determine_Temporary_Directory;

   ------------
   -- Exists --
   ------------

   function Exists (Self : Object) return Boolean is
   begin
      return Length (Self.Value) > 0
        and then Directories.Exists (To_String (Self.Value));
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return False;
   end Exists;

   ---------------
   -- Extension --
   ---------------

   function Extension (Self : Object) return Filename_Optional is
      SN : constant GPR2.Simple_Name := Self.Simple_Name;
   begin
      return SN (SN'First + Length (Self.Base_Name) .. SN'Last);
   end Extension;

   -----------------------
   -- Filesystem_String --
   -----------------------

   function Filesystem_String (Self : Object) return VFS.Filesystem_String is
   begin
      if Self.Is_Defined then
         if Self.Has_Dir_Name then
            return VFS.Filesystem_String (To_String (Self.Value));
         else
            return VFS.Filesystem_String (Simple_Name (Self));
         end if;

      else
         return "";
      end if;
   end Filesystem_String;

   -----------------
   -- Is_Root_Dir --
   -----------------

   function Is_Root_Dir (Self : Object) return Boolean is
   begin
      return Self.Is_Directory and then
        Match (To_String (Self.Value), Root_Path);
   end Is_Root_Dir;

   ----------
   -- Name --
   ----------

   function Name
     (Self      : Object;
      Extension : Boolean := True) return Filename_Type
   is
      Name : constant String := To_String (Self.As_Is);
      Ext  : Natural;
      Sep  : Natural;
   begin
      if Extension or else Self.Is_Dir then
         return Filename_Type (Name);
      else
         Sep := Strings.Fixed.Index (Name, Dir_Seps,
                                     Going => Strings.Backward);

         --  Handle trailing directory separators

         if Sep = Name'Last then
            Sep := Strings.Fixed.Index
              (Name (Name'First .. Name'Last - 1),
               Dir_Seps, Going => Strings.Backward);
         end if;

         Ext := Strings.Fixed.Index (Name, ".", Going => Strings.Backward);

         if Ext = 0 or else Ext < Sep + 2 then
            --  .filename is treated as a whole file name without extension
            Ext := Name'Last;
         else
            Ext := Ext - 1;
         end if;

         return Filename_Type (Name (Name'First .. Ext));
      end if;
   end Name;

   -------------------
   -- Relative_Path --
   -------------------

   function Relative_Path (Self, From : Object) return Filename_Type is
      use Ada.Strings.Fixed;
      use GNATCOLL.Utils;

      P       : constant String := To_String (Self.Dir_Name);
      T       : constant String := To_String (From.Dir_Name);

      Pi : Positive := P'First; -- common prefix ending
      Ti : Positive := P'First;
      N  : Natural;

   begin
      --  First check for common prefix

      loop
         if Is_Directory_Separator (P (Ti))
           and then Is_Directory_Separator (T (Ti))
         then
            Pi := Ti;

         elsif To_OS_Case (P (Ti)) /= To_OS_Case (T (Ti)) then
            if Ti = P'First then
               --  First character differ, it can be only on Windows because
               --  UNIX path starts with a directory separator.
               --  "From" path is on another drive, returns original path.

               return Filename_Type (Unchecked_Value (Self));
            end if;

            exit;
         end if;

         exit when Ti in P'Last | T'Last;

         Ti := Ti + 1;
      end loop;

      --  Count directory under prefix in P, these will be replaced by the
      --  corresponding number of "..".

      N := Strings.Fixed.Count (T (Pi + 1 .. T'Last), Dir_Seps);

      return Filename_Optional
        (String'(N * (".." & GNAT.OS_Lib.Directory_Separator)
         & (if Pi = P'Last and then N = 0
            then (if Self.Is_Dir then "./" else "")
            else P (Pi + 1 .. P'Last))))
         & (if Self.Is_Dir then "" else Filename_Type (Self.Simple_Name));
   end Relative_Path;

   -----------------
   -- Simple_Name --
   -----------------

   function Simple_Name (Self : Object) return GPR2.Simple_Name is
   begin
      --  Ada.Directories.Simple_Name cannot be used here as
      --  Path can contain '*' character that will be rejected on windows
      --  by Ada.Directories.Validity.Is_Valid_Path_Name check.
      return Simple_Name (-Self.As_Is);
   end Simple_Name;

   function Simple_Name (Path : Filename_Optional) return GPR2.Simple_Name is

      Cut_Start : Natural :=
                    Strings.Fixed.Index
                      (String (Path), Dir_Seps, Going => Strings.Backward);

      --  Cut_End points to the last simple name character

      Cut_End   : Natural := Path'Last;

   begin
      --  Handle trailing directory separators

      if Cut_Start = Path'Last then
         Cut_End   := Path'Last - 1;
         Cut_Start := Strings.Fixed.Index
           (String (Path (Path'First .. Path'Last - 1)),
            Dir_Seps, Going => Strings.Backward);
      end if;

      --  Cut_Start points to the first simple name character

      Cut_Start := (if Cut_Start = 0 then Path'First else Cut_Start + 1);

      Check_For_Standard_Dirs : declare
         BN               : constant Filename_Type :=
                              Path (Cut_Start .. Cut_End);
         Has_Drive_Letter : constant Boolean :=
                              OS_Lib.Path_Separator /= ':';
         --  If Path separator is not ':' then we are on a DOS based OS
         --  where this character is used as a drive letter separator.

      begin
         if BN = "." or else BN = ".." then
            return GPR2.Simple_Name (BN);

         elsif Has_Drive_Letter
           and then BN'Length > 2
           and then Characters.Handling.Is_Letter (BN (BN'First))
           and then BN (BN'First + 1) = ':'
         then
            --  We have a DOS drive letter prefix, remove it

            return GPR2.Simple_Name (BN (BN'First + 2 .. BN'Last));

         else
            return GPR2.Simple_Name (BN);
         end if;
      end Check_For_Standard_Dirs;
   end Simple_Name;

   -------------------------
   -- Temporary_Directory --
   -------------------------

   function Temporary_Directory return Object is
   begin
      return Temp_Directory;
   end Temporary_Directory;

   ---------------------
   -- Unchecked_Value --
   ---------------------

   function Unchecked_Value (Self : Object) return String is
   begin
      if Self.Is_Dir and then not Self.Is_Root_Dir then
         return String (Remove_Last_DS (-Self.Value));
      else
         return To_String (Self.Value);
      end if;
   end Unchecked_Value;

   -----------
   -- Value --
   -----------

   function Value (Self : Object) return Full_Name is
   begin
      return Full_Name (Unchecked_Value (Self));
   end Value;

begin
   Determine_Temporary_Directory;
end GPR2.Path_Name;
