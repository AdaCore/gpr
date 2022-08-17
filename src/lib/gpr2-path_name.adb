--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
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

   function "+"
     (Str : String) return Unbounded_String renames To_Unbounded_String;

   function To_OS_Case (Name : String) return String is
     (if File_Names_Case_Sensitive
      then Name
      else Characters.Handling.To_Lower (Name));

   function To_OS_Case (Name : Unbounded_String) return Unbounded_String is
     (if File_Names_Case_Sensitive
      then Name
      else +Characters.Handling.To_Lower (To_String (Name)));
   --  If filenames is case insensitive converts path name to lowercase,
   --  returns the same value othervise.

   function To_OS_Case (C : Character) return Character is
     (if File_Names_Case_Sensitive
      then C
      else Characters.Handling.To_Lower (C));
   --  If filenames is case insensitive converts char to lowercase,
   --  returns the same value othervise.

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

   function Base_Name (Path : String) return String;
   --  Return Path basename (so simple name without extension).
   --  Assumes Path is a file.

   function Ensure_Directory (Path : String) return String is
     (if Path (Path'Last) = OS_Lib.Directory_Separator
      then Path
      else Path & OS_Lib.Directory_Separator);

   function Remove_Last_DS (Path : String) return String is
     (if Path'Length > 0
        and then Path (Path'Last) in OS_Lib.Directory_Separator | '/' | '\'
      then Path (Path'First .. Path'Last - 1)
      else Path);

   function Containing_Directory (Path : String) return String;
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
      Resolve_Links : Boolean := False) return String
   is
     (OS_Lib.Normalize_Pathname
        ((if OS_Lib.Is_Absolute_Path (String (Name)) or else Directory = ""
          then ""
          else Ensure_Directory (String (Directory)))
         & String (Name),
         Resolve_Links => Resolve_Links));

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (Path : String) return String is
      Simple : constant String := Simple_Name (Path);
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

      for Pos in reverse Simple'Range loop
         if Simple (Pos) = '.' then
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
     (Self : Object; Extension : Value_Type) return Object
   is
      Result  : Object;
      Old_Ext : constant Value_Type :=
                  Directories.Extension (To_String (Self.Value));
      New_Ext : constant Value_Type :=
                  (if Extension /= ""
                     and then Extension (Extension'First) = '.'
                   then Extension (Extension'First + 1 .. Extension'Last)
                   else Extension);

      procedure Replace_Extension
        (Path : in out Unbounded_String; New_Ext : Value_Type);
      --  Replaces the file extension in Path to the New_Ext

      -----------------------
      -- Replace_Extension --
      -----------------------

      procedure Replace_Extension
        (Path : in out Unbounded_String; New_Ext : Value_Type)
      is
         Low  :  constant Positive := Length (Path) - Old_Ext'Length +
                   (if Old_Ext = "" then 1 else 0);
         Suff : constant Value_Type :=
                   (if New_Ext = "" then "" else '.' & New_Ext);
      begin
         Replace_Slice (Path, Low, Length (Path), Suff);
      end Replace_Extension;

   begin
      if New_Ext = Old_Ext then
         return Self;
      end if;

      Result := Self;

      pragma Assert (Directories.Extension (To_String (Self.As_Is)) = Old_Ext);
      pragma Assert
        (Directories.Extension (To_String (Self.Comparing))
         = To_OS_Case (Old_Ext));

      Replace_Extension (Result.Value,     New_Ext);
      Replace_Extension (Result.As_Is,     New_Ext);
      Replace_Extension (Result.Comparing, To_OS_Case (New_Ext));

      pragma Assert
        (Directories.Extension (To_String (Result.As_Is)) = New_Ext);
      pragma Assert
        (Directories.Extension (To_String (Result.Value)) = New_Ext);
      pragma Assert
        (Directories.Extension (To_String (Result.Comparing))
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
      N1, N2 : Natural;

   begin
      loop
         --  Object.Dir_Name always end with a dir separator, so no need to
         --  check for the case where P1 or P2 end with a subdir name.
         N1 := Ada.Strings.Fixed.Index (String (P1), Dir_Seps, I1);
         N2 := Ada.Strings.Fixed.Index (String (P2), Dir_Seps, I2);

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

   function Containing_Directory (Path : String) return String is

      --  Ada.Directories.Containing_Directory cannot be used here as
      --  Path can contain '*' character that will be rejected on windows
      --  by Ada.Directories.Validity.Is_Valid_Path_Name check.
      use Ada.Directories.Hierarchical_File_Names;

      Last_DS : constant Natural :=
                  Strings.Fixed.Index
                    (Path, Dir_Seps, Going => Strings.Backward);

   begin
      if Last_DS = 0 then
         --  There is no directory separator, so return ".", representing
         --  the current working directory.

         return ".";

      else
         declare
            Result : String := Path (Path'First .. Last_DS);

         begin
            Result := Path (Path'First .. Last_DS);

            --  Remove any trailing directory separator, except as the
            --  first character or the first character following a drive
            --  number on Windows.

            if Is_Root_Directory_Name (Result) then
               return Result;
            else
               return Result (1 .. Result'Last - 1);
            end if;
         end;
      end if;
   end Containing_Directory;

   function Containing_Directory (Self : Object) return Object is
   begin
      if Self.Is_Directory then
         return Create_Directory
           (Filename_Type
              (Containing_Directory (Remove_Last_DS (Dir_Name (Self)))));
      elsif Self.Has_Dir_Name then
         return Create_Directory (Filename_Type (Dir_Name (Self)));
      else
         return Create_Directory
           (Filename_Type
              (Containing_Directory (String (Name (Self)))));
      end if;
   end Containing_Directory;

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

   function Create (Name, Path_Name : Filename_Type) return Object is
      Value : constant Unbounded_String := +String (Path_Name);
   begin
      return Object'
        (Is_Dir    => False,
         As_Is     => +String (Name),
         Value     => Value,
         Comparing => To_OS_Case (Value),
         Base_Name => +Base_Name (String (Path_Name)),
         Dir_Name  =>
           +Ensure_Directory (Containing_Directory (String (Path_Name))));
   end Create;

   ----------------------
   -- Create_Directory --
   ----------------------

   function Create_Directory
     (Name          : Filename_Type;
      Directory     : Filename_Optional := "";
      Resolve_Links : Boolean := False) return Object
   is
      NN : constant String :=
             Ensure_Directory (Make_Absolute (Name, Directory, Resolve_Links));
      VN : constant Unbounded_String := +NN;

   begin
      return Object'
        (Is_Dir    => True,
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
      Directory : Filename_Optional := Resolve_On_Current) return Object
   is
   begin
      if Directory = No_Resolution
        and then not OS_Lib.Is_Absolute_Path (String (Name))
      then
         return Object'
           (As_Is     => +String (Name),
            Comparing => +To_OS_Case (String (Name)),
            Base_Name => +Base_Name (String (Name)),
            others    => <>);
      else
         declare
            NN : constant String := Make_Absolute (Name, Directory);
            VN : constant Unbounded_String := +NN;
         begin
            return Object'
              (Is_Dir    => False,
               As_Is     => +String (Name),
               Value     => VN,
               Comparing => To_OS_Case (VN),
               Base_Name => +Base_Name (NN),
               Dir_Name  => +Ensure_Directory (Containing_Directory (NN)));
         end;
      end if;
   end Create_File;

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
                  String (Relative_Path (To, Self).Name)
                  & String (Simple_Name (To)) & ASCII.NUL;
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
         use Ada.Environment_Variables;
      begin
         if Exists (Name) then
            return Check_Directory (Value (Name));
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
            raise Program_Error with
              "Unsupported directory separator " & OS_Lib.Directory_Separator;
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
      Ext : Natural;
   begin
      if Extension or else Self.Is_Dir then
         return Filename_Type (Name);
      else
         Ext := Strings.Fixed.Index (Name, ".", Going => Strings.Backward);

         if Ext = 0 then
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

   function Relative_Path (Self, From : Object) return Object is
      use Ada.Strings.Fixed;
      use GNATCOLL.Utils;

      P : constant String := To_String (Self.Dir_Name);
      T : constant String := To_String (From.Dir_Name);

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
               --  unix path started from directory separator.
               --  "From" path is on another drive, returns original path.

               return Self;
            end if;

            exit;
         end if;

         exit when Ti in P'Last | T'Last;

         Ti := Ti + 1;
      end loop;

      --  Count directory under prefix in P, these will be replaced by the
      --  corresponding number of "..".

      N := Strings.Fixed.Count (T (Pi + 1 .. T'Last), Dir_Seps);

      return Create_Directory
        (Filename_Type (String'(N * "../")
         & (if Pi = P'Last and then N = 0
           then "./"
           else P (Pi + 1 .. P'Last))),
         Filename_Optional (T));
   end Relative_Path;

   -----------------
   -- Simple_Name --
   -----------------

   function Simple_Name (Self : Object) return GPR2.Simple_Name is
   begin
      --  Ada.Directories.Simple_Name cannot be used here as
      --  Path can contain '*' character that will be rejected on windows
      --  by Ada.Directories.Validity.Is_Valid_Path_Name check.
      return GPR2.Simple_Name
        (Simple_Name (To_String (Self.As_Is)));
   end Simple_Name;

   function Simple_Name (Path : String) return String is

      Cut_Start : Natural :=
                    Strings.Fixed.Index
                      (Path, Dir_Seps, Going => Strings.Backward);

      --  Cut_End points to the last simple name character

      Cut_End   : Natural := Path'Last;

   begin
      --  Handle trailing directory separators

      if Cut_Start = Path'Last then
         Cut_End   := Path'Last - 1;
         Cut_Start := Strings.Fixed.Index
           (Path (Path'First .. Path'Last - 1),
            Dir_Seps, Going => Strings.Backward);
      end if;

      --  Cut_Start points to the first simple name character

      Cut_Start := (if Cut_Start = 0 then Path'First else Cut_Start + 1);

      Check_For_Standard_Dirs : declare
         BN : constant String := Path (Cut_Start .. Cut_End);

         Has_Drive_Letter : constant Boolean :=
                              OS_Lib.Path_Separator /= ':';
         --  If Path separator is not ':' then we are on a DOS based OS
         --  where this character is used as a drive letter separator.

      begin
         if BN = "." or else BN = ".." then
            return BN;

         elsif Has_Drive_Letter
           and then BN'Length > 2
           and then Characters.Handling.Is_Letter (BN (BN'First))
           and then BN (BN'First + 1) = ':'
         then
            --  We have a DOS drive letter prefix, remove it

            return BN (BN'First + 2 .. BN'Last);

         else
            return BN;
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
         return Remove_Last_DS (To_String (Self.Value));
      else
         return To_String (Self.Value);
      end if;
   end Unchecked_Value;

   -----------
   -- Value --
   -----------

   function Value (Self : Object) return Full_Name is
   begin
      return Unchecked_Value (Self);
   end Value;

begin
   Determine_Temporary_Directory;
end GPR2.Path_Name;
