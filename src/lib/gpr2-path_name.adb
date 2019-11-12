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

with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Directories.Hierarchical_File_Names;
with Ada.Environment_Variables;
with Ada.Streams.Stream_IO;

with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with GNAT.OS_Lib;
with GNAT.Regexp;

with System;

package body GPR2.Path_Name is

   use Ada;

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
   --  Base_Name for / is '.'

   function Ensure_Directory (Path : String) return String is
     (if Path (Path'Last) = OS_Lib.Directory_Separator
      then Path
      else Path & OS_Lib.Directory_Separator);

   function Remove_Last_DS (Path : String) return String is
     (if Path'Length > 0
        and then Path (Path'Last) in OS_Lib.Directory_Separator | '/' | '\'
      then Path (Path'First .. Path'Last - 1)
      else Path);

   function Simple_Name (Path : String) return String;
   --  Returns the simple name portion of the file name specified by Name.
   --  This is Ada.Directories.Simple_Name implementation with
   --  valid path name check removed to allow '*' chars.

   function Containing_Directory (Path : String) return String;
   --  Containing directory for / is '/'
   --  This is Ada.Directories.Containing_Directory implementation with
   --  valid path name check removed to allow '*' chars.

   -------------------
   -- Make_Absolute --
   -------------------

   function Make_Absolute
     (Name      : Name_Type;
      Directory : Optional_Name_Type := "") return String
   is
     (OS_Lib.Normalize_Pathname
        ((if OS_Lib.Is_Absolute_Path (String (Name)) or else Directory = ""
          then ""
          else Ensure_Directory (String (Directory)))
         & String (Name)));

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (Path : String) return String is
   begin
      if Match (Path, Root_Path) then
         return ".";
      else
         --  Ada.Directories.Base_Name cannot be used here as
         --  Path can contain '*' character that will be rejected on windows
         --  by Ada.Directories.Validity.Is_Valid_Path_Name check.
         --  The following code is Ada.Directories.Base_Name implementation
         --  calling a Simple_Name version allowing '*' chars in Path.
         declare

            Simple : constant String := Simple_Name (Path);
            --  Simple'First is guaranteed to be 1

         begin
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
         end;
      end if;
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
      P1 : constant String := To_String (Self.Value);
      P2 : constant String := To_String (Path.Value);
      I1 : Positive := P1'First;
      I2 : Positive := P2'First;
   begin
      while I1 <= P1'Last
        and then I2 <= P2'Last
        and then To_OS_Case (P1 (I1)) = To_OS_Case (P2 (I2))
      loop
         I1 := I1 + 1;
         I2 := I2 + 1;
      end loop;

      declare
         CP : constant Name_Type := Name_Type (P1 (P1'First .. I1 - 1));
      begin
         if Self.Is_Dir then
            return Create_Directory (CP);
         else
            return Create_File (CP, No_Resolution);
         end if;
      end;
   end Common_Prefix;

   -------------
   -- Compose --
   -------------

   function Compose
     (Self      : Object;
      Name      : Name_Type;
      Directory : Boolean := False) return Object
   is
      Filename : constant Name_Type :=
                   Name_Type (Dir_Name (Self) & String (Name));
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
   begin
      if Match (Path, Root_Path) then
         return Path;
      else
         --  Ada.Directories.Containing_Directory cannot be used here as
         --  Path can contain '*' character that will be rejected on windows
         --  by Ada.Directories.Validity.Is_Valid_Path_Name check.
         declare
            use Ada.Directories.Hierarchical_File_Names;

            Last_DS : constant Natural :=
                        Strings.Fixed.Index
                          (Path, Dir_Seps, Going => Strings.Backward);

         begin
            --  If Path indicates a root directory, raise Use_Error, because
            --  it has no containing directory.

            if Is_Parent_Directory_Name (Path)
              or else Is_Current_Directory_Name (Path)
              or else Is_Root_Directory_Name (Path)
            then
               raise Ada.Directories.Use_Error with
                 "directory """ & Path & """ has no containing directory";

            elsif Last_DS = 0 then
               --  There is no directory separator, so return ".", representing
               --  the current working directory.

               return ".";

            else
               declare
                  Last   : Positive := Last_DS - Path'First + 1;
                  Result : String (1 .. Last);

               begin
                  Result := Path (Path'First .. Last_DS);

                  --  Remove any trailing directory separator, except as the
                  --  first character or the first character following a drive
                  --  number on Windows.

                  while Last > 1 loop
                     exit when Is_Root_Directory_Name (Result (1 .. Last))
                       or else (Result (Last) /= OS_Lib.Directory_Separator
                                and then Result (Last) /= '/');

                     Last := Last - 1;
                  end loop;

                  return Result (1 .. Last);
               end;
            end if;
         end;
      end if;
   end Containing_Directory;

   function Containing_Directory (Self : Object) return Object is
   begin
      return Create_Directory
        (Name_Type
           (Containing_Directory (Remove_Last_DS (Dir_Name (Self)))));
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

   function Create (Name, Path_Name : Name_Type) return Object is
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
     (Name      : Name_Type;
      Directory : Optional_Name_Type := "") return Object
   is
      NN : constant String :=
             Ensure_Directory (Make_Absolute (Name, Directory));
      VN : constant Unbounded_String := +NN;
   begin
      return Object'
        (Is_Dir    => True,
         As_Is     => +String (Name),
         Value     => VN,
         Comparing => To_OS_Case (VN),
         Base_Name => +Base_Name (NN),
         Dir_Name  => +Ensure_Directory (Containing_Directory (NN)));
   end Create_Directory;

   -----------------
   -- Create_File --
   -----------------

   function Create_File
     (Name      : Name_Type;
      Directory : Optional_Name_Type := Resolve_On_Current) return Object
   is
   begin
      if Directory = No_Resolution
        and then not OS_Lib.Is_Absolute_Path (String (Name))
      then
         return Object'
           (As_Is     => +String (Name),
            Comparing => +String (Name),
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
      pragma Unreferenced (Success, Result);

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
              Create_Directory (Name_Type (OS_Lib.Normalize_Pathname (Name)));
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
      return Directories.Exists (To_String (Self.Value));
   end Exists;

   ----------
   -- Name --
   ----------

   function Name
     (Self      : Object;
      Extension : Boolean := True) return Name_Type
   is
      Name : constant String := To_String (Self.As_Is);
      Ext : Natural;
   begin
      if Extension then
         return Name_Type (Name);
      else
         Ext := Strings.Fixed.Index (Name, ".", Going => Strings.Backward);

         if Ext = 0 then
            Ext := Name'Last;
         else
            Ext := Ext - 1;
         end if;

         return Name_Type (Name (Name'First .. Ext));
      end if;
   end Name;

   -------------------
   -- Relative_Path --
   -------------------

   function Relative_Path (Self, To : Object) return Object is

      use Ada.Strings.Maps;
      use Ada.Strings.Fixed;

      Pathname : constant String := To_String (Self.Dir_Name);
      To_Path  : constant String := To_String (To.Dir_Name);

      --  Local variables

      Dir_Sep_Map : constant Character_Mapping := To_Mapping ("\", "/");

      P  : String (1 .. Pathname'Length) := Pathname;
      T  : String (1 .. To_Path'Length) := To_Path;

      Pi : Natural; -- common prefix ending
      N  : Natural := 0;

   begin
      --  Use canonical directory separator

      Strings.Fixed.Translate (Source => P, Mapping => Dir_Sep_Map);
      Strings.Fixed.Translate (Source => T, Mapping => Dir_Sep_Map);

      --  First check for common prefix

      Pi := 1;

      while Pi < P'Last
        and then Pi < T'Last
        and then To_OS_Case (P (Pi)) = To_OS_Case (T (Pi)) loop
         Pi := Pi + 1;
      end loop;

      --  Cut common prefix at a directory separator

      while Pi > P'First and then P (Pi) /= '/' loop
         Pi := Pi - 1;
      end loop;

      --  Count directory under prefix in P, these will be replaced by the
      --  corresponding number of "..".

      N := Strings.Fixed.Count (T (Pi + 1 .. T'Last), "/");

      if T (T'Last) /= '/' then
         N := N + 1;
      end if;

      return Create_Directory
        (Name_Type (String'(N * "../")
         & (if Pi = P'Last and then N = 0
           then "./"
           else P (Pi + 1 .. P'Last))),
         Optional_Name_Type (To_Path));
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
        (Simple_Name (Remove_Last_DS (To_String (Self.As_Is))));
   end Simple_Name;

   function Simple_Name (Path : String) return String is

      use Ada.Directories.Hierarchical_File_Names;

      Cut_Start : Natural :=
                    Strings.Fixed.Index
                      (Path, Dir_Seps, Going => Strings.Backward);

      --  Cut_End points to the last simple name character

      Cut_End   : Natural := Path'Last;

   begin
      --  Root directories are considered simple

      if Is_Root_Directory_Name (Path) then
         return Path;
      end if;

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

   -----------
   -- Value --
   -----------

   function Value (Self : Object) return Full_Name is
   begin
      if Self.Is_Dir then
         return Remove_Last_DS (To_String (Self.Value));
      else
         return To_String (Self.Value);
      end if;
   end Value;

begin
   Determine_Temporary_Directory;
end GPR2.Path_Name;
