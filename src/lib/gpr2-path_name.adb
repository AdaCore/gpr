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

   function To_OS_Case (Name : Unbounded_String) return Unbounded_String is
     (if File_Names_Case_Sensitive
      then Name
      else +Characters.Handling.To_Lower (To_String (Name)));
   --  If filernames is case insensitive converts path name to lowercase,
   --  returns the same value othervise.

   Root_Path : constant GNAT.Regexp.Regexp :=
                 Compile ("/+|[A-Z]:\\+", Case_Sensitive => False);

   Temp_Directory : Object;
   --  The name of the temporary directory, computed once at elaboration time

   --  From old GPR

   procedure Determine_Temporary_Directory;
   --  Determine temporary directory

   function Ensure_Directory (Path : String) return String is
     (if Path (Path'Last) = OS_Lib.Directory_Separator
      then Path
      else Path & OS_Lib.Directory_Separator);

   function Base_Name (Path : String) return String is
     (if Match (Path, Root_Path)
      then "."
      else Directories.Base_Name (Path));
   --  Base_Name for / is '.'

   function Remove_Last_DS (Path : String) return String is
     (if Path'Length > 0
        and then Path (Path'Last) in OS_Lib.Directory_Separator | '/' | '\'
      then Path (Path'First .. Path'Last - 1)
      else Path);

   function Containing_Directory (Path : String) return String is
     (if Match (Path, Root_Path)
      then Path
      else Directories.Containing_Directory (Path));
   --  Containing directroy for / is '/'

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
        and then P1 (I1) = P2 (I2)
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
            return Create_File (CP);
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
         return Create_File (Filename);
      end if;
   end Compose;

   --------------------------
   -- Containing_Directory --
   --------------------------

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
      Directory : Optional_Name_Type := "") return Object
   is
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

      while Pi < P'Last and then Pi < T'Last and then P (Pi) = T (Pi) loop
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
      return GPR2.Simple_Name
        (Directories.Simple_Name (Remove_Last_DS (To_String (Self.As_Is))));
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
