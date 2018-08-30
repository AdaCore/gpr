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

with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with System;

with GPR.Tempdir;
with GPR.Util;

with GNAT.OS_Lib;

package body GPR2.Path_Name is

   use GNAT;

   --  From old GPR

   function Temporary_Directory
     return String renames GPR.Tempdir.Temporary_Directory_Path;

   function Ensure_Directory
     (Path : String) return String renames GPR.Util.Ensure_Directory;

   -------------------
   -- Make_Absolute --
   -------------------

   function Make_Absolute
     (Name      : Name_Type;
      Directory : Optional_Name_Type := "") return Name_Type
   is
     (Name_Type
        ((if OS_Lib.Is_Absolute_Path (String (Name)) or else Directory = ""
          then ""
          else Ensure_Directory (String (Directory)))
         & String (Name)));

   -------------
   -- Compose --
   -------------

   function Compose
     (Self : Object; Name : Name_Type) return Object is
   begin
      return Create_File
        (Name_Type
           (Ensure_Directory (To_String (Self.As_Is)) & String (Name)));
   end Compose;

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
      use Ada;

      function "+"
        (Str : String) return Unbounded_String renames To_Unbounded_String;
   begin
      return Object'
        (Is_Dir    => False,
         As_Is     => +String (Name),
         Value     => +String (Path_Name),
         Base_Name => +Directories.Base_Name (String (Path_Name)),
         Dir_Name  =>
           +Ensure_Directory
           (Directories.Containing_Directory (String (Path_Name))));
   end Create;

   ----------------------
   -- Create_Directory --
   ----------------------

   function Create_Directory
     (Name      : Name_Type;
      Directory : Optional_Name_Type := "") return Object
   is
      use Ada;

      function "+"
        (Str : String) return Unbounded_String renames To_Unbounded_String;

      N  : constant String := String (Make_Absolute (Name, Directory));
      NN : constant String := Ensure_Directory (OS_Lib.Normalize_Pathname (N));

   begin
      return Object'
        (Is_Dir    => True,
         As_Is     => +String (Name),
         Value     => +NN,
         Base_Name => +Directories.Base_Name (N),
         Dir_Name  =>
           +Ensure_Directory (Directories.Containing_Directory (NN)));
   end Create_Directory;

   -----------------
   -- Create_File --
   -----------------

   function Create_File
     (Name      : Name_Type;
      Directory : Optional_Name_Type := "") return Object
   is
      use Ada;

      function "+"
        (Str : String) return Unbounded_String renames To_Unbounded_String;

      N : constant String := String (Make_Absolute (Name, Directory));

   begin
      return Object'
        (Is_Dir    => False,
         As_Is     => +String (Name),
         Value     => +OS_Lib.Normalize_Pathname (N),
         Base_Name => +Directories.Base_Name (N),
         Dir_Name  =>
           +Ensure_Directory (Directories.Containing_Directory (N)));
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
                  & To_String (To.Base_Name) & ASCII.NUL;
      Result  : Integer;
      Success : Boolean;
      pragma Unreferenced (Success, Result);

   begin
      OS_Lib.Delete_File (To_String (Self.Value), Success);
      Result := Symlink (C_To'Address, C_From'Address);
   end Create_Sym_Link;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Self : Object) return Boolean is
   begin
      return OS_Lib.Is_Regular_File (Value (Self));
   end Is_Regular_File;

   -------------------
   -- Relative_Path --
   -------------------

   function Relative_Path (Self, To : Object) return Object is

      use Ada;
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

      return Create_File
        (Name_Type
           (String'(N * "../") & Ensure_Directory (P (Pi + 1 .. P'Last))));
   end Relative_Path;

   -------------------------
   -- Temporary_Directory --
   -------------------------

   function Temporary_Directory return Object is
   begin
      return Create_Directory (Name_Type (String'(Temporary_Directory)));
   end Temporary_Directory;

   -----------
   -- Value --
   -----------

   function Value (Self : Object) return Full_Name is
   begin
      return To_String (Self.Value);
   end Value;

end GPR2.Path_Name;
