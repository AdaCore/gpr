------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2017, Free Software Foundation, Inc.          --
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

with Ada.Directories;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Less_Case_Insensitive;

with GNAT.OS_Lib;

package body GPR2 is

   ---------
   -- "<" --
   ---------

   overriding function "<" (Left, Right : Optional_Name_Type) return Boolean is
      use Ada.Strings;
   begin
      return Less_Case_Insensitive (String (Left), String (Right));
   end "<";

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Optional_Name_Type) return Boolean is
      use Ada.Strings;
   begin
      return Equal_Case_Insensitive (String (Left), String (Right));
   end "=";

   -----------------
   -- Create_File --
   -----------------

   function Create_File (Name : Name_Type) return Path_Name_Type is
      use Ada;
      use GNAT;

      function "+"
        (Str : String) return Unbounded_String renames To_Unbounded_String;

      N : constant String := String (Name);
   begin
      return Path_Name_Type'
        (As_Is     => +N,
         Value     => +OS_Lib.Normalize_Pathname (N),
         Base_Name => +Directories.Base_Name (N),
         Dir_Name  => +Directories.Containing_Directory (N));
   end Create_File;

   -------------
   -- Unquote --
   -------------

   function Unquote (Str : Value_Type) return Value_Type is
   begin
      if Str'Length > 2
        and then
          ((Str (Str'First) = ''' and then Str (Str'Last) = ''')
           or else (Str (Str'First) = '"' and then Str (Str'Last) = '"'))
      then
         return Str (Str'First + 1 .. Str'Last - 1);
      else
         return Str;
      end if;
   end Unquote;

   -----------
   -- Value --
   -----------

   function Value (File : Path_Name_Type) return Full_Path_Name is
   begin
      return To_String (File.Value);
   end Value;

end GPR2;
