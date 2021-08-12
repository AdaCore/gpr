------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Less_Case_Insensitive;

with GNAT.OS_Lib;

package body GPR2 is

   package Languages_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Natural,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);
   package Languages_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String,
      "="          => Ada.Strings.Equal_Case_Insensitive);

   Languages_To_Id : Languages_Maps.Map;
   Id_To_Languages : Languages_Vectors.Vector;

   ---------
   -- "+" --
   ---------

   function "+" (L : Optional_Name_Type) return Language_Id is
      C      : Languages_Maps.Cursor;
      Result : Natural;
   begin
      if L'Length = 0 then
         return No_Language;
      end if;

      --  ??? Not thread-safe
      C := Languages_To_Id.Find (String (L));

      if Languages_Maps.Has_Element (C) then
         return Language_Id (Languages_Maps.Element (C));
      else
         declare
            Value : constant String :=
                      Ada.Characters.Handling.To_Lower (String (L));
         begin
            Id_To_Languages.Append (Value);
            Result := Natural (Id_To_Languages.Last_Index);
            Languages_To_Id.Insert (Value, Result);
         end;

         return Language_Id (Result);
      end if;
   end "+";

   ---------
   -- "<" --
   ---------

   overriding function "<" (Left, Right : Optional_Name_Type) return Boolean is
      use Ada.Strings;
   begin
      return Less_Case_Insensitive (String (Left), String (Right));
   end "<";

   overriding function "<" (Left, Right : Filename_Optional) return Boolean is
   begin
      return (if File_Names_Case_Sensitive
              then String (Left) < String (Right)
              else Ada.Strings.Less_Case_Insensitive
                     (String (Left), String (Right)));
   end "<";

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Optional_Name_Type) return Boolean is
      use Ada.Strings;
   begin
      return Equal_Case_Insensitive (String (Left), String (Right));
   end "=";

   overriding function "=" (Left, Right : Filename_Optional) return Boolean is
   begin
      return (if File_Names_Case_Sensitive
              then String (Left) = String (Right)
              else Ada.Strings.Equal_Case_Insensitive
                     (String (Left), String (Right)));
   end "=";

   -------------------------
   -- Get_Tools_Directory --
   -------------------------

   function Get_Tools_Directory return String is
      use type GNAT.OS_Lib.String_Access;

      GPRls : GNAT.OS_Lib.String_Access :=
                GNAT.OS_Lib.Locate_Exec_On_Path ("gprls");
      --  Check for GPRls executable
   begin
      if GPRls = null then
         return "";
      else
         return Result : constant String :=
                           Directories.Containing_Directory
                             (Directories.Containing_Directory
                                (GNAT.OS_Lib.Normalize_Pathname
                                   (GPRls.all, Resolve_Links => True)))
         do
            GNAT.OS_Lib.Free (GPRls);
         end return;
      end if;
   end Get_Tools_Directory;

   ----------
   -- Hash --
   ----------

   function Hash (L : Language_Id) return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Containers.Hash_Type (L);
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (L : Language_Id) return String is
   begin
      return To_Mixed (String (Name (L)));
   end Image;

   --------------------------
   -- Is_Runtime_Unit_Name --
   --------------------------

   function Is_Runtime_Unit_Name (Name : Name_Type) return Boolean is

      LN : constant String := To_Lower (Name);

      function Is_It (Root : String) return Boolean is
        (GNATCOLL.Utils.Starts_With (LN, Root)
         and then (LN'Length = Root'Length
                   or else (LN'Length > Root'Length + 1
                            and then LN (LN'First + Root'Length) = '.')));
      --  Returns True if LN equal to Root or starts with Root & '.' and has
      --  length more than Root'Length + 2.

   begin
      return Is_It ("ada")
        or else Is_It ("system")
        or else Is_It ("interfaces")
        or else Is_It ("gnat")
        or else LN in "direct_io"
                    | "calendar"
                    | "io_exceptions"
                    | "machine_code"
                    | "unchecked_conversion"
                    | "unchecked_deallocation";
   end Is_Runtime_Unit_Name;

   ----------
   -- Name --
   ----------

   function Name (L : Language_Id) return Optional_Name_Type is
   begin
      if L = No_Language then
         return "";
      end if;

      return Optional_Name_Type (Id_To_Languages.Element (Natural (L)));
   end Name;

   -----------------
   -- Parent_Name --
   -----------------

   function Parent_Name (Name : Name_Type) return Optional_Name_Type is
   begin
      for J in reverse Name'Range loop
         if Name (J) = '.'  then
            return Name (Name'First .. J - 1);
         end if;
      end loop;

      return No_Name;
   end Parent_Name;

   -----------
   -- Quote --
   -----------

   function Quote
     (Str        : Value_Type;
      Quote_With : Character := '"') return Value_Type is
   begin
      return Quote_With & Str & Quote_With;
   end Quote;

   ---------------
   -- Set_Debug --
   ---------------

   procedure Set_Debug (Enable : Boolean) is
   begin
      Debug := Enable;
   end Set_Debug;

   -------------------
   -- To_Hex_String --
   -------------------

   function To_Hex_String (Num : Word) return String is
      Hex_Digit : constant array (Word range 0 .. 15) of Character :=
                    "0123456789abcdef";
      Result : String (1 .. 8);
      Value  : Word := Num;
   begin
      for J in reverse Result'Range loop
         Result (J) := Hex_Digit (Value mod 16);
         Value := Value / 16;
      end loop;

      return Result;
   end To_Hex_String;

   --------------
   -- To_Mixed --
   --------------

   function To_Mixed (A : String) return String is
      use Ada.Characters.Handling;
      Ucase  : Boolean := True;
      Result : String (A'Range);
   begin
      for J in A'Range loop
         if Ucase then
            Result (J) := To_Upper (A (J));
         else
            Result (J) := To_Lower (A (J));
         end if;

         Ucase := A (J) in '_' | '.' | ' ';
      end loop;

      return Result;
   end To_Mixed;

   -------------
   -- Unquote --
   -------------

   function Unquote (Str : Value_Type) return Value_Type is
   begin
      if Str'Length >= 2
        and then
          ((Str (Str'First) = ''' and then Str (Str'Last) = ''')
           or else (Str (Str'First) = '"' and then Str (Str'Last) = '"'))
      then
         return Str (Str'First + 1 .. Str'Last - 1);
      else
         return Str;
      end if;
   end Unquote;

begin

   Id_To_Languages.Append ("Ada");
   Languages_To_Id.Insert ("Ada", 1);

end GPR2;
