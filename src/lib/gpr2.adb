--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Strings.Less_Case_Insensitive;

pragma Warnings (Off, "* is an internal GNAT unit");
with System.Soft_Links;               use System.Soft_Links;
pragma Warnings (On, "* is an internal GNAT unit");

with GNAT.OS_Lib;

package body GPR2 is

   Is_Multitasking : constant Boolean :=
      System.Soft_Links.Lock_Task /= System.Soft_Links.Task_Lock_NT'Access;

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

   --------
   -- Id --
   --------

   function Id (List : in out Name_List;
                Name : Optional_Name_Type) return Natural
   is
      C          : Name_Maps.Cursor;
      Result     : Natural;
      Value      : constant String :=
                     Ada.Characters.Handling.To_Lower (String (Name));
   begin
      if Name'Length = 0 then
         return 0;
      end if;

      --  Note: if we just read the value, the operation is multithread-safe.
      --  So let's not add a penalty for the read operation, that should be
      --  the most common operation.
      C := List.Name_To_Id.Find (Value);

      if Name_Maps.Has_Element (C) then
         return Name_Maps.Element (C);
      end if;

      --  We need to add the value: as this operation is not atomic
      --  and the tables are global, we need to ensure the operation
      --  cannot be interrupted.
      begin
         System.Soft_Links.Lock_Task.all;

         if Is_Multitasking then
            --  In a multitasking environment, the value could have been
            --  inserted by someone else since we've checked it above.
            --  So let's retry:
            C := List.Name_To_Id.Find (Value);

            if Name_Maps.Has_Element (C) then
               --  return with the value
               System.Soft_Links.Unlock_Task.all;
               return Name_Maps.Element (C);
            end if;
         end if;

         --  Still not in there, so let's add the value to the list
         List.Id_To_Name.Append (Value);
         Result := Natural (List.Id_To_Name.Last_Index);
         List.Name_To_Id.Insert (Value, Result);

      exception
         when others =>
            System.Soft_Links.Unlock_Task.all;
            raise;
      end;

      --  Don't need the lock anymore
      System.Soft_Links.Unlock_Task.all;

      return Result;
   end Id;

   -----------
   -- Image --
   -----------

   function Image (List : Name_List;
                   Id   : Natural) return String is
   begin
      return To_Mixed (String (Name (List, Id)));
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

   function Name (List : Name_List;
                  Id   : Natural) return Optional_Name_Type is
   begin
      if Id = 0 then
         return "";
      end if;

      return Optional_Name_Type (List.Id_To_Name.Element (Id));
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

   procedure Set_Debug (Mode : Character; Enable : Boolean := True) is
   begin
      if Mode = '*' then
         Debug := (others => Enable);
      else
         Debug (Mode) := Enable;
      end if;
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
   Language_List.Id_To_Name.Append ("ada");
   Language_List.Id_To_Name.Append ("c");
   Language_List.Id_To_Name.Append ("c++");
   Language_List.Name_To_Id.Insert ("ada", 1);
   Language_List.Name_To_Id.Insert ("c",   2);
   Language_List.Name_To_Id.Insert ("c++", 3);
end GPR2;
