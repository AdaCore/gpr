--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Directories;

with GNATCOLL.Hash.xxHash;

pragma Warnings (Off, "* is an internal GNAT unit");
with System.Soft_Links;               use System.Soft_Links;
pragma Warnings (On, "* is an internal GNAT unit");

with GNAT.OS_Lib;

package body GPR2 is

   Is_Multitasking : constant Boolean :=
      System.Soft_Links.Lock_Task /= System.Soft_Links.Task_Lock_NT'Access;

   --  Names are compared and hashed constantly. The runtime's
   --  Less/Equal/Hash_Case_Insensitive fold through
   --  Ada.Strings.Maps.Value, a call per character, so use a table.

   type Fold_Table is array (Character) of Character;

   function Build_Fold_Table return Fold_Table;
   --  Not an aggregate: "others => <>" would leave the entries uninitialized

   function Equal_CI (Left, Right : String) return Boolean;
   function Hash_CI (Key : String) return Ada.Containers.Hash_Type;
   function Less_CI (Left, Right : String) return Boolean;
   --  Ada.Strings.Equal/Hash/Less_Case_Insensitive, folding through Lower

   ---------
   -- "<" --
   ---------

   overriding function "<" (Left, Right : Optional_Name_Type) return Boolean is
   begin
      return Less_CI (String (Left), String (Right));
   end "<";

   overriding function "<" (Left, Right : Filename_Optional) return Boolean is
   begin
      return (if File_Names_Case_Sensitive
              then String (Left) < String (Right)
              else Less_CI (String (Left), String (Right)));
   end "<";

   overriding function "<" (Left, Right : External_Name_Type) return Boolean is
   begin
      return (if File_Names_Case_Sensitive
              then String (Left) < String (Right)
              else Less_CI (String (Left), String (Right)));
   end "<";

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Optional_Name_Type) return Boolean is
   begin
      return Equal_CI (String (Left), String (Right));
   end "=";

   overriding function "=" (Left, Right : Filename_Optional) return Boolean is
   begin
      return (if File_Names_Case_Sensitive
              then String (Left) = String (Right)
              else Equal_CI (String (Left), String (Right)));
   end "=";

   overriding function "=" (Left, Right : External_Name_Type) return Boolean is
   begin
      return (if File_Names_Case_Sensitive
              then String (Left) = String (Right)
              else Equal_CI (String (Left), String (Right)));
   end "=";

   -----------------------
   -- Build_Fold_Table --
   -----------------------

   function Build_Fold_Table return Fold_Table is
      Result : Fold_Table;
   begin
      for C in Character loop
         Result (C) := Ada.Characters.Handling.To_Lower (C);
      end loop;

      return Result;
   end Build_Fold_Table;

   Lower : constant Fold_Table := Build_Fold_Table;

   --------------
   -- Equal_CI --
   --------------

   function Equal_CI (Left, Right : String) return Boolean is
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;

      for J in 0 .. Left'Length - 1 loop
         if Lower (Left (Left'First + J)) /= Lower (Right (Right'First + J))
         then
            return False;
         end if;
      end loop;

      return True;
   end Equal_CI;

   ---------------------------
   -- Get_Executable_Suffix --
   ---------------------------

   function Get_Executable_Suffix return String is
      Exec_Suffix : GNAT.OS_Lib.String_Access
                      := GNAT.OS_Lib.Get_Executable_Suffix;
   begin
      return Result : constant String := Exec_Suffix.all
      do
         GNAT.OS_Lib.Free (Exec_Suffix);
      end return;
   end Get_Executable_Suffix;

   -------------------------
   -- Get_Tools_Directory --
   -------------------------

   function Get_Tools_Directory return String is
      GPRls : constant String := Locate_Exec_On_Path ("gprls");
      --  Check for GPRls executable
   begin
      return (if GPRls = ""
              then ""
              else Directories.Containing_Directory
                     (Directories.Containing_Directory
                        (GNAT.OS_Lib.Normalize_Pathname
                            (GPRls, Resolve_Links => True))));
   end Get_Tools_Directory;

   ----------
   -- Hash --
   ----------

   function Hash (N : Optional_Name_Type) return Ada.Containers.Hash_Type is
   begin
      return Hash_CI (String (N));
   end Hash;

   function Hash (Fname : Filename_Optional) return Ada.Containers.Hash_Type is
   begin
      return (if File_Names_Case_Sensitive
              then Hash (String (Fname))
              else Hash_CI (String (Fname)));
   end Hash;

   function Hash (Name : String) return Ada.Containers.Hash_Type is
   begin
      if Name'Length = 0 then
         return Empty_Hash;
      end if;

      return GNATCOLL.Hash.xxHash.XXH3 (Name);
   end Hash;

   -------------
   -- Hash_CI --
   -------------

   function Hash_CI (Key : String) return Ada.Containers.Hash_Type is
      use GNATCOLL.Hash.xxHash;

      Folded : String (1 .. 256);
      --  Fixed size to say on the stack. Longer keys are folded and
      --  hashed by chunks.

      First  : Positive;
      Len    : Natural;
      Ctx    : XXH3_Context;

   begin
      if Key'Length = 0 then
         return Empty_Hash;
      end if;

      if Key'Length <= Folded'Length then
         for J in 1 .. Key'Length loop
            Folded (J) := Lower (Key (Key'First + (J - 1)));
         end loop;

         return XXH3 (Folded (1 .. Key'Length));
      end if;

      Init_Hash_Context (Ctx);
      First := Key'First;

      loop
         Len := Natural'Min (Folded'Length, Key'Last - First + 1);

         for J in 1 .. Len loop
            Folded (J) := Lower (Key (First + (J - 1)));
         end loop;

         Update_Hash_Context (Ctx, Folded (1 .. Len));

         --  The last index may be Positive'Last: do not advance past it.

         exit when Len = Key'Last - First + 1;
         First := First + Len;
      end loop;

      return Ada.Containers.Hash_Type'Mod (XXH3_Hash'(Hash_Digest (Ctx)));
   end Hash_CI;

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

      --  Note: if we just read the value, the operation is thread-safe.
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

   -------------
   -- Less_CI --
   -------------

   function Less_CI (Left, Right : String) return Boolean is
      Len : constant Natural := Natural'Min (Left'Length, Right'Length);
   begin
      for J in 0 .. Len - 1 loop
         declare
            LC : constant Character := Lower (Left (Left'First + J));
            RC : constant Character := Lower (Right (Right'First + J));
         begin
            if LC /= RC then
               return LC < RC;
            end if;
         end;
      end loop;

      return Left'Length < Right'Length;
   end Less_CI;

   -------------------------
   -- Locate_Exec_On_Path --
   -------------------------

   function Locate_Exec_On_Path (Exec_Name : String) return String is
      use type GNAT.OS_Lib.String_Access;

      Path     : GNAT.OS_Lib.String_Access
                   := GNAT.OS_Lib.Locate_Exec_On_Path (Exec_Name);
      Path_Str : constant String := (if Path = null then "" else Path.all);
   begin
      GNAT.OS_Lib.Free (Path);
      return Path_Str;
   end Locate_Exec_On_Path;

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
      return Quote_With &
        GNATCOLL.Utils.Replace (Str, "" & Quote_With,
                                Quote_With & Quote_With) &
        Quote_With;
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
   -- To_Lower --
   --------------

   function To_Lower (Name : String) return String is
      Result : String (Name'Range);
   begin
      for J in Name'Range loop
         Result (J) := Lower (Name (J));
      end loop;

      return Result;
   end To_Lower;

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
         if Str (Str'First) = ''' then
            return GNATCOLL.Utils.Replace
              (S           => Str (Str'First + 1 .. Str'Last - 1),
               Pattern     => "''",
               Replacement => "'");
         else
            return GNATCOLL.Utils.Replace
              (S           => Str (Str'First + 1 .. Str'Last - 1),
               Pattern     => """""",
               Replacement => """");
         end if;
      else
         return Str;
      end if;
   end Unquote;

begin
   Language_List.Id_To_Name.Append ("ada");
   Language_List.Id_To_Name.Append ("c");
   Language_List.Id_To_Name.Append ("c++");
   Language_List.Id_To_Name.Append ("rust");
   Language_List.Name_To_Id.Insert ("ada",  1);
   Language_List.Name_To_Id.Insert ("c",    2);
   Language_List.Name_To_Id.Insert ("c++",  3);
   Language_List.Name_To_Id.Insert ("rust", 4);
end GPR2;
