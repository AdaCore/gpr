------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2016, Free Software Foundation, Inc.            --
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
with Ada.Environment_Variables;

with GNAT.OS_Lib;

package body GPR2 is

   ------------
   -- Create --
   ------------

   function Create (Name : Name_Type) return Path_Name_Type is
      use Ada;
      use GNAT;

      GPR_Name : constant Name_Type :=
                   (if Directories.Extension (Name) = "gpr"
                    then Name
                    else Directories.Compose
                      (Name => Name, Extension => "gpr"));

      use type OS_Lib.String_Access;

   begin
      --  If the file exists or an absolute path has been specificed or there
      --  is no ADA_PROJECT_PATH, just create the Path_Name_Type using the
      --  given Name.

      if OS_Lib.Is_Absolute_Path (GPR_Name)
        or else OS_Lib.Is_Regular_File (GPR_Name)
        or else not Environment_Variables.Exists ("ADA_PROJECT_PATH")
      then
         return Path_Name_Type'
           (As_Is => To_Unbounded_String (GPR_Name),
            Value => To_Unbounded_String
                       (OS_Lib.Normalize_Pathname (GPR_Name)));

      else
         --  Otherwise, let's try to check Name in ADA_PROJECT_PATH

         declare
            File : OS_Lib.String_Access :=
                     OS_Lib.Locate_Regular_File
                       (GPR_Name,
                        Environment_Variables.Value ("ADA_PROJECT_PATH"));
            N    : Unbounded_String;
         begin
            if File = null then
               return Path_Name_Type'
                 (As_Is => To_Unbounded_String (GPR_Name),
                  Value => To_Unbounded_String (GPR_Name));

            else
               N := To_Unbounded_String (File.all);
               OS_Lib.Free (File);

               return Path_Name_Type'
                 (As_Is => N,
                  Value => To_Unbounded_String
                    (OS_Lib.Normalize_Pathname (To_String (N))));
            end if;
         end;
      end if;
   end Create;

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
