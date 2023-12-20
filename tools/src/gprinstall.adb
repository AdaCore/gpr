------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2023, AdaCore                     --
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

with Ada.Directories;

with GNATCOLL.OS.Dir;

package body GPRinstall is

   use GNATCOLL.OS.Dir;

   Delete_Main_Dir : Boolean := True;
   Delete_Dir      : Boolean := True;

   function Process_Dir (Dir : Dir_Handle; Element : Dir_Entry)
                         return Boolean;

   procedure Process_File (Dir : Dir_Handle; Element : Dir_Entry);

   ----------------------------
   -- Delete_Empty_Directory --
   ----------------------------

   procedure Delete_Install_Directory (Root_Dir : String) is
   begin
      --  Recursively delete directory inside the install dir
      Walk
        (Path         => Root_Dir,
         File_Handler => Process_File'Access,
         Dir_Handler  => Process_Dir'Access,
         Max_Depth    => 1);

      --  Launch a last walk to delete the install root directory
      Delete_Main_Dir := True;

      Walk
        (Path         => Root_Dir,
         File_Handler => Process_File'Access,
         Dir_Handler  => Process_Dir'Access,
         Max_Depth    => 1);

      if Delete_Main_Dir then
         Ada.Directories.Delete_Directory (Directory => Root_Dir);
      end if;
   end Delete_Install_Directory;

   function Process_Dir (Dir : Dir_Handle; Element : Dir_Entry) return Boolean
   is
      Dir_Path : constant String := Path (Dir) & "/" & Name (Element);
   begin
      Delete_Main_Dir := False;
      Delete_Dir      := True;

      Walk
        (Path         => Dir_Path,
         File_Handler => Process_File'Access,
         Dir_Handler  => Process_Dir'Access,
         Max_Depth    => 1);

      if Delete_Dir then
         Ada.Directories.Delete_Directory (Directory => Dir_Path);
      end if;

      return True;
   end Process_Dir;

   procedure Process_File (Dir : Dir_Handle; Element : Dir_Entry) is
      pragma Unreferenced (Dir, Element);
   begin
      Delete_Main_Dir := False;
      Delete_Dir      := False;
   end Process_File;

end GPRinstall;
