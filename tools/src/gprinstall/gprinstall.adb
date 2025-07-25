------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2025, AdaCore                     --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Directories;
with Ada.Strings.Hash;

with GNATCOLL.OS.Dir;

package body GPRinstall is

   use GNATCOLL.OS.Dir;
   use GNAT.OS_Lib;

   Delete_Dir : Boolean := True;

   package P_Directory_Map is
     new Ada.Containers.Indefinite_Hashed_Maps
       (String, Boolean, Ada.Strings.Hash, "=");

   Directory_Map : P_Directory_Map.Map;

   procedure Delete_Directory (Root_Dir, Dir : String);
   --  Deletes Dir if it is empty
   --  If so, recursively calls Delete_Directory on containing directories
   --  until it cannot delete (not empty) or it reaches Root_Dir.

   function Process_Dir (Dir : Dir_Handle; Element : Dir_Entry)
                            return Boolean;
   --  Used by GNATCOLL.OS.Dir.Walk. It detects Dir contains a directory and
   --  tags it not to be deleted.

   procedure Process_File (Dir : Dir_Handle; Element : Dir_Entry);
   --  Used by GNATCOLL.OS.Dir.Walk. It detects Dir contains a file and
   --  tags it not to be deleted.

   ----------------------
   -- Delete_Directory --
   ----------------------

   procedure Delete_Directory (Root_Dir, Dir : String) is
   begin
      Delete_Dir := True;

      --  Walk in the directory so detect if it is empty
      Walk
        (Path         => Dir,
         File_Handler => Process_File'Access,
         Dir_Handler  => Process_Dir'Access,
         Max_Depth    => 1);

      if Delete_Dir then
         Ada.Directories.Delete_Directory (Directory => Dir);

         --  if the Map contained the element, tag it "deleted" so we won't
         --  try to delete it again.
         if Directory_Map.Contains (Dir) then
            Directory_Map.Replace
              (Key       => Dir,
               New_Item  => True);
         end if;

         --  Recursively call Delete_Directory until we can't delete the dir
         --  (not empty) or we reach Root_Dir.
         if Dir /= Root_Dir then
            Delete_Directory
              (Root_Dir => Root_Dir,
               Dir      => Ada.Directories.Containing_Directory (Dir));
         end if;
      end if;
   end Delete_Directory;

   ---------------------------------
   -- Delete_Registered_Directory --
   ---------------------------------

   procedure Delete_Registered_Directory (Root_Dir : String) is
      N_Root_Dir : constant String := Normalize_Pathname (Root_Dir);

      procedure Delete (Position : P_Directory_Map.Cursor);

      procedure Delete (Position : P_Directory_Map.Cursor) is
         Dir     : constant String :=
                     P_Directory_Map.Key (Position => Position);
         Deleted : constant Boolean :=
                     P_Directory_Map.Element (Position => Position);
      begin
         --  Only call Delete_Directory if the element was not already
         --  deleted by a previous Delete_Directory recursive call.
         if not Deleted then
            Delete_Directory (Root_Dir => N_Root_Dir, Dir => Dir);
         end if;
      end Delete;
   begin
      --  Iteratively delete all registered directories
      Directory_Map.Iterate (Process => Delete'Access);
   end Delete_Registered_Directory;

   -----------------
   -- Process_Dir --
   -----------------

   function Process_Dir
     (Dir : Dir_Handle; Element : Dir_Entry) return Boolean
   is
      pragma Unreferenced (Dir, Element);
   begin
      Delete_Dir := False;
      return False;
   end Process_Dir;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File (Dir : Dir_Handle; Element : Dir_Entry) is
      pragma Unreferenced (Dir, Element);
   begin
      Delete_Dir := False;
   end Process_File;

   ------------------------
   -- Register_Directory --
   ------------------------

   procedure Register_Directory (Dir : String) is
      N_Dir : constant String := Normalize_Pathname (Dir);
   begin
      --  Only cache the directory if the map does not already contains the
      --  element and if the directory actually exists.
      if not Directory_Map.Contains (N_Dir)
        and then Is_Directory (N_Dir)
      then
         Directory_Map.Insert
           (Key       => N_Dir,
            New_Item  => False);
      end if;
   end Register_Directory;

end GPRinstall;
