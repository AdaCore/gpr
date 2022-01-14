------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

--  This package provides API to deal with GPR2.Path_Name.Object /
--  GNATCOLL.VFS.Filesystem_String & GNATCOLL.VFS.Virtual_File conversion.

with GNATCOLL;  use GNATCOLL;
with GNATCOLL.VFS;

package GPR2.Path_Name.GNATCOLL is

   function To_Filesystem_String
     (Path : GPR2.Path_Name.Object) return VFS.Filesystem_String;
   --  GPR2.Path_Name.Object to GNATCOLL.VFS.Filesystem_String conversion

   function To_Virtual_File
     (Path : GPR2.Path_Name.Object) return VFS.Virtual_File is
     (if Path.Is_Defined
      then VFS.Create (To_Filesystem_String (Path))
      else VFS.No_File);
   --  GPR2.Path_Name.Object to GNATCOLL.VFS.Virtual_File conversion

   function To_Pathname (Filename : VFS.Filesystem_String)
                         return GPR2.Path_Name.Object is
     (if Filename'Length > 1
      then GPR2.Path_Name.Create
        (GPR2.Filename_Type (Filename), GPR2.Filename_Type (Filename))
      else GPR2.Path_Name.Undefined);
   --  GNATCOLL.VFS.Filesystem_String to GPR2.Path_Name.Object conversion

   function To_Pathname (File : VFS.Virtual_File)
                         return GPR2.Path_Name.Object is
     (if VFS."/=" (File, VFS.No_File)
      then GPR2.Path_Name.Create (GPR2.Filename_Type (File.Display_Full_Name),
        GPR2.Filename_Type (File.Display_Full_Name))
      else GPR2.Path_Name.Undefined);
   --  GNATCOLL.VFS.Virtual_File to GPR2.Path_Name.Object to conversion

private

   -------------------------------
   -- Remove_Trailing_Separator --
   -------------------------------

   function Remove_Trailing_Separator (Path : String) return String is
     (if Path'Length > 0 and then Path (Path'Last) in '/' | '\'
      then Path (Path'First .. Path'Last - 1)
      else Path);

   --------------------------
   -- To_Filesystem_String --
   --------------------------

   function To_Filesystem_String
     (Path : GPR2.Path_Name.Object) return VFS.Filesystem_String is
     (if Path.Is_Defined
      then (if Path.Has_Dir_Name
        then (if Path.Is_Directory
          then VFS.Filesystem_String
            (Remove_Trailing_Separator (Dir_Name (Path)))
          else VFS.Filesystem_String
            (String (Dir_Name (Path)) & String (Path.Simple_Name)))
        else VFS.Filesystem_String (Path.Simple_Name))
      else ""
     );

end GPR2.Path_Name.GNATCOLL;
