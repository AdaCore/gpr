------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with Ada.Text_IO;

with GNATCOLL.VFS;

with GPR2.Path_Name;
with GPR2.Path_Name.GNATCOLL;

procedure Main is

   Path : GPR2.Path_Name.Object;

   -------------------------------
   -- To_GNATCOLL_Projects_Test --
   -------------------------------

   procedure To_GNATCOLL_Projects_Test (Path : GPR2.Path_Name.Object);

   procedure To_GNATCOLL_Projects_Test (Path : GPR2.Path_Name.Object) is
      VF : constant GNATCOLL.VFS.Filesystem_String :=
        GPR2.Path_Name.GNATCOLL.To_Filesystem_String
          (GPR2.Path_Name.GNATCOLL.To_Pathname
             (GPR2.Path_Name.GNATCOLL.To_Virtual_File (Path)));
      S  : constant GNATCOLL.VFS.Filesystem_String :=
        GPR2.Path_Name.GNATCOLL.To_Filesystem_String (Path);
   begin
      Ada.Text_IO.Put_Line ("<" & String (VF) & ">");
      Ada.Text_IO.Put_Line ("<" & String (S) & ">");
   end To_GNATCOLL_Projects_Test;

   ----------------------------
   -- To_GPR2_Path_Name_Test --
   ----------------------------

   procedure To_GPR2_Path_Name_Test (VF : GNATCOLL.VFS.Virtual_File);

   procedure To_GPR2_Path_Name_Test (VF : GNATCOLL.VFS.Virtual_File) is
      Path : constant GPR2.Path_Name.Object :=
        GPR2.Path_Name.GNATCOLL.To_Pathname (VF);
   begin
      Ada.Text_IO.Put ("<");
      if Path.Is_Defined then
         if Path.Has_Dir_Name then
            Ada.Text_IO.Put
              (String (Path.Dir_Name) & String (Path.Simple_Name));
         else
            Ada.Text_IO.Put (String (Path.Simple_Name));
         end if;
      else
         Ada.Text_IO.Put ("undefined");
      end if;
      Ada.Text_IO.Put_Line (">");
   end To_GPR2_Path_Name_Test;

   ----------------------------
   -- To_GPR2_Path_Name_Test --
   ----------------------------

   procedure To_GPR2_Path_Name_Test (S : GNATCOLL.VFS.Filesystem_String);

   procedure To_GPR2_Path_Name_Test (S : GNATCOLL.VFS.Filesystem_String) is
      Path : constant GPR2.Path_Name.Object :=
        GPR2.Path_Name.GNATCOLL.To_Pathname (S);
   begin
      Ada.Text_IO.Put ("<");
      if Path.Is_Defined then
         if Path.Has_Dir_Name then
            Ada.Text_IO.Put
              (String (Path.Dir_Name) & String (Path.Simple_Name));
         else
            Ada.Text_IO.Put (String (Path.Simple_Name));
         end if;
      else
         Ada.Text_IO.Put ("undefined");
      end if;
      Ada.Text_IO.Put_Line (">");
   end To_GPR2_Path_Name_Test;

begin
   Ada.Text_IO.Put_Line ("=== testing GNATCOLL.VFS conversion ===");
   To_GNATCOLL_Projects_Test (Path);
   Path := GPR2.Path_Name.Create_File ("gpr2.path.no.res",
                                       GPR2.Path_Name.No_Resolution);
   To_GNATCOLL_Projects_Test (Path);
   Path := GPR2.Path_Name.Create_File ("gpr2.path");
   To_GNATCOLL_Projects_Test (Path);
   Ada.Text_IO.Put_Line ("=== testing GPR2.Path_Name conversion ===");
   To_GPR2_Path_Name_Test ("");
   To_GPR2_Path_Name_Test (GNATCOLL.VFS.Create (""));
   To_GPR2_Path_Name_Test ("gpr2.path.no.res");
   To_GPR2_Path_Name_Test (GNATCOLL.VFS.Create ("gpr2.path.no.res"));
   To_GPR2_Path_Name_Test (GNATCOLL.VFS.Filesystem_String (Path.Value));
   To_GPR2_Path_Name_Test
     (GNATCOLL.VFS.Create (GNATCOLL.VFS.Filesystem_String (Path.Value)));
end Main;
