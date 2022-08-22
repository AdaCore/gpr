--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;

with GNATCOLL.VFS;

with GPR2.Path_Name;

procedure Main is

   Path : GPR2.Path_Name.Object;

   -------------------------------
   -- To_GNATCOLL_Projects_Test --
   -------------------------------

   procedure To_GNATCOLL_Projects_Test (Path : GPR2.Path_Name.Object);

   procedure To_GNATCOLL_Projects_Test (Path : GPR2.Path_Name.Object) is
      VF : constant GNATCOLL.VFS.Virtual_File :=
              GPR2.Path_Name.Virtual_File (Path);
      VFS : constant String := VF.Display_Full_Name;
      S  : constant GNATCOLL.VFS.Filesystem_String :=
        GPR2.Path_Name.Filesystem_String (Path);
   begin
      Ada.Text_IO.Put_Line ("<" & String (VFS) & ">");
      Ada.Text_IO.Put_Line ("<" & String (S) & ">");
   end To_GNATCOLL_Projects_Test;

   ----------------------------
   -- To_GPR2_Path_Name_Test --
   ----------------------------

   procedure To_GPR2_Path_Name_Test (VF : GNATCOLL.VFS.Virtual_File);

   procedure To_GPR2_Path_Name_Test (VF : GNATCOLL.VFS.Virtual_File) is
      Path : constant GPR2.Path_Name.Object :=
        GPR2.Path_Name.Create (VF);
   begin
      if Path.Is_Defined then
         Ada.Text_IO.Put ("<");
         if Path.Is_Directory then
            Ada.Text_IO.Put (String (Path.Dir_Name));
         elsif Path.Has_Dir_Name then
            Ada.Text_IO.Put
              (String (Path.Dir_Name) & String (Path.Simple_Name));
         else
            Ada.Text_IO.Put (String (Path.Simple_Name));
         end if;
         Ada.Text_IO.Put (">");
         if Path.Is_Directory then
            Ada.Text_IO.Put (" is a directory");
         end if;
         Ada.Text_IO.Put_Line ("");
      else
         Ada.Text_IO.Put_Line ("<undefined>");
      end if;
   end To_GPR2_Path_Name_Test;

   ----------------------------
   -- To_GPR2_Path_Name_Test --
   ----------------------------

   procedure To_GPR2_Path_Name_Test (S : GNATCOLL.VFS.Filesystem_String);

   procedure To_GPR2_Path_Name_Test (S : GNATCOLL.VFS.Filesystem_String) is
      Path : constant GPR2.Path_Name.Object :=
        GPR2.Path_Name.Create (S);
   begin
      if Path.Is_Defined then
         Ada.Text_IO.Put ("<");
         if Path.Is_Directory then
            Ada.Text_IO.Put (String (Path.Dir_Name));
         elsif Path.Has_Dir_Name then
            Ada.Text_IO.Put
              (String (Path.Dir_Name) & String (Path.Simple_Name));
         else
            Ada.Text_IO.Put (String (Path.Simple_Name));
         end if;
         Ada.Text_IO.Put (">");
         if Path.Is_Directory then
            Ada.Text_IO.Put (" is a directory");
         end if;
         Ada.Text_IO.Put_Line ("");
      else
         Ada.Text_IO.Put_Line ("<undefined>");
      end if;
   end To_GPR2_Path_Name_Test;

begin
   Ada.Text_IO.Put_Line ("=== testing GNATCOLL.VFS conversion ===");
   To_GNATCOLL_Projects_Test (Path);
   Path := GPR2.Path_Name.Create_Directory ("gpr2.path.directory");
   To_GNATCOLL_Projects_Test (Path);
   Path := GPR2.Path_Name.Create_File ("gpr2.path.no.res",
                                       GPR2.Path_Name.No_Resolution);
   To_GNATCOLL_Projects_Test (Path);
   Path := GPR2.Path_Name.Create_File ("gpr2.path");
   To_GNATCOLL_Projects_Test (Path);
   Ada.Text_IO.Put_Line ("=== testing GPR2.Path_Name conversion ===");
   To_GPR2_Path_Name_Test ("");
   To_GPR2_Path_Name_Test (GNATCOLL.VFS.Create (""));
   To_GPR2_Path_Name_Test ("gpr2.path.directory/");
   To_GPR2_Path_Name_Test (GNATCOLL.VFS.Create ("gpr2.path.directory/"));
   To_GPR2_Path_Name_Test ("gpr2.path.no.res");
   To_GPR2_Path_Name_Test (GNATCOLL.VFS.Create ("gpr2.path.no.res"));
   To_GPR2_Path_Name_Test (GNATCOLL.VFS.Filesystem_String (Path.Value));
   To_GPR2_Path_Name_Test
     (GNATCOLL.VFS.Create (GNATCOLL.VFS.Filesystem_String (Path.Value)));
end Main;
