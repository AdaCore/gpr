------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2018, Free Software Foundation, Inc.            --
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
with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Load (Tree : in out Project.Tree.Object);

   ----------
   -- Load --
   ----------

   procedure Load (Tree : in out Project.Tree.Object) is
      Ctx : Context.Object;
   begin
      Project.Tree.Load (Tree, Create ("demo.gpr"), Ctx);
   exception
      when GPR2.Project_Error =>
         if Tree.Has_Messages then
            Text_IO.Put_Line ("Messages found:");

            for M of Tree.Log_Messages.all loop
               declare
                  Mes : constant String := M.Format;
                  L   : constant Natural :=
                          Strings.Fixed.Index (Mes, "/project-paths");
               begin
                  if L /= 0 then
                     Text_IO.Put_Line (Mes (L .. Mes'Last));
                  else
                     Text_IO.Put_Line (Mes);
                  end if;
               end;
            end loop;
         end if;
   end Load;

   Prj1, Prj2, Prj3 : Project.Tree.Object;

begin
   --  Prj1

   Text_IO.Put_Line ("=== Prj1");
   Load (Prj1);

   --  Prj2

   Text_IO.Put_Line ("=== Prj2");
   Prj2.Register_Project_Search_Path
     (Path_Name.Create_Directory
        (Name_Type (Directories.Current_Directory & "/one")));
   Load (Prj2);

   --  Prj3

   Text_IO.Put_Line ("=== Prj3");
   Prj3.Register_Project_Search_Path
     (Path_Name.Create_Directory
        (Name_Type (Directories.Current_Directory & "/one")));
   Prj3.Register_Project_Search_Path
     (Path_Name.Create_Directory
        (Name_Type (Directories.Current_Directory & "/two")));
   Load (Prj3);
end Main;
