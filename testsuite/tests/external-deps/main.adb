------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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
with Ada.Directories;

with GPR2.Context;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Variable.Set;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   package PRA renames GPR2.Project.Registry.Attribute;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Ctx.Include ("OS", "Linux");
   Project.Tree.Load (Prj, Create ("demo.gpr"), Ctx);

   Text_IO.Put_Line
     (Prj.Root_Project.Attribute (PRA.Object_Dir).Value.Text);

   Ctx.Include ("OS", "Windows");
   Prj.Set_Context (Ctx);

   Text_IO.Put_Line
     (Prj.Root_Project.Attribute (PRA.Object_Dir).Value.Text);

exception
   when GPR2.Project_Error =>
      if Prj.Has_Messages then
         Text_IO.Put_Line ("Messages found:");

         for M of Prj.Log_Messages.all loop
            Text_IO.Put_Line (M.Format);
         end loop;
      end if;
end Main;
