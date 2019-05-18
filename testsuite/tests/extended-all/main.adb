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

with GPR2.Context;
with GPR2.Project.View;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object);

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
      use Text_IO;
   begin
      Put (String (Prj.Name) & " ");
      Set_Col (10);
      Put_Line (Prj.Qualifier'Img);
      Put_Line ("Has Extended: " & Boolean'Image (Prj.Is_Extending));
      Put_Line ("Is Extended all: " & Boolean'Image (Prj.Is_Extending_All));
   end Display;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;
begin
   Project.Tree.Load (Prj, Project.Create ("prj2.gpr"), Ctx);
   Display (Prj.Root_Project);
end Main;
