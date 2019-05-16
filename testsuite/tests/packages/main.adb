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
with GPR2.Project.Attribute.Set;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object);

   procedure Display (Att : Project.Attribute.Object);

   -------------
   -- Display --
   -------------

   procedure Display (Att : Project.Attribute.Object) is
   begin
      Text_IO.Put ("   " & String (Att.Name.Text));

      if Att.Has_Index then
         Text_IO.Put (" (" & Att.Index.Text & ")");
      end if;

      Text_IO.Put (" ->");

      for V of Att.Values loop
         Text_IO.Put (" " & V.Text);
      end loop;
      Text_IO.New_Line;
   end Display;

   procedure Display (Prj : Project.View.Object) is
      use GPR2.Project.Attribute.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Kind'Img);

      if Prj.Has_Attributes then
         for A of Prj.Attributes loop
            Display (A);
         end loop;
      end if;

      if Prj.Has_Packages then
         for Pck of Prj.Packages loop
            Text_IO.Put_Line (" " & String (Pck.Name));

            for A of Pck.Attributes loop
               Display (A);
            end loop;
         end loop;
      end if;
   end Display;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Project.Tree.Load (Prj, Create ("demo.gpr"), Ctx);
   Display (Prj.Root_Project);
end Main;
