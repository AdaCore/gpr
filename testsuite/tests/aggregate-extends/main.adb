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
with GPR2.Project.Variable.Set;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object);

   procedure Changed_Callback (Prj : Project.View.Object);

   ----------------------
   -- Changed_Callback --
   ----------------------

   procedure Changed_Callback (Prj : Project.View.Object) is
   begin
      Text_IO.Put_Line (">>> Changed_Callback for " & String (Prj.Name));
   end Changed_Callback;

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
   begin
      Text_IO.Put ('[' & String (Prj.Name) & "] ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Prj.Has_Attributes then
         for A in Prj.Attributes.Iterate loop
            Text_IO.Put
              ("A:   " & String (Attribute.Set.Element (A).Name.Text));
            Text_IO.Put (" ->");

            for V of Element (A).Values loop
               Text_IO.Put (" " & V.Text);
            end loop;
            Text_IO.New_Line;
         end loop;
      end if;

      if Prj.Has_Variables then
         for V in Prj.Variables.Iterate loop
            Text_IO.Put ("V:   " & String (Key (V)));
            Text_IO.Put (" -> ");
            Text_IO.Put (Element (V).Value.Text);
            Text_IO.New_Line;
         end loop;
      end if;
      Text_IO.New_Line;
   end Display;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Ctx.Include ("KVAL", "one");

   Project.Tree.Load (Prj, Create ("agg.gpr"), Ctx);

   for P of Prj loop
      Display (P);
   end loop;
end Main;
