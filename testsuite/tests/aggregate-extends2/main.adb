------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2017, Free Software Foundation, Inc.            --
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

   procedure Display (Att : Project.Attribute.Object);

   procedure Display (Var : Project.Variable.Object);

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

   procedure Display (Att : Project.Attribute.Object) is
   begin
      Text_IO.Put ("   " & String (Att.Name));

      if Att.Has_Index then
         Text_IO.Put (" (" & Att.Index & ")");
      end if;

      Text_IO.Put (" ->");

      for V of Att.Values loop
         Text_IO.Put (" " & V);
      end loop;
      Text_IO.New_Line;
   end Display;

   procedure Display (Var : Project.Variable.Object) is
   begin
      Text_IO.Put ("   " & String (Var.Name) & " =");
      for V of Var.Values loop
         Text_IO.Put (" " & V);
      end loop;
      Text_IO.New_Line;
   end Display;

   procedure Display (Prj : Project.View.Object) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
   begin
      Text_IO.Put ('[' & String (Prj.Name) & "] ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Prj.Has_Attributes then
         for A of Prj.Attributes loop
            Display (A);
         end loop;
      end if;

      if Prj.Has_Variables then
         for V of Prj.Variables loop
            Display (V);
         end loop;
      end if;

      if Prj.Has_Packages then
         for Pck of Prj.Packages loop
            Text_IO.Put_Line ("   Pck:   " & String (Pck.Name));
            for A of Pck.Attributes loop
               Text_IO.Put ("   ");
               Display (A);
            end loop;

            if Pck.Has_Variables then
               for Var of Prj.Variables loop
                  Text_IO.Put ("   ");
                  Display (Var);
               end loop;
            end if;
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
