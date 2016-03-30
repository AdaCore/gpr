------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2016, Free Software Foundation, Inc.            --
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
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Name_Values;
with GPR2.Project.Variable.Set;
with GPR2.Context;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   use all type GPR2.Project.Name_Values.Value_Kind;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
      use GPR2.Project.Attribute.Set.Set;
      use GPR2.Project.Variable.Set.Set;
   begin
      Text_IO.Put (Prj.Name & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Full then
         if Prj.Has_Attributes then
            for A in Prj.Attributes.Iterate loop
               Text_IO.Put ("A:   " & String (Key (A)));
               Text_IO.Put (" ->");

               for V of Element (A).Values loop
                  Text_IO.Put (" " & V);
               end loop;
               Text_IO.New_Line;
            end loop;

            for A in Prj.Attributes.Iterate_Filter ("Object_Dir") loop
               Text_IO.Put ("A2:  " & String (Key (A)));
               Text_IO.Put (" ->");

               for V of Element (A).Values loop
                  Text_IO.Put (" " & V);
               end loop;
               Text_IO.New_Line;
            end loop;

            for A of Prj.Attributes.Filter ("Object_Dir") loop
               Text_IO.Put_Line ("A3:  " & A.Name);
            end loop;

         end if;

         if Prj.Has_Variables then
            for V in Prj.Variables.Iterate loop
               Text_IO.Put ("V:   " & String (Key (V)));
               Text_IO.Put (" ->");

               if Element (V).Kind = Single then
                  Text_IO.Put (" " & String (Element (V).Value));

               else
                  for Val of Element (V).Values loop
                     Text_IO.Put (" " & Val);
                  end loop;
               end if;
               Text_IO.New_Line;
            end loop;
         end if;
      end if;
   end Display;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Prj := Project.Tree.Load (Create ("demo.gpr"));

   Ctx := Prj.Context;
   Ctx.Include ("OS", "Linux");
   Prj.Set_Context (Ctx);

   Display (Prj.Root_Project);
end Main;
