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

with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Log;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Variable.Set;
with GPR2.Context;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

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

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Full then
         if Prj.Has_Attributes then
            for A of Prj.Attributes loop
               Display (A);
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

         if Prj.Has_Packages then
            for Pck of Prj.Packages loop
               Text_IO.Put_Line (" " & String (Pck.Name));
               for A of Pck.Attributes loop
                  Display (A);
               end loop;
            end loop;
         end if;
      end if;
   end Display;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Project.Tree.Load (Prj, Create ("build.gpr"), Ctx);

   if Prj.Has_Messages then
      Text_IO.Put_Line ("Messages found:");

      for C in Prj.Log_Messages.Iterate (False, True, True, True, True) loop
         Text_IO.Put_Line (Log.Element (C).Format);
      end loop;
   end if;

   Display (Prj.Root_Project);

   Prj.Unload;
   Project.Tree.Load (Prj, Create ("prj.gpr"), Ctx);

   if Prj.Has_Messages then
      Text_IO.Put_Line ("Messages found:");

      for C in Prj.Log_Messages.Iterate (False, True, True, True, True) loop
         Text_IO.Put_Line (Log.Element (C).Format);
      end loop;
   end if;

exception
   when GPR2.Project_Error =>
      if Prj.Has_Messages then
         Text_IO.Put_Line ("Messages found:");

         for C in Prj.Log_Messages.Iterate (False, True, True, True, True) loop
            Text_IO.Put_Line (Log.Element (C).Format);
         end loop;
      end if;
end Main;
