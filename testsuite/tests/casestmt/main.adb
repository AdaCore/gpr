--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Context;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Context;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
      use GPR2.Project.Attribute.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Full then
         for A of Prj.Attributes (With_Defaults => False) loop
            Text_IO.Put
              ("   " & Image (A.Name.Id.Attr));
            Text_IO.Put (" ->");

            for V of A.Values loop
               Text_IO.Put (" " & V.Text);
            end loop;
            Text_IO.New_Line;
         end loop;
      end if;
   end Display;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Ctx.Include ("OS", "Linux");
   Project.Tree.Load (Prj, Create ("demo.gpr"), Ctx);

   Display (Prj.Root_Project);

   Ctx.Clear;
   Ctx.Include ("OS", "Windows");
   Prj.Set_Context (Ctx);
   Display (Prj.Root_Project);

exception
   when GPR2.Project_Error =>
      if Prj.Has_Messages then
         Text_IO.Put_Line ("Messages found:");

         for M of Prj.Log_Messages.all loop
            Text_IO.Put_Line (M.Format);
         end loop;
      end if;
end Main;
