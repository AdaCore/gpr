--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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

   procedure Display (Prj : Project.View.Object);

   procedure Display (Att : Project.Attribute.Object);

   -------------
   -- Display --
   -------------

   procedure Display (Att : Project.Attribute.Object) is
   begin
      Text_IO.Put ("   " & Image (Att.Name.Id.Attr));

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

      for A of Prj.Attributes (With_Defaults => False) loop
         Display (A);
      end loop;

      for Pck of Prj.Packages (With_Defaults => False) loop
         Text_IO.Put_Line (" " & Image (Pck));

         for A of Prj.Attributes (Pack => Pck, With_Defaults => False) loop
            Display (A);
         end loop;
      end loop;
   end Display;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Project.Tree.Load (Prj, Create ("demo.gpr"), Ctx);

   Display (Prj.Root_Project);
end Main;
