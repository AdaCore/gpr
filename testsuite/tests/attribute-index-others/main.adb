--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Log;
with GPR2.Message;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Variable.Set;
with GPR2.Context;

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
      Text_IO.Put ("   " & Image (Att.Name.Id.Attr));

      if Att.Has_Index then
         Text_IO.Put (" (" & Att.Index.Text & ")");
      end if;

      Text_IO.Put (" ->");

      for V of Att.Values loop
         Text_IO.Put (" " & V.Text);
      end loop;

      if Att.Has_Index and then Att.Index.Is_Others then
         Text_IO.Put ("  - this is the others index");
      else
         Text_IO.Put ("  - this is the source 'others'");
      end if;
      Text_IO.New_Line;
      Text_IO.Put_Line (Att.Image);
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

exception
   when GPR2.Project_Error =>
      if Prj.Has_Messages then
         Text_IO.Put_Line ("Messages found:");

         for C in Prj.Log_Messages.Iterate
           (False, False, True, True, True)
         loop
            declare
               M   : constant Message.Object := Log.Element (C);
               Mes : constant String := M.Format;
               L   : constant Natural :=
                       Strings.Fixed.Index (Mes, "/attribute-index-others");
            begin
               if L /= 0 then
                  Text_IO.Put_Line (Mes (L .. Mes'Last));
               else
                  Text_IO.Put_Line (Mes);
               end if;
            end;
         end loop;
      end if;
end Main;
