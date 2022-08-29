--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with Ada.Exceptions;

with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Context;
with GPR2.Message;
with GPR2.Log;

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
      Text_IO.Put ("   " & String (Image (Att.Name.Id.Attr)));

      if Att.Has_Index then
         Text_IO.Put (" (" & Att.Index.Value & ")");
      end if;

      Text_IO.Put (" -> ");

      for V of Att.Values loop
         Text_IO.Put (V.Text & " ");
      end loop;
      Text_IO.New_Line;
   end Display;

   procedure Display (Prj : Project.View.Object) is
      use GPR2.Project.Attribute.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Kind'Img);

      for A of Prj.Attributes loop
         Display (A);
      end loop;

      for Pck of Prj.Packages loop
         Text_IO.Put_Line (" " & Image (Pck));

         for A of Prj.Attributes (Pck) loop
            Display (A);
         end loop;
      end loop;

      Text_IO.New_Line;
   end Display;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

   procedure Print_Messages is
   begin
      if Prj.Has_Messages then
         for C in Prj.Log_Messages.Iterate
           (False, True, True, True, True)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
      end if;
   end Print_Messages;

begin
   Project.Tree.Load
     (Self => Prj, Filename => Create ("demo.gpr"), Context => Ctx);
   Display (Prj.Root_Project);

exception
   when Ex : others =>
      Text_IO.Put_Line (Ada.Exceptions.Exception_Message (Ex));
      Print_Messages;

end Main;
