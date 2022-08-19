--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Fixed;

with GPR2.Context;
with GPR2.Log;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Variable.Set;

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

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Full then
         for A of Prj.Attributes (With_Defaults => False,
                                  With_Config   => False)
         loop
            Text_IO.Put
              ("A:   " & Image (A.Name.Id.Attr));
            Text_IO.Put (" ->");

            for V of A.Values loop
               Text_IO.Put (" " & V.Text);
            end loop;
            Text_IO.New_Line;
         end loop;

         if Prj.Has_Variables then
            for V in Prj.Variables.Iterate loop
               Text_IO.Put ("V:   " & String (Key (V)));
               Text_IO.Put (" -> ");
               Text_IO.Put (String (Element (V).Value.Text));
               Text_IO.New_Line;
            end loop;
         end if;

         for Pck of Prj.Packages (With_Defaults => False,
                                  With_Config   => False) loop
            Text_IO.Put_Line (" " & Image (Pck));

            for A of Prj.Attributes (Pack => Pck,
                                     With_Defaults => False,
                                     With_Config   => False)
            loop
               Display (A);
            end loop;
         end loop;
      end if;
   end Display;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Ctx.Include ("OS", "Linux");
   Project.Tree.Load (Prj, Create ("demo.gpr"), Ctx);
   Project.Tree.Load_Configuration (Prj, Create ("config.cgpr"));

   Display (Prj.Root_Project);

exception
   when GPR2.Project_Error =>
      if Prj.Has_Messages then
         Text_IO.Put_Line ("Messages found:");

         for M of Prj.Log_Messages.all loop
            declare
               F : constant String := M.Sloc.Filename;
               I : constant Natural := Strings.Fixed.Index (F, "/config");
            begin
               Text_IO.Put_Line ("> " & F (I .. F'Last));
               Text_IO.Put_Line (M.Level'Img);
               Text_IO.Put_Line (M.Format);
            end;
         end loop;
      end if;
end Main;
