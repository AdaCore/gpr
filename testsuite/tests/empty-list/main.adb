--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Variable.Set;
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
      use GPR2.Project.Variable.Set.Set;
      use type GPR2.Project.Registry.Attribute.Value_Kind;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Full then
         for A in Prj.Attributes (With_Defaults => False).Iterate loop
            Text_IO.Put
              ("A:   " & Image (Attribute.Set.Element (A).Name.Id.Attr));
            Text_IO.Put (" ->");

            if not Element (A).Values.Is_Empty then
               for V of Element (A).Values loop
                  Text_IO.Put (" " & V.Text);
               end loop;
            end if;
            Text_IO.New_Line;
         end loop;

         if Prj.Has_Variables then
            for V in Prj.Variables.Iterate loop
               Text_IO.Put ("V:   " & String (Key (V)));
               Text_IO.Put (" -> ");
               if Element (V).Kind = Registry.Attribute.Single then
                  Text_IO.Put (Element (V).Value.Text);
               elsif not Element (V).Values.Is_Empty then
                  for Va of Element (V).Values loop
                     Text_IO.Put (" " & Va.Text);
                  end loop;
               end if;
               Text_IO.New_Line;
            end loop;
         end if;
         Text_IO.New_Line;
      end if;
   end Display;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Project.Tree.Load (Prj, Create ("common.gpr"), Ctx);

   for P of Prj loop
      Display (P, Full => True);
   end loop;
exception
   when GPR2.Project_Error =>
      if Prj.Has_Messages then
         Text_IO.Put_Line ("Messages found:");

         for M of Prj.Log_Messages.all loop
            declare
               Mes : constant String := M.Format;
               L   : constant Natural :=
                       Strings.Fixed.Index (Mes, "/common");
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
