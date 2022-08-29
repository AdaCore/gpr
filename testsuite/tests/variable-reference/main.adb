--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Context;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Tree;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object);
   procedure Display (Att : Project.Attribute.Object);
   procedure Load (Filename : Filename_Type);

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
      use GPR2.Project.Variable.Set.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      for A of Prj.Attributes (With_Defaults => False) loop
         Display (A);
      end loop;

      if Prj.Has_Variables then
         for V in Prj.Variables.Iterate loop
            Text_IO.Put ("V:   " & String (Key (V)));
            Text_IO.Put (" -> ");
            Text_IO.Put (Element (V).Value.Text);
            Text_IO.New_Line;
         end loop;
      end if;

      for Pck of Prj.Packages (With_Defaults => False) loop
         Text_IO.Put_Line (" " & Image (Pck));

         for A of Prj.Attributes (Pack => Pck, With_Defaults => False) loop
            Display (A);
         end loop;
      end loop;

      Text_IO.New_Line;
   end Display;

   ----------
   -- Load --
   ----------

   procedure Load (Filename : Filename_Type) is
      Prj : Project.Tree.Object;
      Ctx : Context.Object;
   begin
      Project.Tree.Load (Prj, Create (Filename), Ctx);
      Display (Prj.Root_Project);

   exception
      when GPR2.Project_Error =>
         if Prj.Has_Messages then
            Text_IO.Put_Line ("Messages found for " & String (Filename));

            for M of Prj.Log_Messages.all loop
               declare
                  Mes : constant String := M.Format;
                  L   : constant Natural :=
                          Strings.Fixed.Index (Mes, "aggregate-dup");
               begin
                  if L /= 0 then
                     Text_IO.Put_Line (Mes (L - 1 .. Mes'Last));
                  else
                     Text_IO.Put_Line (Mes);
                  end if;
               end;
            end loop;

            Text_IO.New_Line;
         end if;
   end Load;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Load ("demo.gpr");
   Load ("demo1.gpr");
end Main;
