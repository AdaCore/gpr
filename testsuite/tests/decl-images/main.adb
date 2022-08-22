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

   procedure Load (Filename : Filename_Type);

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      Text_IO.New_Line;
      for A of Prj.Attributes (With_Defaults => False) loop
         Text_IO.Put_Line (A.Image);
      end loop;

      Text_IO.New_Line;
      for A of Prj.Attributes (With_Defaults => False) loop
         Text_IO.Put_Line (A.Image (15));
      end loop;

      if Prj.Has_Types then
         Text_IO.New_Line;
         for T of Prj.Types loop
            Text_IO.Put_Line (T.Image);
         end loop;
      end if;

      if Prj.Has_Variables then
         Text_IO.New_Line;
         for V of Prj.Variables loop
            Text_IO.Put_Line (V.Image);
         end loop;

         Text_IO.New_Line;
         for V of Prj.Variables loop
            Text_IO.Put_Line (V.Image (5));
         end loop;
      end if;
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
                          Strings.Fixed.Index (Mes, "decl-images");
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

begin
   Load ("demo.gpr");
end Main;
