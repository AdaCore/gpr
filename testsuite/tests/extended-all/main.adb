--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;

with GPR2.Context;
with GPR2.Project.View;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object);

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
      use Text_IO;
   begin
      Put (String (Prj.Name) & " ");
      Set_Col (10);
      Put_Line (Prj.Qualifier'Img);
      Put_Line ("Has Extended: " & Boolean'Image (Prj.Is_Extending));
      Put_Line ("Is Extended all: " & Boolean'Image (Prj.Is_Extending_All));
   end Display;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;
begin
   Project.Tree.Load (Prj, Project.Create ("prj2.gpr"), Ctx);
   Display (Prj.Root_Project);
end Main;
