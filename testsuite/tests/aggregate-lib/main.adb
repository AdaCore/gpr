--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object);

   procedure Changed_Callback (Prj : Project.View.Object);

   procedure Output_Filename (Filename : Path_Name.Full_Name);

   ----------------------
   -- Changed_Callback --
   ----------------------

   procedure Changed_Callback (Prj : Project.View.Object) is
   begin
      Text_IO.Put_Line (">>> Changed_Callback for " & String (Prj.Name));
   end Changed_Callback;

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

      for A in Prj.Attributes (With_Defaults => False).Iterate loop
         Text_IO.Put
           ("A:   " & Image (Attribute.Set.Element (A).Name.Id.Attr));
         Text_IO.Put (" ->");

         for V of Element (A).Values loop
            Text_IO.Put (" " & V.Text);
         end loop;
         Text_IO.New_Line;
      end loop;
   end Display;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      I : constant Positive := Strings.Fixed.Index (Filename, "aggregate-lib");
   begin
      Text_IO.Put_Line (" > " & Filename (I + 13 .. Filename'Last));
   end Output_Filename;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Text_IO.Put_Line ("//// OS set to Linux");
   Ctx.Include ("OS", "Linux");

   Project.Tree.Load (Prj, Create ("demo.gpr"), Ctx);

   for P of Prj loop
      Display (P);
   end loop;

   Text_IO.Put_Line ("sources:");
   for S of Prj.Root_Project.Sources loop
      Output_Filename (S.Path_Name.Value);
   end loop;

   Text_IO.New_Line;
   Text_IO.Put_Line ("//// OS set to Windows");
   Ctx := Prj.Context;
   Ctx.Include ("OS", "Windows");
   Prj.Set_Context (Ctx, Changed_Callback'Access);

   for P of Prj loop
      Display (P);
   end loop;

   Text_IO.Put_Line ("sources:");
   for S of Prj.Root_Project.Sources loop
      Output_Filename (S.Path_Name.Value);
   end loop;
end Main;
