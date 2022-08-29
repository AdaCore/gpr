--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Source;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

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

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
   begin
      Text_IO.Put ("Project: " & String (Prj.Name) & " ");
      Text_IO.Set_Col (20);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      for Pck of Prj.Packages (With_Defaults => False) loop
         Text_IO.Put_Line (" " & Image (Pck));
         for A of Prj.Attributes (Pack => Pck, With_Defaults => False) loop
            Display (A);
         end loop;
      end loop;
   end Display;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      I : constant Positive := Strings.Fixed.Index (Filename, "extended/");
   begin
      Text_IO.Put (" > " & Filename (I + 8 .. Filename'Last));
   end Output_Filename;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Project.Tree.Load (Prj, Project.Create ("src/a.gpr"), Ctx);

   for P of Prj loop
      Display (P, Full => False);
   end loop;
end Main;
