--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Unit;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;
with GPR2.Source;

with GPR2.Source_Info.Parser.Ada_Language;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
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

      if Prj.Has_Variables then
         for V in Prj.Variables.Iterate loop
            Text_IO.Put ("V:   " & String (Key (V)));
            Text_IO.Put (" -> ");
            Text_IO.Put (Element (V).Value.Text);
            Text_IO.New_Line;
         end loop;
      end if;

      for Source of Prj.Sources loop
         declare
            U : constant Optional_Name_Type := Source.Unit_Name;
         begin
            Output_Filename (Source.Path_Name.Value);

            Text_IO.Set_Col (16);
            Text_IO.Put ("   language: " & Image (Source.Language));

            Text_IO.Set_Col (33);
            Text_IO.Put ("   Kind: "
                         & GPR2.Unit.Library_Unit_Type'Image (Source.Kind));

            if U /= "" then
               Text_IO.Put ("   unit: " & String (U));
            end if;

            Text_IO.New_Line;
         end;
      end loop;
   end Display;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      I : constant Positive :=
            Strings.Fixed.Index (Filename, "extended-definitions2");
   begin
      Text_IO.Put (" > " & Filename (I + 22 .. Filename'Last));
   end Output_Filename;

   Prj1, Prj2 : Project.Tree.Object;
   Ctx        : Context.Object;
begin
   Project.Tree.Load (Prj1, Project.Create ("prj1.gpr"), Ctx);
   Project.Tree.Load (Prj2, Project.Create ("prj2.gpr"), Ctx);

   Text_IO.Put_Line ("**************** Iterator Prj1");

   for P of Prj1 loop
      Display (P, Full => False);
   end loop;

   Text_IO.Put_Line ("**************** Iterator Prj2");

   for C in Prj2.Iterate
     (Kind => (I_Project | I_Imported | I_Recursive => True, others => False))
   loop
      Display (Project.Tree.Element (C), Full => False);
   end loop;
end Main;
