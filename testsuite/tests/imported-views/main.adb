--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Source.Set;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Source;
with GPR2.Project.View.Set;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Check (Project_Name : Filename_Type);
   --  Do check the given project's sources

   function Filter_Filename (Filename : Path_Name.Object) return String;
   --  Remove the leading tmp directory

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : Filename_Type) is
      Prj  : Project.Tree.Object;
      Ctx  : Context.Object;
      View : Project.View.Object;
   begin
      Project.Tree.Load (Prj, Create (Project_Name), Ctx);

      View := Prj.Root_Project;
      Text_IO.Put_Line ("Project: " & String (View.Name));

      Text_IO.Put_Line ("   imports:");
      for I of View.Imports loop
         Text_IO.Put_Line ("     > " & Filter_Filename (I.Path_Name));
      end loop;

      Text_IO.Put_Line ("   imports recursively:");
      for I of View.Imports (Recursive => True) loop
         Text_IO.Put_Line ("     > " & Filter_Filename (I.Path_Name));
      end loop;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   function Filter_Filename (Filename : Path_Name.Object) return String is
      F : constant Path_Name.Full_Name := Filename.Value;
      D : constant String := "imported-views";
      I : constant Positive := Strings.Fixed.Index (F, D);
   begin
      return F (I + D'Length .. F'Last);
   end Filter_Filename;

begin
   Check ("demo1.gpr");
   Check ("demo2.gpr");
end Main;
