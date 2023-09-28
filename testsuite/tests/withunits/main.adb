--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Source.Sets;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Source_Reference.Identifier;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Check (Project_Name : Filename_Type);
   --  Do check the given project's sources

   procedure Output_Filename (Filename : Path_Name.Object; V : View.Object);
   --  Remove the leading tmp directory

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : Filename_Type) is
      Prj  : Project.Tree.Object;
      Ctx  : Context.Object;
      View : Project.View.Object;
      Log  : GPR2.Log.Object;
   begin
      Project.Tree.Load_Autoconf
        (Prj,
         Create (Project_Name),
         Ctx);
      Prj.Update_Sources (Sources_Units_Dependencies, Log);

      View := Prj.Root_Project;
      Text_IO.Put_Line ("Project: " & String (View.Name));

      for Source of View.Sources loop
         declare
            U  : constant Optional_Name_Type :=
                   Build.Source.Full_Name (Source.Unit);
            CU : constant Build.Compilation_Unit.Object :=
                   View.Unit (Source.Unit.Name);
         begin
            Text_IO.New_Line;
            Output_Filename (Source.Path_Name, View);

            if U /= "" then
               Text_IO.Put ("   unit: " & String (U));
               Text_IO.New_Line;

               for Dep of Source.Unit.Dependencies loop
                  Text_IO.Put_Line ("   " & String (Dep));
               end loop;
            end if;
         end;
      end loop;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Object; V : View.Object) is
   begin
      Text_IO.Put (" > " & String (Filename.Relative_Path (V.Dir_Name)));
   end Output_Filename;

begin
   Check ("demo.gpr");
end Main;
