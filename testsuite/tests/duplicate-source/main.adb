with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Build.Source.Sets;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Check (Project_Name : String);
   --  Do check the given project's sources

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : String) is
      Prj  : Project.Tree.Object;
      Opt  : Options.Object;
      View : Project.View.Object;

   begin
      Opt.Add_Switch (Options.P, Project_Name);
      if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         View := Prj.Root_Project;
         Text_IO.Put_Line ("Project: " & String (View.Name));

         Prj.Update_Sources (GPR2.Sources_Only);

         for Source of View.Sources loop
            Text_IO.Put (" > " & String (Source.Path_Name.Relative_Path (View.Path_Name)));

            Text_IO.Set_Col (20);
            Text_IO.Put ("   language: " & Image (Source.Language));

            Text_IO.Set_Col (37);
            Text_IO.Put ("   Kind: " & Source.Kind'Image);

            if Source.Has_Units then
               Text_IO.Put ("   unit: " & String (Source.Unit.Name));
            end if;

            Text_IO.New_Line;
         end loop;
      end if;
   end Check;

begin
   Check ("prj_c.gpr");
   Check ("prj_c_2.gpr");
   Check ("prj_c_3.gpr");
   Check ("prj_ada.gpr");
   Check ("prj_ada_2.gpr");
   Check ("prj_ada_3.gpr");
end Main;
