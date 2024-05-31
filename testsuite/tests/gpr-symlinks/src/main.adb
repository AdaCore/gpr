with Ada.Text_IO;

with GPR2.Build.Source.Sets;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   procedure Test (Opt : in out GPR2.Options.Object);

   procedure Test (Opt : in out GPR2.Options.Object) is
      Prj : GPR2.Project.Tree.Object;
   begin
      if not Prj.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
         return;
      end if;

      Prj.Update_Sources;

      for V of Prj.Root_Project.Imports loop
         if not V.Is_Runtime then
            Ada.Text_IO.Put_Line (String (V.Name));
            for Src of V.Sources loop
               Ada.Text_IO.Put (" - ");
               Ada.Text_IO.Put_Line
                 (String (Src.Path_Name.Relative_Path
                    (V.Path_Name.Containing_Directory)));
            end loop;
         end if;
      end loop;

      Prj.Unload;
   end Test;

   Opt : GPR2.Options.Object;
begin
   Opt.Add_Switch (GPR2.Options.P, "project/prj.gpr");
   Ada.Text_IO.Put_Line ("## Loading WITHOUT symlinks support");
   Test (Opt);

   Opt.Add_Switch (GPR2.Options.Resolve_Links);
   Ada.Text_IO.Put_Line ("## Loading WITH symlinks support");
   Test (Opt);
end Main;
