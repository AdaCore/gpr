with Ada.Text_IO;

with GPR2.Build.Source.Sets;
with GPR2.Log;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   procedure Test (Opt : in out GPR2.Options.Object);

   procedure Test (Opt : in out GPR2.Options.Object) is
      Prj : GPR2.Project.Tree.Object;
      Log : GPR2.Log.Object;
   begin
      if not Opt.Load_Project
        (Tree         => Prj,
         With_Runtime => False)
      then
         return;
      end if;

      Prj.Update_Sources (Messages => Log);

      Log.Output_Messages;

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
   Opt.Finalize;
   Ada.Text_IO.Put_Line ("## Loading WITHOUT symlinks support");
   Test (Opt);

   Opt := GPR2.Options.Empty_Options;
   Opt.Add_Switch (GPR2.Options.P, "project/prj.gpr");
   Opt.Add_Switch (GPR2.Options.Resolve_Links);
   Opt.Finalize;
   Ada.Text_IO.Put_Line ("## Loading WITH symlinks support");
   Test (Opt);
end Main;
