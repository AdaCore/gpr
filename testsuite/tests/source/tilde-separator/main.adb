with Ada.Text_IO;
with GPR2.Build.Compilation_Unit;
with GPR2.Build;
pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Reporter.Console;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;

begin
   Opt.Add_Switch (GPR2.Options.P, "tree/default.gpr");

   if Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
      Tree.Set_Reporter (GPR2.Reporter.Console.Create (GPR2.Reporter.Verbose));
      Tree.Update_Sources;

      for Source of Tree.Root_Project.Sources loop
         Ada.Text_IO.Put_Line (String (Source.Path_Name.Simple_Name));
      end loop;

      for Unit of Tree.Root_Project.Units loop
         Ada.Text_IO.Put_Line (String (Unit.Name));
         if Unit.Has_Part (GPR2.S_Separate) then
            for S in Unit.Separates.Iterate loop
               Ada.Text_IO.Put_Line
                 (String (Unit.Name) & "." &
                  String (GPR2.Build.Compilation_Unit.Separate_Maps.Key (S)));
            end loop;
         end if;
      end loop;
   end if;
end Main;
