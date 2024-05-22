with Ada.Text_IO;
with GPR2.Project.Tree;
with GPR2.Build;
with GPR2.Build.Compilation_Unit;
pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Log;
with GPR2.Path_Name;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;
   Log  : GPR2.Log.Object;

begin
   Opt.Add_Switch (GPR2.Options.P, "default.gpr");
   if Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
      Tree.Update_Sources (Messages => Log);
      Log.Output_Messages (Information => False);

      for Source of Tree.Root_Project.Sources loop
         Ada.Text_IO.Put_Line (String (Source.Path_Name.Name));
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
