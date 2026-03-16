with Ada.Text_IO;

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is
   use GPR2;

   Tree : Project.Tree.Object;
   Opt  : GPR2.Options.Object;
begin

   Opt.Add_Switch (Options.P, "tree/p.gpr");

   if not Tree.Load (Opt, Absent_Dir_Error => No_Error, With_Runtime => False)
   then
      Ada.Text_IO.Put_Line ("Failed to load the tree");

      return;
   end if;

   Tree.Update_Sources (Matching_Units => False);

   for S of Tree.Root_Project.Sources loop
      Ada.Text_IO.Put_Line
        (String (S.Path_Name.Simple_Name) & " - " & String (S.Unit.Name));
   end loop;

   Tree.Unload;
end Main;
