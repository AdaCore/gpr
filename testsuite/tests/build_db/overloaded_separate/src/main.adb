with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is
   Opt : GPR2.Options.Object;
   Tree : GPR2.Project.Tree.Object;
   function Image (Path : GPR2.Path_Name.Object) return String
   is (String (Path.Relative_Path (Tree.Root_Project.Dir_Name)));

begin
   Opt.Add_Switch (GPR2.Options.P, "tree/ext.gpr");
   if not Tree.Load
     (Opt,
      With_Runtime     => True,
      Absent_Dir_Error => GPR2.No_Error)
   then
      Put_Line ("error loading the tree");
      return;
   end if;

   if not Tree.Update_Sources (GPR2.Sources_Units) then
      Put_Line ("could not update the sources");
      return;
   end if;

   for U of Tree.Root_Project.Own_Units loop
      Put_Line ("Name: " & String (U.Name));
      Put_Line ("Spec: " & Image (U.Spec.Source));
      Put_Line ("Body: " & Image (U.Main_Body.Source));
      for S of U.Separates loop
         Put_LIne ("Sep: " & Image (S.Source));
      end loop;
   end loop;

end Main;
