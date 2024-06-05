with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   use GPR2;

   procedure Test (Project_Name : String) is
      Tree : GPR2.Project.Tree.Object;
      Opt  : Options.Object;
      Dead : Boolean;
   begin
      Opt.Add_Switch (Options.P, Project_Name);
      Dead := Tree.Load (Opt, Absent_Dir_Error => No_Error);
   end Test;

begin
   Test ("1/prj.gpr");
   Test ("2/prj.gpr");
   Test ("3/prj.gpr");
   Test ("4/prj.gpr");
   Test ("5/prj.gpr");
   Test ("6/prj.gpr");
   Test ("7/prj.gpr");
   Test ("8/prj.gpr");
   Test ("9/prj.gpr");
   Test ("10/prj.gpr");
   Test ("11/prj.gpr");
   Test ("12/prj.gpr");
   Test ("13/prj.gpr");
   Test ("14/prj.gpr");
   Test ("15/prj.gpr");
   Test ("16/prj.gpr");
   Test ("17/prj.gpr");
   Test ("18/prj.gpr");
   Test ("1/prj2.gpr");
   Test ("2/prj2.gpr");
   Test ("3/prj2.gpr");
   Test ("4/prj2.gpr");
   Test ("1/prj4.gpr");
   Test ("2/prj4.gpr");
end Main;
