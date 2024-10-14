with Ada.Text_IO;
with GPR2.Options;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   use GPR2;

   procedure Test (Project_Name : String) is
      Opt : Options.Object;
      Res : Boolean;
   begin
      Ada.Text_IO.Put_Line ("testing " & Project_Name);
      Tree.Unload;
      Opt.Add_Switch (Options.P, Project_Name);
      Res := Tree.Load (Opt);
   end Test;

begin
   Test ("aggr1.gpr");
   Test ("aggr2.gpr");
   Test ("aggr3.gpr");
   Test ("aggr4.gpr");
   Test ("aggr5.gpr");
   Test ("aggr6.gpr");
   Test ("aggr7.gpr");
end Main;
