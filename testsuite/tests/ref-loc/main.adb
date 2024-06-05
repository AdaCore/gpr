with Ada.Text_IO;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   use GPR2;

   procedure Test (Project_Name : String) is
      Opt : GPR2.Options.Object;
      Res : Boolean;
   begin
      Ada.Text_IO.Put_Line ("testing " & Project_Name);
      Opt.Add_Switch (Options.P, Project_Name);
      Res := Tree.Load (Opt);
   end Test;

begin
   Test ("data/var.gpr");
   Test ("data/attr.gpr");
end Main;
