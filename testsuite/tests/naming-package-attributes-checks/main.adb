with Ada.Text_IO;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   use GPR2;

   procedure Test (Project_Name : String) is
      Tree : GPR2.Project.Tree.Object;
      Opt  : Options.Object;
      Res  : Boolean;
   begin
      Ada.Text_IO.Put_Line ("testing " & String (Project_Name));
      Opt.Add_Switch (Options.P, Project_Name);
      Res := Tree.Load (Opt, Absent_Dir_Error => No_Error);
   end Test;

begin
   Test ("abst.gpr");
   Test ("abst2.gpr");
   Test ("agg.gpr");
   Test ("invalidcasing.gpr");
end Main;
