with Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;

procedure Main is
   use GPR2;
   package PRP renames GPR2.Project.Registry.Pack;

   procedure Test (Project_Name : String) is
      Tree : GPR2.Project.Tree.Object;
      Opt  : Options.Object;
      Res  : Boolean;
   begin
      Ada.Text_IO.Put_Line ("testing " & Project_Name);
      Opt.Add_Switch (Options.P, Project_Name);
      Res := Tree.Load (Opt, Absent_Dir_Error => No_Error);
   end Test;

begin
   PRP.Check_Attributes (PRP.Builder);
   PRP.Check_Attributes (PRP.Naming);
   Test ("gpr/err_unknown_toplevel.gpr");
   Test ("gpr/err_unknown_package.gpr");
   Test ("gpr/err_single_value.gpr");
   Test ("gpr/err_list_value.gpr");
   Test ("gpr/err_unexp_index.gpr");
   Test ("gpr/err_empty_value.gpr");
   Test ("gpr/warn_empty_value.gpr");
   Test ("gpr/err_no_index.gpr");
   Test ("gpr/err_unexp_index.gpr");
   Test ("gpr/err_unexp_others.gpr");
end Main;
