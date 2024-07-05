with Ada.Text_IO;

with GPR2;
with GPR2.Build.Compilation_Unit;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;

   procedure Test (Name : GPR2.Name_Type) is
      CU   : GPR2.Build.Compilation_Unit.Object;
   begin
      Ada.Text_IO.Put_Line ("Lookup compilation unit for " & String (Name));
      CU := Tree.Root_Project.Unit (Name);
      Ada.Text_IO.Put_Line (String (CU.Name));
   end Test;

   use GPR2.Options;
begin
   Opt.Add_Switch (P, "trees/demo.gpr");

   if not Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
      return;
   end if;

   if not Tree.Update_Sources then
      return;
   end if;

   Test ("Pkg.Child");
   Test ("PKG.CHILD.child2");
end Main;
