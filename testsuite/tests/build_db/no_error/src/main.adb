with Ada.Text_IO;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;
begin

   Opt.Add_Switch (GPR2.Options.P, "tree/prj.gpr");

   if not Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
      return;
   end if;

   Ada.Text_IO.Put_Line ("regular update:");

   if Tree.Update_Sources then
      Ada.Text_IO.Put_Line ("!! should have reported an issue");
   else
      Ada.Text_IO.Put_Line ("  issue correctly detected.");
   end if;

   Tree.Clear_Sources;

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("no_error option is now set:");

   if Tree.Update_Sources (No_Error => True) then
      Ada.Text_IO.Put_Line ("  no issue detected.");
   else
      Ada.Text_IO.Put_Line ("!! should not have reported an issue");
   end if;
end Main;
