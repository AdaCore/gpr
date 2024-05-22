with Ada.Text_IO;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is

   use GPR2;

   Tree    : GPR2.Project.Tree.Object;

   procedure Test (Project_Name : String) is
      Opt : GPR2.Options.Object;
   begin
      Ada.Text_IO.Put_Line ("testing " & Project_Name);
      Tree.Unload;
      Opt.Add_Switch (GPR2.Options.P, String (Project_Name));
      Opt.Finalize;
      if not Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         Ada.Text_IO.Put_Line ("Cannot load project");
      else
         Ada.Text_IO.Put_Line ("OK!");
      end if;
   end Test;

begin
   Test ("good/aggl.gpr");
   Test ("aggl/aggl.gpr");
end Main;
