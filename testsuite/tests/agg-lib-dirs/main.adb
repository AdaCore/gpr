with Ada.Text_IO;
with GPR2.Context;
with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   use GPR2;

   procedure Test (Project_Name : String) is
      Opt : GPR2.Options.Object;
   begin
      Ada.Text_IO.Put_Line ("testing " & Project_Name);
      Tree.Unload;
      Opt.Add_Switch (GPR2.Options.P, String (Project_Name));
      Opt.Finalize;
      if not Tree.Load (Opt) then
         Ada.Text_IO.Put_Line ("Cannot load project");
      end if;
   end Test;

begin
   Test ("sa_lib3.gpr");
   Test ("sa_lib4.gpr");
end Main;
