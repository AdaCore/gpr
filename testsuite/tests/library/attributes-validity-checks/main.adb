with Ada.Exceptions;
with Ada.Text_IO;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Log;

procedure Main is

   procedure Test (Filename : String) is
      Tree : GPR2.Project.Tree.Object;
      Opt  : GPR2.Options.Object;
      Res  : Boolean;
      use GPR2;
   begin
      Ada.Text_IO.Put_Line (Filename);
      Opt.Add_Switch (Options.P, Filename);
      Res := Tree.Load (Opt);
   end Test;

begin
   Test ("prj1.gpr");
   Test ("abstractprj1.gpr");
   Test ("libprj1.gpr");
   Test ("libprj1_not_standalone.gpr");
   Test ("libprj1_standalone_auto_init_not_supported.gpr");
   Test ("aggrprj1.gpr");
   Test ("aggrlibprj1.gpr");
   Test ("library_name_duplicate/aggregate.gpr");

end main;
