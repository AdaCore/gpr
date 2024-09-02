with Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;

use GPR2;

procedure Main is
   Opt  : GPR2.Options.Object;
   Tree : GPR2.Project.Tree.Object;
begin
   Opt.Add_Switch (Options.P, "tree/demo.gpr");
   if Tree.Load (Opt) then
      for Lang of Tree.Languages loop
         Ada.Text_IO.Put_Line (GPR2.Image (Lang));
      end loop;
   end if;
end Main;
