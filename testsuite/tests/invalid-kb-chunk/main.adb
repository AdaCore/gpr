with GPR2; use GPR2;
with GPR2.Options;
with GPR2.Project.Tree;

with Ada.Text_IO;

procedure Main is
   PT   : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;

begin
   Opt.Add_Switch (Options.P, "a");
   Opt.Add_Switch (Options.Db, "./kb");
   if PT.Load (Opt) then
      Ada.Text_IO.Put_Line ("Invalid KB chunk ignored");
   end if;
end Main;
