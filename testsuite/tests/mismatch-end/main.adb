with Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;

   Projects : constant array (1 .. 2) of String (1 .. 1) := ("a", "b");

begin
   for P of Projects loop
      declare
         Prj : Project.Tree.Object;
         Opt : Options.Object;

      begin
         Opt.Add_Switch (Options.P, P);
         if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
            Text_IO.Put_Line ("All good, no message.");
         end if;
      end;
   end loop;
end Main;
