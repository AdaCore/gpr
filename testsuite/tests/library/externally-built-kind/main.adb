with Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use GPR2;

   Tree : Project.Tree.Object;
   Opt  : Options.Object;

begin
   Opt.Add_Switch (Options.P, "root.gpr");

   if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
      for View of Tree loop
         Ada.Text_IO.Put_Line
           (String (View.Name) & ": " & Image (View.Kind));
      end loop;
   end if;
end Main;
