with Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;

   Prj : Project.Tree.Object;
   Opt : Options.Object;

begin
   Opt.Add_Switch (Options.P, "projects/demo.gpr");

   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      for V of Prj loop
         Text_IO.Put_Line (Item => "Closure from " & String (V.Name));
         for V_Closure of V.Closure loop
            Text_IO.Put_Line (Item => "   " & String (V_Closure.Name));
         end loop;
      end loop;
   end if;
end Main;
