with Ada.Text_IO; use Ada.Text_IO;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   O : GPR2.Options.Object;
   T : GPR2.Project.Tree.Object;
begin
   O.Add_Switch (GPR2.Options.P, "prj/prj.gpr");

   if not T.Load (O) then
      raise Program_Error;
   end if;

   if not T.Update_Sources then
      raise Program_Error;
   end if;

   for Root of T.Namespace_Root_Projects loop
      for U of Root.Units loop
         Put_Line ("U : " & String (U.Name));
      end loop;
   end loop;
end Main;
