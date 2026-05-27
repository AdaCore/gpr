with Ada.Directories;
with Ada.Environment_Variables;

with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   O : GPR2.Options.Object;
   T : GPR2.Project.Tree.Object;
begin
   Ada.Environment_Variables.Set
     ("GPR_PROJECT_PATH", Ada.Directories.Current_Directory);
   Ada.Directories.Set_Directory ("obj");

   O.Add_Switch (GPR2.Options.P, "p.gpr");
   if not T.Load (O) then
      raise Program_Error;
   end if;
end Main;
