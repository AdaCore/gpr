with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GPR2.Build.Source.Sets;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   Options : GPR2.Options.Object;
   Tree    : GPR2.Project.Tree.Object;
begin
   Options.Add_Switch (GPR2.Options.P, "tree/prj.gpr");
   if not Tree.Load (Options) or else not Tree.Update_Sources then
      raise Program_Error;
   end if;

   for S of Tree.Root_Project.Sources loop
      Put_Line (String (S.Path_Name.Value));
   end loop;
end Main;
