with Ada.Text_IO;

with GPR2.Build.Source.Sets;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   Options : GPR2.Options.Object;
   Tree    : GPR2.Project.Tree.Object;

begin
   Options.Add_Switch (GPR2.Options.P, "test/test.gpr");

   if Tree.Load (Options) then
      null;
   end if;

   if Tree.Root_Project.View_Db.Is_Defined then
      --  This test requires that `View_DB` is undefined

      raise Program_Error;
   end if;

   for Source of Tree.Root_Project.Sources loop
      Ada.Text_IO.Put_Line (String (Source.Path_Name.Simple_Name));
   end loop;
end Main;
