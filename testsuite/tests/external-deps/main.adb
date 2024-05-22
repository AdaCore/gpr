with Ada.Text_IO;
with Ada.Directories;

with GPR2.Context;
with GPR2.Options;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Variable.Set;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   package PRA renames GPR2.Project.Registry.Attribute;

   Prj : Project.Tree.Object;
   Opt : Options.Object;
   Ctx : Context.Object;

begin
   Opt.Add_Switch (Options.P, "demo");
   Opt.Add_Switch (Options.X, "OS=Linux");
   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Text_IO.Put_Line (Prj.Root_Project.Attribute (PRA.Object_Dir).Value.Text);

      Ctx.Include ("OS", "Windows");
      Prj.Set_Context (Ctx);

      Text_IO.Put_Line (Prj.Root_Project.Attribute (PRA.Object_Dir).Value.Text);
   end if;
end Main;
