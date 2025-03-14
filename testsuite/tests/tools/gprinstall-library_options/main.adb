with Ada.Text_IO;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Reporter.Console;

procedure Main is
   use Ada;
   use GPR2;

   Tree : GPR2.Project.Tree.Object;
   Opts : GPR2.Options.Object;

begin
   Opts.Add_Switch (Options.P, "inst/share/gpr/lib1");
   Tree.Set_Reporter (GPR2.Reporter.Console.Create (GPR2.Reporter.Quiet));
   if not Tree.Load (Opts) then
      Text_IO.Put_Line ("could not load the project");
   end if;

   if not Tree.Root_Project.Has_Attribute
     (Name => GPR2.Project.Registry.Attribute.Linker.Linker_Options)
   then
      Text_IO.Put_Line ("OK");
   else
      Text_IO.Put_Line ("Library_Options should be empty in installed project:");
      declare
         Library_Options : GPR2.Containers.Source_Value_List :=
           Tree.Root_Project.Attribute
             (Name => GPR2.Project.Registry.Attribute.Linker.Linker_Options).Values;
      begin
         for Library_Option of Library_Options loop
            Text_IO.Put_Line ("   - " & Library_Option.Text);
         end loop;
      end;
   end if;
end Main;
