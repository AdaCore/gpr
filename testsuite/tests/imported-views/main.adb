with Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.View;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;

   procedure Check (Project_Name : String);
   --  Do check the given project's sources

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : String) is
      Prj  : Project.Tree.Object;
      Opt  : Options.Object;
      View : Project.View.Object;
   begin
      Opt.Add_Switch (Options.P, Project_Name);
      if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         View := Prj.Root_Project;
         Text_IO.Put_Line ("Project: " & String (View.Name));

         Text_IO.Put_Line ("   imports:");
         for I of View.Imports loop
            Text_IO.Put_Line ("     > " & String (I.Name));
         end loop;

         Text_IO.Put_Line ("   imports recursively:");
         for I of View.Imports (Recursive => True) loop
            if not I.Is_Runtime then
               Text_IO.Put_Line ("     > " & String (I.Name));
            end if;
         end loop;
      end if;

      Prj.Unload;
   end Check;

begin
   Check ("demo1.gpr");
   Check ("demo2.gpr");
   Check ("demo4.gpr");
end Main;
