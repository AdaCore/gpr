with Ada.Text_IO;
with GPR2.Options;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.Variable;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   use GPR2;

   procedure Print_Variable
     (Variable : GPR2.Project.Variable.Object;
      Pack     : GPR2.Package_Id := GPR2.Project_Level_Scope) is
      use GPR2.Project.Registry.Attribute;
      use type GPR2.Package_Id;
   begin
      if Pack /= GPR2.Project_Level_Scope then
         Ada.Text_IO.Put (GPR2.Image (Pack) & '.');
      end if;

      Ada.Text_IO.Put(String (Variable.Name.Text) & ":" & Variable.Kind'Img & "=");
      if Variable.Kind = GPR2.Project.Registry.Attribute.Single then
         Ada.Text_IO.Put_Line (Variable.Value.Text);
      else
         for V of Variable.Values loop
            Ada.Text_IO.Put (V.Text & ";");
         end loop;
         Ada.Text_IO.Put_Line ("");
      end if;
   end Print_Variable;

   procedure Test (Project : String) is
      Opt  : Options.Object;
   begin
      Ada.Text_IO.Put_Line (String (Project) & ".gpr:");
      Opt.Add_Switch (Options.P, Project);
      if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         for V of Tree.Root_Project.Variables loop
            Print_Variable (V);
         end loop;

         for Pack of Tree.Root_Project.Packages loop
            for V of Tree.Root_Project.Variables (Pack) loop
               Print_Variable (V, Pack);
            end loop;
         end loop;
      end if;
   end Test;

begin
   Test ("prj");
   Test ("prj1");
   Test ("prj2");
   Test ("prj3");
   Test ("prj4");
   Test ("prj5");
   Test ("prj6");
end Main;
