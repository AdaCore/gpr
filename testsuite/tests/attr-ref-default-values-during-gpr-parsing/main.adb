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

   procedure Print_Variable (Variable : GPR2.Project.Variable.Object) is
      use GPR2.Project.Registry.Attribute;
   begin
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

   procedure Test (Prj : String) is
      Opt  : GPR2.Options.Object;
   begin
      Ada.Text_IO.Put_Line (Prj & ".gpr:");
      Opt.Add_Switch (Options.P, Prj);
      if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         for V of Tree.Root_Project.Variables loop
            Print_Variable (V);
         end loop;
      end if;
      Tree.Unload;
   end Test;

begin
   Test ("prj");
   Test ("prj2");
   Test ("prj3");
   Test ("prj4");
end Main;
