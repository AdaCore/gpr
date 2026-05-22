with GPR2.Build.Source.Sets;
with GPR2.Project.Tree;
with GPR2.Options;
with GPR2.Path_Name;

with Ada.Text_IO;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;
   use GPR2;
begin
   Opt.Add_Switch (Options.P, "g");
   if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
      Tree.Update_Sources;

      for S of Tree.Root_Project.Sources loop
         Ada.Text_IO.Put_Line (String (S.Path_Name.Simple_Name));
         for CU of S.Units loop
            Ada.Text_IO.Put_Line ("  - compilation unit at" & CU.Index'Image);
            Ada.Text_IO.Put_Line ("    unit name    = " & String (CU.Name));
            Ada.Text_IO.Put_Line ("    kind         = " & CU.Kind'Image);
         end loop;
      end loop;
   end if;
end Main;
