with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Options;
with GPR2.Build.Source.Sets;
with GPR2.Project.Tree;

procedure Main is
   O : GPR2.Options.Object;
   T : GPR2.Project.Tree.Object;
   N : Natural := 0;
begin
   O.Add_Switch (GPR2.Options.P, "tree/root.gpr");
   if not T.Load
     (O, With_Runtime => True, Artifacts_Info_Level => GPR2.Sources_Units)
   then
      raise Program_Error;
   end if;

   Put_Line ("## Runtime Sources ##");
   New_Line;
   N := 0;
   for S of T.Runtime_Project.Sources loop
      Put_Line (String (S.Path_Name.Value) & " -> " & String
         (S.Owning_View.Name));
      N := N + 1;
      exit when N = 2;
   end loop;
   New_Line;

   Put_Line ("## Root Visible_Sources ##");
   New_Line;
   N := 0;
   for S of T.Root_Project.Visible_Sources loop
      Put_Line (String (S.Path_Name.Value) & " -> " & String
         (S.Owning_View.Name));
      N := N + 1;
      exit when N = 2;
   end loop;
end Main;
