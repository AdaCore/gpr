with Ada.Exceptions;
with Ada.Text_IO;

with GPR2.Build.Source.Sets;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;

with Test;

procedure Main is
   Tree       : GPR2.Project.Tree.Object;
   Opt        : GPR2.Options.Object;
   Main_Found : Integer := 0;
   Test_Found : Integer := 0;

   use GPR2;

begin
   Opt.Add_Switch (Options.P, "test");

   if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
      Tree.Update_Sources;

      for Prj of Tree loop
         for S of Prj.Sources loop
            if S.Path_Name.Simple_Name = "main.adb" then
               Main_Found := Main_Found + 1;
            elsif S.Path_Name.Simple_Name = "test.ads" then
               Test_Found := Test_Found + 1;
            elsif S.Path_Name.Simple_Name = "ignored.ads" then
               Ada.Text_IO.Put_Line
                 ("unexpected " & String (S.Path_Name.Value) & " found");
            end if;
         end loop;
      end loop;
      if Main_Found /= 1 then
         Ada.Text_IO.Put_Line ("main.adb found" & Main_Found'Img & " times");
      end if;
      if Test_Found /= 1 then
         Ada.Text_IO.Put_Line ("test.ads found" & Test_Found'Img & " times");
      end if;
   end if;
end Main;
