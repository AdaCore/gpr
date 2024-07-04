with Ada.Text_IO;
with GPR2.Build.Compilation_Unit;
with GPR2.Build.Source.Sets;
with GPR2.Options; use GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is
   use GPR2;
   use Ada.Text_IO;

   Tree : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;
   Part : Build.Compilation_Unit.Unit_Location;
   CWD  : constant Path_Name.Object := Path_Name.Create_Directory (".");

   procedure Display
     (Kind : GPR2.Unit_Kind;
      Part : Build.Compilation_Unit.Unit_Location)
   is
   begin
      Put ("   ");
      Put (Kind'Image);
      Put (": ");
      Put (String (Part.Source.Relative_Path (CWD)));
      if Part.Index /= GPR2.No_Index then
         Put (" @");
         Put_Line (Part.Index'Image);
      else
         New_Line;
      end if;
   end Display;

begin
   Opt.Add_Switch (P, "trees/c.gpr");

   if not Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
      return;
   end if;

   if not Tree.Update_Sources then
      return;
   end if;

   Put_Line ("LIST OF UNITS");

   for U of Tree.Root_Project.Units loop
      Put (" - ");
      Put_Line (String (U.Name));
      if U.Has_Part (S_Spec) then
         Display (S_Spec, U.Spec);
      end if;
      if U.Has_Part (S_Body) then
         Display (S_Body, U.Main_Body);
      end if;
   end loop;

   Put_Line ("LIST OF SOURCES");

   for S of Tree.Root_Project.Sources loop
      Put (" - ");
      Put_Line (String (S.Path_Name.Simple_Name));

      for U of S.Units loop
         Put ("   ");

         if U.Index /= No_Index then
            Put ("@" & U.Index'Image & " ");
         end if;

         Put (U.Kind'Image);
         Put (" ");
         Put_Line (String (U.Name));
      end loop;
   end loop;
end Main;
