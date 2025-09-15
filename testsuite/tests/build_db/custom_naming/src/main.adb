with Ada.Command_Line;
with Ada.Text_IO;

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is
   use GPR2;

   procedure Test (Gpr : String)
   is
      Tree        : Project.Tree.Object;
      Opt         : GPR2.Options.Object;

   begin
      Ada.Text_IO.Put_Line ("=========================================");
      Ada.Text_IO.Put_Line ("Testing " & String (Gpr));
      Ada.Text_IO.Put_Line ("=========================================");

      Opt.Add_Switch (Options.P, Gpr);

      if not Tree.Load
        (Opt,
         Absent_Dir_Error => No_Error,
         With_Runtime     => False)
      then
         Ada.Text_IO.Put_Line ("Failed to load the tree");

         return;
      end if;

      Tree.Update_Sources;

      Ada.Text_IO.Put_Line ("* Sources:");

      for S of Tree.Root_Project.Sources loop
         Ada.Text_IO.Put_Line
           (String (S.Path_Name.Simple_Name) & " - " &
              GPR2.Image (S.Language) & " " &
              S.Kind'Image);
      end loop;

      Tree.Unload;
   end Test;

begin
   if Ada.Command_Line.Argument_Count = 0 then
      Test ("tree/prj.gpr");
   else
      Test (Ada.Command_Line.Argument (1));
   end if;

end Main;
