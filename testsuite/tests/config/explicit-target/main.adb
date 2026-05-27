with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Containers;
with GPR2.Context;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

pragma Warnings (Off);
with GPR2.Reporter;
with GPR2.Reporter.Console;
with System.OS_Constants;
pragma Warnings (On);

procedure Main is
   use Ada;

   use GPR2;
   use GPR2.Containers;

   Project_Tree : Project.Tree.Object;
   Opt          : Options.Object;
   procedure Load_Project_And_Check_Target
     (Project_File, Target_Switch, Config_File : String := "") is
   begin
      Opt := Options.Empty_Options;
      Opt.Add_Switch (Options.P, Project_File);

      if Target_Switch /= "" then
         Opt.Add_Switch (Options.Target, Target_Switch);
      end if;

      if Config_File /= "" then
         Opt.Add_Switch (Options.Config, Config_File);
      end if;

      if not Project_Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         Put_Line ("Failed to load project");
         return;
      end if;

      Put_Line ("Loaded " & Project_File);
      if Target_Switch /= "" then
         Put_Line ("  - target switch: " & Target_Switch);
      end if;

      if Config_File /= "" then
         Put_Line ("  - config file: " & Config_File);
      end if;

      Put_Line
        ("  - Has an explicit target: "
         & Project_Tree.Has_Explicit_Target'Image);
      Project_Tree.Unload;

   end Load_Project_And_Check_Target;
begin
   Load_Project_And_Check_Target ("projects/target_in_project_file.gpr");
   Load_Project_And_Check_Target
     ("projects/target_in_project_file.gpr", "linux-fake-target");
   Load_Project_And_Check_Target
     ("projects/no_target.gpr", "linux-fake-target");
   Load_Project_And_Check_Target
     ("projects/no_target.gpr", "", "linux-fake-target.cgpr");
   Load_Project_And_Check_Target ("projects/no_target.gpr");
end Main;
