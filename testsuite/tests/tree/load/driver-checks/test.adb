with Ada.Text_IO; use Ada.Text_IO;

with GPR2; use GPR2;
with GPR2.Options;
with GPR2.Project.Tree;
with Ada.Command_Line;

procedure Test is
	Options      : GPR2.Options.Object;
	Project_File : constant String := "demo.gpr";
   Config_File  : constant String := Ada.Command_Line.Argument (1);
	Project      : GPR2.Project.Tree.Object;
   New_Lang    : Language_Id;


begin
	Options.Add_Switch (GPR2.Options.P, Project_File);
	Options.Add_Switch (GPR2.Options.Config, Config_File);

	if not Project.Load (Options, With_Runtime => True) then
		Put_Line ("Loading failure");
		return;
	end if;
end Test;
