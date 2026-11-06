with Ada.Text_IO; use Ada.Text_IO;

with GPR2;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Test is
   procedure Check (Mode : String; Severity : GPR2.Error_Level);

   procedure Check (Mode : String; Severity : GPR2.Error_Level) is
      Tree    : GPR2.Project.Tree.Object;
      Options : GPR2.Options.Object;
      Loaded  : Boolean with Unreferenced;
   begin
      Put_Line (Mode & " / " & Severity'Image);
      Options.Add_Switch (GPR2.Options.P, "lib.gpr");
      Options.Add_Switch (GPR2.Options.X, "ALI_MODE=" & Mode);
      Loaded := Tree.Load
        (Options,
         Absent_Dir_Error => Severity,
         Check_Drivers    => False);
   end Check;

begin
   for Severity in GPR2.Warning .. GPR2.Error loop
      Check ("default", Severity);
      Check ("same", Severity);
      Check ("different", Severity);
   end loop;
end Test;
