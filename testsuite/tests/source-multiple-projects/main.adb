with Ada.Text_IO;

with GPR2.Options;
with GPR2.Log;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;

   procedure Check (Project_Name : String);
   --  Do check the given project's sources

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : String) is
      Prj  : Project.Tree.Object;
      Opt  : Options.Object;
      Log  : GPR2.Log.Object;
   begin
      Opt.Add_Switch (Options.P, Project_Name);
      if not Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         return;
      end if;

      Text_IO.Put_Line ("Project: " & String (Prj.Root_Project.Name));

      Prj.Update_Sources (Messages => Log);
      Log.Output_Messages;
   end Check;

begin
   Check ("main.gpr");
   Check ("mainc.gpr");
   Check ("maine.gpr");
   Check ("maina.gpr");
end Main;
