with Ada.Text_IO;
with GPR2.Log;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   use GPR2;

   procedure Test (Project_Name : String) is
      Tree : Project.Tree.Object;
      Opt  : Options.Object;
      Log  : GPR2.Log.Object;
   begin
      Ada.Text_IO.Put_Line ("testing " & Project_Name);
      Opt.Add_Switch (Options.P, Project_Name);
      if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         Tree.Update_Sources (Messages => Log);
         Log.Output_Messages (Information => False);
      end if;
   end Test;

begin
   Test ("files/test.gpr");
end Main;
