with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Reporter.Log;

procedure Main is
   Tree   : GPR2.Project.Tree.Object;
   Opt    : GPR2.Options.Object;
   Loaded : Boolean;
begin
   Opt.Add_Switch (GPR2.Options.P, "demo.gpr");
   Opt.Add_Switch (GPR2.Options.Config, "config.cgpr");

   Loaded := Tree.Load
     (Opt,
      With_Runtime => True,
      Reporter     => GPR2.Reporter.Log.Create);
   Put_Line ("Loaded: " & Loaded'Image);
   Put_Line ("Has runtime: " & Tree.Has_Runtime_Project'Image);

   for Msg of Tree.Log_Messages.all loop
      if Msg.Message = "runtime directory ""dir"" does not exist" then
         Put_Line (Msg.Format);
      end if;
   end loop;
end Main;
