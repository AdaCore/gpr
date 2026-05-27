
with Ada.Text_IO;

with GPR2.Log;
with GPR2.Message;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is

   use type GPR2.Message.Level_Value;

   Opt : GPR2.Options.Object;
   Prj : GPR2.Project.Tree.Object;
   Log : GPR2.Log.Object;
   Res : Boolean;

begin
   Opt.Add_Switch (GPR2.Options.P, "prj");
   Res := Prj.Load (Opt);

   Prj.Update_Sources;

   if Prj.Has_Messages then
      for Message of Prj.Log_Messages.all loop
         if Message.Level /= GPR2.Message.Hint then
            Ada.Text_IO.Put_Line (Message.Format);
         end if;
      end loop;
   end if;
end Main;
