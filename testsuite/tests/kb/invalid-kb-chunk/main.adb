with GPR2; use GPR2;
with GPR2.Message;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Reporter.Console; use GPR2.Reporter;

with Ada.Text_IO;

procedure Main is
   PT   : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;
   use type GPR2.Message.Level_Value;

begin
   Opt.Add_Switch (Options.P, "a");
   Opt.Add_Switch (Options.Db, "./kb");

   if PT.Load (Opt, Reporter => Console.Create (Quiet)) then
      Ada.Text_IO.Put_Line ("Invalid KB chunk ignored");
   else
      for M of PT.Configuration.Log_Messages loop
         if M.Level = GPR2.Message.Error then
            --  Ignore line/column to have the output not dependent on
            --  the actual autoconf project that depends on the host/kb
            Ada.Text_IO.Put_Line
              (String (GPR2.Path_Name.Simple_Name (M.Sloc.Filename))
               & ": " & M.Message);
         end if;
      end loop;
   end if;
end Main;
