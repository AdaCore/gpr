with Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;

   procedure Load (Filename : String);

   ----------
   -- Load --
   ----------

   procedure Load (Filename : String) is
      Prj : Project.Tree.Object;
      Opt : Options.Object;
   begin
      Opt.Add_Switch (Options.P, Filename);
      if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         Text_IO.Put_Line ("All good, no message.");
      end if;
   end Load;

begin
   Load ("demo.gpr");
   Load ("demo2.gpr");
   Load ("demo3.gpr");
end Main;
