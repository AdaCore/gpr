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
      Res : Boolean;
   begin
      Opt.Add_Switch (Options.P, Filename);
      Res := Prj.Load (Opt, Absent_Dir_Error => No_Error);
   end Load;

begin
   Load ("demo.gpr");
end Main;
