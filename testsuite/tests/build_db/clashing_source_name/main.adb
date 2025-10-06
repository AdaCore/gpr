with Ada.Text_IO;
with GPR2;

with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is

   use GPR2;
   use Ada.Text_IO;

   ----------
   -- Load --
   ----------

   procedure Load (Tree : in out Project.Tree.Object;
                   Opt  : in out Options.Object)
   is
      Res : Boolean;
   begin
      Opt.Add_Switch (Options.P, "prj/prj.gpr");
      Res := Tree.Load (Opt, Absent_Dir_Error => No_Error);
   end Load;

   Prj : Project.Tree.Object;
   Opt : Options.Object;
begin
   Load (Prj, Opt);
   Prj.Update_Sources;
end Main;
