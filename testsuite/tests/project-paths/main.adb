with Ada.Text_IO;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;

   ----------
   -- Load --
   ----------

   procedure Load (Tree : in out Project.Tree.Object;
                   Opt  : in out Options.Object)
   is
      Res : Boolean;
   begin
      Opt.Add_Switch (Options.P, "demo.gpr");
      Res := Tree.Load (Opt, Absent_Dir_Error => No_Error);
   end Load;

   Prj1, Prj2, Prj3  : Project.Tree.Object;
   Opt1, Opt2, Opt3  : Options.Object;

begin
   --  Prj1

   Text_IO.Put_Line ("=== Prj1");
   Load (Prj1, Opt1);

   --  Prj2

   Text_IO.Put_Line ("=== Prj2");
   Opt2.Add_Switch (Options.AP, "one");
   Load (Prj2, Opt2);

   --  Prj3

   Text_IO.Put_Line ("=== Prj3");
   Opt3.Add_Switch (Options.AP, "one");
   Opt3.Add_Switch (Options.AP, "two");
   Load (Prj3, Opt3);
end Main;
