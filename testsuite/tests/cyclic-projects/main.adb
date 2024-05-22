with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is

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
   Load ("a.gpr");     --  a -> b -> d -> a
   Load ("agg.gpr");   --  no circularity
   Load ("agg2.gpr");  --  agg2 -> f -> agg2
   Load ("multi.gpr"); --  multiple circularities
end Main;
