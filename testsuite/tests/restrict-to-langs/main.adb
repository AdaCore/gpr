with GPR2;
with GPR2.Project.Tree;
with GPR2.Containers;
with GPR2.Options;

with Ada.Text_IO;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;
   Res  : Boolean;

begin
   Opt.Add_Switch (GPR2.Options.P, "prj1.gpr");

   Ada.Text_IO.Put_Line ("Restrict to existing language:");
   Tree.Restrict_Autoconf_To_Languages
     (GPR2.Containers.Language_Id_Set.To_Set (GPR2.Ada_Language));
   Res := Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error);
   Tree.Unload;

   Ada.Text_IO.Put_Line ("Restrict to missing language:");
   Tree.Restrict_Autoconf_To_Languages
     (GPR2.Containers.Language_Id_Set.To_Set (GPR2.CPP_Language));
   Res := Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error);
   Tree.Unload;

end Main;
