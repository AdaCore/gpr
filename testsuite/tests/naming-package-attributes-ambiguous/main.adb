with Ada.Text_IO;

with GPR2.Options;
with GPR2.Message;
with GPR2.Build.Source;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;

   Prj : Project.Tree.Object;
   Opt : Options.Object;

   procedure Display_Source (Name : Simple_Name);

   procedure Display_Source (Name : Simple_Name) is
      Src : GPR2.Build.Source.Object;
   begin
      if Prj.Root_Project.Has_Source (Name) then
         Src := Prj.Root_Project.Source (Name);
         Text_IO.Put_Line (String (Name) & ": " & Src.Kind'Image);
      else
         Text_IO.Put_Line ("no such source: " & String (Name));
      end if;
   end Display_Source;

begin
   Opt.Add_Switch (Options.P, "./data/prj.gpr");
   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Prj.Update_Sources;
      Display_Source ("pkg.a");
      Display_Source ("pkg_b.a");
      Display_Source ("pkg-execute_s_b.a");
   end if;
end Main;
