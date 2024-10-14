with Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.View;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;

   procedure Display (Prj : Project.View.Object);

   Prj : Project.Tree.Object;
   Opt : Options.Object;

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);
      Text_IO.Put_Line (Image (Prj.Kind));
   end Display;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");
   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      for P of Prj loop
         Display (P);
      end loop;
   end if;
   Prj.Unload;

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "demo2.gpr");
   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      null;
   end if;
end Main;
