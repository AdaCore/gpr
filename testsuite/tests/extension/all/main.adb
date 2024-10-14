with Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.View;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object);

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
      use Text_IO;
   begin
      Put (String (Prj.Name) & " ");
      Set_Col (10);
      Put_Line (Prj.Qualifier'Img);
      Put_Line ("Has Extended: " & Boolean'Image (Prj.Is_Extending));
      Put_Line ("Is Extended all: " & Boolean'Image (Prj.Is_Extending_All));
   end Display;

   Prj : Project.Tree.Object;
   Opt : Options.Object;
begin
   Opt.Add_Switch (Options.P, "prj2.gpr");
   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Display (Prj.Root_Project);
   end if;
end Main;
