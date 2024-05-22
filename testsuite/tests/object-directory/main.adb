with Ada.Text_IO;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use GPR2;

   CWD : constant GPR2.Path_Name.Object := Path_Name.Create_Directory (".");

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
      use Ada;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);
      Text_IO.Put_Line (String (Prj.Object_Directory.Relative_Path (CWD)));
   end Display;

   ----------
   -- Test --
   ----------

   procedure Test (Gpr : String; Subdirs : String := "") is
      Tree : Project.Tree.Object;
      Opt  : Options.Object;

   begin
      Opt.Add_Switch (Options.P, Gpr);

      if Subdirs'Length > 0 then
         Opt.Add_Switch (Options.Subdirs, Subdirs);
      end if;

      if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         Display (Tree.Root_Project);
      end if;
   end Test;

begin
   Test ("demo1.gpr");
   Test ("demo2.gpr");
   Test ("demo2.gpr", "debug");
end Main;
