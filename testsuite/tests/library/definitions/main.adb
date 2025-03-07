with Ada.Text_IO;

with GPR2.Context;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;

   procedure Display (Prj : Project.View.Object);

   Prj : Project.Tree.Object;
   Opt : Options.Object;
   Ctx : Context.Object;
   CWD : constant GPR2.Path_Name.Object := Path_Name.Create_Directory (".");

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
      First : Boolean;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Kind'Img);
      Text_IO.Put_Line
        ("Library_Name : " & String (Prj.Library_Name));
      Text_IO.Put_Line
        ("Library_Directory : " & String (Prj.Library_Directory.Name));
      Text_IO.Put_Line
        ("                  : " &
           String (Prj.Library_Directory.Relative_Path (CWD)));
      Text_IO.Put_Line
        ("Library Filename : " &
           String (Prj.Library_Filename.Simple_Name));
      Text_IO.Put_Line
        ("                 : " &
           String (Prj.Library_Filename.Relative_Path (CWD)));
      First := True;
      for Var of Prj.Library_Filename_Variants loop
         if First then
            Text_IO.Put ("Library Variants : ");
            First := False;
         else
            Text_IO.Put ("                   ");
         end if;
         Text_IO.Put_Line (String (Var));
      end loop;
      Text_IO.Put_Line
        ("Library_Standalone : " & Prj.Library_Standalone'Img);
   end Display;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");

   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Display (Prj.Root_Project);
   end if;

   Ctx.Insert ("VERSION", "1.4");

   if Prj.Set_Context (Ctx) then
      Display (Prj.Root_Project);
   end if;

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "demo.gpr");
   Opt.Add_Switch (Options.Subdirs, "release");
   Opt.Add_Switch (Options.X, "VERSION=1.4");

   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Display (Prj.Root_Project);
   end if;

   Ctx.Replace ("VERSION", "A.B");

   if Prj.Set_Context (Ctx) then
      Display (Prj.Root_Project);
   end if;
end Main;
