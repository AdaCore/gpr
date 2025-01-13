with Ada.Text_IO; use Ada.Text_IO;

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2; use GPR2;

procedure Main is

   procedure Display_Source (Src : GPR2.Build.Source.Object);
   procedure Test_Prj (Prj_Path : String);

   procedure Display_Source (Src : GPR2.Build.Source.Object) is
   begin
      Put_Line (String (Src.Path_Name.Simple_Name) & ": " & Src.Kind'Image);
   end Display_Source;

   procedure Test_Prj (Prj_Path : String)
   is
      Prj : Project.Tree.Object;
      Opt : Options.Object;

   begin
      Put_Line ("GPR file " & Prj_Path);

      Opt.Add_Switch (Options.P, Prj_Path);
      if not Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         return;
      end if;

      Prj.Update_Sources;

      for V of reverse Prj.Ordered_Views loop
         Put_Line (String (V.Name));
         for S of V.Sources loop
            Display_Source (S);
         end loop;
      end loop;

      Prj.Unload;
      New_Line;
   end Test_Prj;

begin
   Test_Prj ("./prj.gpr");
end Main;
