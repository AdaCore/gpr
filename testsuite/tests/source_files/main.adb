with Ada.Environment_Variables;
with Ada.Text_IO;

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;

   procedure Display_Source (Src : GPR2.Build.Source.Object);
   procedure Test_Prj (Fname : String);

   procedure Display_Source (Src : GPR2.Build.Source.Object) is
   begin
      Text_IO.Put_Line (String (Src.Path_Name.Simple_Name) & ": " & Src.Kind'Image);
   end Display_Source;

   procedure Test_Prj (Fname : String)
   is
      Prj : Project.Tree.Object;
      Opt : Options.Object;
      Log : GPR2.Log.Object;

   begin
      Text_IO.Put_Line ("GPR file: " & Fname);
      Opt.Add_Switch (Options.P, Fname);
      if not Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         return;
      end if;

      Prj.Update_Sources (Messages => Log);
      Log.Output_Messages;

      for V of reverse Prj.Ordered_Views loop
         Text_IO.Put_Line (String (V.Name));
         for S of V.Sources loop
            Display_Source (S);
         end loop;
      end loop;

      Prj.Unload;
      Text_IO.New_Line;
   end Test_Prj;

begin
   Test_Prj ("data/prj.gpr");
   Test_Prj ("data/prj1.gpr");
   Ada.Environment_Variables.Set
     ("SOURCE_LIST_FILE_PATH",
      GPR2.Path_Name.Create_File("data/sources_absolute.lst").String_Value);
   Test_Prj ("data/prj1.gpr");
   Test_Prj ("data/prj2.gpr");
   Test_Prj ("data/prj3.gpr");
end Main;
