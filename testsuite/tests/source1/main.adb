with Ada.Strings.Fixed;
with Ada.Text_IO;

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;

   procedure Check (Project_Name : String);
   --  Do check the given project's sources

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : String) is
      Prj  : Project.Tree.Object;
      Opt  : Options.Object;
      View : Project.View.Object;
      Log  : GPR2.Log.Object;
   begin
      Opt.Add_Switch (Options.P, Project_Name);
      if not Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         return;
      end if;

      View := Prj.Root_Project;
      Text_IO.Put_Line ("Project: " & String (View.Name));

      Prj.Update_Sources (Messages => Log);
      Log.Output_Messages;

      for Source of View.Sources loop
         Output_Filename (Source.Path_Name.Value);

         Text_IO.Set_Col (20);
         Text_IO.Put ("   language: " & Image (Source.Language));

         Text_IO.Set_Col (37);
         Text_IO.Put ("   Kind: " & Source.Kind'Image);

         if Source.Has_Units then
            Text_IO.Put ("   unit: " & String (Source.Unit.Name));
         end if;

         Text_IO.New_Line;
      end loop;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      S : constant String := String (Filename);
      Test : constant String := "source1";
      I : constant Positive := Strings.Fixed.Index (S, Test);
   begin
      Text_IO.Put (" > " & S (I + Test'Length + 1 .. S'Last));
   end Output_Filename;

begin
   Check ("demo1.gpr");
   Check ("demo2.gpr");
end Main;
