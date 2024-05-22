with Ada.Strings.Fixed;
with Ada.Text_IO;

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;
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
      Log  : GPR2.Log.Object;
   begin
      Opt.Add_Switch (Options.P, Project_Name);
      if not Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         return;
      end if;

      Text_IO.Put_Line ("Project: " & String (Prj.Root_Project.Name));
      Prj.Update_Sources (Messages => Log);
      Log.Output_Messages;

      for Source of Prj.Root_Project.Sources loop
         declare
            U : constant Optional_Name_Type := Source.Unit.Full_Name;
         begin
            Output_Filename (Source.Path_Name.Value);

            Text_IO.Set_Col (20);
            Text_IO.Put ("   language: " & Image (Source.Language));

            Text_IO.Set_Col (36);
            Text_IO.Put ("   Kind: " & Source.Kind'Image);

            if U /= "" then
               Text_IO.Set_Col (57);
               Text_IO.Put ("   unit: " & String (U));
            end if;

            Text_IO.New_Line;
         end;
      end loop;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      S : constant String := String (Filename);
      Test : constant String := "source3";
      I : constant Positive := Strings.Fixed.Index (S, Test);
   begin
      Text_IO.Put (" > " & S (I + Test'Length + 1 .. S'Last));
   end Output_Filename;

begin
   Check ("demo.gpr");
end Main;
