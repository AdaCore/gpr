with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;

   procedure List_Sources (View : Project.View.Object);

   ------------------
   -- List_Sources --
   ------------------

   procedure List_Sources (View : Project.View.Object) is

      procedure Output_Filename (Filename : Path_Name.Full_Name);
      --  Remove the leading tmp directory

      ---------------------
      -- Output_Filename --
      ---------------------

      procedure Output_Filename (Filename : Path_Name.Full_Name) is
         S    : constant String := String (Filename);
         Test : constant String := "case-sensitive-suffixes";
         I    : constant Positive := Strings.Fixed.Index (S, Test);
      begin
         Text_IO.Put (" > " & S (I + Test'Length + 1 .. S'Last));
      end Output_Filename;
   begin
      Text_IO.Put_Line ("----------");

      for Source of View.Sources loop
         Output_Filename (Source.Path_Name.Value);
         Text_IO.Set_Col (20);
         Text_IO.Put ("   language: " & Image (Source.Language));
         Text_IO.Set_Col (36);
         Text_IO.Put ("   Kind: " & Source.Kind'Image);
         Text_IO.New_Line;
      end loop;
   end List_Sources;

   Prj : Project.Tree.Object;
   Opt : Options.Object;
begin
   Opt.Add_Switch (options.P, "demo.gpr");

   if not Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      return;
   end if;

   Text_IO.Put_Line ("Project: " & String (Prj.Root_Project.Name));
   Prj.Update_Sources;
   List_Sources (Prj.Root_Project);
end Main;
