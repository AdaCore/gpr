with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;

   procedure Display (Prj : Project.View.Object);

   function Filter_Path (Filename : Path_Name.Full_Name) return String;

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);
      Text_IO.Put_Line (Filter_Path (Prj.Executable_Directory.Value));
   end Display;

   -----------------
   -- Filter_Path --
   -----------------

   function Filter_Path (Filename : Path_Name.Full_Name) return String is
      S : constant String := String (Filename);
      Test : constant String := "executable-directory";
      I : constant Positive := Strings.Fixed.Index (S, Test);
   begin
      return S (I + Test'Length + 1 .. S'Last);
   end Filter_Path;

   Prj : Project.Tree.Object;
   Opt : Options.Object;

begin
   Opt.Add_Switch (Options.P, "demo1.gpr");
   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Display (Prj.Root_Project);
   end if;

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "demo2.gpr");
   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Display (Prj.Root_Project);
   end if;

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "demo3.gpr");
   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Display (Prj.Root_Project);
   end if;

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "demo4.gpr");
   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Display (Prj.Root_Project);
   end if;

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "demo1.gpr");
   Opt.Add_Switch (Options.Subdirs, "experiment");
   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Display (Prj.Root_Project);
   end if;
end Main;
