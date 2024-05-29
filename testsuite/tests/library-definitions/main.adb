with Ada.Strings.Fixed;
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

   function Filter_Path (Line : Filename_Type) return String;
   --  Remove the tmp directory where test is processing

   Prj : Project.Tree.Object;
   Opt : Options.Object;
   Ctx : Context.Object;

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Kind'Img);
      Text_IO.Put_Line
        ("Library_Name : " & String (Prj.Library_Name));
      Text_IO.Put_Line
        ("Library_Directory : " & String (Prj.Library_Directory.Name));
      Text_IO.Put_Line
        ("                  : " & Filter_Path (Prj.Library_Directory.Value));
      Text_IO.Put_Line
        ("Library Filename : " & String (Prj.Library_Filename.Name));
      Text_IO.Put_Line
        ("                 : " & Filter_Path (Prj.Library_Filename.Value));
      Text_IO.Put_Line
        ("Library Version Filename : "
         & String (Prj.Library_Version_Filename.Name));
      Text_IO.Put_Line
        ("                         : "
         & Filter_Path (Prj.Library_Version_Filename.Value));
       Text_IO.Put_Line
        ("Library Major Version Filename : "
         & String (Prj.Library_Major_Version_Filename.Name));
       Text_IO.Put_Line
        ("                               : "
         & Filter_Path (Prj.Library_Major_Version_Filename.Value));
      Text_IO.Put_Line
        ("Library_Standalone : " & Prj.Library_Standalone'Img);
   end Display;

   -----------------
   -- Filter_Path --
   -----------------

   function Filter_Path (Line : Filename_Type) return String is
      S : constant String := String (Line);
      Test : constant String := "library-definitions";
      I : constant Positive := Strings.Fixed.Index (S, Test);
   begin
      return S (I + Test'Length + 1 .. S'Last);
   end Filter_Path;

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
