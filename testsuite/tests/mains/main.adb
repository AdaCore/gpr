with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

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

      for M of Prj.Executables loop
         Text_IO.Put_Line (Filter_Path (M.Dir_Name & String (M.Base_Name)));
      end loop;
   end Display;

   -----------------
   -- Filter_Path --
   -----------------

   function Filter_Path (Filename : Path_Name.Full_Name) return String is
      D : constant String := "mains";
      I : constant Positive := Strings.Fixed.Index (Filename, D);
   begin
      return Filename (I .. Filename'Last);
   end Filter_Path;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Project.Tree.Load (Prj, Create ("prj/demo1.gpr"), Ctx);
   Prj.Update_Sources;
   Display (Prj.Root_Project);
   Prj.Log_Messages.Output_Messages
     (Information    => False,
      Warning        => True,
      Error          => False);
   Prj.Unload;

   Project.Tree.Load (Prj, Create ("prj/demo2.gpr"), Ctx);
   Prj.Update_Sources;
   Display (Prj.Root_Project);
   Prj.Log_Messages.Output_Messages
     (Information    => False,
      Warning        => True,
      Error          => False);
   Prj.Unload;

   Project.Tree.Load (Prj, Create ("prj/demo3.gpr"), Ctx);
   Prj.Update_Sources;
   Display (Prj.Root_Project);
   Prj.Log_Messages.Output_Messages
     (Information    => False,
      Warning        => True,
      Error          => False);
   Prj.Unload;

   Project.Tree.Load (Prj, Create ("prj/demo4.gpr"), Ctx);
   Prj.Update_Sources;
   Display (Prj.Root_Project);
   Prj.Log_Messages.Output_Messages
     (Information    => False,
      Warning        => True,
      Error          => False);
   Prj.Unload;

   Project.Tree.Load (Prj, Create ("prj/demo5.gpr"), Ctx);
   Prj.Update_Sources;
   Display (Prj.Root_Project);
   Prj.Log_Messages.Output_Messages
     (Information    => False,
      Warning        => True,
      Error          => False);
   Prj.Unload;

   Project.Tree.Load (Prj, Create ("prj/demo6.gpr"), Ctx);
   Prj.Update_Sources;
   Display (Prj.Root_Project);
   Prj.Log_Messages.Output_Messages
     (Information    => False,
      Warning        => True,
      Error          => False);
   Prj.Unload;

   Project.Tree.Load (Prj, Create ("prj/demo7.gpr"), Ctx);
   Prj.Update_Sources;
   Display (Prj.Root_Project);
   Prj.Log_Messages.Output_Messages
     (Information    => False,
      Warning        => True,
      Error          => False);
   Prj.Unload;

   Project.Tree.Load (Prj, Create ("prj/demo8.gpr"), Ctx);
   Prj.Update_Sources;
   Display (Prj.Root_Project);
   Prj.Log_Messages.Output_Messages
     (Information    => False,
      Warning        => True,
      Error          => False);
   Prj.Unload;

   Project.Tree.Load (Prj, Create ("prj/demo9.gpr"), Ctx);
   Prj.Update_Sources;
   Display (Prj.Root_Project);
   Prj.Log_Messages.Output_Messages
     (Information    => False,
      Warning        => True,
      Error          => False);
   Prj.Unload;

   Project.Tree.Load (Prj, Create ("prj/demo10.gpr"), Ctx);
   Prj.Update_Sources;
   Display (Prj.Root_Project);
   Prj.Log_Messages.Output_Messages
     (Information    => False,
      Warning        => True,
      Error          => False);
   Prj.Unload;

   Project.Tree.Load (Prj, Create ("prj/demo11.gpr"), Ctx);
   Prj.Update_Sources;
   Display (Prj.Root_Project);
   Prj.Log_Messages.Output_Messages
     (Information    => False,
      Warning        => True,
      Error          => False);
   Prj.Unload;

   Project.Tree.Load (Prj, Create ("prj/demo12.gpr"), Ctx);
   Prj.Update_Sources;
   Display (Prj.Root_Project);
   Prj.Log_Messages.Output_Messages
     (Information    => False,
      Warning        => True,
      Error          => False);
   Prj.Unload;
end Main;