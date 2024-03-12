with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Build.Compilation_Unit;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object);

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
      Src : Build.Compilation_Unit.Unit_Location;
   begin
      Text_IO.Put_Line("list of mains:");

      for M of Prj.Mains loop
         Text_IO.Put_Line (String (M.Source.Relative_Path  (Prj.Dir_Name)));

      end loop;

      Text_IO.Put_Line("list of executables:");

      for M of Prj.Executables loop
         Text_IO.Put_Line (String (M.Relative_Path (Prj.Dir_Name)));
         Src := Prj.Main (M.Simple_Name);
         if Src.Source.Is_Defined then
            Text_IO.Put_Line
              ("  " & String (Src.Source.Relative_Path (Prj.Dir_Name)));
         end if;
      end loop;
   end Display;

   Tree : Project.Tree.Object;
   Ctx  : Context.Object;
   Log  : GPR2.Log.Object;

begin
   for J in 1 .. 16 loop
      declare
         Num : constant String := J'Image;
         Prj_File : constant String :=
           "prj/demo" & Num (Num'First + 1 .. Num'Last) & ".gpr";
      begin
         Project.Tree.Load (Tree, Create (Filename_Type (Prj_File)), Ctx);
         Text_IO.Put (String (Tree.Root_Project.Name) & " ");
         Text_IO.Set_Col (10);
         Text_IO.Put_Line (Tree.Root_Project.Qualifier'Img);

         Tree.Log_Messages.Output_Messages (Information => False);
         Tree.Update_Sources (Messages => Log);
         Log.Output_Messages;
         Display (Tree.Root_Project);
      exception
         when GPR2.Project_Error =>
            Tree.Log_Messages.Output_Messages (Information => False);
      end;
   end loop;
end Main;
