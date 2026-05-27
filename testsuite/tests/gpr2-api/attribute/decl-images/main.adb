with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Options;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Tree;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;

   procedure Display (Prj : Project.View.Object);

   procedure Load (Filename : String);

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      Text_IO.New_Line;
      for A of Prj.Attributes (With_Defaults => False, With_Config => False) loop
         Text_IO.Put_Line (A.Image);
      end loop;

      Text_IO.New_Line;
      for A of Prj.Attributes (With_Defaults => False, With_Config => False) loop
         Text_IO.Put_Line (A.Image (15));
      end loop;

      if Prj.Has_Types then
         Text_IO.New_Line;
         for T of Prj.Types loop
            Text_IO.Put_Line (T.Image);
         end loop;
      end if;

      if Prj.Has_Variables then
         Text_IO.New_Line;
         for V of Prj.Variables loop
            Text_IO.Put_Line (V.Image);
         end loop;

         Text_IO.New_Line;
         for V of Prj.Variables loop
            Text_IO.Put_Line (V.Image (5));
         end loop;
      end if;
   end Display;

   ----------
   -- Load --
   ----------

   procedure Load (Filename : String) is
      Prj : Project.Tree.Object;
      Opt : Options.Object;
   begin
      Opt.Add_Switch (Options.P, Filename);
      if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         Display (Prj.Root_Project);
      end if;
   end Load;

begin
   Load ("demo.gpr");
end Main;
