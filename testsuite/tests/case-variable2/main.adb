with Ada.Text_IO;

with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Variable.Set;
with GPR2.Options;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Full then
         for A of Prj.Attributes (With_Defaults => False, With_Config => False) loop
            Text_IO.Put
              ("A:   " & Image (A.Name.Id.Attr));
            Text_IO.Put (" ->");

            for V of A.Values loop
               Text_IO.Put (" " & V.Text);
            end loop;
            Text_IO.New_Line;
         end loop;

         if Prj.Has_Variables then
            for V in Prj.Variables.Iterate loop
               Text_IO.Put ("V:   " & String (Key (V)));
               Text_IO.Put (" -> ");
               Text_IO.Put (Element (V).Value.Text);
               Text_IO.New_Line;
            end loop;
         end if;
         Text_IO.New_Line;
      end if;
   end Display;

   Prj1, Prj2 : Project.Tree.Object;
   Opt        : Options.Object;
   Res        : Boolean;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");
   Opt.Add_Switch (Options.X, "DEF=MyDefault");
   Res := Prj1.Load (Opt, Absent_Dir_Error => No_Error);

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "demo.gpr");
   Opt.Add_Switch (Options.X, "DEF=MyDefault");
   Opt.Add_Switch (Options.X, "arch=Linux");
   Res := Prj2.Load (Opt, Absent_Dir_Error => No_Error);

   Display (Prj1.Root_Project);
   Display (Prj2.Root_Project);
end Main;
