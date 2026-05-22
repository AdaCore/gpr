with Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Context;

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
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Full then
         for A of Prj.Attributes (With_Defaults => False, With_Config => False) loop
            Text_IO.Put
              ("   " & Image (A.Name.Id.Attr));
            Text_IO.Put (" ->");

            for V of A.Values loop
               Text_IO.Put (" " & V.Text);
            end loop;
            Text_IO.New_Line;
         end loop;
      end if;
   end Display;

   Prj : Project.Tree.Object;
   Opt : Options.Object;
   Ctx : Context.Object;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");
   Opt.Add_Switch (Options.X, "OS=Linux");
   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Display (Prj.Root_Project);
   end if;

   Ctx.Clear;
   Ctx.Include ("OS", "Windows");
   if Prj.Set_Context (Ctx) then
      Display (Prj.Root_Project);
   end if;
end Main;
