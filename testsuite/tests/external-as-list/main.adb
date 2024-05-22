with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Context;
with GPR2.Options;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Variable.Set;

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
              ("A:   " & Image (A.Name.Id));
            Text_IO.Put (" ->");

            for V of A.Values loop
               Text_IO.Put (" " & '"' & V.Text & '"');
            end loop;
            Text_IO.New_Line;
         end loop;

         if Prj.Has_Variables then
            for V in Prj.Variables.Iterate loop
               Text_IO.Put ("V:   " & String (Key (V)));
               Text_IO.Put (" ->");
               for Val of Element (V).Values loop
                  Text_IO.Put (" " & '"' & Val.Text & '"');
               end loop;
               Text_IO.New_Line;
            end loop;
         end if;
      end if;
   end Display;

   Prj : Project.Tree.Object;
   Opt : Options.Object;
   Ctx : Context.Object;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");
   Opt.Add_Switch (Options.X, "SWITCHES=-O2,-g");
   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Display (Prj.Root_Project);

      Ctx.Clear;
      Ctx.Insert ("SWITCHES", ",-O2,-g,");

      if Prj.Set_Context (Ctx) then
         Display (Prj.Root_Project);
      else
         Prj.Log_Messages.Output_Messages (Information => False);
      end if;

      Ctx.Clear;
      Ctx.Insert ("SWITCHES", "-gnatv");

      if Prj.Set_Context (Ctx) then
         Display (Prj.Root_Project);
      else
         Prj.Log_Messages.Output_Messages (Information => False);
      end if;

      Ctx.Clear;
      Ctx.Insert ("SWITCHES", ",,");

      if Prj.Set_Context (Ctx) then
         Display (Prj.Root_Project);
      else
         Prj.Log_Messages.Output_Messages (Information => False);
      end if;

      Ctx.Clear;
      Ctx.Insert ("SWITCHES", ",");

      if Prj.Set_Context (Ctx) then
         Display (Prj.Root_Project);
      else
         Prj.Log_Messages.Output_Messages (Information => False);
      end if;
   end if;
end Main;
