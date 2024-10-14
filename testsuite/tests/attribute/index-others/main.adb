with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Log;
with GPR2.Message;
with GPR2.Options;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Variable.Set;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object);

   procedure Display (Att : Project.Attribute.Object);

   -------------
   -- Display --
   -------------

   procedure Display (Att : Project.Attribute.Object) is
   begin
      Text_IO.Put ("   " & Image (Att.Name.Id.Attr));

      if Att.Has_Index then
         Text_IO.Put (" (" & Att.Index.Text & ")");
      end if;

      Text_IO.Put (" ->");

      for V of Att.Values loop
         Text_IO.Put (" " & V.Text);
      end loop;

      if Att.Has_Index and then Att.Index.Is_Others then
         Text_IO.Put ("  - this is the others index");
      else
         Text_IO.Put ("  - this is the source 'others'");
      end if;
      Text_IO.New_Line;
      Text_IO.Put_Line (Att.Image);
   end Display;

   procedure Display (Prj : Project.View.Object) is
      use GPR2.Project.Attribute.Set;
      First : Boolean;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Kind'Img);

      for A of Prj.Attributes (With_Defaults => False, With_Config => False) loop
         Display (A);
      end loop;

      for Pck of Prj.Packages (With_Defaults => False) loop
         First := True;

         for A of Prj.Attributes (Pack => Pck, With_Defaults => False, With_Config => False) loop
            if First then
               First := False;
               Text_IO.Put_Line (" " & Image (Pck));
            end if;

            Display (A);
         end loop;
      end loop;
   end Display;

   Prj : Project.Tree.Object;
   Opt : Options.Object;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");
   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Display (Prj.Root_Project);
   end if;
end Main;
