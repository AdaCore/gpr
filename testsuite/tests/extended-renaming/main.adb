with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   procedure Display (Att : Project.Attribute.Object);

   -------------
   -- Display --
   -------------

   procedure Display (Att : Project.Attribute.Object) is
   begin
      Text_IO.Put (" " & Image (Att.Name.Id));

      if Att.Has_Index then
         Text_IO.Put (" (" & Att.Index.Text & ")");
      end if;

      Text_IO.Put (" ->");

      for V of Att.Values loop
         Text_IO.Put (" " & V.Text);
      end loop;
      Text_IO.New_Line;
   end Display;

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
   begin
      Text_IO.Put ("Project: " & String (Prj.Name) & " ");
      Text_IO.Set_Col (20);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      for Pck of Prj.Packages loop
         for A of Prj.Attributes (Pack => Pck, With_Defaults => False, With_Config => False) loop
            Display (A);
         end loop;
      end loop;
   end Display;

   Prj : Project.Tree.Object;
   Opt : Options.Object;

begin
   Opt.Add_Switch (Options.P, "src/a.gpr");

   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then

      for P of Prj loop
         Display (P, Full => False);
      end loop;
   end if;
end Main;
