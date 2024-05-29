with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.OS_Lib;

with GPR2.Context;
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

   procedure Changed_Callback (Prj : Project.View.Object);

   ----------------------
   -- Changed_Callback --
   ----------------------

   procedure Changed_Callback (Prj : Project.View.Object) is
   begin
      Text_IO.Put_Line (">>> Changed_Callback for " & String (Prj.Name));
   end Changed_Callback;

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

      for A in Prj.Attributes (With_Defaults => False,
                               With_Config   => False).Iterate loop
         Text_IO.Put
           ("A:   " & Image (Attribute.Set.Element (A).Name.Id.Attr));
         Text_IO.Put (" ->");

         for V of Element (A).Values loop
            Text_IO.Put (" " & V.Text);
         end loop;
         Text_IO.New_Line;
      end loop;
   end Display;

   Prj : Project.Tree.Object;
   Opt : Options.Object;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");

   Text_IO.Put_Line ("//// OS set to Linux");
   Opt.Add_Switch (Options.X, "OS=Linux");
   Opt.Finalize;

   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      for P of Prj loop
         Display (P);
      end loop;
   end if;

   Text_IO.Put_Line ("//// OS set to Windows");
   if Prj.Set_Context (Context.Empty, Changed_Callback'Access) then
      for P of Prj loop
         Display (P);
      end loop;
   end if;
end Main;
