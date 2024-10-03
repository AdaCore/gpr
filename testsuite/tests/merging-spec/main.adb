with Ada.Text_IO;

with GPR2.Log;
with GPR2.Options;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;

   procedure Display (Prj : Project.View.Object);

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
      Naming : Package_Id renames Project.Registry.Pack.Naming;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      for A of Prj.Attributes (Pack => Naming) loop
         declare
            Name : constant String := Image (A.Name.Id.Attr);
         begin
            --  Check only Spec
            if Name = "Spec" then
               Text_IO.Put_Line (A.Image);
            end if;
         end;
      end loop;
   end Display;

   Prj : Project.Tree.Object;
   Opt : Options.Object;

begin
   Opt.Add_Switch (Options.P, "p");
   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Display (Prj.Root_Project);

      for C in Prj.Log_Messages.Iterate (Hint    => False,
                                         Warning => False,
                                         Error   => False,
                                         Lint    => True)
      loop
         Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
      end loop;
   end if;
end Main;
