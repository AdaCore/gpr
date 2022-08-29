with Ada.Text_IO;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree.View_Builder;
with GPR2.Source_Info;

procedure Main
is
   use GPR2, GPR2.Path_Name, GPR2.Project.Tree;
   package PRA renames GPR2.Project.Registry.Attribute;
   package PRP renames GPR2.Project.Registry.Pack;

   Root : View_Builder.Object :=
            View_Builder.Create (Create_Directory ("demo"), "Custom_Project");
   Src_Dirs : Containers.Value_List;
   Mains    : Containers.Value_List;
   Tree     : Project.Tree.Object;
   Ctxt     : GPR2.Context.Object;

   procedure Print_Attrs (Pck : GPR2.Package_Id) is
   begin
      for A of Tree.Root_Project.Attributes (Pack        => Pck,
                                             With_Config => False)
      loop
         declare
            use type PRA.Value_Kind;
            Attr_Name : constant String := Image (A.Name.Id.Attr);
            First     : Boolean := True;
         begin
            if Pck /= Project_Level_Scope then
               Ada.Text_IO.Put (Image (Pck) & "'");
            end if;

            Ada.Text_IO.Put (Attr_Name);

            if A.Has_Index then
               Ada.Text_IO.Put (" (""" & String (A.Index.Value) & """)");
            end if;

            Ada.Text_IO.Put (": ");

            if A.Kind = PRA.Single then
               Ada.Text_IO.Put_Line
                 ("""" & String (A.Value.Text) & """");
            else
               for V of A.Values loop
                  if not First then
                     Ada.Text_IO.Put (", ");
                  else
                     First := False;
                  end if;

                  Ada.Text_IO.Put ("""" & V.Text & """");
               end loop;

               Ada.Text_IO.New_Line;
            end if;
         end;
      end loop;
   end Print_Attrs;

begin
   Src_Dirs.Append ("src1");
   Src_Dirs.Append ("src2");
   Root.Set_Attribute (PRA.Source_Dirs, Src_Dirs);
   Root.Set_Attribute (PRA.Object_Dir, "obj");
   Mains.Append ("main.adb");
   Root.Set_Attribute (PRA.Main, Mains);
   Root.Set_Attribute (PRA.Builder.Executable,
                       "main.adb", "mymain");

   View_Builder.Load_Autoconf (Tree, Root, Ctxt);
   if Tree.Log_Messages.Has_Error then
      Tree.Log_Messages.Output_Messages;
      return;
   end if;

   Ada.Text_IO.Put_Line ("Attributes:");
   Print_Attrs (Project_Level_Scope);
   Print_Attrs (PRP.Builder);

   Ada.Text_IO.Put_Line ("Sources:");
   Tree.Update_Sources (Backends => Source_Info.No_Backends);

   for S of Tree.Root_Project.Sources loop
      Ada.Text_IO.Put_Line (String (S.Path_Name.Value));
   end loop;

exception
   when GPR2.Project_Error =>
      Tree.Log_Messages.Output_Messages;
end Main;
