with GPR2;
with GPR2.Log;
with GPR2.Project.Tree;
with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name; use GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.View;
with GPR2.Project.Source;

with Ada.Text_IO;

procedure Main is
   Tree : GPR2.Project.Tree.Object;

   procedure Print_Messages is
   begin
      if Tree.Has_Messages then
         for C in Tree.Log_Messages.Iterate
           (False, True, True, True, True)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
      end if;
   end Print_Messages;

begin
   Ada.Text_IO.Put_Line ("Restrict to existing language:");
   Tree.Restrict_Autoconf_To_Languages
     (GPR2.Containers.Language_Id_Set.To_Set (GPR2.Ada_Language));

   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        ("prj1.gpr"),
      Context  => GPR2.Context.Empty);

   Print_Messages;

   Tree.Unload;

   Ada.Text_IO.Put_Line ("Restrict to missing language:");
   Tree.Restrict_Autoconf_To_Languages
     (GPR2.Containers.Language_Id_Set.To_Set (GPR2.CPP_Language));

   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        ("prj1.gpr"),
      Context  => GPR2.Context.Empty);

   Print_Messages;

   Tree.Unload;

end Main;
