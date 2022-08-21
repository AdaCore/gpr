with GPR2.Log;
with GPR2.Project.Tree;
with GPR2.Context;
with GPR2.Path_Name;

with Ada.Text_IO;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
begin
   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File ("p.gpr"),
      Context  => GPR2.Context.Empty);

   Ada.Text_IO.Put_Line ("invalid source list file not detected");

exception
   when GPR2.Project_Error =>
      For M in Tree.Log_Messages.Iterate (Information => False) loop
         Ada.Text_IO.Put_Line (GPR2.Log.Element (M).Format);
      end loop;
      Tree.Unload;

end Main;
