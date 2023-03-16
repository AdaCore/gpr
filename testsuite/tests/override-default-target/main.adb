with GPR2.KB;
with GPR2.Log;
with GPR2.Project.Tree;
with GPR2.Context;
with GPR2.Path_Name;

with Ada.Text_IO;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
begin
   GPR2.KB.Set_Default_Target ("not-a-target");

   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        ("prj1.gpr"),
      Context  => GPR2.Context.Empty);

   for Msg_Cur in Tree.Log_Messages.Iterate
     (Information => False)
   loop
      Ada.Text_IO.Put_Line (GPR2.Log.Element (Msg_Cur).Format);
   end loop;

   Tree.Unload;

end Main;
