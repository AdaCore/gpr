with GPR2.Log;
with GPR2.Project.Tree;
with GPR2.Context;
with GPR2.Path_Name;

with Ada.Text_IO;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   Log  : GPR2.Log.Object;

begin
   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File ("p.gpr"),
      Context  => GPR2.Context.Empty);
   Tree.Log_Messages.Output_Messages (Information => False);
   Tree.Update_Sources (Messages => Log);
   Log.Output_Messages;

   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File ("p2.gpr"),
      Context  => GPR2.Context.Empty);
   Tree.Log_Messages.Output_Messages (Information => False);
   Tree.Update_Sources (Messages => Log);
   Log.Output_Messages;
end Main;
