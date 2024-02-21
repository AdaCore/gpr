with GPR2;
with GPR2.KB;
with GPR2.Log;
with GPR2.Project.Tree;
with GPR2.Context;
with GPR2.Path_Name; use GPR2.Path_Name;

with Ada.Text_IO;
with Ada.Environment_Variables;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   KB   : GPR2.KB.Object := GPR2.KB.Create_Default (GPR2.KB.Default_Flags);
   Msg  : GPR2.Log.Object;
begin
   Ada.Environment_Variables.Set ("GPR_CONFIG", "");

   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        ("prj1.gpr"),
      Context  => GPR2.Context.Empty);

   if Tree.Has_Messages then
      for C in Tree.Log_Messages.Iterate (False)
      loop
         Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
      end loop;
   end if;

   Tree.Unload;

end Main;
