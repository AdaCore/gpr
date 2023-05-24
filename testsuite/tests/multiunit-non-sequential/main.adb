with GPR2.Build.Source.Sets;
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
     (Filename => GPR2.Path_Name.Create_File ("g.gpr"),
      Context  => GPR2.Context.Empty);
   Tree.Update_Sources (Messages => Log);

      for S of Tree.Root_Project.Sources loop
      Ada.Text_IO.Put_Line (String (S.Path_Name.Simple_Name));
      for CU of S.Units loop
         Ada.Text_IO.Put_Line ("  - compilation unit at" & CU.Index'Image);
         Ada.Text_IO.Put_Line ("    unit name    = " & String (CU.Name));
         Ada.Text_IO.Put_Line ("    kind         = " & CU.Kind'Image);
      end loop;
   end loop;

   Tree.Unload;

exception
   when GPR2.Project_Error =>
      For M in Tree.Log_Messages.Iterate (Information => False) loop
         Ada.Text_IO.Put_Line (GPR2.Log.Element (M).Format);
      end loop;
      Tree.Unload;

end Main;
