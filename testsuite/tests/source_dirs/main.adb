with Ada.Text_IO;
with GPR2.Log;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Tree;

procedure Main is
   use GPR2;

   Project_Tree : Project.Tree.Object;
   Ctx          : Context.Object := Context.Empty;

   Src_Dirs : GPR2.Path_Name.Set.Object;

   function "<" (L, R : GPR2.Path_Name.Object) return Boolean
     is (L.Value < R.Value);
   package Sort is new GPR2.Path_Name.Set.Set.Generic_Sorting;
begin

   Project_Tree.Load_Autoconf
     (Filename          => Project.Create ("p.gpr"),
      Context           => Ctx);

   Src_Dirs := Project_Tree.Root_Project.Source_Directories;

   Sort.Sort (Src_Dirs);

   Ada.Text_IO.Put_Line ("p.gpr:");
   for Src_Dir of Src_Dirs loop
      Ada.Text_IO.Put_Line
        (Src_Dir.Value &
         (if not Src_Dir.Is_Directory then " is not a directory" else ""));
   end loop;

   Project_Tree.Unload;

   Project_Tree.Load_Autoconf
     (Filename          => Project.Create ("q.gpr"),
      Context           => Ctx);

   Src_Dirs := Project_Tree.Root_Project.Source_Directories;

   Ada.Text_IO.Put_Line ("q.gpr:");
   for Src_Dir of Src_Dirs loop
      Ada.Text_IO.Put_Line (Src_Dir.Value);
   end loop;

   Project_Tree.Unload;

exception
   when Project_Error =>
      for M of Project_Tree.Log_Messages.all loop
         Ada.Text_IO.Put_Line (M.Format);
      end loop;
end Main;
