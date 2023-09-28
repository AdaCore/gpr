with Ada.Text_IO;
with GPR2.Log;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Tree;

procedure Main is
   use GPR2;

   procedure Test (GPR : Filename_Type) is

      Project_Tree : Project.Tree.Object;
      Ctx          : Context.Object := Context.Empty;

      Src_Dirs : GPR2.Path_Name.Set.Object;

      function "<" (L, R : GPR2.Path_Name.Object) return Boolean
        is (L.Value < R.Value);
      package Sort is new GPR2.Path_Name.Set.Set.Generic_Sorting;
   begin

      Project_Tree.Load_Autoconf
        (Filename          => Project.Create (GPR),
         Context           => Ctx);

      Src_Dirs := Project_Tree.Root_Project.Source_Directories;

      Sort.Sort (Src_Dirs);

      Ada.Text_IO.Put_Line (String (GPR) & ":");
      for Src_Dir of Src_Dirs loop
         Ada.Text_IO.Put_Line
           (String (Src_Dir.Relative_Path (Project_Tree.Root_Project.Dir_Name)) &
              (if not Src_Dir.Is_Directory then " is not a directory" else ""));
      end loop;

      Project_Tree.Unload;
   exception
      when Project_Error =>
         for M of Project_Tree.Log_Messages.all loop
            Ada.Text_IO.Put_Line (M.Format);
         end loop;
   end Test;

begin
   Test ("p.gpr");
   Test ("q.gpr");
   Test ("r.gpr");
   Test ("s.gpr");
end Main;
