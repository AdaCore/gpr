with Ada.Text_IO;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Path_Name.Set;

procedure Main is
   use GPR2;

   procedure Test (GPR : String) is

      Tree     : Project.Tree.Object;
      Opt      : Options.Object;
      Src_Dirs : GPR2.Path_Name.Set.Object;

      function "<" (L, R : GPR2.Path_Name.Object) return Boolean
      is (L.Value < R.Value);
      package Sort is new GPR2.Path_Name.Set.Set.Generic_Sorting;

   begin
      Opt.Add_Switch (Options.P, GPR);

      if not Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         return;
      end if;

      Ada.Text_IO.Put_Line (String (GPR) & ":");

      Src_Dirs := Tree.Root_Project.Source_Directories;
      Sort.Sort (Src_Dirs);

      for Dir of Src_Dirs loop
         Ada.Text_IO.Put_Line
           (String (Dir.Relative_Path (Tree.Root_Project.Dir_Name)) &
              (if not Dir.Is_Directory then " is not a directory" else ""));
      end loop;
   end Test;

begin
   Test ("p.gpr");
   Test ("q.gpr");
   Test ("r.gpr");
   Test ("s.gpr");
end Main;
