with Ada.Text_IO;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;

use GPR2;

procedure Main is
   procedure Test (Gpr        : String;
                   Subdirs    : String := "";
                   Reloc_Tree : String := "")
   is
      Tree : Project.Tree.Object;
      Opt  : Options.Object;
      Cwd  : constant Path_Name.Object := Path_Name.Create_Directory (".");
   begin
      Opt.Add_Switch (Options.P, Gpr);
      if Subdirs'Length > 0 then
         Opt.Add_Switch (Options.Subdirs, Subdirs);
      end if;
      if Reloc_Tree'Length > 0 then
         Opt.Add_Switch (Options.Relocate_Build_Tree, Reloc_Tree);
      end if;

      if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         Ada.Text_IO.Put_Line
           (String (Tree.Root_Project.Name) & " => " &
              String (Tree.Artifacts_Dir.Relative_Path (Cwd)));
      end if;
   end Test;

begin
   Test ("trees/prj.gpr");
   Test ("trees/agg.gpr");

   Ada.Text_IO.Put_Line ("subdirs set to 'sub'");

   Test ("trees/prj.gpr", "sub");
   Test ("trees/agg.gpr", "sub");

   Ada.Text_IO.Put_Line ("build tree set to 'reloc'");

   Test ("trees/prj.gpr", "", "reloc");
   Test ("trees/agg.gpr", "", "reloc");

   Ada.Text_IO.Put_Line ("both set");

   Test ("trees/prj.gpr", "sub", "reloc");
   Test ("trees/agg.gpr", "sub", "reloc");
end Main;
