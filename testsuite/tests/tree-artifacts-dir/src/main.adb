with Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;

use GPR2;

procedure Main is
   procedure Test (Gpr        : String;
                   Subdirs    : String := "";
                   Reloc_Tree : String := "")
   is
      Tree : Project.Tree.Object;
      Ctx  : Context.Object;
      Cwd  : constant Path_Name.Object := Path_Name.Create_Directory (".");
   begin
      Project.Tree.Load_Autoconf
        (Tree,
         Path_Name.Create_File (Filename_Type (Gpr)),
         Context => Ctx,
         Subdirs => Optional_Name_Type (Subdirs),
         Build_Path => (if Reloc_Tree'Length > 0
                        then Path_Name.Create_Directory
                               (Filename_Type (Reloc_Tree))
                        else Path_Name.Undefined));

      if Tree.Log_Messages.Has_Error then
         Tree.Log_Messages.Output_Messages (Information => False);
         return;
      end if;
      Ada.Text_IO.Put_Line
        (String (Tree.Root_Project.Name) & " => " &
           String (Tree.Artifacts_Dir.Relative_Path (Cwd).Name));
      Tree.Unload;
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
