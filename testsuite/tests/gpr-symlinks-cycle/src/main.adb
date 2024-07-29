with Ada.Text_IO;

with GNATCOLL.OS.FSUtil;

with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Context;

procedure Main is
   procedure Test (Resolve_Links : Boolean);

   procedure Test (Resolve_Links : Boolean) is
      Prj : GPR2.Project.Tree.Object;
      Log : GPR2.Log.Object;
      CWD : constant GPR2.Path_Name.Object :=
              GPR2.Path_Name.Create_Directory (".");
      Ctx : GPR2.Context.Object;

   begin
      GPR2.Project.Tree.Load
        (Prj, GPR2.Project.Create ("./project/prj.gpr"), Ctx,
           Resolve_Links => Resolve_Links);
      Prj.Update_Sources;

      for V of Prj.Ordered_Views loop
         if not V.Is_Runtime then
            Ada.Text_IO.Put_Line (String (V.Name));
            Ada.Text_IO.Put ("  project dir: ");
            Ada.Text_IO.Put_Line
              (String (V.Path_Name.Containing_Directory.Relative_Path (CWD)));
         end if;
      end loop;

      Prj.Unload;
   exception
      when GPR2.Project_Error =>
      Prj.Log_Messages.Output_Messages
        (Information => False, Warning => True, Error => True);
   end Test;

   Ign : Boolean with Unreferenced;
begin
   --  Setup the symlinks
   Ign := GNATCOLL.OS.FSUtil.Create_Symbolic_Link
     ("import_A/link_to_import_B", "../import_B");
   Ign := GNATCOLL.OS.FSUtil.Create_Symbolic_Link
     ("import_B/link_to_import_A", "../import_A");

   Ada.Text_IO.Put_Line ("## Loading WITHOUT symlinks support");
   Test (False);

   Ada.Text_IO.Put_Line ("## Loading WITH symlinks support");
   Test (True);

   --  cleanup before exiting

   Ign := GNATCOLL.OS.FSUtil.Remove_File ("import_A/link_to_import_B");
   Ign := GNATCOLL.OS.FSUtil.Remove_File ("import_B/link_to_import_A");
end Main;
