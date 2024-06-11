with Ada.Text_IO;

with GNATCOLL.OS.FSUtil;

with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is
   procedure Test (Opt : in out GPR2.Options.Object);

   procedure Test (Opt : in out GPR2.Options.Object) is
      Prj : GPR2.Project.Tree.Object;
      Log : GPR2.Log.Object;
      CWD : constant GPR2.Path_Name.Object :=
              GPR2.Path_Name.Create_Directory (".");
   begin
      if not Prj.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
         return;
      end if;

      for V of Prj.Ordered_Views loop
         if not V.Is_Runtime then
            Ada.Text_IO.Put_Line (String (V.Name));
            Ada.Text_IO.Put ("  project dir: ");
            Ada.Text_IO.Put_Line
              (String (V.Path_Name.Containing_Directory.Relative_Path (CWD)));
         end if;
      end loop;

      Prj.Unload;
   end Test;

   Opt : GPR2.Options.Object;
   Ign : Boolean with Unreferenced;
begin
   --  Setup the symlinks
   Ign := GNATCOLL.OS.FSUtil.Create_Symbolic_Link
     ("import_A/link_to_import_B", "../import_B");
   Ign := GNATCOLL.OS.FSUtil.Create_Symbolic_Link
     ("import_B/link_to_import_A", "../import_A");

   Opt.Add_Switch (GPR2.Options.P, "project/prj.gpr");
   Ada.Text_IO.Put_Line ("## Loading WITHOUT symlinks support");
   Test (Opt);

   Opt.Add_Switch (GPR2.Options.Resolve_Links);
   Ada.Text_IO.Put_Line ("## Loading WITH symlinks support");
   Test (Opt);

   --  cleanup before exiting

   Ign := GNATCOLL.OS.FSUtil.Remove_File ("import_A/link_to_import_B");
   Ign := GNATCOLL.OS.FSUtil.Remove_File ("import_B/link_to_import_A");
end Main;
