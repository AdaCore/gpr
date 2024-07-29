with Ada.Text_IO;

with GPR2.Project.Source.Set;
with GPR2.Log;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Context;

with GNATCOLL.OS.FSUtil;

procedure Main is

   procedure Test (Resolve_Links : Boolean);

   procedure Test (Resolve_Links : Boolean) is
      Prj : GPR2.Project.Tree.Object;
      Log : GPR2.Log.Object;
      Ctx : GPR2.Context.Object;
   begin
      GPR2.Project.Tree.Load
        (Prj, GPR2.Project.Create ("./project/prj.gpr"), Ctx,
         Resolve_Links => Resolve_Links);
      Prj.Update_Sources;

      for V of Prj.Root_Project.Imports loop
         if not V.Is_Runtime then
            Ada.Text_IO.Put_Line (String (V.Name));
            for Src of V.Sources loop
               Ada.Text_IO.Put (" - ");
               Ada.Text_IO.Put_Line
                 (String (Src.Path_Name.Relative_Path
                    (V.Path_Name.Containing_Directory)));
            end loop;
         end if;
      end loop;

      Prj.Unload;

   exception
      when GPR2.Project_Error =>
      Prj.Log_Messages.Output_Messages
        (Information => False, Warning => True, Error => True);
   end Test;

   Ign : Boolean;

begin
   --  Setup the symlinks
   Ign := GNATCOLL.OS.FSUtil.Create_Symbolic_Link
     ("./project/subdir/imported_prj.gpr", "../../import/imported_prj.gpr");

   if not Ign then
      Ada.Text_IO.Put_Line ("Failed to create the symlink");
      return;
   end if;

   Ada.Text_IO.Put_Line ("## Loading WITHOUT symlinks support");
   Test (False);

   Ada.Text_IO.Put_Line ("## Loading WITH symlinks support");
   Test (True);
   Ign := GNATCOLL.OS.FSUtil.Remove_File ("project/subdir/imported_prj.gpr");
end Main;
