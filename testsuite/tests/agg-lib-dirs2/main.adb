with Ada.Text_IO;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is

   use GPR2;

   Tree    : GPR2.Project.Tree.Object;
   Context : GPR2.Context.Object;

   procedure Print_Messages is
   begin
      if Tree.Has_Messages then
         for C in Tree.Log_Messages.Iterate
           (False, True, True, True, True)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
      end if;
   end Print_Messages;

   procedure Test (Project_Name : GPR2.Filename_Type) is
   begin
      Ada.Text_IO.Put_Line ("testing " & String (Project_Name));
      Tree.Unload;
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File
           (GPR2.Project.Ensure_Extension (Project_Name),
            GPR2.Path_Name.No_Resolution),
         Context  => Context);
   exception
      when Project_Error =>
         Print_Messages;
   end Test;

begin
   Test ("good/aggl.gpr");
   Test ("aggl/aggl.gpr");
end Main;