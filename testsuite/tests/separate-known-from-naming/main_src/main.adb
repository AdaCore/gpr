with PACK;
with Ada.Text_IO;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   Context      : GPR2.Context.Object;
   use GPR2;

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
      Tree.Unload;
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File
           (GPR2.Project.Ensure_Extension (Project_Name),
            GPR2.Path_Name.No_Resolution),
         Context  => Context);
      Tree.Update_Sources;
      Print_Messages;
   exception
      when Project_Error =>
         Print_Messages;
   end Test;

begin
   Ada.Text_IO.Put_Line (Pack.Functions_Test);
   Test("COMPONENT");
   Context.Insert ("DOT_REPLACEMENT", "-");
   Ada.Text_IO.Put_Line ("DOT_REPLACEMENT=""-""");
   Test("COMPONENT");
end Main;
