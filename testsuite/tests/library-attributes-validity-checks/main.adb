with Ada.Exceptions;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Log;

procedure Main is

   procedure Test (Filename : GPR2.Filename_Type) is
      Tree    : GPR2.Project.Tree.Object;
      Context : GPR2.Context.Object;
   begin
      begin
         Ada.Text_IO.Put_Line (String (Filename));
         Tree.Load_Autoconf
           (Filename  => GPR2.Path_Name.Create_File (Filename),
            Context   => Context);
      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
      end;
      if Tree.Has_Messages  then
         for C in Tree.Log_Messages.Iterate
           (False, True, True, False, True, True)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
      end if;
   end Test;

begin
   Test ("prj1.gpr");
   Test ("abstractprj1.gpr");
   Test ("libprj1.gpr");
   Test ("aggrprj1.gpr");
   Test ("aggrlibprj1.gpr");
end main;
