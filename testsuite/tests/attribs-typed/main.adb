with Ada.Text_IO;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;


   procedure Test (Project_File : String);

   procedure Test (Project_File : String) is
      Prj : Project.Tree.Object;
      Opt : Options.Object;
      Res : Boolean;

   begin
      Opt.Add_Switch (Options.P, Project_File);
      if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         Ada.Text_IO.Put_Line ("Successfully loaded " & Project_File);
      end if;
      Prj.Unload;
   end Test;

begin

   Test ("ko_externally_built.gpr");
   Test ("ok_externally_built.gpr");

   Test ("ok_library_kind.gpr");
   Test ("ko_library_kind.gpr");

end Main;
