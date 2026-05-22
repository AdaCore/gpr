with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   use GPR2;

   function Test (Project_Name : String) return String is
      Tree : Project.Tree.Object;
      Opt  : Options.Object;
   begin
      Opt.Add_Switch (Options.P, Project_Name);
      if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         return Tree.Root_Project.Variable ("Var").Value.Text;
      else
         return "";
      end if;
   end Test;
begin
   declare
      UTF8 : String := Test ("utf8.gpr");
      CP1252 : String := Test ("cp1252.gpr");
   begin
      --  check that Could not decode source as "UTF-8" reported when
      --  file is not using UTF-8 & CP-1252 encoding.

      Put (Test ("cp1252error.gpr"));

      if UTF8 = CP1252 then
         Put_Line ("OK");
      else
         Put_Line ("utf8.gpr returned");
         Put_Line (UTF8);
         Put_Line ("cp1252.gpr returned");
         Put_Line (CP1252);
      end if;
   end;
end Main;
