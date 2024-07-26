with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   Project_File : constant String := "tree/prj.gpr";
   Target       : constant String := "not_a_target";

   procedure Test
     (Target : String;
      With_RTS : Boolean)
   is
      Project : GPR2.Project.Tree.Object;
      Options : GPR2.Options.Object;
   begin

      if Target'Length > 0 then
         Put_Line ("Target: " & Target);
         Options.Add_Switch (GPR2.Options.Target, Target);
      end if;

      Put_Line ("With_RTS : " & With_RTS'Image);

      Options.Add_Switch (GPR2.Options.P, Project_File);

      if Project.Load (Options, With_Runtime => With_RTS)
        and then Project.Update_Sources
      then
            Put_Line ("  Tree loaded");
      end if;

      Put_Line ("  Project has rts: " & Project.Has_Runtime_Project'Image);
      Put_Line ("  Project requested rts: " & Project.Runtime_Requested'Image);
   end Test;

begin
   Test ("", True);
   Test (Target, True);
   Test (Target, False);
end Main;
