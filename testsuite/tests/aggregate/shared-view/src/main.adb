with Ada.Text_IO;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;

use GPR2;

procedure Main is

   procedure Test (Gpr : String) is
      Tree : Project.Tree.Object;
      Opt  : Options.Object;
   begin
      Opt.Add_Switch (Options.P, Gpr);
      if not Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         return;
      end if;

      for V of Tree.Ordered_Views loop
         if V.Kind /= K_Aggregate then
            Ada.Text_IO.Put ("Roots for " & String (V.Name));

            if V.Is_Extended then
               Ada.Text_IO.Put (" extended by " & String (V.Extending.Name));
            end if;

            Ada.Text_IO.New_Line;

            for Root of V.Namespace_Roots loop
               Ada.Text_IO.Put_Line (" - " & String (Root.Name));
            end loop;
         end if;
      end loop;

      Tree.Unload;
   end Test;

begin
   Test ("tree/agg.gpr");
end Main;
