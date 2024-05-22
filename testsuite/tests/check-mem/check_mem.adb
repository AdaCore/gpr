with Ada.Command_Line;
with Ada.Text_IO;

with GPR2.Path_Name;
with GPR2.Options;
with GPR2.Build.Source.Sets;
with GPR2.Project.Tree;
with GPR2.Project.View;

with P1;

procedure Check_Mem is
   use Ada.Command_Line;
   use GPR2;

   Count : constant Natural :=
             (if Argument_Count > 0 then Integer'Value (Argument (1)) else 1);
begin
   P1; -- Call generated dummy sources

   for J in 1 .. Count loop
      declare
         T   : Project.Tree.Object;
         Opt : Options.Object;
      begin
         Opt.Add_Switch (Options.P, "check_mem.gpr");
         if T.Load (Opt) then
            for J of T.Root_Project.Sources loop
               exit;
            end loop;

            if Argument_Count > 1 then
               for V of T loop
                  Ada.Text_IO.Put_Line (V.Path_Name.String_Value);
                  for S of V.Sources loop
                     Ada.Text_IO.Put_Line (ASCII.HT & S.Path_Name.String_Value);
                  end loop;
               end loop;
            end if;
         end if;
      end;
   end loop;
end Check_Mem;
