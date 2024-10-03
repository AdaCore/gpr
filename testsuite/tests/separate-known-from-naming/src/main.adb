with Ada.Text_IO;
with GPR2.Build.Compilation_Unit;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Reporter.Console;

procedure Main is
   use GPR2;
   use GPR2.BUild;

   procedure Test (Project_Name : String; Ext : String) is
      Tree : Project.Tree.Object;
      Opt  : Options.Object;

   begin
      Opt.Add_Switch (Options.P, Project_Name);
      if Ext'Length > 0 then
         Opt.Add_Switch (Options.X, Ext);
      end if;

      if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         Tree.Set_Reporter
           (GPR2.Reporter.Console.Create (GPR2.Reporter.Verbose));
         Tree.Update_Sources;

         for U of Tree.Root_Project.Units loop
            Ada.Text_IO.Put_Line (String (U.Name));
            for Part in S_Spec .. S_Body loop
               if U.Has_Part (Part) then
                  Ada.Text_IO.Put ("- ");
                  Ada.Text_IO.Put (Part'Image);
                  Ada.Text_IO.Put (": ");
                  Ada.Text_IO.Put_Line (String (U.Get (Part).Source.Simple_Name));
               end if;
            end loop;

            for C in U.Separates.Iterate loop
               declare
                  Key  : constant Name_Type :=
                           GPR2.Build.Compilation_Unit.Separate_Maps.Key (C);
                  Part : Compilation_Unit.Unit_Location :=
                           Compilation_Unit.Separate_Maps.Element (C);
               begin
                  Ada.Text_IO.Put ("- SEP. ");
                  Ada.Text_IO.Put (String (Key));
                  Ada.Text_IO.Put (": ");
                  Ada.Text_IO.Put_Line (String (Part.Source.Simple_Name));
               end;
            end loop;
         end loop;
      end if;
   end Test;

begin
   Test("tree/spec", "");
   Ada.Text_IO.Put_Line ("DOT_REPLACEMENT=""-""");
   Test("tree/spec", "DOT_REPLACEMENT=-");
end Main;
