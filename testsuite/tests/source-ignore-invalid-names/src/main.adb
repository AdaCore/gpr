with Ada.Text_IO;

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;

   procedure Check (Project_Name : String;
                    Variant      : String);
   --  Do check the given project's sources

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : String;
                    Variant      : String) is
      Prj  : Project.Tree.Object;
      Opt  : Options.Object;
      View : Project.View.Object;

   begin
      Opt.Add_Switch (Options.P, Project_Name);
      Opt.Add_Switch (Options.X, "VARIANT=" & Variant);
      GPR2.Project.Tree.Verbosity := GPR2.Project.Tree.Warnings_And_Errors;

      if not Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         return;
      end if;

      View := Prj.Root_Project;
      Text_IO.Put_Line ("Project: " & String (View.Name) &
                          " with variant=" & Variant);

      GPR2.Project.Tree.Verbosity := GPR2.Project.Tree.Info;

      if Prj.Update_Sources then
         for Source of View.Sources loop
            Text_IO.Put (" - " & String (Source.Path_Name.Simple_Name));

            Text_IO.Set_Col (20);
            Text_IO.Put ("   language: " & Image (Source.Language));

            Text_IO.Set_Col (37);
            Text_IO.Put ("   Kind: " & Source.Kind'Image);

            if Source.Has_Units then
               Text_IO.Put ("   unit: " & String (Source.Unit.Name));
            end if;

            Text_IO.New_Line;
         end loop;
      end if;
   end Check;

begin
   Check ("tree/prj.gpr", "v1");
   Check ("tree/prj.gpr", "v2");
end Main;
