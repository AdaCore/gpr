with Ada.Text_IO;

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;

   procedure Check (Project_Name : String);
   --  Do check the given project's sources

   procedure Output_Filename (Filename : Path_Name.Object; V : Project.View.Object);
   --  Remove the leading tmp directory

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : String) is
      Prj  : Project.Tree.Object;
      Opt  : Options.Object;
      View : Project.View.Object;
      Log  : GPR2.Log.Object;
   begin
      Opt.Add_Switch (Options.P, Project_Name);

      if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         Prj.Update_Sources (Sources_Units_Artifacts, Log);
         Log.Output_Messages;

         View := Prj.Root_Project;
         Text_IO.Put_Line ("Project: " & String (View.Name));

         for Source of View.Sources loop
            declare
               U  : constant Optional_Name_Type := Source.Unit.Full_Name;
            begin
               Text_IO.New_Line;
               Output_Filename (Source.Path_Name, View);

               if U /= "" then
                  Text_IO.Put ("   unit: " & String (U));
                  Text_IO.New_Line;

                  for Dep of Source.Unit.Dependencies loop
                     Text_IO.Put_Line ("   " & String (Dep));
                  end loop;
               end if;
            end;
         end loop;
      end if;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Object; V : Project.View.Object) is
   begin
      Text_IO.Put (" > " & String (Filename.Relative_Path (V.Dir_Name)));
   end Output_Filename;

begin
   Check ("demo.gpr");
end Main;
