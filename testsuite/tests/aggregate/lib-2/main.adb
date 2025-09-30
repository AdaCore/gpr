with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Build.Actions.Compile;
with GPR2.Build.Actions_Population;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Options;
with GPR2.Build.Source.Sets;
with GPR2.Path_Name;
with GPR2.Options;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Tree;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object);

   Cwd : constant Path_Name.Object := Path_Name.Create_Directory (".");

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
      use type GPR2.Project.View.Object;
      use GPR2.Build;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      for A of Prj.Tree.Artifacts_Database.All_Actions loop
         if A.View = Prj then
            Text_IO.Put ("   ");
            if A in Actions.Compile.Object'Class then
               Text_IO.Put_Line
                 (String
                      (Actions.Compile.Object (A).Input.Path_Name.Base_Name));
            else
               Text_IO.Put_Line (A.UID.Action_Class);
            end if;

            for File of Prj.Tree.Artifacts_Database.Outputs (A.UID) loop
               if File in Artifacts.Files.Object'Class then
                  Text_IO.Put_Line
                    ("      "
                     & String
                         (Artifacts.Files.Object (File).Path.Relative_Path
                            (Cwd)));
               end if;
            end loop;
         end if;
      end loop;
   end Display;

   Prj     : Project.Tree.Object;
   Opt     : Options.Object;
   DAG_Opt : Build.Options.Build_Options;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");

   if not Prj.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
      return;
   end if;

   if not Prj.Update_Sources then
      return;
   end if;

   if not Build.Actions_Population.Populate_Actions (Prj, DAG_Opt, True) then
      return;
   end if;

   for P of Prj loop
      Display (P);
   end loop;
end Main;
