with Ada.Command_Line;
with Ada.Text_IO;

with GPR2.Build.Actions.Ada_Compile;
with GPR2.Build.Artifact_Ids;
with GPR2.Build.Artifacts.ALI;
with GPR2.Build.Artifacts.Object_File;
with GPR2.Build.Artifacts.Source;
with GPR2.Build.Artifacts.Source.Ada;
with GPR2.Build.DAG;

with GPR2.Log;
with GPR2.Options;

with GPR2.Project.Tree;
with GPR2.Project.View;

function Main return Natural is
   Tree        : GPR2.Project.Tree.Object;
   Opts        : GPR2.Options.Object;
   Log         : GPR2.Log.Object;
   Ada_Compile : GPR2.Build.Actions.Ada_Compile.Object;
   Project     : constant String :=
                   (if Ada.Command_Line.Argument_Count > 0
                    then Ada.Command_Line.Argument (1)
                    else "tree/agg.gpr");

   procedure Test (Class : GPR2.Artifact_Class) is
   begin
      Ada.Text_IO.Put_Line (GPR2.Image (Class));
      for C in Tree.Artifacts_Database.DAG.Iterate (Class) loop
         declare
            use GPR2.Build;

            A : GPR2.Build.DAG.Constant_Reference_Type :=
                  Tree.Artifacts_Database.DAG.Constant_Reference (C);
            View : GPR2.Project.View.Object :=
                     Tree.Get_View (Artifact_Ids.View (A.Id));
         begin
            Ada.Text_IO.Put (String (View.Name));
            Ada.Text_IO.Put (": ");
            Ada.Text_IO.Put_Line (Artifact_Ids.Path (A.Id));
         end;
      end loop;
   end Test;

begin
   Opts.Add_Switch (GPR2.Options.P, Project);
   Opts.Add_Switch (GPR2.Options.Subdirs, "subdir");
   Opts.Finalize;

   if not Opts.Load_Project
     (Tree             => Tree,
      With_Runtime     => False)
   then
      Tree.Log_Messages.Output_Messages (Information => False);
      return 1;
   end if;

   GPR2.Build.Actions.Ada_Compile.Register (Tree.Artifacts_Database.DAG);

   Tree.Update_Sources
     (Option   => GPR2.Sources_Units_Artifacts,
      Messages => Log);
   Log.Output_Messages;

   Test (GPR2.Build.Artifacts.Source.A_Class);
   Test (GPR2.Build.Artifacts.Source.Ada.A_Class);
   Test (GPR2.Build.Artifacts.ALI.A_Class);
   Test (GPR2.Build.Artifacts.Object_File.A_Class);

   return 0;
end Main;
