with Ada.Command_Line;
with Ada.Text_IO;

with GPR2.Build.Actions.Compile;
with GPR2.Build.Actions.Compile.Ada;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Artifacts.File_Part;
with GPR2.Build.Source.Sets;

with GPR2.Options;

with GPR2.Project.Tree;
with GPR2.Project.View;

use GPR2;

function Main return Natural is
   use type GPR2.Language_Id;
   Tree        : GPR2.Project.Tree.Object;
   Opts        : GPR2.Options.Object;
   Project     : constant String :=
                   (if Ada.Command_Line.Argument_Count > 0
                    then Ada.Command_Line.Argument (1)
                    else "tree/agg.gpr");

begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   if not Tree.Load (Opts) then
      return 1;
   end if;

   Tree.Update_Sources (Option   => GPR2.Sources_Units_Artifacts);

   for NS of Tree.Namespace_Root_Projects loop
      for Unit of NS.Units loop
         declare
            A : GPR2.Build.Actions.Compile.Ada.Object;
         begin
            A.Initialize (Unit);

            if not Tree.Artifacts_Database.Has_Action (A.UID) then
               if not Tree.Artifacts_Database.Add_Action (A) then
                  return 0;
               end if;
            end if;
         end;
      end loop;

      for Src of NS.Sources (Compilable_Only => True) loop
         if Src.Language /= Ada_Language
           and then Src.Kind = S_Body
         then
            declare
               A : GPR2.Build.Actions.Compile.Object;
            begin
               A.Initialize (Src);

               if not Tree.Artifacts_Database.Has_Action (A.UID) then
                  if not Tree.Artifacts_Database.Add_Action (A) then
                     return 0;
                  end if;
               end if;
            end;
         end if;
      end loop;
   end loop;

   for A of Tree.Artifacts_Database.All_Actions loop
      Ada.Text_IO.Put_Line (A.UID.Image);
      Ada.Text_IO.Put_Line ("  inputs:");
      for Input of Tree.Artifacts_Database.Inputs (A.UID) loop
         Ada.Text_IO.Put_Line ("  - " & Input.Image);
      end loop;

      Ada.Text_IO.Put_Line ("  outputs:");
      for Output of Tree.Artifacts_Database.Outputs (A.UID) loop
         Ada.Text_IO.Put_Line ("  - " & Output.Image);
      end loop;
   end loop;

   return 0;
end Main;
