with Ada.Command_Line;
with Ada.Text_IO;

with GPR2.Build.Actions.Ada_Compile;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Artifacts.File_Part;
with GPR2.Log;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;

function Main return Natural is
   Tree    : GPR2.Project.Tree.Object;
   Opts    : GPR2.Options.Object;
   Log     : GPR2.Log.Object;
   Project : constant String :=
               (if Ada.Command_Line.Argument_Count > 0
                then Ada.Command_Line.Argument (1)
                else "tree/agg.gpr");

begin
   Opts.Add_Switch (GPR2.Options.P, Project);
   Opts.Finalize;

   if not Tree.Load (Opts, With_Runtime => False)
   then
      return 1;
   end if;

   Tree.Update_Sources (Option   => GPR2.Sources_Units_Artifacts);

   for NS of Tree.Namespace_Root_Projects loop
      for Unit of NS.Units loop
         declare
            A : GPR2.Build.Actions.Ada_Compile.Object :=
                  GPR2.Build.Actions.Ada_Compile.Create (Unit);
         begin
            if not Tree.Artifacts_Database.Has_Action (A.UID) then
               Tree.Artifacts_Database.Add_Action (A, Log);
               Log.Output_Messages;
            end if;
         end;
      end loop;
   end loop;

   for A of Tree.Artifacts_Database.All_Actions loop
      if not A.Valid_Signature then
         Ada.Text_IO.Put_Line (A.UID.Image);
         Ada.Text_IO.Put_Line ("  inputs:");
         for Input of Tree.Artifacts_Database.Inputs (A.UID) loop
            Ada.Text_IO.Put_Line ("  - " & Input.Image);
         end loop;

         Ada.Text_IO.Put_Line ("  outputs:");
         for Output of Tree.Artifacts_Database.Outputs (A.UID) loop
            Ada.Text_IO.Put_Line ("  - " & Output.Image);
         end loop;

         Tree.Artifacts_Database.Execute (A.UID);
      end if;
   end loop;

   return 0;
end Main;
