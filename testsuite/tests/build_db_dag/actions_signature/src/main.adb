with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;

with GPR2.Build.Actions.Compile.Ada;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Artifacts.File_Part;
with GPR2.Build.Tree_Db;
with GPR2.Log;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;

function Main return Natural is
   Tree    : GPR2.Project.Tree.Object;
   Opts    : GPR2.Options.Object;
   Log     : GPR2.Log.Object;
   Actions : GPR2.Build.Tree_Db.Actions_List;
   Project : constant String :=
     (if Ada.Command_Line.Argument_Count > 0 then Ada.Command_Line.Argument (1)
      else "tree/agg.gpr");

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, String);
   package String_Vectors_Sorting is new String_Vectors.Generic_Sorting;

begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   if not Tree.Load (Opts, With_Runtime => False) then
      return 1;
   end if;

   Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts);

   for NS of Tree.Namespace_Root_Projects loop
      for Unit of NS.Units loop
         declare
            A               : GPR2.Build.Actions.Compile.Ada.Object;
            Sorted_Messages : String_Vectors.Vector;
         begin
            A.Initialize (Unit);

            if not Tree.Artifacts_Database.Has_Action (A.UID) then
               Tree.Artifacts_Database.Add_Action (A, Log);

               --  Sort the messages to ensure that the test output comparison
               --  always has the same result.

               for Message_Curs in Log.Iterate loop
                  Sorted_Messages.Append
                    (GPR2.Log.Element (Message_Curs).Format);
               end loop;
               Log.Clear;

               String_Vectors_Sorting.Sort (Sorted_Messages);

               for Message of Sorted_Messages loop
                  Ada.Text_IO.Put_Line (Message);
               end loop;

            end if;
         end;
      end loop;
   end loop;

   Actions := GPR2.Build.Tree_Db.Actions_List
                (Tree.Artifacts_Database.All_Actions);

   for A of Actions loop
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

         A.Compute_Signature;
      end if;
   end loop;

   return 0;
end Main;
