--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  For each path given on the command line, store a signature holding that
--  file as an output, load it back and add the artifact again, as a build
--  does: it is only accepted if the path read from the signature compares
--  equal to the original.

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Signature;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is
   use Ada.Text_IO;
   use GPR2;

   package CL renames Ada.Command_Line;

   Tree : Project.Tree.Object;
   Opts : Options.Object;

begin
   Opts.Add_Switch (Options.P, "tree/prj.gpr");

   if not Tree.Load (Opts, With_Runtime => True) then
      Put_Line ("KO: cannot load the project tree");
      return;
   end if;

   for J in 1 .. CL.Argument_Count loop
      declare
         Path  : constant String := CL.Argument (J);
         Art   : constant Build.Artifacts.Files.Object :=
                   Build.Artifacts.Files.Create
                     (Path_Name.Create_File (Filename_Type (Path)));
         Db    : constant Path_Name.Object :=
                   Path_Name.Create_File
                     (Filename_Type
                        ("sig"
                         & Ada.Strings.Fixed.Trim (J'Image, Ada.Strings.Left)
                         & ".json"));
         Saved  : Build.Signature.Object;
         Loaded : Build.Signature.Object;
         Dummy  : Boolean;
      begin
         Saved.Initialize (Tree.Artifacts_Database.File_Indexer);
         Dummy := Saved.Add_Output (Art);
         Saved.Store (Db);

         Loaded := Build.Signature.Load (Db, Tree.Root_Project);

         if not Loaded.Add_Output (Art) then
            Put_Line ("KO: not restored from its signature: " & Path);
         end if;
      end;
   end loop;
end Main;
