--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions.Write_File;
with GPR2.Options;
with GPR2.Path_Name;
with Ada.Strings;
with Ada.Strings.Fixed;

with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Reporter.Console;

with Test_Assert; use Test_Assert;

use GPR2, GPR2.Reporter;

function Test return Integer is
   Tree      : GPR2.Project.Tree.Object;
   Opts      : GPR2.Options.Object;
   Project   : constant String := "tree/main.gpr";
   Root_View : GPR2.Project.View.Object;

   package GBA renames GPR2.Build.Actions;
begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   Assert (Tree.Load (Opts, True, Console.Create (Quiet)), "Loading the tree");

   Root_View := Tree.Namespace_Root_Projects.First_Element;

   for Action_Index in 1 .. 3 loop
      declare
         use all type Ada.Strings.Trim_End;
         A : GBA.Write_File.Object;
      begin
         A.Initialize
            (Root_View, Action_Index);
         Assert (Tree.Artifacts_Database.Add_Action (A));

         --  Check that calling Add_Action twice does not fail

         Assert (Tree.Artifacts_Database.Add_Action (A));
         Assert
           (String (Tree.Artifacts_Database.Db_Filename_Path
              (GBA.Object'Class (A).UID).Simple_Name) =
               ".write_file_" & Ada.Strings.Fixed.Trim
                  (Action_Index'Img, Both) & ".json");
      end;
   end loop;

   return Report;
end Test;
