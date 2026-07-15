with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GPR2;
with GPR2.Log;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Reporter.Log;

with Test_Assert; use Test_Assert;

function Test return Integer is
   Tree     : GPR2.Project.Tree.Object;
   Options  : GPR2.Options.Object;
   Reporter : GPR2.Reporter.Log.Object := GPR2.Reporter.Log.Create;
   --  A log reporter so messages are captured rather than printed

   Nb_Warnings : Natural := 0;
   Warnings    : Unbounded_String;

begin
   Options.Add_Switch (GPR2.Options.P, "p.gpr");

   --  'p.gpr' is an externally built library project. Loading it with the
   --  driver check enabled must not check the project's languages.

   Assert (Tree.Load (Options,
                      Reporter      => Reporter,
                      Check_Drivers => True),
           "tree loading");

   Tree.Update_Sources (GPR2.Sources_Units_Artifacts);

   for C in GPR2.Reporter.Log.Object(Tree.Reporter).Log.Iterate
              (Error    => False,
               Warning  => True,
               End_User => False,
               Hint     => False,
               Lint     => False)
   loop
      Nb_Warnings := Nb_Warnings + 1;
      Append (Warnings, ASCII.LF & GPR2.Log.Element (C).Format);
   end loop;

   Assert (Nb_Warnings = 0,
           "no warning expected when loading an externally built project:"
           & To_String (Warnings));

   return Report;
end Test;
