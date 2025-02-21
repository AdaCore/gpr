with Ada.Exceptions;
with Ada.Text_IO;

with GNATCOLL.OS.Process;

with GPR2.Build.Actions_Population;
with GPR2.Build.Options;
with GPR2.Build.Process_Manager;
with GPR2.Build.Source.Sets;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Reporter.Console;

procedure Main is
   procedure Test (Gpr : String);

   procedure Test (Gpr : String) is
      use Ada.Text_IO;
      use GPR2;
      use GPR2.Build.Process_Manager;
      use GPR2.Options;
      use GPR2.Reporter;

      Tree       : GPR2.Project.Tree.Object;
      Opts       : GPR2.Options.Object;
      Build_Opts : constant GPR2.Build.Options.Build_Options := (others => <>);
      Exec_Opts  : constant GPR2.Build.Process_Manager.PM_Options := (others => <>);
      PM         : GPR2.Build.Process_Manager.Object;
      Reporter   : GPR2.Reporter.Console.Object :=
                     GPR2.Reporter.Console.Create
                       (Regular,
                        Important_Only);

   begin
      Put_Line ("=== Loading " & Gpr & " ===");
      Opts.Add_Switch (P, Gpr);

      if not Tree.Load (Opts,
                        With_Runtime        => True,
                        Create_Missing_Dirs => GPR2.Project.Tree.Create_Always,
                        Reporter            => Reporter)
        or else not Tree.Update_Sources
      then
         return;
      end if;

      for V of Tree.Ordered_Views loop
         Put_Line (String (V.Name));
         for Imp of V.Imports loop
            Put_Line ("  " & String (Imp.Name));
         end loop;
      end loop;

      if not GPR2.Build.Actions_Population.Populate_Actions
        (Tree,
         Build_Opts)
      then
         return;
      end if;

      --  Tree.Set_Reporter (GPR2.Reporter.Console.Create (Quiet));

      if Tree.Artifacts_Database.Execute (PM, Exec_Opts) /= Success then
         return;
      end if;

      Put_Line ("main execution:");
      if GNATCOLL.OS.Process.Run
        (GNATCOLL.OS.Process.Arg_Lists.To_Vector
           (Tree.Root_Project.Executables.First_Element.String_Value, 1)) /= 0
      then
         Put_Line ("!!! execution failed");
      end if;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Test;

begin
   Test ("tree/master.gpr");
   Test ("tree2/master_ext.gpr");
end Main;
