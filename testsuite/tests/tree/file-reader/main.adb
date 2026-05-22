with GNAT.IO;  use GNAT.IO;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Source.Sets;
with GPR2.File_Readers;
with GPR2.Options;
with GPR2.Path_Name; use GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

with My_File_Reader;

----------
-- main --
----------

procedure main is

   Reference : constant GPR2.File_Readers.File_Reader_Reference :=
     My_File_Reader.Reference;

   Prj : GPR2.Project.Tree.Object;
   Opt : GPR2.Options.Object;
   Cwd : constant GPR2.Path_Name.Object :=
     GPR2.Path_Name.Create_Directory (".");

   M_ADB : constant GPR2.Path_Name.Object :=
             GPR2.Path_Name.Create_File ("m.adb");

   procedure Test;

   ----------
   -- Test --
   ----------

   procedure Test is
      CU : GPR2.Build.Compilation_Unit.Object;
      Dep : GPR2.Build.Compilation_Unit.Object;
   begin

      if not Prj.Load (Opt, File_Reader => Reference) then
         return;
      end if;

      --  Get dependencies

      Prj.Update_Sources;

      --  Print m.adb dependencies

      for NS of Prj.Namespace_Root_Projects loop
         if Prj.Artifacts_Database (NS).Has_Compilation_Unit ("M") then
            CU := Prj.Artifacts_Database (NS).Compilation_Unit ("M");

            for D of CU.Known_Dependencies loop
               Dep := Prj.Artifacts_Database (NS).Compilation_Unit (D);
               if Dep.Has_Part (GPR2.S_Spec) then
                  Put_Line (String (Dep.Spec.Source.Relative_Path (Cwd)));
               end if;
            end loop;
         end if;
      end loop;
   end Test;

begin

   --  Set GPR files content through file reader

   My_File_Reader.Add
     ("aggregating.gpr",
     "aggregate project aggregating is"
      & " for Project_Files use (""aggregated.gpr"");"
      & "end aggregating;");

   My_File_Reader.Add
     ("aggregated.gpr",
      "project aggregated extends ""extended"" is"
      & " for Main use (""m.adb"");"
      & "end aggregated;");

   My_File_Reader.Add
     ("extended.gpr",
      "with ""withed"";"
      & "project extended is"
      & " end extended;");

   My_File_Reader.Add
     ("withed.gpr",
      "project withed is" & " for Source_Dirs use(""a"");" & "end withed;");

   Opt.Add_Switch (GPR2.Options.P, "aggregating.gpr");

   --  Test all GPR files load are using file reader.

   Test;

   --  Unload parsed m.adb

   Prj.Unload;

   --  Let m.adb depends on b.ads (instead of a.ads)

   My_File_Reader.Add
     ("m.adb",
      "with b;procedure m is begin null;end m;");

   --  Test m.adb parsed from Using file reader.

   Test;

end main;
