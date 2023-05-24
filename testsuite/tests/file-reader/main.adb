with GPR2.Build.Source.Sets;
with GPR2.Context;
with GPR2.File_Readers;
with GNAT.IO;  use GNAT.IO;
with GPR2.Log;
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
   Ctx : GPR2.Context.Object;
   Log : GPR2.Log.Object;

   M_ADB : constant GPR2.Path_Name.Object :=
             GPR2.Path_Name.Create_File ("m.adb");

   procedure Test;

   ----------
   -- Test --
   ----------

   procedure Test is
   begin

      Prj.Load
        (Filename         => GPR2.Path_Name.Create_File ("aggregating.gpr"),
         Context          => Ctx,
         File_Reader      => Reference);
      Prj.Log_Messages.Output_Messages (Information => False);

      --  Get dependencies

      Prj.Update_Sources (Messages => Log);
      Log.Output_Messages;

      --  Print m.adb dependencies

      for Source of Prj.Root_Project.Aggregated.First_Element.Sources loop
         if Source.Path_Name = M_ADB then
            for D of Source.Dependencies (Closure => True) loop
               if D.Source.Path_Name /= M_ADB then
                  Put_Line (D.Source.Path_Name.Value);
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
      "project withed is"
      & " for Source_Dirs use(""a"");"
      & "end withed;");

   --  Test all GPR files load are using file reader.

   Test;

   --  Unload parsed m.adb

   Prj.Unload (Full => True);

   --  Let m.adb depends on b.ads (instead of a.ads)

   My_File_Reader.Add
     ("m.adb",
      "with b;procedure m is begin null;end m;");

   --  Test m.adb parsed from Using file reader.

   Test;

end main;
