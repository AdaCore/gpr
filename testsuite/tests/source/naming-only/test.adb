with Ada.Directories;

with GPR2.Build.Source;
with GPR2.Build.View_Db;
with GPR2.File_Readers;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Reader;
with Test_Assert;

function Test return Integer is
   use GPR2;
   use type Path_Name.Object;
   Tree : Project.Tree.Object;
   Opt  : Options.Object;
   Ref  : constant File_Readers.File_Reader_Reference :=
            File_Readers.Create_File_Reader_Reference
              (Reader.Object'(null record));

   procedure Check_Naming;

   procedure Check_Naming is
      View : constant Project.View.Object := Tree.Root_Project;
      Src  : constant Build.Source.Object :=
               View.View_Db.Source ("normal.ads");
   begin
      Test_Assert.Assert (Reader.Source_Reads = 0);
      Test_Assert.Assert (Src.Is_Defined);
      Test_Assert.Assert
        (Src.Path_Name = Path_Name.Create_File ("tree/first/normal.ads"));
      Test_Assert.Assert (not Src.Unit.Is_Parsed);
      Test_Assert.Assert (Src.Unit.Dependencies.Is_Empty);
      Test_Assert.Assert
        (not View.View_Db.Source ("renamed.ads").Is_Defined);
      Test_Assert.Assert
        (View.View_Db.Source ("exception.ads").Unit.Name = "Renamed");
      Test_Assert.Assert
        (View.View_Db.Source ("outer-inner.adb").Kind = S_Body);
   end Check_Naming;

begin
   Opt.Add_Switch (Options.P, "tree/root.gpr");
   Test_Assert.Assert
     (Tree.Load (Opt, File_Reader => Ref, Absent_Dir_Error => No_Error));
   Test_Assert.Assert (Tree.Update_Sources (Sources_Only));
   Check_Naming;
   Test_Assert.Assert (Tree.Update_Sources (Sources_Only));
   Check_Naming;

   Test_Assert.Assert (Tree.Update_Sources (Sources_Units));
   Test_Assert.Assert (Reader.Source_Reads > 0);
   Test_Assert.Assert
     (Tree.Root_Project.View_Db.Source ("normal.ads").Unit.Is_Parsed);
   Test_Assert.Assert
     (not Tree.Root_Project.View_Db.Source
       ("normal.ads").Unit.Dependencies.Is_Empty);
   Test_Assert.Assert
     (Tree.Root_Project.View_Db.Source ("outer-inner.adb").Kind = S_Separate);

   Ada.Directories.Delete_File ("tree/first/normal.ads");
   Reader.Source_Reads := 0;
   Test_Assert.Assert (Tree.Update_Sources (Sources_Only));
   Test_Assert.Assert (Reader.Source_Reads = 0);
   Test_Assert.Assert
     (Tree.Root_Project.View_Db.Source ("normal.ads").Path_Name =
        Path_Name.Create_File ("tree/second/normal.ads"));
   return Test_Assert.Report;
end Test;
