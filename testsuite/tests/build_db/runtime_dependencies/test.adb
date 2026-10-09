with Ada.Directories;
with Ada.Text_IO;

with GPR2.Build.View_Db;
with GPR2.Containers;
with GPR2.Log;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Test_Assert;

function Test return Integer is
   use GPR2;
   use type Containers.Name_Set;

   Tree  : Project.Tree.Object;
   Opt   : Options.Object;
   Left  : Project.View.Object;
   Right : Project.View.Object;
   Multi : Project.View.Object;

   procedure Check_Dependency
     (View   : Project.View.Object;
      File   : Filename_Type;
      First  : Optional_Name_Type := "";
      Second : Optional_Name_Type := "");
   function Find_View (Name : Name_Type) return Project.View.Object;
   procedure Write_Override (Suffix : String);

   procedure Check_Dependency
     (View   : Project.View.Object;
      File   : Filename_Type;
      First  : Optional_Name_Type := "";
      Second : Optional_Name_Type := "")
   is
      Dependencies : Containers.Filename_Set;
      Expected     : Containers.Name_Set;
   begin
      Dependencies.Include (File);
      if First'Length > 0 then
         Expected.Include (Name_Type (First));
      end if;
      if Second'Length > 0 then
         Expected.Include (Name_Type (Second));
      end if;
      Test_Assert.Assert
        (View.View_Db.Runtime_Dependencies (Dependencies) = Expected);
   end Check_Dependency;

   function Find_View (Name : Name_Type) return Project.View.Object is
   begin
      for View of Tree.Ordered_Views loop
         if View.Name = Name then
            return View;
         end if;
      end loop;
      raise Program_Error with "missing view " & String (Name);
   end Find_View;

   procedure Write_Override (Suffix : String) is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create
        (File, Ada.Text_IO.Out_File, "tree/right/s-conca3." & Suffix);
      Ada.Text_IO.Put_Line
        (File, "package " & (if Suffix = "adb" then "body " else "") &
           "System.Concat_3 is end System.Concat_3;");
      Ada.Text_IO.Close (File);
   end Write_Override;

begin
   Opt.Add_Switch (Options.P, "tree/root.gpr");
   Test_Assert.Assert
     (Tree.Load (Opt, With_Runtime => True, Absent_Dir_Error => No_Error));
   Test_Assert.Assert (Tree.Update_Sources);
   Left := Find_View ("Left");
   Right := Find_View ("Right");
   Multi := Find_View ("Multi");

   Test_Assert.Assert (Left.View_Db.Has_Runtime_Overrides);
   Test_Assert.Assert (not Right.View_Db.Has_Runtime_Overrides);
   Test_Assert.Assert (Multi.View_Db.Has_Runtime_Overrides);

   --  An extending project's imports expose renamed overrides.
   Check_Dependency (Left, "concat_override.adb", "System.Concat_3");
   Check_Dependency (Left, "s-conca3.adb", "System.Concat_3");
   Check_Dependency (Left, "s-conca3.ads", "System.Concat_3");
   Check_Dependency
     (Find_View ("Overrides"), "concat_override.adb", "System.Concat_3");
   Check_Dependency (Right, "s-conca3.adb");
   Check_Dependency (Left, "main.adb");
   Check_Dependency (Left, "s-regpat.adb");
   Check_Dependency (Left, "tree/overrides/s-conca3.ads");
   Check_Dependency (Left, "missing.ads");
   Check_Dependency
     (Multi, "overrides.ada", "System.Concat_2", "System.Concat_3");
   if not File_Names_Case_Sensitive then
      Check_Dependency (Left, "CONCAT_OVERRIDE.ADB", "System.Concat_3");
   end if;

   --  Per-view and tree refreshes discard empty and populated caches.
   Write_Override ("ads");
   Write_Override ("adb");
   declare
      Messages : Log.Object;
   begin
      Right.View_Db.Update (Messages);
      Test_Assert.Assert (not Messages.Has_Error);
   end;
   Test_Assert.Assert (Left.View_Db.Has_Runtime_Overrides);
   Test_Assert.Assert (Right.View_Db.Has_Runtime_Overrides);
   Test_Assert.Assert (Multi.View_Db.Has_Runtime_Overrides);
   Check_Dependency (Left, "concat_override.adb", "System.Concat_3");
   Check_Dependency (Right, "s-conca3.adb", "System.Concat_3");
   Check_Dependency
     (Multi, "overrides.ada", "System.Concat_2", "System.Concat_3");

   Ada.Directories.Delete_File ("tree/right/s-conca3.ads");
   Ada.Directories.Delete_File ("tree/right/s-conca3.adb");
   Test_Assert.Assert (Tree.Update_Sources);
   Test_Assert.Assert (not Right.View_Db.Has_Runtime_Overrides);
   Test_Assert.Assert (Left.View_Db.Has_Runtime_Overrides);
   Check_Dependency (Right, "s-conca3.adb");

   return Test_Assert.Report;
end Test;
