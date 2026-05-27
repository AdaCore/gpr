--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Exceptions;
with Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Test_Assert;

function Test return Integer is

   use GPR2;

   Tree          : GPR2.Project.Tree.Object;
   Opts          : GPR2.Options.Object;

   Project       : constant String := "prj/prj.gpr";
   Project_Upper : constant String := "prj/prj_upper.gpr";
   Project_Mixed : constant String := "prj/prj_mixed.gpr";
   Project_Multi : constant String := "prj/prj_multi.gpr";

   procedure Generate_CU_Filename
     (Self     : GPR2.Project.View.Object;
      Name     : GPR2.Name_Type;
      Kind     : GPR2.Valid_Unit_Kind;
      Expected : String);
   procedure Generate_CU_Filename
     (Self     : GPR2.Project.View.Object;
      Name     : Name_Type;
      Kind     : Valid_Unit_Kind;
      Expected : String)
   is
      CU_Filename : constant Simple_Name :=
                      Self.Filename_For_Unit (Name, Kind);
   begin
      Ada.Text_IO.Put_Line
        ("Generating file name for unit '"
         & String (Name) & "' for view '" & String (Self.Name) & "'");
      Ada.Text_IO.Put_Line
        ("Filename = '" & String (CU_Filename) & "'");
      Test_Assert.Assert (String (CU_Filename), Expected, "");
   end Generate_CU_Filename;
begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   if not Tree.Load (Opts, True)
     or else not Tree.Update_Sources
       (GPR2.Sources_Units_Artifacts, No_Error => True)
   then
      return 1;
   end if;

   for View of Tree.Namespace_Root_Projects loop
      Ada.Text_IO.Put_Line ("Existing CU : ");
      for CU of View.Units loop
         Ada.Text_IO.Put_Line ("   - " & String (CU.Name));
      end loop;
      Generate_CU_Filename (View, "Main", S_Body, "main.adb");
      Generate_CU_Filename (View, "Pkg", S_Body, "pkg.adb");
      Generate_CU_Filename (View, "Pkg", S_Spec, "pkg.ads");
      Generate_CU_Filename (View, "U.V", S_Separate, "sep.adb");
      Generate_CU_Filename (View, "U.W", S_Separate, "sep.adb");
      Generate_CU_Filename (View, "U", S_Body, "u.adb");
      Generate_CU_Filename (View, "Bla", S_Spec, "bla.2.ada");
      Generate_CU_Filename (View, "Bla", S_Body, "bla.1.ada");
      Generate_CU_Filename (View, "Foo.Bla", S_Separate, "foo~bla.3.ada");
      Generate_CU_Filename (View, "Foo.Bla", S_Body, "foo~bla.1.ada");
      Generate_CU_Filename (View, "Foo.Bla", S_Spec, "foo~bla.2.ada");
      Generate_CU_Filename
        (View, "Foo.Replace", S_Body, "this_is_a_naming_exception.adb");
      Generate_CU_Filename
        (View, "Foo.Replace", S_Spec, "this_is_a_naming_exception.ads");

      begin
         Generate_CU_Filename (View, "Foo_.Bla", S_Separate, "invalid");
      exception
         when E: others =>
            Test_Assert.Assert
              (Ada.Exceptions.Exception_Name (E),
               "ADA.ASSERTIONS.ASSERTION_ERROR");
            Test_Assert.Assert
              (Ada.Exceptions.Exception_Message (E),
               "invalid name for unit 'Foo_.Bla'");
      end;
   end loop;

   Opts := GPR2.Options.Empty_Options;
   Opts.Add_Switch (GPR2.Options.P, Project_Mixed);

   if not Tree.Load (Opts, True)
     or else not Tree.Update_Sources
       (GPR2.Sources_Units_Artifacts, No_Error => True)
   then
      return 1;
   end if;

   for View of Tree.Namespace_Root_Projects loop
      Generate_CU_Filename (View, "Foo.Bla", S_Body, "Foo-Bla.adb");
   end loop;

   Opts := GPR2.Options.Empty_Options;
   Opts.Add_Switch (GPR2.Options.P, Project_Upper);

   if not Tree.Load (Opts, True)
     or else not Tree.Update_Sources
       (GPR2.Sources_Units_Artifacts, No_Error => True)
   then
      return 1;
   end if;

   for View of Tree.Namespace_Root_Projects loop
      Generate_CU_Filename (View, "Foo.Bla", S_Body, "FOO-BLA.adb");
   end loop;

   Opts := GPR2.Options.Empty_Options;
   Opts.Add_Switch (GPR2.Options.P, Project_Multi);

   if not Tree.Load (Opts, True)
     or else not Tree.Update_Sources
       (GPR2.Sources_Units_Artifacts, No_Error => True)
   then
      return 1;
   end if;

   for View of Tree loop
      if View.Is_Namespace_Root or else View.Is_Runtime then
         Generate_CU_Filename (View, "Foo.Bla", S_Body, "foo-bla.adb");
      else
         Generate_CU_Filename (View, "Foo.Bla", S_Body, "foo~bla.1.ada");
      end if;
   end loop;

   return Test_Assert.Report;
end Test;
