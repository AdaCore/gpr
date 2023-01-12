--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure test is
   Imported_Begin : constant String := "project imported is for Source_Dirs use (""";
   Imported_End : constant String := """); end imported;";
   Extended_Begin : constant String := "project extended is for Source_Dirs use (""";
   Extended_End : constant String := """); end extended;";
   Extending_Begin : constant String := "with ""imported""; project extending extends ""extended"" is for Source_Dirs use (""";
   Extending_End : constant String := """); end extending;";

   Imported : constant GPR2.Path_Name.Object := GPR2.Path_Name.Create_File ("imported.gpr");
   Extended : constant GPR2.Path_Name.Object := GPR2.Path_Name.Create_File ("extended.gpr");
   Extending : constant GPR2.Path_Name.Object := GPR2.Path_Name.Create_File ("extending.gpr");

   procedure Write (Path : GPR2.Path_Name.Object; Before : String; Middle : String; After : String) is
      File : File_Type;
   begin
      Create (File => File,
              Name => Path.Value);
      Put_Line (File, Before & Middle & After);
      Close (File);
   end Write;

   Tree : GPR2.Project.Tree.Object;

   procedure Print_Source_Dirs (Header : String) is
   begin
      Put_Line (Header);
      Put_Line (String (Tree.Root_Project.Source_Directories.First_Element.Simple_Name));
      Put_Line (String (Tree.Root_Project.Imports.First_Element.Source_Directories.First_Element.Simple_Name));
      Put_Line (String (Tree.Root_Project.Extended_Root.Source_Directories.First_Element.Simple_Name));
   end Print_Source_Dirs;
begin
   Write (Imported, Imported_Begin, "imported_1", Imported_End);
   Write (Extended, Extended_Begin, "extended_1", Extended_End);
   Write (Extending, Extending_Begin, "extending_1", Extending_End);
   Tree.Load_Autoconf (Extending, GPR2.Context.Empty);
   Print_Source_Dirs ("Expecting source dirs ended by 1");
   Write (Imported, Imported_Begin, "imported_2", Imported_End);
   Write (Extended, Extended_Begin, "extended_2", Extended_End);
   Write (Extending, Extending_Begin, "extending_2", Extending_End);
   Tree.Load_Autoconf (Extending, GPR2.Context.Empty);
   Print_Source_Dirs ("Expecting source dirs ended by 2");
end test;
