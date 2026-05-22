with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure test is
   Imported_Begin  : constant String := "project imported is for Source_Dirs use (""";
   Imported_End    : constant String := """); end imported;";
   Extended_Begin  : constant String := "project extended is for Source_Dirs use (""";
   Extended_End    : constant String := """); end extended;";
   Extending_Begin : constant String := "with ""imported""; project extending extends ""extended"" is for Source_Dirs use (""";
   Extending_End   : constant String := """); end extending;";

   Imported  : constant String := "imported.gpr";
   Extended  : constant String := "extended.gpr";
   Extending : constant String := "extending.gpr";

   procedure Write (Path : String; Before : String; Middle : String; After : String) is
      File : File_Type;
   begin
      Create (File => File,
              Name => Path);
      Put_Line (File, Before & Middle & After);
      Close (File);
   end Write;

   Tree : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;

   procedure Print_Source_Dirs (Header : String) is
   begin
      Put_Line (Header);
      for Dir of Tree.Root_Project.Source_Directories loop
         Put_Line (String (Dir.Simple_Name));
      end loop;
      for Import of Tree.Root_Project.Imports loop
         if not Import.Is_Externally_Built then
            for Dir of Import.Source_Directories loop
               Put_Line (String (Dir.Simple_Name));
            end loop;
         end if;
      end loop;
      for Dir of Tree.Root_Project.Extended_Root.Source_Directories loop
         Put_Line (String (Dir.Simple_Name));
      end loop;
   end Print_Source_Dirs;

begin
   Opt.Add_Switch (GPR2.Options.P, Extending);

   Write (Imported, Imported_Begin, "imported_1", Imported_End);
   Write (Extended, Extended_Begin, "extended_1", Extended_End);
   Write (Extending, Extending_Begin, "extending_1", Extending_End);

   if Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
      Print_Source_Dirs ("Expecting source dirs ended by 1");
   end if;

   Write (Imported, Imported_Begin, "imported_2", Imported_End);
   Write (Extended, Extended_Begin, "extended_2", Extended_End);
   Write (Extending, Extending_Begin, "extending_2", Extending_End);

   if Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
      Print_Source_Dirs ("Expecting source dirs ended by 2");
   end if;
end test;
