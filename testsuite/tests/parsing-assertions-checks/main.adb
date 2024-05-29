with Ada.Text_IO;

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Build;

   procedure Display_Source (Src : Source.Object'Class);
   procedure Test_Prj (Fname : String);

   --------------------
   -- Display_Source --
   --------------------

   procedure Display_Source (Src : Source.Object'Class) is
   begin
      Text_IO.Put_Line (String (Src.Path_Name.Simple_Name) & ": " & Src.Kind'Image);
   end Display_Source;

   --------------
   -- Test_Prj --
   --------------

   procedure Test_Prj (Fname : String)
   is
      Prj : Project.Tree.Object;
      Opt : Options.Object;

   begin
      Text_IO.Put_Line ("GPR file: " & String (Fname));
      Opt.Add_Switch (Options.P, Fname);
      if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         Prj.Update_Sources;

         for V of reverse Prj.Ordered_Views loop
            Text_IO.Put_Line (String (V.Name));
            for S of V.Sources loop
               Display_Source (S);
            end loop;
         end loop;
      end if;
    end Test_Prj;

begin
   Test_Prj ("files/abstract_empty_value_checks.gpr");
   Test_Prj ("files/aggregate_empty_value_checks.gpr");
   Test_Prj ("files/library_empty_value_checks.gpr");
   Test_Prj ("files/library_empty_space_value_checks.gpr");

   Test_Prj ("files/excluded_source_list_file_empty_value_checks.gpr");
   Test_Prj ("files/external_empty_value_checks.gpr");
   Test_Prj ("files/externally_built_empty_value_checks.gpr");
   Test_Prj ("files/library_kind_empty_value_checks.gpr");
   Test_Prj ("files/library_symbol_file_empty_value_checks.gpr");
   Test_Prj ("files/library_symbol_policy_empty_value_checks.gpr");
   Test_Prj ("files/name_empty_value_checks.gpr");
   Test_Prj ("files/naming_body_empty_value_checks.gpr");
   Test_Prj ("files/naming_casing_empty_value_checks.gpr");
   Test_Prj ("files/naming_dot_replacement_empty_value_checks.gpr");
   Test_Prj ("files/naming_implementation_empty_value_checks.gpr");
   Test_Prj ("files/naming_separate_suffix_empty_value_checks.gpr");
   Test_Prj ("files/naming_spec_empty_value_checks.gpr");
   Test_Prj ("files/naming_specification_empty_value_checks.gpr");
   Test_Prj ("files/project_dir_empty_value_checks.gpr");
   Test_Prj ("files/source_list_file_empty_value_checks.gpr");
   Test_Prj ("files/warning_message_empty_value_checks.gpr");

   Test_Prj ("files/excluded_source_dirs_empty_space_value_checks.gpr");
   Test_Prj ("files/interfaces_empty_string_value_checks.gpr");
   Test_Prj ("files/interfaces_space_value_checks.gpr");
   Test_Prj ("files/library_interfaces_empty_string_value_checks.gpr");
   Test_Prj ("files/library_interfaces_space_value_checks.gpr");
   Test_Prj ("files/languages_empty_string_checks.gpr");
   Test_Prj ("files/locally_removed_files_empty_string_checks.gpr");
   Test_Prj ("files/source_dirs_space_checks.gpr");
   Test_Prj ("files/source_files_empty_string_checks.gpr");
   Test_Prj ("files/naming_exceptions_space_checks.gpr");
   Test_Prj ("files/naming_implementation_exceptions_empty_string_checks.gpr");
   Test_Prj ("files/naming_specification_exceptions_empty_string_checks.gpr");
   Test_Prj ("files/aggregate_space_value_checks.gpr");

   Test_Prj ("files/import_empty_string_check.gpr");
   Test_Prj ("files/import_space_check.gpr");
   Test_Prj ("files/extends_empty_string_check.gpr");
   Test_Prj ("files/extends_space_check.gpr");
end Main;
