with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Exceptions;
with GNAT.OS_Lib;

with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Source;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Source;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display_Source (Src : GPR2.Source.Object'Class);
   procedure Test_Prj (Fname : Filename_Type);

   procedure Display_Source (Src : GPR2.Source.Object'Class) is
   begin
      Text_IO.Put_Line (String (Src.Path_Name.Simple_Name) & ": " & Src.Kind'Image);
   end Display_Source;

   procedure Test_Prj (Fname : Filename_Type)
   is
      Prj : Project.Tree.Object;
      Ctx : Context.Object;

   begin
      Text_IO.Put_Line ("GPR file: " & String (Fname));
      Project.Tree.Load (Prj, Create (Fname), Ctx);
      Prj.Update_Sources;
      for V of reverse Prj.Ordered_Views loop
         Text_IO.Put_Line (String (V.Name));
         for S of V.Sources loop
            Display_Source (S);
         end loop;
      end loop;
   exception
      when Project_Error =>
         Text_IO.Put_Line ("Messages found:");

         for C in Prj.Log_Messages.Iterate
           (False, True, True, True, True)
         loop
            declare
               use Ada.Strings;
               use Ada.Strings.Fixed;
               DS  : Character renames GNAT.OS_Lib.Directory_Separator;
               M   : constant Message.Object := Log.Element (C);
               Mes : constant String := M.Format;
               L   : constant Natural :=
                       Fixed.Index (Mes, DS & "source_files" & DS);
            begin
               if L /= 0 then
                  Text_IO.Put_Line
                    (Replace_Slice
                       (Mes,
                        Fixed.Index
                          (Mes (1 .. L), """", Going => Backward) + 1,
                        L - 1,
                        "<path>"));
               else
                  Text_IO.Put_Line (Mes);
               end if;
            end;
         end loop;
      when E : others =>
         Ada.Text_IO.Put_Line
           ("Exception raised " & Ada.Exceptions.Exception_Name (E) & " : " &
              Ada.Exceptions.Exception_Message (E));

         for C in Prj.Log_Messages.Iterate
           (False, True, True, True, True)
         loop
            Text_IO.Put_Line (Log.Element (C).Format);
         end loop;
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