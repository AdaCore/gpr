with Ada.Text_IO;
with GPR2.Build.ALI_Parser;
with GPR2.Containers;
with GPR2.Path_Name;
with GNATCOLL.Traces;

procedure Main is
   procedure Test (ALI_File : String);
   -- Print dependencies name

   procedure Test (ALI_File : String) is
      Dep_Names : GPR2.Containers.Filename_Set;
   begin
      Ada.Text_IO.Put_Line ("== ALI file: " & ALI_File);

      if GPR2.Build.ALI_Parser.Dependencies
        (GPR2.Path_Name.Create_File (GPR2.Filename_Type (ALI_File)),
         Dep_Names)
      then
         for Dep of Dep_Names loop
            Ada.Text_IO.Put_Line (String (Dep));
         end loop;
      else
         Ada.Text_IO.Put_Line ("Failed to parse dependencies");
      end if;
      Ada.Text_IO.Put_Line ("");
   end Test;

begin

   GNATCOLL.Traces.Parse_Config_File;

   Test ("ali_files/5.04a1/main.ali");
   Test ("ali_files/7.2.2/main.ali");
   Test ("ali_files/7.3.2/main.ali");
   Test ("ali_files/wave/main.ali");
   Test ("ali_files/name_with_quote.ali");

   Test ("ali_files/invalid/character_after_ending_quote.ali");
   Test ("ali_files/invalid/line_feed_before_closing_quote.ali");
   Test ("ali_files/invalid/unclosed_quote.ali");
   Test ("ali_files/invalid/missing_dependency_field.ali");
   Test ("ali_files/invalid/no_blank_line_at_the_end.ali");
   Test ("ali_files/invalid/dependency_concatenated.ali");
end Main;
