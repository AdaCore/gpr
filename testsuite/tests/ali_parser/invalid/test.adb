with GPR2;
with GPR2.Build.ALI_Parser;
with GPR2.Path_Name;

with Test_Assert; use Test_Assert;

function Test return Integer is
   package Parser renames GPR2.Build.ALI_Parser;

   procedure Check_Invalid (Name : String) is
      Path : constant GPR2.Path_Name.Object := GPR2.Path_Name.Create_File
        (GPR2.Filename_Type ("files/" & Name & ".ali"));
      ALI : Parser.Object := Parser.Create (Path);
   begin
      Assert (Path.Exists, "Fixture exists: " & Name);
      Assert (not ALI.Parse, "Reject " & Name);
      Assert (not ALI.Is_Parsed, "Invalid ALI is not marked parsed: " & Name);
   end Check_Invalid;

   Missing : Parser.Object := Parser.Create
     (GPR2.Path_Name.Create_File ("does-not-exist.ali"));
begin
   Assert (not Missing.Parse, "Reject missing ALI");
   Assert (not Missing.Is_Parsed, "Missing ALI is not marked parsed");

   Check_Invalid ("empty");
   Check_Invalid ("header_only");
   Check_Invalid ("missing_dependency_separator");
   Check_Invalid ("missing_dependency_filename");
   Check_Invalid ("missing_timestamp");
   Check_Invalid ("missing_checksum");
   Check_Invalid ("missing_unit_separator");
   Check_Invalid ("missing_unit_name");
   Check_Invalid ("invalid_unit_name");
   Check_Invalid ("missing_with_separator");
   Check_Invalid ("missing_with_name");
   Check_Invalid ("invalid_with_name");
   Check_Invalid ("unclosed_quote");
   Check_Invalid ("newline_in_quote");
   Check_Invalid ("character_after_quote");

   return Report;
end Test;
