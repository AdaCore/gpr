with GPR2.Build.ALI_Parser;
with GPR2.Path_Name;

with Test_Assert; use Test_Assert;

function Test return Integer is
   package Parser renames GPR2.Build.ALI_Parser;
   ALI : Parser.Object := Parser.Create
     (GPR2.Path_Name.Create_File ("input.ali"));
   Header_Only : Parser.Object := Parser.Create
     (GPR2.Path_Name.Create_File ("header_only.ali"));
   Dependencies_Only : Parser.Object := Parser.Create
     (GPR2.Path_Name.Create_File ("dependencies_only.ali"));
begin
   Assert
     (ALI.Parse and then ALI.Version = "v27.20260922",
      "Read the version from the first line");
   Assert
     (not Header_Only.Parse and then not Header_Only.Is_Parsed,
      "Reject ALIs without dependency records");
   Assert
     (Dependencies_Only.Parse
      and then not Dependencies_Only.Dependencies.Is_Empty,
      "Dependency parsing does not require version or unit records");
   return Report;
end Test;
