with GPR2.Build.ALI_Parser;
with GPR2.Path_Name;

with Test_Assert; use Test_Assert;

function Test return Integer is
   package Parser renames GPR2.Build.ALI_Parser;
   ALI : Parser.Object := Parser.Create
     (GPR2.Path_Name.Create_File ("input.ali"));
begin
   Assert
     (ALI.Parse and then ALI.Version = "v27.20260922",
      "Read the version from the first line");
   return Report;
end Test;
