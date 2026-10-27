with GPR2.Build.ALI_Parser;
with GPR2.Path_Name;

with Test_Assert; use Test_Assert;

function Test return Integer is
   package ALI_Parser renames GPR2.Build.ALI_Parser;

   ALI : ALI_Parser.Object := ALI_Parser.Create
     (GPR2.Path_Name.Create_File ("ali_files/pkg.ali"));
begin
   Assert
     (ALI.Parse and then ALI.Spec_Needs_Body,
      "Read the BN flag after other unit flags");
   return Report;
end Test;
