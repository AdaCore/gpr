with GPR2.Build.ALI_Parser;
with GPR2.Path_Name;

with Test_Assert; use Test_Assert;

function Test return Integer is
   package Parser renames GPR2.Build.ALI_Parser;
   ALI : Parser.Object := Parser.Create
     (GPR2.Path_Name.Create_File ("input.ali"));
begin
   Assert (ALI.Parse, "Parse the ALI file");

   if ALI.Is_Parsed then
      Assert (ALI.Spec_Needs_Body, "Read the final BN flag after NE and PK");
   end if;

   return Report;
end Test;
