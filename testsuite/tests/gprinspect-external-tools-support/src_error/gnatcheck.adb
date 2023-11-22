with GNAT.OS_Lib;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Exchange;
procedure gnatcheck is
   use GPR2;
   Check_Package : constant GPR2.Package_Id := +"Check_Test";
   Check_Attribute : constant GPR2.Q_Attribute_Id :=
                       (Check_Package, +"Check_Test");
begin
   GPR2.Project.Registry.Pack.Add
     (Name     => Check_Package,
      Projects => GPR2.Project.Registry.Pack.Everywhere);
   GPR2.Project.Registry.Attribute.Add
     (Name                  => Check_Attribute,
      Index_Type            => GPR2.Project.Registry.Attribute.No_Index,
      Value                 => GPR2.Project.Registry.Attribute.Single,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => GPR2.Project.Registry.Attribute.Everywhere);
   GPR2.Project.Registry.Exchange.Export;
   GNAT.OS_Lib.OS_Exit (1);
end gnatcheck;
