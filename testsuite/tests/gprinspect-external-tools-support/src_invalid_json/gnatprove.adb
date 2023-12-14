with Ada.Text_IO;
with GNAT.OS_Lib;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Exchange;
procedure gnatprove is
   use GPR2;
   Prove_Package : constant GPR2.Package_Id := +"Prove_Test";
   Prove_Attribute : constant GPR2.Q_Attribute_Id :=
                       (Prove_Package, +"Prove_Test");
begin
   GPR2.Project.Registry.Pack.Add
     (Name     => Prove_Package,
      Projects => GPR2.Project.Registry.Pack.Everywhere);
   GPR2.Project.Registry.Attribute.Add
     (Name                  => Prove_Attribute,
      Index_Type            => GPR2.Project.Registry.Attribute.No_Index,
      Value                 => GPR2.Project.Registry.Attribute.Single,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => GPR2.Project.Registry.Attribute.Everywhere);
   Ada.Text_IO.Put ("invalid json");
   GPR2.Project.Registry.Exchange.Export;
   GNAT.OS_Lib.OS_Exit (0);
end gnatprove;
