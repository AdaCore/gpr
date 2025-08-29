with Ada.Command_Line;
with GNAT.OS_Lib;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Exchange;
with GPR2.Options;
procedure gnatprove is
   use GPR2;
   Prove_Package : constant GPR2.Package_Id := +"Prove_Test";
   Prove_Attribute : constant GPR2.Q_Attribute_Id :=
                       (Prove_Package, +"Prove_Test");
   Options : GPR2.Options.Object;
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
   if Ada.Command_Line.Argument_Count > 0 and then
     Ada.Command_Line.Argument (1) = GPR2.Options.Print_GPR_Registry_Option
   then
      Options.Add_Switch (GPR2.Options.Print_GPR_Registry);
      Options.Print_GPR_Registry;
   end if;
   GNAT.OS_Lib.OS_Exit (1);
end gnatprove;
