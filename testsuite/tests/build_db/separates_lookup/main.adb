with Ada.Text_IO;

with GPR2;
with GPR2.Build.Compilation_Unit;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;

   procedure Test (Name : GPR2.Name_Type) is
      use type GPR2.Optional_Name_Type;
      use GPR2.Build.Compilation_Unit;
      use Ada.Text_IO;

      CU   : GPR2.Build.Compilation_Unit.Object;
      Loc  : GPR2.Build.Compilation_Unit.Unit_Location;

   begin
      --  Check retrieval of the full compilation unit
      Put_Line ("Lookup compilation unit for " & String (Name));
      CU := Tree.Root_Project.Unit (Name);

      if not CU.Is_Defined then
         Put_Line ("... not found");
         return;
      end if;

      Put_Line ("... found CU: " & String (CU.Name));

      --  Check the retrieval of the individual parts
      --  Spec:
      Loc := Tree.Root_Project.Unit_Part (CU.Name, True);
      if Loc = No_Unit then
         Put_Line ("    no spec for " & String (CU.Name));
      else
         Put_Line ("    spec: " & String (Loc.Source.Simple_Name));
      end if;

      --  Body:
      Loc := Tree.Root_Project.Unit_Part (CU.Name, False);
      if Loc = No_Unit then
         Put_Line ("    no body for " & String (CU.Name));
      else
         Put_Line ("    body: " & String (Loc.Source.Simple_Name));
      end if;

      --  Separate:
      if Name /= CU.Name then
         Loc := Tree.Root_Project.Unit_Part (Name, False);
         Put_Line ("    sep.: " & String (Loc.Source.Simple_Name));
      end if;
   end Test;

   use GPR2.Options;
begin
   Opt.Add_Switch (P, "trees/demo.gpr");

   if not Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
      return;
   end if;

   if not Tree.Update_Sources then
      return;
   end if;

   Test ("Pkg.Child");
   Test ("PKG.CHILD.child2");
   Test ("Pkg2.Child");
   Test ("Pkg3");
   Test ("PkgUnk");
   Test ("P1.P");

end Main;
