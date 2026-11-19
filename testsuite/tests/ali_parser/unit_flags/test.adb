with Ada.Containers;

with GPR2.Build.ALI_Parser;
with GPR2.Path_Name;

with Test_Assert; use Test_Assert;

function Test return Integer is
   package Parser renames GPR2.Build.ALI_Parser;
   use type Ada.Containers.Count_Type;
   use type GPR2.Name_Type;
   use type Parser.Library_Unit_Kind;
   use type Parser.Spec_Body;

   Pkg : Parser.Object := Parser.Create
     (GPR2.Path_Name.Create_File ("tree/obj/pkg.ali"));
   Main : Parser.Object := Parser.Create
     (GPR2.Path_Name.Create_File ("tree/obj/main.ali"));

   procedure Check_Package_Spec
     (Name : GPR2.Name_Type; Attribute : Parser.Unit_Attribute);

   procedure Check_Package_Spec
     (Name : GPR2.Name_Type; Attribute : Parser.Unit_Attribute)
   is
      ALI : Parser.Object := Parser.Create
        (GPR2.Path_Name.Create_File
           (GPR2.Filename_Type ("tree/obj/" & String (Name) & ".ali")));
   begin
      Assert (ALI.Parse, "Parse " & String (Name));
      if not ALI.Is_Parsed then
         return;
      end if;

      Assert (ALI.Units.Length = 1, String (Name) & ": one unit");
      for Info of ALI.Units loop
         Assert (Info.Name = Name, "Unit name");
         Assert (Info.Part = Parser.U_Spec, "Specification");
         Assert (Info.Kind = Parser.Is_Package, "Package");
         Assert (Info.Attributes (Attribute), Attribute'Image);
      end loop;
   end Check_Package_Spec;
begin
   Assert (Pkg.Parse, "Parse pkg");
   if Pkg.Is_Parsed then
      Assert (Pkg.Units.Length = 2, "Package body and spec");
      if Pkg.Units.Length = 2 then
         Assert (Pkg.Units.First_Element.Part = Parser.U_Body, "Body first");
         Assert (Pkg.Units.Last_Element.Part = Parser.U_Spec, "Spec second");
      end if;
      for Info of Pkg.Units loop
         Assert (Info.Name = "pkg", "Package name");
         Assert (Info.Kind = Parser.Is_Package, "Package kind");
         Assert
           (Info.Attributes (Parser.Elaborate_Body)
              = (Info.Part = Parser.U_Spec),
            "EB belongs to the spec");
         Assert (not Info.Attributes (Parser.Is_Generic), "Not generic");
      end loop;
   end if;

   Assert (Main.Parse, "Parse main");
   if Main.Is_Parsed then
      Assert (Main.Units.Length = 1, "One procedure body");
      for Info of Main.Units loop
         Assert (Info.Name = "main", "Procedure name");
         Assert (Info.Part = Parser.U_Body, "Body");
         Assert (Info.Kind = Parser.Is_Subprogram, "Subprogram");
      end loop;
   end if;

   Check_Package_Spec ("pure_pkg", Parser.Pure);
   Check_Package_Spec ("preelab_pkg", Parser.Preelab);
   Check_Package_Spec ("generic_pkg", Parser.Is_Generic);

   return Report;
end Test;
