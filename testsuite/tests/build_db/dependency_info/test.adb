with GPR2.Build.Actions.Process.Compile.Ada;
with GPR2.Build.Compilation_Unit;
with GPR2.Build.Dependencies;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Reporter.Console;

with Test_Assert; use Test_Assert;

function Test return Integer is
   package Compile renames GPR2.Build.Actions.Process.Compile.Ada;
   package BD      renames GPR2.Build.Dependencies;

   use type BD.Set;

   Tree : GPR2.Project.Tree.Object;
   Opts : GPR2.Options.Object;
   Comp : Compile.Object;

   procedure Check_Unavailable (Name : GPR2.Name_Type);

   procedure Check_Unavailable (Name : GPR2.Name_Type) is
      Unit : constant GPR2.Build.Compilation_Unit.Object :=
        Tree.Root_Project.Own_Unit (Name);
      Act : Compile.Object;
   begin
      Assert (Unit.Is_Defined, "Find unit " & String (Name));
      if not Unit.Is_Defined then
         return;
      end if;
      Act.Initialize (Unit);
      Assert (not Act.ALI_Is_Parsed, "ALI is initially unparsed");
      Assert
        (Act.Dependencies.Is_Empty,
         "No dependency details for " & String (Name));
      Assert
        (not Act.ALI_Is_Parsed, "An unavailable ALI is not marked parsed");
   end Check_Unavailable;
begin
   Opts.Add_Switch (GPR2.Options.P, "tree/prj.gpr");
   Assert
     (Tree.Load
        (Opts, True, GPR2.Reporter.Console.Create (GPR2.Reporter.Quiet)),
      "Load the project");
   if not Tree.Is_Defined then
      return Report;
   end if;
   Tree.Update_Sources (GPR2.Sources_Units);

   declare
      Unit : constant GPR2.Build.Compilation_Unit.Object :=
        Tree.Root_Project.Own_Unit ("Pkg");
   begin
      Assert (Unit.Is_Defined, "Find package compilation unit");
      if not Unit.Is_Defined then
         return Report;
      end if;
      Comp.Initialize (Unit);
   end;

   Assert
     (not Comp.ALI_Is_Parsed, "Initialize does not eagerly parse the ALI");
   declare
      Info : constant BD.Set := Comp.Dependencies;
   begin
      Assert (Comp.ALI_Is_Parsed, "Dependencies parses the ALI lazily");
      Assert
        (BD.Filenames.Element (Info, "pkg.ads").Units.Contains
           ((Name_Length => 3, Name => "pkg", Part => GPR2.S_Spec)),
         "Return the spec filename and unit name");
      Assert
        (BD.Filenames.Element (Info, "pkg.adb").Units.Contains
           ((Name_Length => 3, Name => "pkg", Part => GPR2.S_Body)),
         "Return the body filename and unit name");
      Assert
        (BD.Filenames.Contains (Comp.ALI_Dependencies, "pkg.ads")
         and then BD.Filenames.Contains (Comp.ALI_Dependencies, "pkg.adb"),
         "The filename API agrees with the compiled dependency metadata");
      Assert
        (Comp.Dependencies = Info,
         "Repeated metadata requests preserve the parsed result");
   end;

   Check_Unavailable ("Missing");

   return Report;
end Test;
