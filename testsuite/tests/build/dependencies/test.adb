with Ada.Containers;
with GPR2.Build.Dependencies;
with Test_Assert; use Test_Assert;

function Test return Integer is
   package BD renames GPR2.Build.Dependencies;
   use type Ada.Containers.Count_Type;
   use type GPR2.Filename_Type;
   Deps : BD.Set;
begin
   BD.Include (Deps, "helper.c");
   Assert
     (BD.Filenames.Element (Deps, "helper.c").Units.Is_Empty,
      "A dependency can exist without unit information");
   BD.Include
     (Deps, "b.ada", (Name_Length => 1, Name => "b", Part => GPR2.S_Spec));
   BD.Include
     (Deps, "b.ada", (Name_Length => 1, Name => "b", Part => GPR2.S_Body));
   BD.Include
     (Deps, "b.ada", (Name_Length => 1, Name => "b", Part => GPR2.S_Spec));
   BD.Include (Deps, "b.ada");
   Assert (Deps.Length = 2, "Each filename has exactly one entry");
   Assert
     (BD.Filenames.Element (Deps, "b.ada").Units.Length = 2,
      "Keep both parts without duplicates or erasing existing information");
   BD.Include
     (Deps, "b.ada", (Name_Length => 1, Name => "c", Part => GPR2.S_Spec));
   Assert
     (BD.Filenames.Element (Deps, "b.ada").Units.Length = 3,
      "A source can also contain a different unit");
   for Dep of Deps loop
      if Dep.Filename = "helper.c" then
         Assert (Dep.Units.Is_Empty, "Iterate a complete unitless dependency");
      else
         Assert
           (Dep.Filename = "b.ada" and then Dep.Units.Length = 3,
            "Iterate the filename and its merged unit parts together");
      end if;
   end loop;
   return Report;
end Test;
