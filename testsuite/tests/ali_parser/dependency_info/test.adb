with Ada.Containers;
with Ada.Directories;

with GPR2.Build.ALI_Parser;
with GPR2.Build.Dependencies;
with GPR2.Path_Name;

with Test_Assert; use Test_Assert;

function Test return Integer is
   package Parser renames GPR2.Build.ALI_Parser;
   package BD     renames GPR2.Build.Dependencies;

   use type Ada.Containers.Count_Type;
   use type BD.Set;

   ALI   : Parser.Object :=
             Parser.Create
               (GPR2.Path_Name.Create_File ("tree/obj/main.ali"));
   Multi : Parser.Object :=
             Parser.Create
               (GPR2.Path_Name.Create_File ("tree/obj/shared~2.ali"));

   Spacing : Parser.Object :=
               Parser.Create
                 (GPR2.Path_Name.Create_File ("tree/obj/token_spacing.ali"));

   function Has
     (Deps : BD.Set; Filename, Name : String; Is_Spec : Boolean)
      return Boolean
   is
     (BD.Filenames.Contains (Deps, GPR2.Filename_Type (Filename))
      and then
        BD.Filenames.Element
          (Deps, GPR2.Filename_Type (Filename)).Units.Contains
            ((Name_Length => Name'Length,
              Name        => GPR2.Name_Type (Name),
              Part        => (if Is_Spec then GPR2.S_Spec else GPR2.S_Body))));
begin
   Assert (ALI.Parse, "Parse the compiled main's ALI");
   Assert (Multi.Parse, "Parse the compiled multi-unit source's ALI");
   Assert (Spacing.Parse, "Parse single-space fields and a legacy filename");
   if not ALI.Is_Parsed
     or else not Multi.Is_Parsed
     or else not Spacing.Is_Parsed
   then
      return Report;
   end if;

   Assert
     (Has (Spacing.Dependencies, "main.adb", "main", False)
      and then Has
        (Spacing.Dependencies, "file with spaces.ads", "quoted", True),
      "Whitespace-delimited fields preserve dependency units and filenames");

   declare
      Info       : constant BD.Set := ALI.Dependencies;
      Multi_Info : constant BD.Set := Multi.Dependencies;
   begin
      Assert
        (Has (Info, "main.adb", "main", False),
         "The main body retains its filename and unit name");
      Assert
        (Has (Info, "shared.ada", "multi", True),
         "A withed unit retains its spec identity in a multi-unit source");
      Assert
        (Has (Info, "file with spaces.ads", "quoted", True),
         "Quoted dependency filenames retain their spaces");
      Assert
        (Has (Multi_Info, "shared.ada", "multi", False)
         and then Has (Multi_Info, "shared.ada", "multi", True),
         "Spec and body dependencies sharing a filename remain distinct");
      Assert
        (BD.Filenames.Contains (ALI.Dependencies, "file with spaces.ads")
         and then BD.Filenames.Contains (ALI.Dependencies, "shared.ada"),
         "Dependency filenames remain available as dependency records");

      Assert
        (BD.Filenames.Element (Multi_Info, "shared.ada").Units.Length = 2,
         "One file entry groups both spec and body dependencies");
   end;

   return Report;
end Test;
