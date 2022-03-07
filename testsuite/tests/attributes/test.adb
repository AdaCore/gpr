with Test_GPR;
with Test_Assert;
with GPR2.Project.Tree;
with GPR2.Containers; use GPR2.Containers;
with GPR2.Source_Reference.Value;
with GNAT.IO;
with System.OS_Constants;

function Test return Integer is

   package TGPR renames Test_GPR;
   package A renames Test_Assert;
   package IO renames GNAT.IO;

   Tree          : GPR2.Project.Tree.Object;
   Comp_Switches : Name_List;
   Languages     : Name_List;
   File_List     : Name_List;
begin
   TGPR.Load_With_No_Errors
      (Tree,
       "./gpr/root.gpr",
       Config_Filename => "./gpr/config.gpr",
       Load_Source_List => True);

   --  Basic attribute retrieval
   IO.Put_Line ("Check object dir value defined explicitely");
   TGPR.Assert_Attribute
      (Tree.Root_Project, "object_dir", Value => "myobj");
   IO.Put_Line ("Check that exec_dir default to object_dir value");
   TGPR.Assert_Attribute
      (Tree.Root_Project, "exec_dir", Value => "myobj");

   IO.Put_Line ("Check that library_name is inherited");
   TGPR.Assert_Attribute
      (Tree.Root_Project, "library_name", Value => "mylib");

   IO.Put_Line ("Check default value for Library_Kind");
   TGPR.Assert_Attribute
     (Tree.Root_Project, "library_kind", Value => "static");

   IO.Put_Line
      ("Check that archive_suffix is not inherited and that default is used");
   TGPR.Assert_Attribute
      (Tree.Root_Project, "archive_suffix", Value => ".a");

   IO.Put_Line ("Check package inheritance mechanism");
   TGPR.Assert_Attribute
      (Tree.Root_Project, "install_name", Pkg => "install", Value => "myname");
   IO.Put_Line
      ("  Mode value should not be inherited from extended as " &
       "packages are inherited as a whole");
   TGPR.Assert_Attribute_Not_Defined
      (Tree.Root_Project, "mode", Pkg => "install");

   IO.Put_Line ("Check languages (inherited and concatenated from extended)");
   Languages.Append("C");
   Languages.Append("Ada");
   TGPR.Assert_Attribute
      (Tree.Root_Project,
       Name  => "Languages",
       Value => Languages);

   IO.Put_Line ("Check multi value assignment");
   --  full associative array in pkg
   Comp_Switches.Clear;
   Comp_Switches.Append ("ada_switch");
   TGPR.Assert_Attribute
     (Tree.Root_Project,
      Name  => "Switches",
      Pkg   => "Linker",
      Index => "ada",
      Value => Comp_Switches);
   Comp_Switches.Clear;
   Comp_Switches.Append ("c_switch");
   TGPR.Assert_Attribute
     (Tree.Root_Project,
      Name  => "Switches",
      Pkg   => "Linker",
      Index => "c",
      Value => Comp_Switches);
   --  full associative array at top level
   TGPR.Assert_Attribute
     (Tree.Root_Project,
      Name  => "Runtime",
      Index => "c",
      Value => "foo");
   TGPR.Assert_Attribute
     (Tree.Root_Project,
      Name  => "Runtime",
      Index => "ada",
      Value => "bar");
   --  full associative array concatenation from config
   TGPR.Assert_Attribute
     (Tree.Root_Project,
      Name  => "Runtime",
      Index => "c++",
      Value => "baz");

   IO.Put_Line ("test default value for config attributes");
   TGPR.Assert_Attribute
     (Tree.Root_Project,
      Name  => "Target",
      Value => "all2");


   IO.Put_Line ("test pattern matching");
   Comp_Switches.Clear;
   --  Ada switch from config
   Comp_Switches.Append ("-config-ada");
   --  File-specific switch from project
   Comp_Switches.Append ("-Ospecial");

   TGPR.Assert_Attribute
      (Tree.Root_Project,
       Name  => "Switches",
       Pkg   => "Compiler",
       Index => "myadaunit.ads",
       Value => Comp_Switches);

   IO.Put_Line ("test pattern matching vs. language index");
   Comp_Switches.Clear;
   Comp_Switches.Append ("-g");

   TGPR.Assert_Attribute
     (Tree.Root_Project,
       Name  => "Switches",
       Pkg   => "Compiler",
       Index => "c",
       Value => Comp_Switches);

   IO.Put_Line ("test config concatenation");
   Comp_Switches.Clear;
   Comp_Switches.Append ("-config-ada");
   Comp_Switches.Append ("-test-switch");
   TGPR.Assert_Attribute
      (Tree.Root_Project,
       Name  => "Switches",
       Pkg   => "Compiler",
       Index => "ada",
       Value => Comp_Switches);


   IO.Put_Line ("test invalid attributes");
   TGPR.Assert_Attribute_Error
     (Tree.Root_Project,
      Name  => "Inexistent_Attribute");

   TGPR.Assert_Attribute_Error
     (Tree.Root_Project,
      Name => "Inexistent_Attribute",
      Pkg  => "Compiler");

   TGPR.Assert_Attribute_Error
     (Tree.Root_Project,
      Name => "Inexistent_Attribute",
      Pkg  => "Inexistent_Package");

   IO.Put_Line ("test attribute alias names");
   TGPR.Assert_Attribute
     (Tree.Root_Project,
      Name  => "Spec",
      Pkg   => "Naming",
      Index => "myadaunit",
      Value => "myadaunit.ads");
   TGPR.Assert_Attribute
     (Tree.Root_Project,
      Name  => "Specification",
      Pkg   => "Naming",
      Index => "myadaunit",
      Value => "myadaunit.ads");

   File_List.Append("file.adb");
   File_List.Append("file.ads");
   TGPR.Assert_Attribute
     (Tree.Root_Project,
      Name  => "Locally_Removed_Files",
      Value => File_List);
   TGPR.Assert_Attribute
     (Tree.Root_Project,
      Name  => "Excluded_Source_Files",
      Value => File_List);

   return A.Report;
end Test;
