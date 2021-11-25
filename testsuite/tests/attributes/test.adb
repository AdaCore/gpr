with Test_GPR;
with Test_Assert;
with GPR2.Project.Tree;
with GPR2.Containers; use GPR2.Containers;
with GPR2.Source_Reference.Value;
with GNAT.IO;

function Test return Integer is

   package TGPR renames Test_GPR;
   package A renames Test_Assert;
   package IO renames GNAT.IO;

   Tree          : GPR2.Project.Tree.Object;
   Comp_Switches : Name_List;
   Languages     : Name_List;
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
   Languages.Append("ada");
   Languages.Append("C");
   TGPR.Assert_Attribute
      (Tree.Root_Project,
       Name  => "Languages",
       Value => Languages);

   IO.Put_Line ("test pattern matching");
   Name_Type_List.Append (Comp_Switches, "-Ospecial");

   TGPR.Assert_Attribute
      (Tree.Root_Project,
       Name  => "Switches",
       Pkg   => "Compiler",
       Index => "myadaunit.ads",
       Value => Comp_Switches);

   IO.Put_Line ("test config concatenation");
   Comp_Switches.Clear;
   Comp_Switches.Append ("-config-ada");
   Comp_Switches.Append ("-test-switch");
   TGPR.Assert_Attribute
      (Tree.Root_Project,
       Name  => "Switches",
       Pkg   => "Compiler",
       Index => "Ada",
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

   return A.Report;
end Test;
