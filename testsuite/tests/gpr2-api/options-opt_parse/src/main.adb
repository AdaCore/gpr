with GPR2.Project.Tree;      use GPR2.Project.Tree;
with GNATCOLL.Opt_Parse;     use GNATCOLL.Opt_Parse;
with GPR2.Options.Opt_Parse; use GPR2.Options.Opt_Parse;
with Ada.Text_IO;            use Ada.Text_IO;
with GNAT.OS_Lib;        use GNAT.OS_Lib;

procedure Main is
    Parser : Argument_Parser := Create_Argument_Parser (Help => "");

   package Load_Project is new
     Parse_Flag
       (Parser => Parser,
        Long   => "--load-project",
        Help   => "actually load the project and print the errors");

    package GPR_Args is new GPR2.Options.Opt_Parse.Args (Parser => Parser);

    Tree    : GPR2.Project.Tree.Object;
    Options : GPR2.Options.Object;
    Success : Boolean;
begin
   if not Parser.Parse then
      --  Handle the error
      Put_Line ("Could not parse the arguments.");
      OS_Exit (1);
   end if;

   Options := GPR_Args.Parsed_GPR2_Options;

   --  At this point, Options is initialized and ready to use
   if Load_Project.Get then
      Success :=
        Tree.Load
          (Options,
           With_Runtime     => False,
           Absent_Dir_Error => GPR2.No_Error);
   end if;

end Main;