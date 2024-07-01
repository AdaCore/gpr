--  This example helps gnatcoll-projects based tools conversion to gpr2.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNAT.Strings;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Tribooleans;
with GNATCOLL.VFS;

pragma Warnings (Off, ".* is not referenced");
--  need visibility on it, else source iterator is not visible
--  as the Project.View package only has limited visibility on it.
with GPR2.Build.Source.Sets;
pragma Warnings (On, ".* is not referenced");
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

with GPR2_GNATCOLL_Projects; use GPR2_GNATCOLL_Projects;

procedure Conversion_Tutorial is

   --  command line argument handling

   package Args is
      Parser : Argument_Parser := Create_Argument_Parser
        (Help =>
           "GPR2 Project parsing example for GNATCOLL.Projects tools.");

      package Project is new Parse_Option
        (Parser      => Parser,
         Short       => "-P",
         Long        => "--project",
         Arg_Type    => Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Help        => "Project file to use");

      package Scenario_Vars is new Parse_Option_List
        (Parser     => Parser,
         Short      => "-X",
         Long       => "--variables",
         Arg_Type   => Unbounded_String,
         Accumulate => True,
         Help       => "Scenario variables to pass to the project file");

      package Target is new Parse_Option
        (Parser       => Parser,
         Short       => "-t",
         Long         => "--target",
         Arg_Type     => Unbounded_String,
         Default_Val  => Null_Unbounded_String,
         Help         => "Custom target to use");

      package Runtime is new Parse_Option
        (Parser       => Parser,
         Long         => "--RTS",
         Arg_Type     => Unbounded_String,
         Default_Val  => Null_Unbounded_String,
         Help         => "Custom runtime to use");

      package Recursive is new Parse_Flag
        (Parser => Parser,
         Short  => "-r",
         Long   => "--recursive",
         Help   => "Include sources from all dependencies");

      package Config_File is new Parse_Option
        (Parser       => Parser,
         Long         => "--config",
         Arg_Type     => Unbounded_String,
         Default_Val  => Null_Unbounded_String,
         Help         => "Specify the main config project file name");

      package Subdirs is new Parse_Option
        (Parser       => Parser,
         Long         => "--subdirs",
         Arg_Type     => Unbounded_String,
         Default_Val  => Null_Unbounded_String,
         Help         => "Use dir as suffix to obj/lib/exec directories");

      package Src_Subdirs is new Parse_Option
        (Parser       => Parser,
         Long         => "--src-subdirs",
         Arg_Type     => Unbounded_String,
         Default_Val  => Null_Unbounded_String,
         Help         =>
            "Prepend <obj>/dir to the list of source dirs for each project");
   end Args;

   function Init_Project return Boolean;
   --  Initializes the project tree using command line options supplied by
   --  Args. Returns True on success, or False otherwise.

   Project_Tree : GPR2.Project.Tree.Object;
   --  GPR2 project tree object.

   --  GNATCOLL.Projects.Project_Type is converted to GPR2.Project.View.Object

   ------------------
   -- Init_Project --
   ------------------

   function Init_Project return Boolean is
      Opt               : GPR2.Options.Object;

      use GPR2;
   begin
      if Args.Project.Get /= Null_Unbounded_String then
         Opt.Add_Switch (Options.P, To_String (Args.Project.Get));
      end if;

      for Assoc of Args.Scenario_Vars.Get loop
         Opt.Add_Switch (Options.X, To_String (Assoc));
      end loop;

      if Args.Target.Get /= Null_Unbounded_String then
         Opt.Add_Switch (Options.Target, To_String (Args.Target.Get));
      end if;

      if Args.Runtime.Get /= Null_Unbounded_String then
         Opt.Add_Switch (Options.RTS, To_String (Args.Runtime.Get), "Ada");
      end if;

      if Args.Config_File.Get /= Null_Unbounded_String then
         Opt.Add_Switch (Options.Config, To_String (Args.Config_File.Get));
      end if;

      if Args.Subdirs.Get /= Null_Unbounded_String then
         Opt.Add_Switch (Options.Subdirs, To_String (Args.Subdirs.Get));
      end if;

      if Args.Src_Subdirs.Get /= Null_Unbounded_String then
         Opt.Add_Switch (Options.Src_Subdirs,
                         To_String (Args.Src_Subdirs.Get));
      end if;

      return Project_Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error);
   end Init_Project;

   Last_Source : GPR2.Path_Name.Object;

   use GPR2;

begin

   if not Args.Parser.Parse then
      return;
   end if;

   if not Init_Project then
      Put_Line ("Could not initialize project.");
      return;
   end if;

   --  List the source files for project
   for Cursor in Project_Tree.Iterate
     (Kind =>
        (GPR2.Project.I_Project => True,
         GPR2.Project.I_Runtime => False,
         GPR2.Project.I_Configuration => False,
         others => Args.Recursive.Get),
      Status =>
        (GPR2.Project.S_Externally_Built => GNATCOLL.Tribooleans.False))
   loop
      for Source of GPR2.Project.Tree.Element (Cursor).Sources loop
         Last_Source := Source.Path_Name;
         Put_Line (Source.Path_Name.String_Value);
      end loop;
   end loop;

   --  GNATCOLL.Projects.Object_Dir
   Put_Line ("Object_Dir:" &
               Object_Dir (Project_Tree.Root_Project).Display_Full_Name);

   --  GNATCOLL.Projects.Artifacts_Dir
   Put_Line ("Artifacts_Dir:" &
               Artifacts_Dir (Project_Tree.Root_Project).Display_Full_Name);

   --  GNATCOLL.Projects.Name
   Put_Line ("Name:" & Name (Project_Tree.Root_Project));

   --  GNATCOLL_Projects.Get_Runtime
   Put_Line ("Runtime:" & Get_Runtime (Project_Tree.Root_Project));

   if Last_Source.Is_Defined then
      --  To_Pathname functions
      Put_Line ("To_Pathnane:" & String
                (GPR2.Path_Name.Create
                   (GNATCOLL.VFS.Filesystem_String
                      (Last_Source.Value)).Name));
      Put_Line ("To_Pathname:" & String
                (GPR2.Path_Name.Create (GNATCOLL.VFS.Create
                   (GNATCOLL.VFS.Filesystem_String
                      (Last_Source.Value))).Name));
   end if;

   --  GNATCOLL.Projects.Register_New_Attribute
   Put_Line ("Register_New_Attribute returned:" &
               Register_New_Attribute
               (Name                 => (+"Conversion_Tutorial", +"Test"),
                Is_List              => True,
                Indexed              => True,
                Case_Sensitive_Index => True));

   declare
      Languages : GNAT.Strings.String_List_Access;
   begin
      Languages := Attribute_Value
        (Project => Project_Tree.Root_Project,
         Name    => (+"", +"Languages"));

      if Languages'Length > 0 then
         Put_Line ("Languages'First:" & Languages (Languages'First).all);
      end if;

      GNAT.Strings.Free (Arg => Languages);

      Put_Line ("Attribute_Value(Object_Dir):" &
                  Attribute_Value
                  (Project => Project_Tree.Root_Project,
                   Name    => (+"", +"Object_Dir")));
   end;

   Project_Tree.Unload;
end Conversion_Tutorial;
