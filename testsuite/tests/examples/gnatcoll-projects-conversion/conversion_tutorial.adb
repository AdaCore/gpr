--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This example helps gnatcoll-projects based tools conversion to gpr2.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNAT.Strings;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Tribooleans;
with GNATCOLL.VFS;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Configuration;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.View;

with GPR2_GNATCOLL_Projects; use GPR2_GNATCOLL_Projects;

procedure Conversion_Tutorial is

   --  command line argument handling

   package Args is

      Parser : Argument_Parser := Create_Argument_Parser
        (Help =>
           "GPR2 Project parsing example for GNATCOLL.Projects tools.");

      package Project_Arg is new Parse_Option
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

      package Target_Arg is new Parse_Option
        (Parser       => Parser,
         Short       => "-t",
         Long         => "--target",
         Arg_Type     => Unbounded_String,
         Default_Val  => Null_Unbounded_String,
         Help         => "Custom target to use");

      function Target return GPR2.Optional_Name_Type is
        (if Target_Arg.Get = Null_Unbounded_String then GPR2.No_Name
         else GPR2.Optional_Name_Type (To_String (Target_Arg.Get)));
      --  target provided in command line (default GPR2.No_Name)

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

      package Config_File_Arg is new Parse_Option
        (Parser       => Parser,
         Long         => "--config",
         Arg_Type     => Unbounded_String,
         Default_Val  => Null_Unbounded_String,
         Help         => "Specify the main config project file name");

      function Config_File return GPR2.Path_Name.Object is
        (if Config_File_Arg.Get = Null_Unbounded_String then
            GPR2.Path_Name.Undefined
         else
            GPR2.Path_Name.Create_File (GPR2.Filename_Type
                                        (To_String (Config_File_Arg.Get))));
      --  The GPR2 project file object (default GPR2.Path_Name.Undefined)

      package Subdirs_Arg is new Parse_Option
        (Parser       => Parser,
         Long         => "--subdirs",
         Arg_Type     => Unbounded_String,
         Default_Val  => Null_Unbounded_String,
         Help         => "Use dir as suffix to obj/lib/exec directories");

      function Subdirs return GPR2.Optional_Name_Type is
        (if Subdirs_Arg.Get = Null_Unbounded_String then GPR2.No_Name
         else GPR2.Optional_Name_Type (To_String (Subdirs_Arg.Get)));
      --  subdirs provided in command line (default GPR2.No_Name)

      package Src_Subdirs_Arg is new Parse_Option
        (Parser       => Parser,
         Long         => "--src-subdirs",
         Arg_Type     => Unbounded_String,
         Default_Val  => Null_Unbounded_String,
         Help         =>
            "Prepend <obj>/dir to the list of source dirs for each project");

      function Src_Subdirs return GPR2.Optional_Name_Type is
        (if Src_Subdirs_Arg.Get = Null_Unbounded_String then GPR2.No_Name
         else GPR2.Optional_Name_Type (To_String (Src_Subdirs_Arg.Get)));
      --  src-subdirs provided in command line (default GPR2.No_Name)

      package Unchecked_Shared_Lib is new Parse_Flag
        (Parser => Parser,
         Long   => "--unchecked-shared-lib-imports",
         Help   => "Shared lib projects may import any project");
   end Args;

   function Init_Project return Boolean;
   --  Initializes the project tree using command line options supplied by
   --  Args. Returns True on success, or False otherwise.

   procedure Insert_Scenario_Vars (Context : in out GPR2.Context.Object);
   --  insert in context scenario variables passed in command line

   function Project_File return GPR2.Path_Name.Object is
     (if Args.Project_Arg.Get = Null_Unbounded_String  then
         GPR2.Path_Name.Undefined
      else GPR2.Path_Name.Create_File
        (GPR2.Project.Ensure_Extension (GPR2.Filename_Type
                                        (To_String (Args.Project_Arg.Get)))));
   --  The GPR2 project file object (default GPR2.Path_Name.Undefined)

   Project_Tree : GPR2.Project.Tree.Object;
   --  GPR2 project tree object.

   Project_Env  : GPR2.Context.Object;
   --  gnatcoll.projects environment should be converted to context objects

   --  GNATCOLL.Projects.Project_Type is converted to GPR2.Project.View.Object

   ------------------
   -- Init_Project --
   ------------------

   function Init_Project return Boolean is
   begin
      declare
         Language_Runtimes : GPR2.Containers.Lang_Value_Map :=
           GPR2.Containers.Lang_Value_Maps.Empty_Map;
         Config  : GPR2.Project.Configuration.Object;

         use GPR2;
      begin
         if not Project_File.Is_Defined then
            Put_Line ("Project file not specified.");
            return False;
         end if;

         Insert_Scenario_Vars (Project_Env);

         Language_Runtimes.Insert (+GPR2.Optional_Name_Type'("ada"),
                                   GPR2.Value_Type'
                                     (To_String (Args.Runtime.Get)));

         if Args.Config_File.Is_Defined then
            Config := GPR2.Project.Configuration.Load (Args.Config_File);

            if Config.Has_Error then
               Output_Messages (Config.Log_Messages);
               return False;
            end if;

            Project_Tree.Load
              (Filename         => Project_File,
               Context          => Project_Env,
               Config           => Config,
               Subdirs          => Args.Subdirs,
               Src_Subdirs      => Args.Src_Subdirs,
               Check_Shared_Lib => not Args.Unchecked_Shared_Lib.Get);
         else

            Project_Tree.Load_Autoconf
              (Filename          => Project_File,
               Context           => Project_Env,
               Subdirs           => Args.Subdirs,
               Src_Subdirs       => Args.Src_Subdirs,
               Target            => Args.Target,
               Language_Runtimes => Language_Runtimes);
         end if;

         if not Project_Tree.Has_Runtime_Project then
            Put_Line ("Toolchain not found for this target/runtime.");
            return False;
         end if;

         return True;
      end;
   end Init_Project;

   --------------------------
   -- Insert_Scenario_Vars --
   --------------------------

   procedure Insert_Scenario_Vars (Context : in out GPR2.Context.Object) is
   begin
      for Assoc of Args.Scenario_Vars.Get loop
         declare
            A        : constant String := To_String (Assoc);
            Eq_Index : Natural := A'First;
         begin
            while Eq_Index <= A'Length and then A (Eq_Index) /= '=' loop
               Eq_Index := Eq_Index + 1;
            end loop;
            if Eq_Index not in A'Range then
               Put_Line ("Invalid scenario variable: -X" & A);
            else
               Context.Insert
                 (GPR2.Optional_Name_Type (A (A'First .. Eq_Index - 1)),
                  A (Eq_Index + 1 .. A'Last));
            end if;
         end;
      end loop;
   end Insert_Scenario_Vars;

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
      declare
         use GPR2.Project.Source.Set;
         --  GPR2.Project.Source.Set package required for Sources iterator
      begin
         for Source of GPR2.Project.Tree.Element (Cursor).Sources loop
            Last_Source := Source.Path_Name;
            Put_Line (Source.Path_Name.Value);
         end loop;
      end;
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
      --  GNATCOLL.Projects.Create
      Put_Line ("Create:" & Create (Project_Tree,
                GNATCOLL.VFS.Filesystem_String
                  (Last_Source.Simple_Name)).Display_Full_Name);
      --  To_Pathname functions
      Put_Line ("To_Pathnane:" & String
                (GPR2.Path_Name.Create
                   (GNATCOLL.VFS.Filesystem_String (Last_Source.Value)).Name));
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

      Put_Line ("Attribute_Value(Object_Dir):" &
                  Attribute_Value
                  (Project => Project_Tree.Root_Project,
                   Name    => (+"", +"Object_Dir")));
   end;

   Project_Tree.Unload;
end Conversion_Tutorial;
