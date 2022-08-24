--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.Opt_Parse;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project;
with GPR2.Project.Configuration;
with GPR2.Project.Tree;

procedure Main is

   use Ada.Strings.Unbounded;

   --  command line argument handling

   package Args is

      use GNATCOLL.Opt_Parse;

      Parser : Argument_Parser := Create_Argument_Parser
        (Help =>
           "GNAT prefix tool test");

      package Project_Arg is new Parse_Option
        (Parser      => Parser,
         Short       => "-P",
         Long        => "--project",
         Arg_Type    => Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Help        => "Project file to use");

      package Config_File_Arg is new Parse_Option
        (Parser       => Parser,
         Long         => "--config",
         Arg_Type     => Unbounded_String,
         Default_Val  => Null_Unbounded_String,
         Help         => "Specify the main config project file name");

      function Project_File return GPR2.Path_Name.Object is
        (if Args.Project_Arg.Get = Null_Unbounded_String
         then GPR2.Path_Name.Undefined
         else GPR2.Path_Name.Create_File
                (GPR2.Project.Ensure_Extension
                   (GPR2.Filename_Type
                      (To_String (Args.Project_Arg.Get)))));
      --  The GPR2 project file object (default GPR2.Path_Name.Undefined)

      function Config_File return GPR2.Path_Name.Object is
        (if Args.Config_File_Arg.Get = Null_Unbounded_String
         then GPR2.Path_Name.Undefined
         else GPR2.Path_Name.Create_File
                (GPR2.Filename_Type (To_String (Args.Config_File_Arg.Get))));
      --  The GPR2 project file object (default GPR2.Path_Name.Undefined)

   end Args;

   Project_Tree : GPR2.Project.Tree.Object;
   --  GPR2 project tree object.

   Project_Env  : GPR2.Context.Object;

begin
   if Args.Parser.Parse then
      if Args.Config_File.Is_Defined then
         Project_Tree.Load
           (Filename => Args.Project_File,
            Context  => Project_Env,
            Config   => GPR2.Project.Configuration.Load (Args.Config_File));
      else
         Project_Tree.Load_Autoconf
           (Filename => Args.Project_File,
            Context  => Project_Env);
      end if;

      Ada.Text_IO.Put_Line (String (Project_Tree.Add_Tool_Prefix ("a")));
   end if;

exception
   when GPR2.Project_Error =>
      for J in Project_Tree.Log_Messages.Iterate (Information => False) loop
         Ada.Text_IO.Put_Line (Project_Tree.Log_Messages.all (J).Format);
      end loop;
end Main;
