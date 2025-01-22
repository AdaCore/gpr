--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.Strings;      use GNATCOLL.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body GPR2.Options.Opt_Parse is

   package body Args is
      package AP is new
        Parse_Option_List
          (Parser      => Parser,
           Name        => "Search path",
           Accumulate  => True,
           Allow_Empty => False,
           Short       => "-aP",
           Arg_Type    => Unbounded_String,
           Help        => "Add directory to the project search path");

      package Autoconf is new
        Parse_Option_List
          (Parser      => Parser,
           Name        => "autoconf",
           Accumulate  => False,
           Allow_Empty => False,
           Long        => "--autoconf",
           Arg_Type    => Unbounded_String,
           Help        => "Specify/create the main config project file name");

      package Config is new
        Parse_Option
          (Parser      => Parser,
           Name        => "config",
           Long        => "--config",
           Default_Val => Null_Unbounded_String,
           Arg_Type    => Unbounded_String,
           Help        => "Specify the configuration project file name");

      package Db is new
        Parse_Option_List
          (Parser      => Parser,
           Name        => "db",
           Accumulate  => True,
           Allow_Empty => False,
           Long        => "--db",
           Arg_Type    => Unbounded_String,
           Help        => "Parse dir as an additional knowledge base");

      package Db_Minus is new
        Parse_Flag
          (Parser => Parser,
           Long   => "--db-",
           Help   => "Do not load the standard knowledge base");

      package Implicit_With is new
        Parse_Option_List
          (Parser      => Parser,
           Name        => "project",
           Accumulate  => True,
           Allow_Empty => False,
           Long        => "--implicit-with",
           Arg_Type    => Unbounded_String,
           Help        =>
             "Add the given projects as a dependency on all loaded projects");

      package Resolve_Links is new
        Parse_Flag
          (Parser => Parser,
           Short  => "-el",
           Name   => "resolve-links",
           Help   => "Follows symlinks for project files");

      package No_Project is new
        Parse_Flag
          (Parser => Parser,
           Long   => "--no-project",
           Help   => "Do not use a project file");

      package P is new
        Parse_Option
          (Parser      => Parser,
           Name        => "project",
           Short       => "-P",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        => "The project file");

      package Print_GPR_Registry is new
        Parse_Flag
          (Parser => Parser,
           Long   => "--print-gpr-registry",
           Help   => "Print the GPR registry");

      package Root_Dir is new
        Parse_Option
          (Parser      => Parser,
           Name        => "root-dir",
           Long        => "--root-dir",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        => "Root directory of obj/lib/exec to relocate");

      package Relocate_Build_Tree is new
        Parse_Option
          (Parser      => Parser,
           Name        => "relocate-build-tree",
           Long        => "--relocate-build-tree",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        =>
             "Root obj/lib/exec dirs are current-directory or dir");

      package RTS is new
        Parse_Option_List
          (Parser      => Parser,
           Name        => "rts",
           Accumulate  => True,
           Allow_Empty => False,
           Long        => "--RTS",
           Arg_Type    => Unbounded_String,
           Help        =>
             " --RTS=<runtime> Use runtime <runtime> for language Ada; "
             & "--RTS:<lang>=<runtime> Use runtime"
             & " <runtime> for language <lang>");

      package Src_Subdirs is new
        Parse_Option
          (Parser      => Parser,
           Name        => "src-subdirs",
           Long        => "--src-subdirs",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        =>
             "prepend <obj>/dir to the list of source dirs for each project");

      package Subdirs is new
        Parse_Option
          (Parser      => Parser,
           Name        => "subdirs",
           Long        => "--subdirs",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        => "Use dir as suffix to obj/lib/exec directories");

      package Target is new
        Parse_Option
          (Parser      => Parser,
           Name        => "target",
           Long        => "--target",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        => "Specify a target for cross platforms");

      package X is new
        Parse_Option_List
          (Parser      => Parser,
           Name        => "scenario",
           Accumulate  => True,
           Allow_Empty => False,
           Short       => "-X",
           Arg_Type    => Unbounded_String,
           Help        => "Specify an external reference for Project Files");

      ------------------------
      -- Parse_GPR2_Options --
      ------------------------

      function Parse_GPR2_Options
        (Arguments : in out GNATCOLL.Opt_Parse.XString_Vector;
         Options   : out GPR2.Options.Object) return Boolean

      is
         Ignored : GPR2.Options.Option := GPR2.Options.Option'First;
         Unb     : Unbounded_String;
      begin
         case Ignored is
            --  This case statement is here to provide maintainability. If you
            --  land here after a compile error, this means you have to
            --  resync this package with the arguments as they are defined
            --  in GPR2.Options.

            when GPR2.Options.AP
               | GPR2.Options.Autoconf
               | GPR2.Options.Config
               | GPR2.Options.Db
               | GPR2.Options.Db_Minus
               | GPR2.Options.Implicit_With
               | GPR2.Options.Resolve_Links
               | GPR2.Options.No_Project
               | GPR2.Options.P
               | GPR2.Options.Print_GPR_Registry
               | GPR2.Options.Relocate_Build_Tree
               | GPR2.Options.Root_Dir
               | GPR2.Options.RTS
               | GPR2.Options.Src_Subdirs
               | GPR2.Options.Subdirs
               | GPR2.Options.Target
               | GPR2.Options.X
            =>
               null;
         end case;

         --  aP
         for AP of Args.AP.Get loop
            Options.Add_Switch (GPR2.Options.AP, To_String (AP));
         end loop;

         --  Autoconf
         for X of Args.Autoconf.Get loop
            Options.Add_Switch (GPR2.Options.Autoconf, To_String (X));
         end loop;

         --  Config
         Unb := Args.Config.Get;
         if Unb /= Null_Unbounded_String then
            Options.Add_Switch (GPR2.Options.Config, To_String (Unb));
         end if;

         --  Db
         for Db of Args.Db.Get loop
            Options.Add_Switch (GPR2.Options.Db, To_String (Db));
         end loop;

         --  Db_Minus
         if Args.Db_Minus.Get then
            Options.Add_Switch (GPR2.Options.Db_Minus);
         end if;

         --  Implicit_With
         for Implicit_With of Args.Implicit_With.Get loop
            Options.Add_Switch
              (GPR2.Options.Implicit_With, To_String (Implicit_With));
         end loop;

         --  Resolve_Links
         if Args.Resolve_Links.Get then
            Options.Add_Switch (GPR2.Options.Resolve_Links);
         end if;

         --  No_Project
         if Args.No_Project.Get then
            Options.Add_Switch (GPR2.Options.No_Project);
         end if;

         --  P
         Unb := Args.P.Get;
         if Unb /= Null_Unbounded_String then
            --  TODO: heuristics for finding project
            Options.Add_Switch (GPR2.Options.P, To_String (Unb));
         end if;

         --  Print_GPR_Registry
         if Args.Print_GPR_Registry.Get then
            Options.Add_Switch (GPR2.Options.Print_GPR_Registry);
         end if;

         --  Relocate_Build_Tree
         Unb := Args.Relocate_Build_Tree.Get;
         if Unb /= Null_Unbounded_String then
            Options.Add_Switch
              (GPR2.Options.Relocate_Build_Tree, To_String (Unb));
         end if;

         --  Root_Dir
         Unb := Args.Root_Dir.Get;
         if Unb /= Null_Unbounded_String then
            Options.Add_Switch (GPR2.Options.Root_Dir, To_String (Unb));
         end if;

         --  RTS
         for RTS of Args.RTS.Get loop
            Options.Add_Switch (GPR2.Options.RTS, To_String (RTS));
         end loop;

         --  Special handling for the "--RTS:x" way of parsing switches
         declare
            New_Args : GNATCOLL.Opt_Parse.XString_Vector;
         begin
            for X of Arguments loop
               if X.Starts_With ("--RTS:") then
                  Options.Add_Switch
                     (GPR2.Options.RTS,
                     To_String (X.Slice (7, Length (X))));
               else
                  New_Args.Append (X);
               end if;
            end loop;
            Arguments := New_Args;
         end;

         --  Src_Subdirs
         Unb := Args.Src_Subdirs.Get;
         if Unb /= Null_Unbounded_String then
            Options.Add_Switch (GPR2.Options.Src_Subdirs, To_String (Unb));
         end if;

         --  Subdirs
         Unb := Args.Subdirs.Get;
         if Unb /= Null_Unbounded_String then
            Options.Add_Switch (GPR2.Options.Subdirs, To_String (Unb));
         end if;

         --  Target
         Unb := Args.Target.Get;
         if Unb /= Null_Unbounded_String then
            Options.Add_Switch (GPR2.Options.Target, To_String (Unb));
         end if;

         --  X
         for X of Args.X.Get loop
            Options.Add_Switch (GPR2.Options.X, To_String (X));
         end loop;

         return True;
      end Parse_GPR2_Options;
   end Args;

end GPR2.Options.Opt_Parse;
