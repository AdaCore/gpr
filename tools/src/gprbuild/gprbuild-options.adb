------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2022-2025, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with GNATCOLL.Traces;

with GPR2.Options;
with GPR2.Path_Name;

with GPRtools.Command_Line;

package body GPRbuild.Options is

   use GPRtools;
   use GPR2;

   procedure On_Switch
     (Parser : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res    : not null access GPRtools.Command_Line.Command_Line_Result'Class;
      Arg    : GPRtools.Command_Line.Switch_Type;
      Index  : String;
      Param  : String);

   procedure On_Section_Switch
     (Parser  : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res     : not null
                  access GPRtools.Command_Line.Command_Line_Result'Class;
      Section : String;
      Index   : String;
      Arg     : String);

   -----------------------------
   -- Build_From_Command_Line --
   -----------------------------

   function Create return GPRbuild_Parser is
      use GPRtools.Command_Line;
      Parser : GPRbuild.Options.GPRbuild_Parser :=
                 (GPRtools.Options.Create
                    ("2022",
                     Cmd_Line =>
                       "[-P<proj>|<proj.gpr>] [opts] [source]" &
                       ASCII.LF &
                       "     [-cargs[:lang] opts] [-largs opts]" &
                       " [-kargs opts] [-gargs opts]",
                     Allow_Autoconf    => True,
                     Check_Shared_Libs => True) with null record);
      Build_Group     : GPRtools.Command_Line.Argument_Group;
      Compiler_Group  : GPRtools.Command_Line.Argument_Group;

   begin
      GPRtools.Options.Setup (GPRtools.Build);

      Build_Group := Parser.Add_Argument_Group
        ("Build control",
         On_Switch'Access);

      Compiler_Group :=
        Parser.Add_Argument_Group
          ("_compiler",
           On_Switch'Access,
           "For compatibility with gnatmake, these " &
             "switches are passed to the Ada compiler",
           Last => True);

      --  BUILD
      Parser.Add_Argument
        (Build_Group,
         Create (Name   => "--single-compiler-per-obj-dir",
                 Help   => "For compatibility reason only",
                 Hidden => True));
      Parser.Add_Argument
        (Build_Group,
         Create (Name           => "--build-script",
                 Help           => "Create a build script script_file",
                 In_Switch_Attr => False,
                 Delimiter      => Equal,
                 Parameter      => "script_file"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name           => "--compiler-subst",
                 Help           => "Use tool for compiling files in " &
                                   "language lang, instead of the normal " &
                                   "compiler",
                 In_Switch_Attr => False,
                 Delimiter      => Equal,
                 Parameter      => "lang,tool"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name => "--no-indirect-imports",
                 Help => "Sources can import only from directly "  &
                         "imported projects"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name => "--indirect-imports",
                 Help => "Sources can import from directly "  &
                         "and indirectly imported projects"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name => "--no-object-check",
                 Help => "Do not check object files"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name           => "--no-sal-binding",
                 Help           => "Reuse binder files when linking SALs",
                 In_Switch_Attr => False));
      Parser.Add_Argument
        (Build_Group,
         Create (Name           => "--no-split-units",
                 Help           => "Check that compilation unit parts are " &
                                   "from the same view"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name           => "--restricted-to-languages",
                 Help           => "Restrict the languages of the sources",
                 In_Switch_Attr => False,
                 Delimiter      => Equal,
                 Parameter      => "<lang1>,<lang2>,..."));
      Parser.Add_Argument
        (Build_Group,
         Create (Name   => "-a",
                 Help   => "Runtime compilation mode",
                 Hidden => True));
      Parser.Add_Argument
        (Build_Group,
         Create (Name => "-b",
                 Help => "Bind only"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name => "-c",
                 Help => "Compile only"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name => "-d",
                 Help => "Display compilation progress"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name      => "-eI",
                 Help      => "Index of main unit in multi-unit source file",
                 Delimiter => None,
                 Parameter => "<nn>"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name => "-f",
                 Help => "Force recompilation"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name      => "-j",
                 Help      => "Use <nn> processes to compile, bind and link." &
                              " If 0, use the number of CPUs on the host.",
                 Delimiter => None,
                 Parameter => "<nn>"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name => "-k",
                 Help => "Keep going after compilation errors"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name => "-l",
                 Help => "Link only"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name           => "-o",
                 Help           => "Choose an alternate executable name",
                 In_Switch_Attr => False,
                 Delimiter      => Space,
                 Parameter      => "name"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name => "-p",
                 Help => "Create missing obj/lib/exec directories"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name           => "-r",
                 Help           => "Recursive (default except when using -u)",
                 In_Switch_Attr => False));
      Parser.Add_Argument
        (Build_Group,
         Create (Name => "-R",
                 Help => "Do not use run path option"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name           => "-u",
                 Help           => "Unique compilation, only compile the " &
                                   "given files",
                 In_Switch_Attr => False));
      Parser.Add_Argument
        (Build_Group,
         Create (Name           => "-U",
                 Help           => "Unique compilation for all sources of" &
                                   " all projects",
                 In_Switch_Attr => False));
      Parser.Add_Argument
        (Build_Group,
         Create (Name   => "-x",
                 Help   => "obsolete: forces the use of include path file",
                 Hidden => True));
      Parser.Add_Argument
        (Build_Group,
         Create (Name           => "-z",
                 Help           => "No main subprogram (zero main)",
                 In_Switch_Attr => False));
      Parser.Add_Argument
        (Build_Group,
         Create (Name => "--create-map-file",
                 Help => "Have the linker generate the map file",
                 In_Switch_Attr => True,
                 Delimiter      => Equal,
                 Parameter      => "file.map",
                 Default        => "__default__"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name => "--keep-temp-files",
                 Help => "Do not delete temporary files"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name   => "--autodetect-jobserver",
                 Help   => "now the default, so no effect",
                 Hidden => True));
      Parser.Add_Argument
        (Build_Group,
         Create (Name           => "--temp-dir",
                 Help           => "Determinte where temporary files should "
                 & "be located. Either in the object directory of the owning "
                 & "project or the default system temporary directory",
                 In_Switch_Attr => False,
                 Delimiter      => Equal,
                 Parameter      => "[os|obj]"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name   => "-vh",
                 Help   => "Very high verbosity level",
                 Hidden => True));


      Parser.Add_Section_Argument
        (Name     => "-gargs",
         Alt_Name => "-margs",
         Callback => null,
         Help     => "options interpreted by gprbuild");
      Parser.Add_Section_Argument
        (Name     => "-cargs",
         Callback => On_Section_Switch'Access,
         Help     => "options to be passed to the compiler",
         Index    => "<lang>");
      Parser.Add_Section_Argument
        (Name     => "-bargs",
         Callback => On_Section_Switch'Access,
         Help     => "options to be passed to the binder",
         Index    => "<lang>");
      Parser.Add_Section_Argument
        (Name     => "-largs",
         Callback => On_Section_Switch'Access,
         Help     => "options to be passed to the linker");
      Parser.Add_Section_Argument
        (Name     => "-kargs",
         Callback => On_Section_Switch'Access,
         Help     => "options to be passed to gprconfig");

      --  hidden argument?
      Parser.Add_Argument
        (Build_Group,
         Create
           ("--json-summary", "",
            In_Switch_Attr => False,
            Hidden         => True));

      Parser.Add_Argument
        (Compiler_Group, Create ("-nostdlib", Help => ""));
      Parser.Add_Argument
        (Compiler_Group, Create ("-nostdinc", Help => ""));
      Parser.Add_Argument
        (Compiler_Group, Create ("-fstack-check", Help => ""));
      Parser.Add_Argument
        (Compiler_Group, Create ("-fno-inline", Help   => ""));
      Parser.Add_Argument
        (Compiler_Group,
         Create
           ("-g",
            Help      => "",
            Delimiter => None,
            Parameter => "<debug_opt>",
            Default   => "default"));
      Parser.Add_Argument
        (Compiler_Group,
         Create
           ("-O",
            Help      => "",
            Delimiter => None,
            Parameter => "<level>",
            Default   => "1"));
      --  if -O is passed, it will default correctly to -O1

      --  Switches there only for compatibility reason but without any
      --  effect

      Parser.Add_Argument
        (Build_Group,
         Create (Name   => "-C",
                 Help   => "for compatibility with older gprbuild",
                 Hidden => True));
      --  ???-C seems to be historical. There's still a test that checks it so
      --  we keep backwards compatibility here.
      Parser.Add_Argument
        (Build_Group,
         Create (Name   => "--complete-output",
                 Help   => "for compatibility with older gprbuild",
                 Hidden => True));
      --  complete-output (e.g. replaying warnings upon successive compilations
      --  even when no action is performed) is now the default, so this
      --  switch is ignored

      Parser.Add_Argument
        (Build_Group,
         Create
           (Name   => "--no-warnings-replay",
            Help   => "Do not display warnings if no other action was taken"));
      --  Do not replay warnings if there are nothing to do for an action

      Parser.Add_Argument
        (Build_Group,
         Create (Name   => "-eS",
                 Help   => "For compatibility with gnatmake only",
                 Hidden => True));
      Parser.Add_Argument
        (Build_Group,
         Create (Name      => "-jc",
                 Help      => "Use <nn> processes to compile." &
                              " If 0, use the number of CPUs on the host.",
                 Delimiter => None,
                 Parameter => "<nn>",
                 Hidden    => True));
      --  Cannot be implemented in gpr2: the dag doesn't differentiate actions
      --  so paralellism cannot be specialized to spedific phases. This remard
      --  is also valid for below -jb and -jl
      Parser.Add_Argument
        (Build_Group,
         Create (Name      => "-jb",
                 Help      => "Use <nn> processes to bind." &
                              " If 0, use the number of CPUs on the host.",
                 Delimiter => None,
                 Parameter => "<nn>",
                 Hidden    => True));
      Parser.Add_Argument
        (Build_Group,
         Create (Name      => "-jl",
                 Help      => "Use <nn> processes to link." &
                              " If 0, use the number of CPUs on the host.",
                 Delimiter => None,
                 Parameter => "<nn>",
                 Hidden    => True));
      Parser.Add_Argument
        (Build_Group,
         Create (Name   => "-m",
                 Help   => "Minimum Ada recompilation",
                 Hidden => True));
      --  -m* involves checking checksum of files which is now the default in
      --  gpr2. This is kept for compatibility reasons but won't have effects.
      Parser.Add_Argument
        (Build_Group,
         Create (Name   => "-m2",
                 Help   => "Checksum based Ada recompilation",
                 Hidden => True));
      Parser.Add_Argument
        (Build_Group,
         Create (Name   => "-s",
                 Help   => "Recompile if compiler switches have changed",
                 Hidden => True));
      Parser.Add_Argument
        (Build_Group,
         Create (Name   => "-dn",
                 Help   => "for compatibility with older gprbuild",
                 Hidden => True));

      return Parser;
   end Create;

   -----------------------
   -- On_Section_Switch --
   -----------------------

   procedure On_Section_Switch
     (Parser  : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res     : not null
                  access GPRtools.Command_Line.Command_Line_Result'Class;
      Section : String;
      Index   : String;
      Arg     : String)
   is
      pragma Unreferenced (Parser);

      Result   : constant access Object := Object (Res.all)'Access;
      Lang_Idx : constant GPR2.Language_Id :=
                   (if Index'Length > 0
                    then GPR2."+" (GPR2.Name_Type (Index))
                    else GPR2.No_Language);

   begin
      if Section = "-cargs" then
         Result.Extra_Args.Register
           (GPR2.Build.External_Options.Compiler,
            Lang_Idx,
            Arg);

      elsif Section = "-bargs" then
         Result.Extra_Args.Register
           (GPR2.Build.External_Options.Binder,
            Lang_Idx,
            Arg);

      elsif Section = "-largs" then
         Result.Extra_Args.Register
           (GPR2.Build.External_Options.Linker,
            GPR2.No_Language,
            Arg);

      elsif Section = "-kargs" then
         --  [eng/gpr/gpr-issues#444] TBD
         Result.Config_Args.Append (Arg);

      else
         raise GPR2.Options.Usage_Error with
           "unexpected arg section " & Section;
      end if;
   end On_Section_Switch;

   ---------------
   -- On_Switch --
   ---------------

   procedure On_Switch
     (Parser : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res    : not null access GPRtools.Command_Line.Command_Line_Result'Class;
      Arg    : GPRtools.Command_Line.Switch_Type;
      Index  : String;
      Param  : String)
   is
      pragma Unreferenced (Parser, Index);
      use type GPRtools.Command_Line.Switch_Type;

      procedure Add_Ada_Compiler_Option (Sw : String);

      Result : constant access Object := Object (Res.all)'Access;
      Failed : Boolean := False;

      -----------------------------
      -- Add_Ada_Compiler_Option --
      -----------------------------

      procedure Add_Ada_Compiler_Option (Sw : String) is
      begin
         Result.Extra_Args.Register
           (GPR2.Build.External_Options.Compiler,
            GPR2.Ada_Language,
            Sw);
      end Add_Ada_Compiler_Option;

   begin
      if Arg = "--build-script" then
         Result.PM_Options.Script_File :=
           GPR2.Path_Name.Create_File (Filename_Type (Param));
         Result.PM_Options.Keep_Temp_Files := True;
         Result.Build_Options.Use_Obj_Dir_As_Temp_Dir := True;

      elsif Arg = "--no-indirect-imports" then
         Result.Build_Options.No_Indirect_Imports := True;

      elsif Arg = "--indirect-imports" then
         Result.Build_Options.No_Indirect_Imports := False;

      elsif Arg = "--no-object-check" then
         Result.No_Object_Check := True;

      elsif Arg = "--no-split-units" then
         Result.No_Split_Units := True;

      elsif Arg = "--no-sal-binding" then
         Result.Build_Options.No_SAL_Binding := True;

      elsif Arg = "--restricted-to-languages" then
         Result.Build_Options.Restricted_To_Languages.Clear;
         declare
            Last : Natural := Param'First;
         begin
            for Idx in Param'Range loop
               if Param (Idx) = ',' then
                  declare
                     Lang : constant String := Param (Last .. Idx - 1);
                  begin
                     if Lang'Length > 0 then
                        Result.Build_Options.Restricted_To_Languages.Include
                          (+Optional_Name_Type (Lang));
                     end if;

                     Last := Idx + 1;
                  end;
               elsif Idx = Param'Last and then Last <= Idx then
                  Result.Build_Options.Restricted_To_Languages.Include
                    (+Optional_Name_Type (Param (Last .. Param'Last)));
               end if;
            end loop;
         end;
      elsif Arg = "--compiler-subst" then
         for Idx in Param'Range loop
            if Param (Idx) = ',' then
               declare
                  Lang     : constant String := Param (Param'First .. Idx - 1);
                  Compiler : constant String := Param (Idx + 1 .. Param'Last);
               begin
                  if Lang'Length > 0 and then Compiler'Length > 0 then
                     Result.Build_Options.Comp_Substr.Include
                       (+Optional_Name_Type (Lang), Value_Type (Compiler));
                  end if;
               end;
            end if;
         end loop;

      elsif Arg = "-b" then
         Result.Build_Options.Restricted_Build_Phase := True;
         Result.Build_Options.Bind_Phase_Mandated := True;

      elsif Arg = "-c" then
         Result.Build_Options.Restricted_Build_Phase := True;
         Result.Build_Options.Compile_Phase_Mandated := True;

      elsif Arg = "-d" then
         Result.PM_Options.Show_Progress := True;

      elsif Arg = "-eI" then
         begin
            Result.Build_Options.Unit_Index := GPR2.Unit_Index'Value (Param);
         exception
            when Constraint_Error =>
               raise GPR2.Options.Usage_Error
                 with "'" & Param & "' is not a valid unit index";
         end;

      elsif Arg = "-f" then
         Result.PM_Options.Force := True;

      elsif Arg = "-j" then
         declare
            Val : Natural;
         begin
            Val := Natural'Value (Param);
            Result.PM_Options.Jobs := Val;
         exception
            when Constraint_Error =>
               raise GPR2.Options.Usage_Error
                 with "'" & Param & "' is not a valid parameter for -j";
         end;

      elsif Arg = "-k" then
         Result.PM_Options.Stop_On_Fail := False;

      elsif Arg = "-l" then
         Result.Build_Options.Restricted_Build_Phase := True;
         Result.Build_Options.Link_Phase_Mandated := True;

      elsif Arg = "-o" then
         Result.Build_Options.Output_File := To_Unbounded_String (Param);

      elsif Arg = "-p" then
         Result.Create_Missing_Dirs := True;

      elsif Arg = "-r" then
         Result.Force_Recursive_Build := True;

      elsif Arg = "-R" then
         Result.Build_Options.No_Run_Path := True;

      elsif Arg = "-u" then
         Result.Build_Options.Unique_Compilation := True;

      elsif Arg = "-U" then
         Result.Build_Options.Unique_Compilation_Recursive := True;

      elsif Arg = "-vh" then
         Result.Verbosity := GPRtools.Options.Verbose;
         GNATCOLL.Traces.Parse_Config (">&2");
         GNATCOLL.Traces.Parse_Config ("GPR.*=yes");

      elsif Arg = "-z" then
         Result.Build_Options.No_Main_Subprogram := True;

      elsif Arg = "-fno-inline" then
         Add_Ada_Compiler_Option (String (Arg));

      elsif Arg = "-fstack-check" then
         Add_Ada_Compiler_Option (String (Arg));

      elsif Arg = "--json-summary" then
         Result.Json_Summary := True;

      elsif Arg = "--create-map-file" then
         Result.Build_Options.Create_Map_File := True;

         if Param /= "__default__" then
            Result.Build_Options.Mapping_File_Name :=
              To_Unbounded_String (Param);
         end if;

      elsif Arg = "--keep-temp-files" then
         Result.PM_Options.Keep_Temp_Files := True;

      elsif Arg = "--autodetect-jobserver" then
         --  This is now the default mode of gprbuild. This switch is not used
         --  per se, but an error will be emitted if the jobserver style is
         --  "named_pipe" (or "--jobserver-style=fifo" used) when this option
         --  is on the command line. Otherwise we can fallback to the normal
         --  gprbuild mode (spawn as many process as the -jX option allows)
         Result.PM_Options.Force_Jobserver := True;

      elsif Arg = "--no-warnings-replay" then
         Result.PM_Options.No_Warnings_Replay := True;

      elsif Arg = "--temp-dir" then
         if Param = "obj" then
            Result.Build_Options.Use_Obj_Dir_As_Temp_Dir := True;
         elsif Param = "os" then
            Result.Build_Options.Use_Obj_Dir_As_Temp_Dir := False;
         else
            raise GPR2.Options.Usage_Error
              with "'" & Param & "' is not a valid parameter for --temp-dir=";
         end if;

      elsif Arg = "-nostdinc" then
         Add_Ada_Compiler_Option (String (Arg));

      elsif Arg = "-nostdlib" then
         Add_Ada_Compiler_Option (String (Arg));

      elsif Arg = "-g" then
         if Param = "default" then
            Add_Ada_Compiler_Option (String (Arg));
         else
            Add_Ada_Compiler_Option (String (Arg) & Param);
         end if;

      elsif Arg = "-O" then
         if Param not in "g" | "s" | "fast" then
            declare
               Int : Integer;
            begin
               Int := Integer'Value (Param);
               if Int < 0 then
                  Failed := True;
               end if;

            exception
               when Constraint_Error =>
                  Failed := True;
            end;

            if Failed then
               raise GPR2.Options.Usage_Error
                 with
                   "argument to '-O' should be a non-negative integer, "
                   & "'g', 's' or 'fast'";
            end if;
         end if;

         Add_Ada_Compiler_Option (String (Arg) & Param);

      elsif Arg = "-a" then
         --  Ignore but tell the customer that this switch doesn't work:
         --  set the option, it will then be reported by gprbuild-main

         Result.Dash_A_Option := True;

      elsif Arg = "-C"
        or else Arg = "-dn"
        or else Arg = "-eS"
        or else Arg = "-jc"
        or else Arg = "-jb"
        or else Arg = "-jl"
        or else Arg = "-m"
        or else Arg = "-m2"
        or else Arg = "-s"
        or else Arg = "-x"
        or else Arg = "--complete-output"
        or else Arg = "--single-compiler-per-obj-dir"
      then
         --  Ignore, only there for compatibility reason
         null;

      else
         raise GPR2.Options.Usage_Error
           with "unexpected switch " & String (Arg);
      end if;
   end On_Switch;

end GPRbuild.Options;
