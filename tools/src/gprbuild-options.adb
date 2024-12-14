------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2022-2024, AdaCore                     --
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

with GPR2.Options;
with GPR2.External_Options;
with GPR2.Reporter.Console;

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
      Arg     : GPRtools.Command_Line.Switch_Type);

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
      Build_Group    : GPRtools.Command_Line.Argument_Group;
      Compiler_Group : GPRtools.Command_Line.Argument_Group;

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
         Create (Name => "--single-compiler-per-obj-dir",
                 Help => "No simultaneous compilations for the same obj dir"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name           => "--build-script",
                 Help           => "Create a build script script_file",
                 In_Switch_Attr => False,
                 Delimiter      => Equal,
                 Parameter      => "script_file"));
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
         Create (Name           => "--restricted-to-languages",
                 Help           => "Restrict the languages of the sources",
                 In_Switch_Attr => False,
                 Delimiter      => Equal,
                 Parameter      => "<lang1>,<lang2>,..."));
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
         Create (Name   => "-C",
                 Help   => "for compatibility with older gprbuild",
                 Hidden => True));
      --  ???-C seems to be historical. There's still a test that checks it so
      --  we keep backwards compatibility here.
      Parser.Add_Argument
        (Build_Group,
         Create (Name           => "-d",
                 Help           => "Display compilation progress",
                 In_Switch_Attr => False));
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
         Create (Name => "-k",
                 Help => "Keep going after compilation errors"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name => "-l",
                 Help => "Link only"));
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
         Create (Name => "-s",
                 Help => "Recompile if compiler switches have changed"));
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
            Help   => "",
            Delimiter => None,
            Parameter => "<level>"));

      return Parser;
   end Create;

   -------------
   -- Get_Opt --
   -------------

   overriding procedure Get_Opt
     (Parser : GPRbuild_Parser;
      Result : in out GPRtools.Command_Line.Command_Line_Result'Class)
   is
      Options : constant access Object := Object (Result)'Access;
   begin
      GPRtools.Options.Command_Line_Parser (Parser).Get_Opt (Result);

      --  Adjust console output verbosity to mimick what gprbuild1 does

      case Options.Console_Reporter.Verbosity is
         when GPR2.Reporter.Quiet =>
            if Options.No_Warnings then
               Options.Console_Reporter.Set_Verbosity
                 (GPR2.Reporter.No_Warnings);
            else
               Options.Console_Reporter.Set_Verbosity
                 (GPR2.Reporter.Regular);
            end if;

            Options.Console_Reporter.Set_User_Verbosity
              (GPR2.Reporter.Important_Only);

         when GPR2.Reporter.No_Warnings | GPR2.Reporter.Regular =>
            null;

         when GPR2.Reporter.Verbose | GPR2.Reporter.Very_Verbose =>
            Options.Console_Reporter.Set_User_Verbosity
              (GPR2.Reporter.Verbose);
            Options.Console_Reporter.Set_Verbosity
              (GPR2.Reporter.Regular);
      end case;
   end Get_Opt;

   -----------------------
   -- On_Section_Switch --
   -----------------------

   procedure On_Section_Switch
     (Parser  : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res     : not null
                  access GPRtools.Command_Line.Command_Line_Result'Class;
      Section : String;
      Index   : String;
      Arg     : GPRtools.Command_Line.Switch_Type)
   is
      pragma Unreferenced (Parser);

      Result   : constant access Object := Object (Res.all)'Access;
      Lang_Idx : constant GPR2.Language_Id :=
                   (if Index'Length > 0
                    then GPR2."+" (GPR2.Name_Type (Index))
                    else GPR2.No_Language);

   begin
      if Section = "-cargs" then
         Result.Register_External_Options
           (GPR2.External_Options.Compiler,
            Lang_Idx,
            String (Arg));

      elsif Section = "-bargs" then
         Result.Register_External_Options
           (GPR2.External_Options.Binder,
            Lang_Idx,
            String (Arg));

      elsif Section = "-largs" then
         Result.Register_External_Options
           (GPR2.External_Options.Linker,
            GPR2.No_Language,
            String (Arg));

      elsif Section = "-kargs" then
         --  [eng/gpr/gpr-issues#444] TBD
         Result.Config_Args.Append (String (Arg));

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

      procedure Add_Ada_Compiler_Option (Sw : String) is
      begin
         Result.Register_External_Options
           (GPR2.External_Options.Compiler,
            GPR2.Ada_Language,
            Sw);
      end Add_Ada_Compiler_Option;

   begin
      if Arg = "--single-compiler-per-obj-dir" then
         Result.Single_Build_Per_Obj_Dir := True;

      elsif Arg = "--build-script" then
         Result.Build_Script :=
           GPR2.Path_Name.Create_File (Filename_Type (Param));

      elsif Arg = "--no-indirect-imports" then
         Result.Build_Options.No_Indirect_Imports := True;

      elsif Arg = "--indirect-imports" then
         Result.Build_Options.No_Indirect_Imports := False;

      elsif Arg = "--no-object-check" then
         Result.No_Object_Check := True;

      elsif Arg = "--no-sal-binding" then
         Result.Build_Options.No_SAL_Binding := True;

      elsif Arg = "--restricted-to-languages" then
         Result.Restricted_To_Languages.Clear;
         declare
            Last : Natural := Param'First;
         begin
            for Idx in Param'Range loop
               if Param (Idx) = ',' then
                  declare
                     Lang : constant String :=
                              Param (Last .. Idx - 1);
                  begin
                     if Lang'Length > 0 then
                        Result.Restricted_To_Languages.Include
                          (+Optional_Name_Type (Lang));
                     end if;

                     Last := Idx + 1;
                  end;
               elsif Idx = Param'Last
                 and then Last <= Idx
               then
                  Result.Restricted_To_Languages.Include
                    (+Optional_Name_Type (Param (Last .. Param'Last)));
               end if;
            end loop;
         end;

      elsif Arg = "-b" then
         Result.Build_Options.Restricted_Build_Phase := True;
         Result.Build_Options.Bind_Phase_Mandated := True;

      elsif Arg = "-c" then
         Result.Build_Options.Restricted_Build_Phase := True;
         Result.Build_Options.Compile_Phase_Mandated := True;

      elsif Arg = "-d" then
         Result.Display_Progress := True;

      elsif Arg = "-eI" then
         begin
            Result.Build_Options.Unit_Index := GPR2.Unit_Index'Value (Param);
         exception
            when Constraint_Error =>
               raise GPR2.Options.Usage_Error with
                 "'" & Param & "' is not a valid unit index";
         end;

      elsif Arg = "-f" then
         Result.Force := True;

      elsif Arg = "-j" then
         declare
            Val : Natural;
         begin
            Val := Natural'Value (Param);
            Result.Parallel_Tasks := Val;
         exception
            when Constraint_Error =>
               raise GPR2.Options.Usage_Error with
                 "'" & Param & "' is not a valid parameter for -j";
         end;

      elsif Arg = "-k" then
         Result.Keep_Going := True;

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

      elsif Arg = "-s" then
         Result.Build_If_Switch_Changes := True;

      elsif Arg = "-u" then
         Result.Build_Options.Unique_Compilation := True;

      elsif Arg = "-U" then
         Result.Build_Options.Unique_Compilation_Recursive := True;

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
         Result.Keep_Temp_Files := True;

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
               raise GPR2.Options.Usage_Error with
                 "argument to '-O' should be a non-negative integer, " &
                 "'g', 's' or 'fast'";
            end if;
         end if;

         Add_Ada_Compiler_Option (String (Arg) & Param);

      elsif Arg = "-C"
        or else Arg = "-jc"
        or else Arg = "-jb"
        or else Arg = "-jl"
        or else Arg = "-m"
        or else Arg = "-m2"
      then
         --  Ignore, only there for compatibility reason
         null;

      else
         raise GPR2.Options.Usage_Error
           with "unexpected switch " & String (Arg);
      end if;
   end On_Switch;

end GPRbuild.Options;
