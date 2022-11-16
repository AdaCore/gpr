------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with GPRtools.Command_Line;

with GPR2.Project.Tree;
with GPR2.Project.View;

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

   function Create return GPRBuild_Parser is
      use GPRtools.Command_Line;
      Parser : GPRtools.Options.Command_Line_Parser :=
                 GPRtools.Options.Create
                   ("2022",
                    Cmd_Line => "[-P<proj>|<proj.gpr>] [opts] [source]" &
                      ASCII.LF &
                      "     [-cargs[:lang] opts] [-largs opts] [-kargs opts]" &
                      " [-gargs opts]",
                    Allow_Autoconf    => True,
                    Allow_Distributed => True);
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
                 Parameter => "<nn>"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name      => "-jb",
                 Help      => "Use <nn> processes to bind." &
                              " If 0, use the number of CPUs on the host.",
                 Delimiter => None,
                 Parameter => "<nn>"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name      => "-jl",
                 Help      => "Use <nn> processes to link." &
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
         Create (Name => "-m",
                 Help => "Minimum Ada recompilation"));
      Parser.Add_Argument
        (Build_Group,
         Create (Name => "-m2",
                 Help => "Checksum based Ada recompilation"));
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
           ("--display-paths", "",
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

      return GPRBuild_Parser'(Parser with null record);
   end Create;

   -----------
   -- Mains --
   -----------

   function Mains
     (Options : Object) return GPR2.Unit.Source_Unit_Vectors.Vector
   is
      use type Ada.Containers.Count_Type;
      Result : GPR2.Unit.Source_Unit_Vectors.Vector;
   begin
      if Options.Args.Is_Empty then
         return GPR2.Unit.Source_Unit_Vectors.Empty_Vector;
      end if;

      if Options.Multi_Unit_Index /= No_Index
        and then Options.Args.Length > 1
      then
         raise Usage_Error with
           "only one source can be specified with multi-unit index " &
           "specified with '-eI'";
      end if;

      for Arg of Options.Args loop
         declare
            Path  : constant GPR2.Path_Name.Object :=
                      Options.Tree.Get_File (Simple_Name (Arg));
            Unit  : GPR2.Unit.Source_Unit_Identifier;
            Found : Boolean := False;
         begin
            if Path.Is_Defined then
               Result.Append ((Path, Options.Multi_Unit_Index));
               Found := True;
            else
               --  Try to find the main unit if executable is specified
               View_Loop :
               for C in Options.Tree.Iterate loop
                  declare
                     View : constant GPR2.Project.View.Object :=
                              GPR2.Project.Tree.Element (C);
                  begin
                     if View.Has_Mains and then not View.Is_Extended then
                        Unit := View.Main (Simple_Name (Arg));
                        if Unit.Source.Is_Defined then
                           Result.Append (Unit);
                           Found := True;
                           exit View_Loop;
                        end if;
                     end if;
                  end;
               end loop View_Loop;
            end if;

            if not Found then
               raise Usage_Error with
                 """" & Arg & """ was not found in the sources of any project";
            end if;
         end;
      end loop;

      return Result;
   end Mains;

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
         if not Result.Compiler_Args.Contains (Lang_Idx) then
            Result.Compiler_Args.Insert
              (Lang_Idx,
               GPR2.Containers.Value_Type_List.Empty_Vector);
         end if;

         Result.Compiler_Args (Lang_Idx).Append (String (Arg));

      elsif Section = "-bargs" then
         if not Result.Binder_Args.Contains (Lang_Idx) then
            Result.Binder_Args.Insert
              (Lang_Idx,
               GPR2.Containers.Value_Type_List.Empty_Vector);
         end if;

         Result.Binder_Args (Lang_Idx).Append (String (Arg));

      elsif Section = "-largs" then
         Result.Linker_Args.Append (String (Arg));

      elsif Section = "-kargs" then
         Result.Config_Args.Append (String (Arg));

      else
         raise GPRtools.Command_Line.Command_Line_Definition_Error with
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

      Result : constant access Object := Object (Res.all)'Access;
      Failed : Boolean := False;

   begin
      if Arg = "--single-compiler-per-obj-dir" then
         Result.Single_Build_Per_Obj_Dir := True;

      elsif Arg = "--build-script" then
         Result.Build_Script :=
           GPR2.Path_Name.Create_File (Filename_Type (Param));

      elsif Arg = "--no-indirect-imports" then
         Result.Indirect_Imports := False;

      elsif Arg = "--indirect-imports" then
         Result.Indirect_Imports := True;

      elsif Arg = "--no-object-check" then
         Result.No_Object_Check := True;

      elsif Arg = "--no-sal-binding" then
         Result.No_SAL_Binding := True;

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
         Result.Restricted_Build_Phase := True;
         Result.Bind_Phase_Mandated := True;

      elsif Arg = "-c" then
         Result.Restricted_Build_Phase := True;
         Result.Compile_Phase_Mandated := True;

      elsif Arg = "-d" then
         Result.Display_Progress := True;

      elsif Arg = "-eI" then
         begin
            Result.Multi_Unit_Index :=
              GPR2.Unit_Index'Value (Param);
         exception
            when Constraint_Error =>
               raise Usage_Error with
                 "'" & Param & "' is not a valid unit index";
         end;

      elsif Arg = "-f" then
         Result.Force := True;

      elsif Arg = "-j" then
         declare
            Val : Natural;
         begin
            Val := Natural'Value (Param);
            Result.Parallel_Compilation := Val;
            Result.Parallel_Bind := Val;
            Result.Parallel_Link := Val;
         exception
            when Constraint_Error =>
               raise Usage_Error with
                 "'" & Param & "' is not a valid parameter for -j";
         end;

      elsif Arg = "-jc" then
         begin
            Result.Parallel_Compilation := Natural'Value (Param);
         exception
            when Constraint_Error =>
               raise Usage_Error with
                 "'" & Param & "' is not a valid parameter for -jc";
         end;

      elsif Arg = "-jb" then
         begin
            Result.Parallel_Bind := Natural'Value (Param);
         exception
            when Constraint_Error =>
               raise Usage_Error with
                 "'" & Param & "' is not a valid parameter for -jb";
         end;

      elsif Arg = "-jl" then
         begin
            Result.Parallel_Link := Natural'Value (Param);
         exception
            when Constraint_Error =>
               raise Usage_Error with
                 "'" & Param & "' is not a valid parameter for -jl";
         end;

      elsif Arg = "-k" then
         Result.Keep_Going := True;

      elsif Arg = "-l" then
         Result.Restricted_Build_Phase := True;
         Result.Link_Phase_Mandated := True;

      elsif Arg = "-m" then
         Result.Mode := Minimal;

      elsif Arg = "-m2" then
         Result.Mode := Checksum;

      elsif Arg = "-o" then
         Result.Output_File :=
           GPR2.Path_Name.Create_File (Filename_Type (Param));

      elsif Arg = "-p" then
         Result.Create_Missing_Dirs := True;

      elsif Arg = "-r" then
         Result.Force_Recursive_Build := True;

      elsif Arg = "-R" then
         Result.No_Run_Path := True;

      elsif Arg = "-s" then
         Result.Build_If_Switch_Changes := True;

      elsif Arg = "-u" then
         Result.Unique_Recompilation := True;

      elsif Arg = "-U" then
         Result.Unique_Recompilation := True;
         Result.Force_Recursive_Build := True;

      elsif Arg = "-nostdlib" then
         if not Result.Compiler_Args.Contains (GPR2.Ada_Language) then
            Result.Compiler_Args.Include
              (GPR2.Ada_Language,
               GPR2.Containers.Value_Type_List.Empty_Vector);
         end if;

         Result.Compiler_Args (GPR2.Ada_Language).Append (String (Arg));

      elsif Arg = "-nostdinc" then
         if not Result.Compiler_Args.Contains (GPR2.Ada_Language) then
            Result.Compiler_Args.Include
              (GPR2.Ada_Language,
               GPR2.Containers.Value_Type_List.Empty_Vector);
         end if;

         Result.Compiler_Args (GPR2.Ada_Language).Append (String (Arg));

      elsif Arg = "-fstack-check" then
         if not Result.Compiler_Args.Contains (GPR2.Ada_Language) then
            Result.Compiler_Args.Include
              (GPR2.Ada_Language,
               GPR2.Containers.Value_Type_List.Empty_Vector);
         end if;

         Result.Compiler_Args (GPR2.Ada_Language).Append (String (Arg));

      elsif Arg = "-fno-inline" then
         if not Result.Compiler_Args.Contains (GPR2.Ada_Language) then
            Result.Compiler_Args.Include
              (GPR2.Ada_Language,
               GPR2.Containers.Value_Type_List.Empty_Vector);
         end if;

         Result.Compiler_Args (GPR2.Ada_Language).Append (String (Arg));

      elsif Arg = "-g" then
         if not Result.Compiler_Args.Contains (GPR2.Ada_Language) then
            Result.Compiler_Args.Include
              (GPR2.Ada_Language,
               GPR2.Containers.Value_Type_List.Empty_Vector);
         end if;

         if Param = "default" then
            Result.Compiler_Args (GPR2.Ada_Language).Append (String (Arg));
         else
            Result.Compiler_Args (GPR2.Ada_Language).Append
              (String (Arg) & Param);
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
               raise Usage_Error with
                 "argument to '-O' should be a non-negative integer, " &
                 "'g', 's' or 'fast'";
            end if;
         end if;

         if not Result.Compiler_Args.Contains (GPR2.Ada_Language) then
            Result.Compiler_Args.Include
              (GPR2.Ada_Language,
               GPR2.Containers.Value_Type_List.Empty_Vector);
         end if;

         Result.Compiler_Args (GPR2.Ada_Language).Append
           (String (Arg) & Param);

      else
         raise GPRtools.Command_Line.Command_Line_Definition_Error
           with "unexpected switch " & String (Arg);
      end if;
   end On_Switch;

end GPRbuild.Options;
