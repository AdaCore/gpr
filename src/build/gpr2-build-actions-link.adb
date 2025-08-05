--
--  Copyright (C) 2024-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with GNAT.Regpat;
with GNATCOLL.OS.FS;
with GNATCOLL.OS.FSUtil;
with GNATCOLL.OS.Process;
with GNATCOLL.OS.Stat;
with GNATCOLL.Traces;
with GNATCOLL.Utils;

with GPR2.Build.Actions.Post_Bind;
with GPR2.Build.Actions.Compile.Ada;
with GPR2.Build.ALI_Parser;
with GPR2.Build.External_Options;
with GPR2.Build.Source;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Tree;
pragma Warnings (Off, "*is not referenced");
with GPR2.Project.View.Vector;
pragma Warnings (On);
with GPR2.Reporter;
with GPR2.Source_Reference;
with GPR2.Source_Reference.Value;
with GPR2.Build.Response_Files;

package body GPR2.Build.Actions.Link is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create
                ("GPR.BUILD.ACTIONS.LINK", GNATCOLL.Traces.Off);

   function Generate_Export_File
     (Self : in out Object;
      From : Path_Name.Object) return Filename_Optional;
   --  Generates the export file that lists the symbols to be exported
   --  by the shared library.
   --  If not supported or an issue occured, Filename_Optional will be empty.

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Objects_From_Attribute
     (Self : Object'Class;
      Id   : Q_Attribute_Id)
   is
      Attr : constant GPR2.Project.Attribute.Object :=
               Self.View.Attribute (Id);
   begin
      if Self.View.Is_Library then
         if Attr.Is_Defined then
            for Val of Attr.Values loop
               declare
                  Path : constant Path_Name.Object :=
                           Path_Name.Create_File
                             (Filename_Type (Val.Text),
                              Self.View.Object_Directory.Value);
               begin
                  if Path.Exists then
                     Self.Tree.Add_Input
                       (Self.UID,
                        Artifacts.Files.Create (Path),
                        False);
                  end if;
               end;
            end loop;
         end if;
      end if;
   end Add_Objects_From_Attribute;

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option (Self : in out Object; Option : String) is
   begin
      Self.Static_Options.Append (Option);
   end Add_Option;

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean)
   is
      pragma Unreferenced (Slot);

      function Add_Attr
        (Id           : Q_Attribute_Id;
         Index        : PAI.Object;
         Is_List      : Boolean;
         In_Signature : Boolean;
         Param        : String := "") return Boolean;

      procedure Append_Rpath
        (Path : Path_Name.Object);

      procedure Check_Ada_Runtime_Needed
        (Libgnat : out Boolean;
         Libgnarl : out Boolean);
      --  When a shared library is linked without a binding phase, we need to
      --  detect if libgnat/libgnarl is needed or not. This uses the ali files
      --  for this end.

      function Is_Partially_Linked
        (View : GPR2.Project.View.Object) return Boolean;
      --  Return true if the Library_Partial_Linker is set with a
      --  non-empty value.
      --  ??? Because we do not support partial links for now, this function
      --  always return False. To be updated once the support has been
      --  implemented.

      Objects      : Tree_Db.Artifact_Sets.Set;
      Rpath        : Unbounded_String;
      Rpath_Origin : constant GPR2.Project.Attribute.Object :=
                       Self.Ctxt.Attribute (PRA.Run_Path_Origin);

      --------------
      -- Add_Attr --
      --------------

      function Add_Attr
        (Id           : Q_Attribute_Id;
         Index        : PAI.Object;
         Is_List      : Boolean;
         In_Signature : Boolean;
         Param        : String := "") return Boolean
      is
         Attr : constant Project.Attribute.Object :=
                  Self.View.Attribute (Id, Index);
         Mode : constant Build.Command_Line.Signature_Mode :=
                  (if In_Signature
                   then Build.Command_Line.In_Signature
                   else Build.Command_Line.Ignore);

         procedure Add (Arg : String);
         --  Check for full path whenever possible

         procedure Add (Arg : String) is
         begin
            if Arg (Arg'First) = '-' then
               if GNATCOLL.Utils.Starts_With
                 (Arg, Self.Tree.Linker_Lib_Dir_Option)
               then
                  declare
                     Sub : constant Value_Type :=
                             Arg
                               (Arg'First +
                                  Self.Tree.Linker_Lib_Dir_Option'Length
                                    .. Arg'Last);
                     Path : constant Path_Name.Object :=
                              Path_Name.Create_Directory
                                (Filename_Type (Sub),
                                 Self.Ctxt.Dir_Name.Value);
                  begin
                     Cmd_Line.Add_Argument
                       (Self.Tree.Linker_Lib_Dir_Option &
                          String
                            (Path.Relative_Path (Self.Working_Directory)));
                  end;
               else
                  Cmd_Line.Add_Argument (Arg, Mode);
               end if;
            else
               declare
                  Full : constant Path_Name.Object :=
                           Path_Name.Create_File
                             (Filename_Type (Arg),
                              Self.Ctxt.Dir_Name.Value);
               begin
                  if Full.Exists then
                     Cmd_Line.Add_Argument (Full, Mode);
                  else
                     Cmd_Line.Add_Argument (Arg, Mode);
                  end if;
               end;
            end if;
         end Add;

      begin
         if not Attr.Is_Defined then
            return False;
         end if;

         if Is_List then
            for Idx in Attr.Values.First_Index .. Attr.Values.Last_Index loop
               if Idx < Attr.Values.Last_Index then
                  if Attr.Values.Element (Idx).Text'Length > 0 then
                     Add (Attr.Values.Element (Idx).Text);
                  end if;

               elsif Param'Length > 0
                 or else Attr.Values.Element (Idx).Text'Length > 0
               then
                  Add (Attr.Values.Element (Idx).Text & Param);
               end if;
            end loop;

         elsif Param'Length > 0 or else Attr.Value.Text'Length > 0 then
            Add (Attr.Value.Text & Param);
         end if;

         return True;
      end Add_Attr;

      ------------------
      -- Append_Rpath --
      ------------------

      procedure Append_Rpath (Path : Path_Name.Object) is
      begin
         if Length (Rpath) /= 0 then
            --  ??? hard coded value: ok for now since this is not
            --  used on windows, but we may need an attribute for that
            --  at some point.
            Append (Rpath, ':');
         end if;

         if Rpath_Origin.Is_Defined then
            --  ??? This processing is unix-oriented with unix
            --  path and directory delimiters. This is somewhat
            --  expected since this mechanism is not available on
            --  windows, but then we still need to properly cross
            --  compilation on windows hosts, so may need to
            --  "posixify" the paths here.

            declare
               From : constant Path_Name.Object :=
                        Self.Working_Directory;
               Rel  : constant String :=
                        String (Path.Relative_Path (From));
               Last : constant Natural :=
                        (if Rel (Rel'Last) = '/'
                         then Rel'Last - 1
                         else Rel'Last);
            begin
               Append
                 (Rpath,
                  Rpath_Origin.Value.Text & "/" & Rel (Rel'First .. Last));
            end;
         else
            Append (Rpath, String (Path.Dir_Name));
         end if;
      end Append_Rpath;

      ------------------------------
      -- Check_Ada_Runtime_Needed --
      ------------------------------

      procedure Check_Ada_Runtime_Needed
        (Libgnat : out Boolean;
         Libgnarl : out Boolean) is
      begin
         Libgnat  := False;
         Libgnarl := False;

         for Obj of Objects loop
            if Self.Tree.Has_Predecessor (Obj)
              and then
                Self.Tree.Predecessor (Obj) in Actions.Compile.Ada.Object'Class
            then
               Libgnat := True;

               declare
                  Comp : constant Actions.Compile.Ada.Object'Class :=
                           Actions.Compile.Ada.Object'Class
                             (Self.Tree.Predecessor (Obj));
                  Deps : Containers.Filename_Set;
               begin
                  if ALI_Parser.Dependencies (Comp.Local_Ali_File.Path, Deps)
                    and then Deps.Contains ("s-osinte.ads")
                  then
                     Libgnarl := True;

                     exit;
                  end if;
               end;
            end if;
         end loop;
      end Check_Ada_Runtime_Needed;

      -------------------------
      -- Is_Partially_Linked --
      -------------------------

      function Is_Partially_Linked
        (View : GPR2.Project.View.Object) return Boolean
      is
      begin
         --  ??? We do not support partial linking for now. To be updated
         --  once this is the case.

         return False;
      end Is_Partially_Linked;

      Status       : Boolean;
      Src_Idx      : constant PAI.Object :=
                       (if not Self.Is_Library
                        then PAI.Create
                          (String (Self.Main_Src.Source.Simple_Name),
                           Case_Sensitive => File_Names_Case_Sensitive,
                           At_Pos         => Self.Main_Src.Index)
                        else PAI.Undefined);
      Link_Exec    : constant Boolean := Src_Idx.Is_Defined;
      Ign          : Boolean with Unreferenced;
      Lib_Dir_Opt  : constant Value_Type :=
                       Self.Tree.Linker_Lib_Dir_Option;
      Dash_l_Opts  : GPR2.Containers.Value_List;
      --  -l needs to be last in the command line, so we add them here and
      --  then append to the command line in the end

   begin
      Objects := Self.Embedded_Objects;

      for Lib of Self.Library_Dependencies loop
         declare
            Link : constant Object'Class :=
                     Object'Class (Self.Tree.Action (Lib));
         begin

            --  Remove from this list of objects the ones that come from
            --  libraries.

            Objects.Difference (Link.Embedded_Objects);

            --  For standalone static libraries, linker options must be
            --  updated to ensure proper elaboration of the library. There
            --  are two possible scenarios:
            --  * If the library is externally built, the linker options are
            --    embedded within the library itself, typically in a custom
            --    section of the object file generated by the binder. An
            --    action should have been created to extract this information.
            --  * If the library is not externally built, the linker options
            --    can be directly retrieved from the associated action, as
            --    demonstrated in the following code.

            if not Self.Is_Library
              and then not Link.View.Is_Externally_Built
              and then Link.View.Is_Library
              and then Link.View.Is_Static_Library
              and then Link.View.Is_Library_Standalone
            then
               for Opt of Link.Options loop
                  Traces.Trace
                    ("Adding the link option """ & Opt & """ to " &
                     Self.UID.Image & " coming from the static standalone " &
                     " library " & String (Link.View.Library_Name));
                  Self.Add_Option (Opt);
               end loop;
            end if;
         end;
      end loop;

      if Self.Is_Static_Library then
         declare
            Attr : constant GPR2.Project.Attribute.Object :=
                     Self.View.Attribute (PRA.Archive_Builder);
            First : Boolean := True;
         begin
            pragma Assert (Attr.Is_Defined, "No archiver is defined");

            for Val of Attr.Values loop
               if First then
                  --  The driver value
                  Cmd_Line.Set_Driver (Val.Text);
                  First := False;

               --  [eng/gpr/gpr-issues#446] Hack to speed up and ease the
               --  generation of archives :
               --  instead of using "ar cr" then use ranlib, we generate
               --  directly the symbol table by using "ar csr".

               elsif Val.Text = "cr" then
                  Cmd_Line.Add_Argument ("csr");
               else
                  Cmd_Line.Add_Argument (Val.Text);
               end if;
            end loop;

            Cmd_Line.Add_Argument (Self.Output.Path);
         end;

      else
         declare
            Attr : constant GPR2.Project.Attribute.Object :=
                     Self.Ctxt.Attribute (PRA.Linker.Driver);
         begin
            if not Attr.Is_Defined then
               return;
            end if;

            Cmd_Line.Set_Driver (Attr.Value.Text);
         end;

         if Src_Idx.Is_Defined then
            Status :=
              Add_Attr (PRA.Linker.Leading_Switches, Src_Idx, True, True);
         end if;

         if Self.Is_Library then

         --  shared lib case, add the leading and minimal options

            Status := Add_Attr
              (PRA.Leading_Library_Options, PAI.Undefined, True, True);
            Status := Add_Attr
              (PRA.Shared_Library_Minimum_Switches, PAI.Undefined, True, True);
         end if;

         --  ??? This shouldn't be hardcoded
         Cmd_Line.Add_Argument ("-o");
         Cmd_Line.Add_Argument (Self.Output.Path);
      end if;

      for Obj of Objects loop
         Cmd_Line.Add_Argument
           (Artifacts.Files.Object'Class (Obj).Path,
            Build.Command_Line.Simple,
            Build.Command_Line.Obj);
      end loop;

      if not Self.Is_Static_Library then
         --  Add the runtime directory to the rpath: it won't be listed in the
         --  library dependencies.

         if Self.Ctxt.Tree.Has_Runtime_Project then
            Append_Rpath (Self.Ctxt.Tree.Runtime_Project.Object_Directory);
         end if;

         if Self.Lib_Dep_Circle then
            Cmd_Line.Add_Argument
              (Self.Ctxt.Attribute (PRA.Linker.Group_Start_Switch).Value.Text);
         end if;

         for Lib of Self.Library_Dependencies loop
            declare
               Link         : constant Object'Class :=
                                Object'Class (Self.Tree.Action (Lib));
               Lib_Artifact : constant GPR2.Path_Name.Object :=
                                Link.Output.Path;

               use GNATCOLL.Utils;
            begin

               --  We can not rely on the view to obtain the library
               --  information, as some libraries dependencies can be created
               --  dynamically in the tree database. For instance if the
               --  project view contains a C main that depends on Ada code,
               --  a standalone Ada library is created to be included by
               --  the C main.

               if Link.Is_Static then

                  --  Add the static archive

                  Cmd_Line.Add_Argument (Lib_Artifact.String_Value);
               else
                  --  Add flags to include the shared library

                  Cmd_Line.Add_Argument
                    (Lib_Dir_Opt &
                       String (Link.View.Library_Directory.Relative_Path
                         (Self.Working_Directory)));

                  --  Add the library directory to the rpath of the
                  --  executable, so that LD_LIBRARY_PATH does not need to
                  --  be set before execution.

                  Append_Rpath (Link.View.Library_Directory);

                  declare
                     Prefix : constant Value_Type :=
                                Link.View.Attribute
                                  (PRA.Shared_Library_Prefix).Value.Text;
                     BN     : constant String :=
                                String (Link.View.Library_Filename
                                         (Without_Version => True).Base_Name);
                  begin
                     pragma Assert
                       (Starts_With (BN, Prefix),
                        "The library artifact name doesn't start with the" &
                          "prefix """ & Prefix & '"');

                     --  ??? -l can be replaced with the value specified with
                     --  the Linker_Lib_Name_Option option. Need to investigate
                     --  to know if this option is required.
                     Dash_l_Opts.Append
                       ("-l" &
                          String (BN (BN'First + Prefix'Length .. BN'Last)));
                  end;
               end if;

               --  Check Library_Options if any
               declare
                  Attr : constant GPR2.Project.Attribute.Object :=
                           Link.View.Attribute (PRA.Library_Options);
                  Path : Path_Name.Object;
               begin
                  if Attr.Is_Defined then
                     for Val of Attr.Values loop
                        declare
                           Path : constant Path_Name.Object :=
                                    Path_Name.Create_File
                                      (Filename_Type (Val.Text),
                                       Link.View.Dir_Name.Value);
                        begin
                           if not Path.Exists then
                              if not Link.Is_Static then
                                 if not Self.No_Rpath
                                   and then Starts_With (Val.Text, Lib_Dir_Opt)
                                 then
                                    --  Amend the RPATH with the directory
                                    --  value
                                    Append_Rpath
                                      (Path_Name.Create_Directory
                                         (Filename_Optional
                                              (Val.Text
                                                   (Val.Text'First +
                                                          Lib_Dir_Opt'Length ..
                                                            Val.Text'Last)),
                                          Link.Working_Directory.Value));
                                 end if;

                                 if Starts_With (Val.Text, "-l") then
                                    Dash_l_Opts.Append (Val.Text);

                                 else
                                    Cmd_Line.Add_Argument (Val.Text);
                                 end if;

                              else
                                 if not Signature_Only then
                                    Self.Tree.Reporter.Report
                                      (GPR2.Message.Create
                                         (GPR2.Message.Error,
                                          "unknown object file """ &
                                            Val.Text & '"',
                                          Val));
                                 end if;

                                 raise Action_Error;
                              end if;
                           end if;
                        end;
                     end loop;
                  end if;
               end;
            end;
         end loop;

         --  Ignore: no presence of Run_Path_Option is expected if
         --  Run_Path_Option is not available, like with windows dlls.

         if not Self.No_Rpath and then Length (Rpath) > 0 then
            Ign := Add_Attr
              (PRA.Run_Path_Option,
               PAI.Undefined,
               True,
               True,
               -Rpath);
         end if;

         for C of Self.View.Closure (True) loop
            declare
               Opt     : constant Project.Attribute.Object :=
                           C.Attribute (PRA.Linker.Linker_Options);
               Lib_Opt : constant Value_Type :=
                           Self.Tree.Linker_Lib_Dir_Option;
               use GNATCOLL.Utils;
               use type GPR2.Project.View.Object;
            begin
               if Opt.Is_Defined then
                  for Val of Opt.Values loop
                     declare
                        Arg  : constant Value_Type := Val.Text;
                        Path : Path_Name.Object;
                     begin
                        if Arg'Length >= Lib_Opt'Length
                          and then Starts_With (Arg, Lib_Opt)
                        then
                           --  Need to check that any -L<path> option has an
                           --  absolute dir.

                           Path := Path_Name.Create_Directory
                             (Filename_Type
                                (Arg (Arg'First + Lib_Opt'Length .. Arg'Last)),
                              C.Dir_Name.Value);
                           Cmd_Line.Add_Argument
                             (Lib_Opt & Path.String_Value);

                        elsif Arg (Arg'First) = '-' then
                           --  ??? How about case where linker switches don't
                           --  start with a dash?

                           if C /= Self.View then
                              --  For self.View, use non-switch parts of
                              --  the linker option only.

                              if Starts_With (Arg, "-l") then
                                 Dash_l_Opts.Append (Arg);
                              else
                                 Cmd_Line.Add_Argument (Val.Text);
                              end if;
                           end if;

                        else
                           --  Check for relative paths and translate them
                           --  as absolute.

                           Cmd_Line.Add_Argument
                             (Path_Name.Create_File
                                (Filename_Type (Val.Text),
                                 C.Dir_Name.Value));
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end loop;

         for Arg of Dash_l_Opts loop
            Cmd_Line.Add_Argument (Arg);
         end loop;

         if Self.Lib_Dep_Circle then
            Cmd_Line.Add_Argument
              (Self.Ctxt.Attribute (PRA.Linker.Group_End_Switch).Value.Text);
         end if;
      end if;

      if Link_Exec then
         --  Add switches for linking an executable
         Status := Add_Attr (PRA.Linker.Switches, Src_Idx, True, True);

         if not Status then
            Status := Add_Attr
              (PRA.Linker.Default_Switches,
               PAI.Create
                 (Self.View.Visible_Source (Self.Main_Src.Source).Language),
               True,
               True);
         end if;
      end if;

      if not Self.Is_Static_Library then
         --  Add -largs

         for Arg
           of Self.Tree.External_Options.Fetch
             (External_Options.Linker, GPR2.No_Language)
         loop
            Cmd_Line.Add_Argument (Arg);
         end loop;
      end if;

      --  Runtime flags usually come from the binder. However, there is no
      --  binding phase when creating a non-standalone library. Therefore,
      --  we need to add the runtime flags manually in this case. Note that
      --  static libraries do not require any runtime flags, so we are
      --  processing only the shared libraries here, and moreover this is only
      --  needed on Windows.

      if Self.View.Tree.Has_Runtime_Project
        and then Self.View.Tree.Is_Windows_Target
        and then Self.View.Is_Library
        and then not Self.View.Is_Library_Standalone
        and then Self.View.Is_Shared_Library
      then
         declare
            Gnat_Version   : constant String :=
                               Self.View.Tree.Ada_Compiler_Version;
            Needs_Libgnat  : Boolean;
            Needs_Libgnarl : Boolean;
         begin
            if Gnat_Version /= "" then
               Check_Ada_Runtime_Needed (Needs_Libgnat, Needs_Libgnarl);

               if Needs_Libgnat then
                  if Needs_Libgnarl then
                     Cmd_Line.Add_Argument ("-lgnarl-" & Gnat_Version);
                  end if;

                  Cmd_Line.Add_Argument ("-lgnat-" & Gnat_Version);
               end if;

               Cmd_Line.Add_Argument
                 (Self.Tree.Linker_Lib_Dir_Option &
                    Self.View.Tree.Runtime_Project.Object_Directory.
                      String_Value);
            end if;
         end;
      end if;

      --  Add options provided by the binder if needed

      if not Self.View.Is_Library
        or else Self.View.Is_Shared_Library
      then
         for Option of Self.Static_Options loop
            --  ??? Weird bug on windows happening when a backslash is ending
            --  the argument, and the arg contains a space, then ld reacts just
            --  as if there was some hidden \" ending the argument and thus
            --  escapes the rest of the command line. Skip any trailing
            --  backslash...

            if Option (Option'Last) = '\' then
               Cmd_Line.Add_Argument
                 (Option (Option'First .. Option'Last - 1));
            else
               Cmd_Line.Add_Argument (Option);
            end if;
         end loop;
      end if;

      if Self.View.Is_Library
        and then not Self.Is_Static
      then
         Ign := Add_Attr (PRA.Library_Options, PAI.Undefined, True, True);
      end if;

      --  For shared libs, use an export symbol file when possible

      if Self.Is_Library and then not Self.Is_Static then
         Self.Handle_Export_File (Cmd_Line, Signature_Only, False);
      end if;

      if Link_Exec then
         Status :=
           Add_Attr (PRA.Linker.Required_Switches, PAI.Undefined, True, True);

         Status :=
           Add_Attr (PRA.Linker.Trailing_Switches, Src_Idx, True, True);
      end if;

      --  Filter out some duplicates that may happen during command line
      --  construction:

      --  on 32-bit windows, contradictory --shared-libgcc and --static-libgcc
      --  lead to invalid link for cross-handling of c++/Ada exceptions. Only
      --  the last one should remain.
      --  ??? That's the linker's job to handle those contradictory switches,
      --  we probably can do something to fix it rather than this very specific
      --  handling. All other linkers seem to handle that properly.
      --  ??? If we want to have a generic answer to this kind of situation,
      --  how to express that in KB terms?

      declare
         Seen          : Boolean := False;
         Static_Arg    : Boolean := False;
         Shared_Arg    : Boolean := False;
      begin
         for J in reverse
           Cmd_Line.Argument_List.First_Index ..
             Cmd_Line.Argument_List.Last_Index
         loop
            Static_Arg :=
              Cmd_Line.Argument_List.Element (J) = "-static-libgcc";
            Shared_Arg :=
              Cmd_Line.Argument_List.Element (J) = "-shared-libgcc";

            if Static_Arg or else Shared_Arg then
               if not Seen then
                  Seen := True;
               else
                  Cmd_Line.Remove (J);
               end if;
            end if;
         end loop;
      end;

      --  Finally remove any duplicated --specs switch as this may cause
      --  trouble by introducing duplicated symbols in the result.

      Cmd_Line.Filter_Duplicate_Switches ("--specs");

      --  Same for linker scripts, this time keeping the first one seen on the
      --  command line so that order is respected.

      Cmd_Line.Filter_Duplicate_Switches ("-T", Keep_Leftmost => True);
   end Compute_Command;

   ----------------------------
   -- Compute_Response_Files --
   ----------------------------

   overriding procedure Compute_Response_Files
     (Self           : in out Object;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean) is
   begin
      if not Signature_Only then
         declare
            use Build.Response_Files;

            A_RFF  : constant Project.Attribute.Object :=
                       Self.View.Attribute (PRA.Linker.Response_File_Format);
            A_RFS  : constant Project.Attribute.Object :=
                       Self.View.Attribute (PRA.Linker.Response_File_Switches);
            RFS    : constant Containers.Source_Value_List :=
                       (if A_RFS.Is_Defined
                        then A_RFS.Values
                        else Containers.Empty_Source_Value_List);
            A_CLML : constant Project.Attribute.Object :=
                       Self.View.Attribute
                         (PRA.Linker.Max_Command_Line_Length);
            CLML   : constant Natural :=
                       (if A_CLML.Is_Defined
                        then Natural'Value (A_CLML.Value.Text)
                        else 0);
            Format : Response_File_Format := None;
         begin
            if A_RFF.Is_Defined then
               declare
                  LV : constant String :=
                         Ada.Characters.Handling.To_Lower (A_RFF.Value.Text);
               begin
                  if LV = "gnu" then
                     Format := GNU;
                  elsif LV = "object_list" then
                     Format := Object_List;
                  elsif LV = "gcc_gnu" then
                     Format := GCC_GNU;
                  elsif LV = "gcc_option_list" then
                     Format := GCC_Option_List;
                  elsif LV = "gcc_object_list" then
                     Format := GCC_Object_List;
                  end if;
               end;
            end if;

            --  [eng/gpr/gpr-issues#446]
            --  The archive builder has a specific format for its response file
            --  which does not correspond to what the attribute
            --  Response_File_Format yields hence the need to override here.
            --  This should probably be dealt with the KB.
            if Self.Is_Static_Library then
               Format := GNU_Archiver;
            end if;

            Self.Response_Files.Initialize (Format, Linker, CLML, RFS);

            if Self.Response_Files.Length_Restriction (Cmd_Line) then
               declare
                  Needs_Formating : constant Boolean :=
                                      Format in GCC_Formatting_Required;
               begin
                  if Needs_Formating then
                     declare
                        Resp_File : constant Tree_Db.Temp_File :=
                                      Self.Get_Or_Create_Temp_File
                                        ("response_file", Local);
                     begin
                        Self.Response_Files.Register
                          (Resp_File.FD,
                           Resp_File.Path,
                           Secondary => True);
                     end;
                  end if;

                  declare
                     RF_Name   : constant Filename_Type :=
                                   (if Needs_Formating
                                    then "encapsulated_"
                                    else "") & "response_file";
                     Resp_File : constant Tree_Db.Temp_File :=
                                   Self.Get_Or_Create_Temp_File
                                     (RF_Name, Local);
                  begin
                     Self.Response_Files.Register
                       (Resp_File.FD,
                        Resp_File.Path);
                  end;
               end;

               Self.Response_Files.Create (Cmd_Line);
            end if;
         end;
      end if;
   end Compute_Response_Files;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature
     (Self      : in out Object;
      Load_Mode : Boolean)
   is
   begin
      for Obj of Self.Embedded_Objects loop
         if not Self.Signature.Add_Input (Obj) and then Load_Mode then
            return;
         end if;
      end loop;

      for Lib of Self.Library_Dependencies loop
         declare
            Link : constant Object'Class :=
                     Object'Class (Self.Tree.Action (Lib));
         begin
            if Link.Is_Static
              and then not Self.Signature.Add_Input (Link.Output)
              and then Load_Mode
            then
               return;
            end if;
         end;
      end loop;

      if Self.Lib_Symbol_File.Is_Defined then
         if not Self.Signature.Add_Input (Self.Lib_Symbol_File)
           and then Load_Mode
         then
            return;
         end if;
      end if;

      if not Self.Signature.Add_Output (Self.Output) and then Load_Mode then
         return;
      end if;
   end Compute_Signature;

   ----------------------
   -- Embedded_Objects --
   ----------------------

   function Embedded_Objects
     (Self : Object'Class) return Build.Tree_Db.Artifact_Sets.Set
   is
   begin
      return Result : Tree_Db.Artifact_Sets.Set do
         for Input of Self.Tree.Inputs (Self.UID) loop
            --  Action inputs can be of various types, but objects at least
            --  inherit from Artifacts.Files.Object.
            --  Libraries are represented by the Artifact.Library class, which
            --  we ignore since we are only interested in the objects contained
            --  within the libraries.

            if Input in Artifacts.Files.Object'Class and then
               Input not in Artifacts.Library.Object'Class
            then
               Result.Include (Input);
            end if;
         end loop;
      end return;
   end Embedded_Objects;

   --------------------------
   -- Generate_Export_File --
   --------------------------

   function Generate_Export_File
     (Self : in out Object;
      From : Path_Name.Object) return Filename_Optional
   is
      procedure Extract_Symbols (Obj : Path_Name.Object);

      Syms    : GPR2.Containers.Value_Set;
      Obj     : Path_Name.Object;
      Nm      : constant Project.Attribute.Object :=
                  Self.View.Attribute (PRA.Object_Lister);
      Nm_Cmd  : GNATCOLL.OS.Process.Argument_List;
      Matcher : constant GNAT.Regpat.Pattern_Matcher :=
                  GNAT.Regpat.Compile
                    (Self.View.Attribute
                       (PRA.Object_Lister_Matcher).Value.Text);
      Attrs   : GNATCOLL.OS.Stat.File_Attributes;

      ---------------------
      -- Extract_Symbols --
      ---------------------

      procedure Extract_Symbols (Obj : Path_Name.Object)
      is
         use GNATCOLL.OS;
         use GPR2.Reporter;
         Args   : Process.Argument_List := Nm_Cmd;
         Status : Integer;
         Output : Unbounded_String;
      begin
         Args.Append (String (Obj.Relative_Path (Self.Working_Directory)));

         if Self.Tree.Reporter.User_Verbosity >= Verbose
           or else (Self.Tree.Reporter.User_Verbosity = Unset
                    and then Self.Tree.Reporter.Verbosity >= Verbose)
         then
            declare
               Img   : Unbounded_String;
               First : Boolean := True;
            begin
               for A of Args loop
                  if First then
                     First := False;
                  else
                     Append (Img, " ");
                  end if;

                  Append (Img, A);
               end loop;

               Self.Tree.Reporter.Report (-Img);
            end;
         end if;

         begin
            Output := Process.Run
              (Args,
               Cwd    => Self.Working_Directory.String_Value,
               Stderr => FS.Standout,
               Status => Status);

         exception
            when Ex : GNATCOLL.OS.OS_Error =>
               Status := -1;
         end;

         declare
            use GNAT.Regpat;

            Outp    : constant String := -Output;
            Idx     : Natural := Outp'First;
            Matches : Match_Array (1 .. Paren_Count (Matcher));
         begin
            loop
               GNAT.Regpat.Match
                 (Self       => Matcher,
                  Data       => Outp,
                  Matches    => Matches,
                  Data_First => Idx);

               exit when Matches (1).First = 0;

               if Outp (Matches (1).Last) = ASCII.CR then
                  Matches (1).Last := Matches (1).Last - 1;
               end if;

               Syms.Include (Outp (Matches (1).First .. Matches (1).Last));
               Idx := Matches (1).Last + 1;
               exit when Idx > Outp'Last;
            end loop;
         end;
      end Extract_Symbols;

   begin
      if not From.Is_Defined then
         --  Add symbold from the Ada interface:
         for V of Nm.Values loop
            Nm_Cmd.Append (V.Text);
         end loop;

         for CU of Self.Interface_Units loop
            declare
               --  Only Ada sources considered here
               UID  : constant Actions.Compile.Ada.Ada_Compile_Id :=
                        Actions.Compile.Ada.Create (CU);
               Comp : constant Actions.Compile.Ada.Object :=
                        Actions.Compile.Ada.Object (Self.Tree.Action (UID));
            begin
               Extract_Symbols (Comp.Object_File.Path);
            end;
         end loop;

         --  Now add symbols from the non-Ada interfaces
         for C in Self.Ctxt.Interface_Sources.Iterate loop
            declare
               Path : constant Filename_Type :=
                        Containers.Source_Path_To_Sloc.Key (C);
               Src  : constant Build.Source.Object :=
                        Self.Ctxt.Visible_Source (Path);
            begin
               if not Src.Has_Units then
                  if Src.Kind = S_Body then
                     --  Found a non-Ada body as part of the interface, add its
                     --  corresponding object file
                     declare
                        Comp_Id : constant Actions.Compile.Compile_Id'Class :=
                                    Actions.Compile.Create
                                      (Src.Path_Name.Simple_Name,
                                       Src.Language,
                                       Src.Owning_View);
                        Comp    : constant GPR2.Build.Actions.Compile.Object :=
                                    Actions.Compile.Object
                                      (Self.Tree.Action (Comp_Id));
                     begin
                        Extract_Symbols (Comp.Object_File.Path);
                     end;

                  else
                     --  ??? A spec is declared as Interfaces, old tool used
                     --  to consider basename.o as a candidate which looks
                     --  sematically wrong. However considering the history,
                     --  let's lookup for an equivalent .c file and use it if
                     --  it exists.

                     --  ??? Ideally we should build the spec with the current
                     --  view's attributes, and extract the undefined symbols
                     --  (which are the symbols the spec wants to include).

                     declare
                        Attr    : constant Project.Attribute.Object :=
                                    Self.Ctxt.Attribute
                                      (PRA.Naming.Body_Suffix,
                                       PAI.Create (Src.Language));
                        pragma Assert (Attr.Is_Defined,
                                       "Source spec found in interfaces " &
                                         "without proper implementation " &
                                         "definition");
                        Suffix  : constant Simple_Name :=
                                    Simple_Name (Attr.Value.Text);
                        Eq_Body : constant Simple_Name :=
                                    Src.Path_Name.Base_Filename & Suffix;
                        Src_B   : constant Build.Source.Object :=
                                    Self.Ctxt.Visible_Source (Eq_Body);
                     begin
                        if Src_B.Is_Defined then
                           declare
                              Comp_Id : constant Compile.Compile_Id'Class :=
                                          Actions.Compile.Create
                                            (Src_B.Path_Name.Simple_Name,
                                             Src_B.Language,
                                             Src.Owning_View);
                              Comp    : constant Compile.Object :=
                                          Actions.Compile.Object
                                            (Self.Tree.Action (Comp_Id));
                           begin
                              Extract_Symbols (Comp.Object_File.Path);
                           end;

                           --  ??? if Src_B is undefined we should probably
                           --  issue a warning that the spec in the interface
                           --  is unused.
                        end if;
                     end;
                  end if;
               end if;
            end;
         end loop;

         --  Finally don't forget the binder-generated object if any

         if Self.Bind.Is_Defined then
            declare
               --  Make sure we retrieve an up-to-date bind object
               Bind : constant Ada_Bind.Object :=
                        Ada_Bind.Object (Self.Tree.Action (Self.Bind.UID));
            begin
               Obj := Bind.Post_Bind.Object_File.Path;
            end;

            Extract_Symbols (Obj);
         end if;

      else
         --  Read the symbols from the specified file

         Attrs := GNATCOLL.OS.Stat.Stat (From.String_Value);

         if not GNATCOLL.OS.Stat.Is_File (Attrs) then
            Self.Tree.Reporter.Report
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  '"' & From.String_Value & """ is not a file",
                  GPR2.Source_Reference.Create
                    (Self.Ctxt.Path_Name.Value, 0, 0)));
            return "";

         elsif not GNATCOLL.OS.Stat.Is_Readable (Attrs) then
            Self.Tree.Reporter.Report
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  '"' & From.String_Value & """ is not a readable",
                  GPR2.Source_Reference.Create
                    (Self.Ctxt.Path_Name.Value, 0, 0)));
            return "";

         elsif GNATCOLL.OS.Stat.Length (Attrs) >
           Long_Long_Integer (Natural'Last)
         then
            Self.Tree.Reporter.Report
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  '"' & From.String_Value & """ is too big",
                  GPR2.Source_Reference.Create
                    (Self.Ctxt.Path_Name.Value, 0, 0)));
            return "";
         end if;

         declare
            Buffer : String (1 .. Natural (GNATCOLL.OS.Stat.Length (Attrs)));
            FD     : constant GNATCOLL.OS.FS.File_Descriptor :=
                       GNATCOLL.OS.FS.Open (From.String_Value);
            Ign    : Natural with Unreferenced;
            First  : Natural := 1;
            Last   : Natural;

         begin
            Ign := GNATCOLL.OS.FS.Read (FD, Buffer);
            GNATCOLL.OS.FS.Close (FD);

            while First < Buffer'Last loop
               Last := GNATCOLL.Utils.Line_End (Buffer, First + 1);

               declare
                  Line : constant String :=
                           Ada.Strings.Fixed.Trim
                             (Buffer (First .. Last),
                              Ada.Strings.Both);
               begin
                  if Line'Length > 0 then
                     Syms.Include (Line);
                  end if;
               end;

               First := GNATCOLL.Utils.Next_Line
                 (Buffer, Natural'Max (First, Last));
            end loop;
         end;
      end if;

      if not Syms.Is_Empty then
         declare
            type Supported_Export_File_Format is (Flat, GNU, DEF);

            File_Format : Supported_Export_File_Format := GNU;
            Attr        : constant GPR2.Project.Attribute.Object :=
                            Self.View.Attribute
                              (PRA.Linker.Export_File_Format);
            Tmp         : Tree_Db.Temp_File :=
                            Actions.Get_Or_Create_Temp_File
                              (Self,
                               "syms_export",
                               Local,
                               ".def");
         begin
            if Attr.Is_Defined then
               File_Format :=
                 Supported_Export_File_Format'Value (Attr.Value.Text);
            end if;

            if File_Format = GNU then
               GNATCOLL.OS.FS.Write
                 (Tmp.FD,
                  "SYMS {" & ASCII.LF &
                    "   global:" & ASCII.LF);
            elsif File_Format = DEF then
               GNATCOLL.OS.FS.Write
                 (Tmp.FD,
                  "EXPORTS" & ASCII.LF);
            end if;

            for S of Syms loop
               GNATCOLL.OS.FS.Write (Tmp.FD, S);

               if File_Format = GNU then
                  GNATCOLL.OS.FS.Write (Tmp.FD, ";");
               end if;

               GNATCOLL.OS.FS.Write (Tmp.FD, "" & ASCII.LF);
            end loop;

            if File_Format = GNU then
               GNATCOLL.OS.FS.Write
                 (Tmp.FD,
                  "   local: *;" & ASCII.LF &
                    "};" & ASCII.LF);
            end if;

            GNATCOLL.OS.FS.Close (Tmp.FD);

            return Tmp.Path;
         end;
      else
         return "";
      end if;
   end Generate_Export_File;

   ------------------------
   -- Handle_Export_File --
   ------------------------

   procedure Handle_Export_File
     (Self           : in out Object;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean;
      No_Warning     : Boolean)
   is
      use GPR2.Project;
      Object_Lister      : constant Project.Attribute.Object :=
                             Self.View.Attribute (PRA.Object_Lister);
      Export_File_Switch : constant Project.Attribute.Object :=
                             Self.View.Attribute
                               (PRA.Linker.Export_File_Switch);
      Symbol_File        : Path_Name.Object;
      Export_Policy_Attr : constant Project.Attribute.Object :=
                             Self.View.Attribute
                               (PRA.Library_Symbol_Policy);
      type Symbol_Policy is (Restricted, Unrestricted);
      Export_Policy      : constant Symbol_Policy :=
                             (if not Export_Policy_Attr.Is_Defined
                              or else Name_Type
                                (Export_Policy_Attr.Value.Text) =
                                "restricted"
                              then Restricted
                              else Unrestricted);

   begin
      if Self.View.Library_Standalone = No then
         --  On Windows, if we are building a standard library or a library
         --  with unrestricted symbol-policy make sure all symbols are
         --  exported.

         if Self.View.Tree.Is_Windows_Target
           and then (Export_Policy = Unrestricted
                     or else Self.Ctxt.Library_Standalone = No
                     or else not Export_File_Switch.Is_Defined)
         then
            --  This is needed if an object contains a declspec(dllexport)
            --  as in this case only the specified symbols will be exported.
            --  That is the linker change from export-all to export only the
            --  symbols specified as dllexport.

            --  ??? Create a proper Linker attribute for that

            Cmd_Line.Add_Argument ("-Wl,--export-all-symbols");
         end if;

         return;
      end if;

      if Signature_Only then
         --  Don't go further, as this generates a temporary file
         return;
      end if;

      if Export_Policy = Unrestricted then
         --  unrestricted symbol policy: export all symbols. Nothing to do
         --  appart emitting a warning if a library symbol file is defined

         if not No_Warning then
            if Self.Lib_Symbol_File.Is_Defined then
               Self.Tree.Reporter.Report
                 (GPR2.Message.Create
                    (GPR2.Message.Warning,
                     "Library_Symbol_File attribute is ignored",
                     Self.View.Attribute
                       (PRA.Library_Symbol_File).Value));
               Self.Tree.Reporter.Report
                 (GPR2.Message.Create
                    (GPR2.Message.Warning,
                     "because Library_Symbol_Policy attribute has """ &
                       Export_Policy_Attr.Value.Text & """ value",
                     Export_Policy_Attr.Value));
            end if;
         end if;

         return;
      end if;

      if Export_File_Switch.Is_Defined then
         declare
            Tmp_File : constant Filename_Optional :=
                         Self.Generate_Export_File
                           (Self.Lib_Symbol_File.Path);
         begin
            if Tmp_File'Length > 0 then
               Symbol_File := Path_Name.Create_File (Tmp_File);
            end if;
         end;

         if Symbol_File.Is_Defined then
            Cmd_Line.Add_Argument
              (Export_File_Switch.Value.Text &
                 String
                 (Symbol_File.Relative_Path (Self.Working_Directory)),
               Build.Command_Line.Ignore);
         end if;
      end if;
   end Handle_Export_File;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Object;
      Kind     : Link_Kind;
      Context  : GPR2.Project.View.Object       := GPR2.Project.View.Undefined;
      Src      : Compilation_Unit.Unit_Location := Compilation_Unit.No_Unit;
      No_Rpath : Boolean                        := True;
      Output   : Filename_Optional              := "")
   is
      Attr : GPR2.Project.Attribute.Object;
   begin
      --  Ensure the object wasn't previously initialized prior to this call
      Self := Undefined;

      case Kind is
         when Executable =>
            declare
               Exec : GPR2.Path_Name.Object;
            begin
               Self.Is_Library := False;
               Self.Main_Src   := Src;
               Self.Ctxt       := Src.View;
               Self.No_Rpath   := No_Rpath;

               if Output'Length = 0 then
                  Exec :=
                    Self.Ctxt.Executable (Src.Source.Simple_Name, Src.Index);
               else
                  declare
                     Suff : constant Filename_Optional :=
                              Self.Ctxt.Executable_Suffix;
                  begin
                     if Ada.Strings.Fixed.Index (String (Output), ".") = 0 then
                        Exec :=
                          Self.Ctxt.Executable_Directory.Compose
                            (Output & Suff);
                     else
                        Exec :=
                          Self.Ctxt.Executable_Directory.Compose (Output);
                     end if;
                  end;
               end if;

               Self.Executable := Artifacts.Files.Create (Exec);
            end;
         when Global_Archive =>
            declare
               Project_Name_Low : constant String :=
                                    Ada.Characters.Handling.To_Lower
                                      (String (Context.Name));
               Library_Filename : constant Simple_Name :=
                                    "lib" & Filename_Type (Project_Name_Low) &
                                  Context.Tree.Configuration.Archive_Suffix;
            begin
               Self.Ctxt       := Context;
               Self.Is_Library := True;
               Self.Is_Static  := True;
               Self.In_Obj     := True;
               Self.Library    := Artifacts.Library.Create
                 (Context.Object_Directory.Compose (Library_Filename));
            end;
         when Library =>
            Self.Ctxt       := Context;
            Self.Is_Library := True;
            Self.Is_Static  := Context.Is_Static_Library;
            Self.Library    :=
              Artifacts.Library.Create (Context.Library_Filename);
            Self.No_Rpath   := No_Rpath;

            Attr := Context.Attribute
              (PRA.Library_Symbol_File);

            if Attr.Is_Defined then
               Self.Lib_Symbol_File :=
                 Artifacts.Files.Create
                   (Path_Name.Create_File
                      (Filename_Type (Attr.Value.Text),
                       Context.Dir_Name.Value));
            end if;
      end case;
   end Initialize;

   ---------------------
   -- Interface_Units --
   ---------------------

   function Interface_Units
     (Self : Object'Class) return Compilation_Unit.Maps.Map
   is
      Units : Compilation_Unit.Maps.Map;
   begin
      if Self.Ctxt.Is_Library_Standalone then
         Units := Self.Ctxt.Interface_Closure;

         if Self.Bind.Is_Defined then
            for CU of Ada_Bind.Object
              (Self.Tree.Action (Self.Bind.UID)).Extended_Interface
            loop
               Units.Include (CU.Name, CU);
            end loop;
         end if;

      else
         Units := Self.Ctxt.Own_Units;
      end if;

      return Units;
   end Interface_Units;

   --------------------------
   -- Library_Dependencies --
   --------------------------

   function Library_Dependencies
     (Self : Object'Class) return Actions.Action_Id_Vectors.Vector
   is
   begin
      return Result : Action_Id_Vectors.Vector do
         for Input of Self.Tree.Inputs (Self.UID) loop
            if Input in Artifacts.Library.Object'Class then
               Result.Append (Self.Tree.Predecessor (Input).UID);
            end if;
         end loop;
      end return;
   end Library_Dependencies;

   --------------------
   -- On_Ready_State --
   --------------------

   overriding function On_Ready_State
     (Self : in out Object) return Boolean
   is
      Units     : Compilation_Unit.Maps.Map;
      Has_Error : Boolean := False;
   begin
      if not Self.Ctxt.Is_Library
        or else Self.Ctxt.Is_Externally_Built
      then
         return True;
      end if;

      Units := Self.Interface_Units;

      --  For each unit, add the corresponding Ali file as an output

      for U of Units loop
         declare
            C_Id   : constant Actions.Compile.Ada.Ada_Compile_Id :=
                       Actions.Compile.Ada.Create (U);
            From   : constant Path_Name.Object :=
                       Actions.Compile.Object
                         (Self.Tree.Action (C_Id)).Dependency_File.Path;
            To     : constant Path_Name.Object :=
                       Self.Ctxt.Library_Ali_Directory.Compose
                         (From.Simple_Name);

         begin
            GPR2.Build.Actions.Compile.Ada.Object
              (Self.Tree.Action_Id_To_Reference
                 (C_Id).Element.all).Change_Intf_Ali_File
                   (To);
         end;
      end loop;

      --  Also add the artifacts for Libarary_Src_Directory if any

      if Self.Ctxt.Has_Library_Src_Directory then
         declare
            Src_Dir : constant Path_Name.Object :=
                        Self.Ctxt.Library_Src_Directory;
         begin
            for CU of Units loop
               declare
                  procedure On_Unit_Part
                    (Kind     : Unit_Kind;
                     View     : GPR2.Project.View.Object;
                     Path     : Path_Name.Object;
                     Index    : Unit_Index;
                     Sep_Name : Optional_Name_Type);

                  ------------------
                  -- On_Unit_Part --
                  ------------------

                  procedure On_Unit_Part
                    (Kind     : Unit_Kind;
                     View     : GPR2.Project.View.Object;
                     Path     : Path_Name.Object;
                     Index    : Unit_Index;
                     Sep_Name : Optional_Name_Type)
                  is
                     Dest : constant Path_Name.Object :=
                              Src_Dir.Compose (Path.Simple_Name);
                  begin
                     if not Self.Tree.Add_Output
                       (Self.UID,
                        GPR2.Build.Artifacts.Files.Create (Dest))
                     then
                        Has_Error := True;
                     end if;
                  end On_Unit_Part;

               begin
                  CU.For_All_Part (On_Unit_Part'Access);

                  if Has_Error then
                     return False;
                  end if;
               end;
            end loop;

            --  Also add the non-ada sources

            for C in Self.Ctxt.Interface_Sources.Iterate loop
               declare
                  Path : constant Filename_Type :=
                           GPR2.Containers.Source_Path_To_Sloc.Key (C);
                  Src  : constant GPR2.Build.Source.Object :=
                           Self.Ctxt.Visible_Source (Path);
                  Dest : Path_Name.Object;
               begin
                  if Src.Language /= Ada_Language then
                     Dest := Src_Dir.Compose (Src.Path_Name.Simple_Name);

                     if not Self.Tree.Add_Output
                       (Self.UID,
                        GPR2.Build.Artifacts.Files.Create (Dest))
                     then
                        return False;
                     end if;
                  end if;
               end;
            end loop;
         end;
      end if;

      return not Has_Error;
   end On_Ready_State;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      if not Db.Add_Output (UID, Self.Output) then
         return False;
      end if;

      if Self.Is_Library and then not Self.Is_Static then
         --  Shared libraries may need symbolic links, reflect that at the
         --  tree db level

         for Variant of Self.Ctxt.Library_Filename_Variants loop
            declare
               Path : constant GPR2.Path_Name.Object :=
                        Self.Ctxt.Library_Directory.Compose (Variant);
            begin
               if not Db.Add_Output (UID, Artifacts.Files.Create (Path)) then
                  return False;
               end if;
            end;
         end loop;
      end if;

      return True;
   end On_Tree_Insertion;

   ------------------
   -- Post_Command --
   ------------------

   overriding function Post_Command
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : Unbounded_String := Null_Unbounded_String;
      Stderr : Unbounded_String := Null_Unbounded_String) return Boolean
   is
   begin
      if Status /= Success then
         return True;
      end if;

      --  No need for post-processing in case we didn't link a library

      if not Self.Ctxt.Is_Library then
         return True;
      end if;

      --  Create symlinks for shared libs when needed

      if not Self.Ctxt.Is_Static_Library then

         for Variant of Self.Ctxt.Library_Filename_Variants loop
            declare
               S_Link : constant GPR2.Path_Name.Object :=
                          Self.Ctxt.Library_Directory.Compose (Variant);
               use type GPR2.Reporter.User_Verbosity_Level;
            begin
               if S_Link.Exists
                 and then not GNATCOLL.OS.FSUtil.Remove_File
                   (S_Link.String_Value)
               then
                  pragma Annotate (Xcov, Exempt_On, "defensive code");
                  Self.Tree.Reporter.Report
                    (GPR2.Message.Create
                       (GPR2.Message.Error,
                        "cannot replace symbolic link " & String (Variant),
                        GPR2.Source_Reference.Create
                          (Self.Ctxt.Path_Name.Value, 0, 0)));

                  return False;
                  pragma Annotate (Xcov, Exempt_Off);
               end if;

               if not GNATCOLL.OS.FSUtil.Create_Symbolic_Link
                 (S_Link.String_Value,
                  String (Self.Output.Path.Simple_Name))
               then
                  pragma Annotate (Xcov, Exempt_On, "defensive code");
                  Self.Tree.Reporter.Report
                    (GPR2.Message.Create
                       (GPR2.Message.Error,
                        "cannot create symbolic link " & String (Variant),
                        GPR2.Source_Reference.Create
                          (Self.Ctxt.Path_Name.Value, 0, 0)));

                  return False;
                  pragma Annotate (Xcov, Exempt_Off);

               elsif Self.Tree.Reporter.User_Verbosity >= Reporter.Verbose then
                  Self.Tree.Reporter.Report
                    ("cd " & Self.Ctxt.Library_Directory.String_Value &
                       " && ln -s " & String (Self.Output.Path.Simple_Name) &
                       " " & String (Variant));
               end if;
            end;
         end loop;
      end if;

      return True;
   end Post_Command;

   -----------------
   -- Pre_Command --
   -----------------

   overriding function Pre_Command (Self : in out Object) return Boolean is
      CU       : GPR2.Build.Compilation_Unit.Object;
      CU_Dep   : GPR2.Build.Compilation_Unit.Object;
      Analyzed : GPR2.Containers.Name_Set;
      Todo     : GPR2.Build.Compilation_Unit.Maps.Map;

   begin
      if Self.Is_Static_Library and then Self.Output.Path.Exists then
         --  Remove the old .a since otherwise ar will just accumulate the
         --  objects there
         if not GNATCOLL.OS.FSUtil.Remove_File
           (Self.Output.Path.String_Value)
         then
            Self.Tree.Reporter.Report
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  "cannot remove the old archive " &
                    String (Self.Output.Path.Simple_Name),
                  GPR2.Source_Reference.Create
                    (Self.Ctxt.Path_Name.Value, 0, 0)));

            return False;
         end if;
      end if;

      if not Self.Ctxt.Is_Library then
         --  Below is specific to libraries, so just exit now
         return True;
      end if;

      declare
         Units : constant Compilation_Unit.Maps.Map := Self.Interface_Units;

      begin
         --  Copy the ali files to the library dir

         --  Two cases: standalone libraries where only the interface is to
         --  be copied, and regular libraries where all units need to be taken
         --  into account.

         for U of Units loop
            declare
               C_Id  : constant Actions.Compile.Ada.Ada_Compile_Id :=
                         Actions.Compile.Ada.Create (U);
               pragma Assert
                 (Self.Tree.Has_Action (C_Id),
                  "interface unit '" & String (U.Name) &
                    "' doesn't have an associated compile action");
               From  : constant Path_Name.Object :=
                         Actions.Compile.Object
                           (Self.Tree.Action (C_Id)).Dependency_File.Path;
               To    : constant Path_Name.Object :=
                         Self.Ctxt.Library_Ali_Directory.Compose
                           (From.Simple_Name);
               Attrs : GNATCOLL.OS.Stat.File_Attributes;

            begin
               GPR2.Build.Actions.Compile.Ada.Object
                 (Self.Tree.Action_Id_To_Reference
                    (C_Id).Element.all).Change_Intf_Ali_File
                      (To);

               if not Self.Ctxt.Is_Library_Standalone then
                  --  Just copy the ali file for standard libraries: they
                  --  need elaboration by the caller.

                  Traces.Trace
                    ("Copying """ & From.String_Value & """ to """ &
                       To.Containing_Directory.String_Value & '"');

                  if not GNATCOLL.OS.FSUtil.Copy_File
                    (From.String_Value, To.String_Value)
                  then
                     Self.Tree.Reporter.Report
                       (GPR2.Message.Create
                          (GPR2.Message.Error,
                           "could not copy ali file " &
                             String (From.Simple_Name) &
                             " to the library directory",
                           GPR2.Source_Reference.Object
                             (GPR2.Source_Reference.Create
                                  (Self.Ctxt.Path_Name.Value, 0, 0))));

                     return False;
                  end if;

               else
                  --  Amend the ALI to add the SL (StandAlone) flag to
                  --  it to prevent multiple elaboration of the unit.

                  Attrs := GNATCOLL.OS.Stat.Stat (From.String_Value);

                  declare
                     use GNATCOLL.OS;
                     use type GNATCOLL.OS.FS.File_Descriptor;

                     Last   : constant Integer :=
                                Integer
                                  (Long_Long_Integer'Min
                                     (64 * 1024, Stat.Length (Attrs)));
                     --  64k length: more than enough to find the P line but
                     --  not too much footprint on the stack to copy the whole
                     --  ALI file if very large.

                     Offset : Long_Long_Integer := 0;
                     --  Current offset, used to copy the whole file

                     Buffer : String (1 .. Last);
                     --  Some ALI files can be pretty large, for example
                     --  in libadalang the generated source comes with a
                     --  24MB ali file. We cannot use strings here, so need
                     --  to move to a more generic solution.

                     Length : Natural;
                     Idx    : Natural := Buffer'First;
                     Ign    : Natural with Unreferenced;
                     Found  : Boolean := False;
                     Input  : GNATCOLL.OS.FS.File_Descriptor;
                     Output : GNATCOLL.OS.FS.File_Descriptor;

                  begin
                     Input := FS.Open (From.String_Value, FS.Read_Mode);

                     if Input = FS.Invalid_FD then
                        Self.Tree.Reporter.Report
                          (GPR2.Message.Create
                             (GPR2.Message.Error,
                              "could not read the ali file """ &
                                String (From.Simple_Name) & '"',
                              GPR2.Source_Reference.Object
                                (GPR2.Source_Reference.Create
                                     (Self.Ctxt.Path_Name.Value, 0, 0))));

                        return False;
                     end if;

                     Output := FS.Open (To.String_Value, FS.Write_Mode);

                     if Output = FS.Invalid_FD then
                        Self.Tree.Reporter.Report
                          (GPR2.Message.Create
                             (GPR2.Message.Error,
                              "could not create the ali file """ &
                                String (To.Simple_Name) & '"',
                              GPR2.Source_Reference.Object
                                (GPR2.Source_Reference.Create
                                     (Self.Ctxt.Path_Name.Value, 0, 0))));
                        FS.Close (Input);

                        return False;
                     end if;

                     Traces.Trace
                       ("Installing """ & From.String_Value & """ to """ &
                          To.Containing_Directory.String_Value &
                          """ as library interface for " &
                          String (Self.Ctxt.Name));

                     Offset := Long_Long_Integer (FS.Read (Input, Buffer));

                     Search_Loop :
                     while Idx < Buffer'Last loop
                        --  Check end of line to retrieve the header char

                        while Buffer (Idx) in ASCII.CR | ASCII.LF loop
                           Idx := Idx + 1;

                           exit when Idx > Buffer'Last;

                           if Buffer (Idx) = 'P' then
                              --  Check if it's followed by a space or a new
                              --  line.

                              if Idx = Buffer'Last
                                or else Buffer (Idx + 1) in
                                  ' ' | ASCII.CR | ASCII.LF
                              then
                                 --  we have the P line
                                 Found := True;
                                 FS.Write (Output, String (Buffer (1 .. Idx)));
                                 FS.Write (Output, " SL");

                                 --  Write the rest of the ALI file

                                 FS.Write
                                   (Output, Buffer (Idx + 1 .. Buffer'Last));

                                 while Offset < Stat.Length (Attrs) loop
                                    Length := FS.Read (Input, Buffer);
                                    Offset :=
                                      Offset + Long_Long_Integer (Length);
                                    FS.Write (Output, Buffer (1 .. Length));
                                 end loop;

                                 exit Search_Loop;
                              end if;
                           end if;
                        end loop;

                        Idx := Idx + 1;
                     end loop Search_Loop;

                     if not Found then
                        Self.Tree.Reporter.Report
                          (GPR2.Message.Create
                             (GPR2.Message.Error,
                              "incorrectly formatted ali file """ &
                                From.String_Value & '"',
                              GPR2.Source_Reference.Create
                                (Self.Ctxt.Path_Name.Value, 0, 0)));

                        FS.Close (Input);
                        FS.Close (Output);

                        if not GNATCOLL.OS.FSUtil.Copy_File
                          (From.String_Value, To.String_Value)
                        then
                           Self.Tree.Reporter.Report
                             (GPR2.Message.Create
                                (GPR2.Message.Error,
                                 "could not copy ali file " &
                                   String (From.Simple_Name) &
                                   " to the library directory",
                                 GPR2.Source_Reference.Object
                                   (GPR2.Source_Reference.Create
                                        (Self.Ctxt.Path_Name.Value, 0, 0))));

                           return False;
                        end if;
                     end if;

                     FS.Close (Input);
                     FS.Close (Output);
                  end;
               end if;
            end;
         end loop;

         --  Copy the interface sources in Library_Src_Dir

         if Self.Ctxt.Is_Library_Standalone
           and then Self.Ctxt.Has_Attribute (PRA.Library_Src_Dir)
         then
            declare
               Src_Dir : constant Path_Name.Object :=
                           Self.Ctxt.Library_Src_Directory;
            begin
               for CU of Units loop
                  declare
                     procedure On_Unit_Part
                       (Kind     : Unit_Kind;
                        View     : GPR2.Project.View.Object;
                        Path     : Path_Name.Object;
                        Index    : Unit_Index;
                        Sep_Name : Optional_Name_Type);

                     Has_Error : Boolean := False;

                     ------------------
                     -- On_Unit_Part --
                     ------------------

                     procedure On_Unit_Part
                       (Kind     : Unit_Kind;
                        View     : GPR2.Project.View.Object;
                        Path     : Path_Name.Object;
                        Index    : Unit_Index;
                        Sep_Name : Optional_Name_Type)
                     is
                        Dest : constant Path_Name.Object :=
                                 Src_Dir.Compose (Path.Simple_Name);
                     begin
                        if not GNATCOLL.OS.FSUtil.Copy_File
                          (Path.String_Value, Dest.String_Value)
                        then
                           Self.Tree.Reporter.Report
                             (Message.Create
                                (Message.Error,
                                 "Cannot copy """ &
                                   String (Path.Simple_Name) &
                                   """ to the Library_Src_Dir """ &
                                   Src_Dir.String_Value & '"',
                                 Self.Ctxt.Attribute
                                   (PRA.Library_Src_Dir).Value));
                           Has_Error := True;
                        end if;

                        if not Self.Tree.Add_Output
                          (Self.UID,
                           GPR2.Build.Artifacts.Files.Create (Dest))
                        then
                           Has_Error := True;
                        end if;
                     end On_Unit_Part;

                     Comp    : constant Actions.Compile.Ada.Object :=
                                 Actions.Compile.Ada.Object
                                   (Self.Tree.Action
                                      (Actions.Compile.Ada.Create (CU)));
                  begin
                     if Comp.Spec_Needs_Body
                       or else not CU.Has_Part (S_Spec)
                     then
                        CU.For_All_Part (On_Unit_Part'Access);
                     else
                        On_Unit_Part
                          (S_Spec,
                           CU.Spec.View,
                           CU.Spec.Source,
                           CU.Spec.Index,
                           "");
                     end if;

                     if Has_Error then
                        return False;
                     end if;
                  end;
               end loop;

               --  Also add the non-ada sources

               for C in Self.Ctxt.Interface_Sources.Iterate loop
                  declare
                     Path : constant Filename_Type :=
                              GPR2.Containers.Source_Path_To_Sloc.Key (C);
                     Src  : constant GPR2.Build.Source.Object :=
                              Self.Ctxt.Visible_Source (Path);
                     Dest : Path_Name.Object;
                  begin
                     if Src.Language /= Ada_Language then
                        Dest := Src_Dir.Compose (Src.Path_Name.Simple_Name);

                        if not GNATCOLL.OS.FSUtil.Copy_File
                          (Src.Path_Name.String_Value, Dest.String_Value)
                        then
                           Self.Tree.Reporter.Report
                             (Message.Create
                                (Message.Error,
                                 "Cannot copy """ &
                                   String (Src.Path_Name.Simple_Name) &
                                   """ to the Library_Src_Dir """ &
                                   Src_Dir.String_Value & '"',
                                 Self.Ctxt.Attribute
                                   (PRA.Library_Src_Dir).Value));
                           return False;
                        end if;

                        if not Self.Tree.Add_Output
                          (Self.UID,
                           GPR2.Build.Artifacts.Files.Create (Dest))
                        then
                           return False;
                        end if;
                     end if;
                  end;
               end loop;
            end;
         end if;
      end;

      return True;
   end Pre_Command;

   ---------------------
   -- Set_Bind_Action --
   ---------------------

   procedure Set_Bind_Action
     (Self : in out Object;
      Bind : Actions.Ada_Bind.Object) is
   begin
      Self.Bind := Bind;
   end Set_Bind_Action;

   ---------------------------------------
   -- Set_Has_Library_Dependency_Circle --
   ---------------------------------------

   procedure Set_Has_Library_Dependency_Circle
     (Self  : in out Object;
      State : Boolean) is
   begin
      Self.Lib_Dep_Circle := State;
   end Set_Has_Library_Dependency_Circle;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      BN     : constant Simple_Name := Self.Output.Path.Simple_Name;
      Result : constant Link_Id :=
                 (Name_Len      => BN'Length,
                  Is_Static_Lib => Self.Is_Library and then Self.Is_Static,
                  View          => Self.Ctxt,
                  Exec_Name     => BN);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Link;
