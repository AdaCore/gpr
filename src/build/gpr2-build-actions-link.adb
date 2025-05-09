--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.IO_Exceptions;
with Ada.Text_IO;

with GNATCOLL.OS.FSUtil;
with GNATCOLL.Traces;
with GNATCOLL.Utils;

with GPR2.Build.Actions.Compile.Ada;
with GPR2.Build.External_Options;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Tree;
pragma Warnings (Off, "*is not referenced");
with GPR2.Project.View.Vector;
pragma Warnings (On);
with GPR2.Reporter;
with GPR2.Source_Reference;
with GPR2.Source_Reference.Value;

package body GPR2.Build.Actions.Link is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create
                ("GPR.BUILD.ACTIONS.LINK", GNATCOLL.Traces.Off);

   procedure Check_Interface
     (Self        : in out Object;
      No_Warnings : Boolean);

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option (Self : in out Object; Option : String) is
   begin
      Self.Static_Options.Append (Option);
   end Add_Option;

   ---------------------
   -- Check_Interface --
   ---------------------

   procedure Check_Interface
     (Self        : in out Object;
      No_Warnings : Boolean)
   is
      CU       : GPR2.Build.Compilation_Unit.Object;
      CU_Dep   : GPR2.Build.Compilation_Unit.Object;
      Analyzed : GPR2.Containers.Name_Set;
      Todo     : GPR2.Build.Compilation_Unit.Maps.Map;
      Pos      : GPR2.Build.Compilation_Unit.Maps.Cursor;
      Inserted : Boolean;

   begin
      if Self.Ctxt.Is_Library
        and then Self.Ctxt.Has_Any_Interfaces
        and then Self.Ctxt.Is_Library_Standalone
      then
         --  Check that the interface is complete: no dependency from specs
         --  should depend on a spec that is not part of the interface.

         Todo := Self.Ctxt.Interface_Closure;

         while not Todo.Is_Empty loop
            CU := Todo.First_Element;
            Todo.Delete_First;
            Analyzed.Insert (CU.Name);

            --  ??? TODO Need to know if CU requires the body (generics or
            --  inlined subprograms), and use Spec_Only parameter accordingly.

            for Dep of CU.Known_Dependencies (Spec_Only => True) loop
               if not Self.Ctxt.Interface_Closure.Contains (Dep)
                 and then not Analyzed.Contains (Dep)
               then
                  if Self.Ctxt.Kind = K_Aggregate_Library then
                     for V of Self.Ctxt.Aggregated loop
                        CU_Dep := V.Own_Unit (Dep);
                        exit when CU_Dep.Is_Defined;
                     end loop;
                  else
                     CU_Dep := Self.Ctxt.Own_Unit (Dep);
                  end if;

                  --  Only warn for internal units, the interface may depend
                  --  on other units

                  if CU_Dep.Is_Defined
                    and then not Analyzed.Contains (CU_Dep.Name)
                  then
                     if not No_Warnings then
                        Self.Tree.Reporter.Report
                          (GPR2.Message.Create
                             (GPR2.Message.Warning,
                              "unit """
                              & String (Dep)
                              & """ is not in the interface set, but it is "
                              & "needed by """
                              & String (CU.Name)
                              & """",
                              GPR2.Source_Reference.Create
                                (Self.Ctxt.Path_Name.Value, 0, 0)));
                     end if;

                     Self.Extra_Intf.Insert
                       (CU_Dep.Name, CU_Dep, Pos, Inserted);

                     if Inserted then
                        declare
                           Ada_Comp : Actions.Compile.Ada.Object;
                        begin
                           Ada_Comp.Initialize (CU_Dep);

                           Self.Tree.Add_Input
                             (Self.UID, Ada_Comp.Local_Ali_File, True);
                        end;

                        Todo.Include (CU_Dep.Name, CU_Dep);
                     end if;
                  end if;
               end if;
            end loop;
         end loop;
      end if;
   end Check_Interface;

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

      function Is_Partially_Linked
        (View : GPR2.Project.View.Object) return Boolean;
      --  Return true if the Library_Partial_Linker is set with a
      --  non-empty value.
      --  ??? Because we do not support partial links for now, this function
      --  always return False. To be updated once the support has been
      --  implemented.

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

      Objects      : Tree_Db.Artifact_Sets.Set;
      Status       : Boolean;
      Src_Idx      : constant PAI.Object :=
                       (if not Self.Is_Library
                        then PAI.Create
                          (String (Self.Main_Src.Source.Simple_Name),
                           Case_Sensitive => File_Names_Case_Sensitive,
                           At_Pos         => Self.Main_Src.Index)
                        else PAI.Undefined);
      Link_Exec    : constant Boolean := Src_Idx.Is_Defined;
      Rpath        : Unbounded_String;
      Rpath_Origin : constant GPR2.Project.Attribute.Object :=
                       Self.Ctxt.Attribute (PRA.Run_Path_Origin);
      Ign          : Boolean with Unreferenced;
      Dash_l_Opts  : GPR2.Containers.Value_List;
      --  -l needs to be last in the command line, so we add them here and
      --  then append to the command line in the end

   begin
      Objects := Self.Embedded_Objects;

      --  Remove from this list of objects the ones that come from
      --  libraries.

      for Lib of Self.Library_Dependencies loop
         declare
            Link : constant Object'Class :=
                     Object'Class (Self.Tree.Action (Lib));
         begin
            Objects.Difference (Link.Embedded_Objects);
         end;
      end loop;

      --  ??? Replace hard coded values
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
            Build.Command_Line.Simple);
      end loop;

      if not Self.Is_Static_Library then
         --  Add the runtime directory to the rpath: it won't be listed in the
         --  library dependencies.

         if Self.Ctxt.Tree.Has_Runtime_Project then
            Rpath :=
              +Self.Ctxt.Tree.Runtime_Project.Object_Directory.String_Value;
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
               Lib_Dir_Opt  : constant Value_Type :=
                                Self.Tree.Linker_Lib_Dir_Option;

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
                    (Lib_Dir_Opt & String (Lib_Artifact.Dir_Name));

                  --  Add the library directory to the rpath of the
                  --  executable, so that LD_LIBRARY_PATH does not need to
                  --  be set before execution.

                  if Length (Rpath) /= 0 then
                     --  ??? hard coded value: ok for now since this is not
                     --  used on windows, but we may need an attribute for that
                     --  at some point.
                     Append (Rpath, ':');
                  end if;

                  if Rpath_Origin.Is_Defined
                    and then not Self.Is_Library
                  then
                     --  ??? $ORIGIN refers to the executable, we would
                     --  need an equivalent attribute for shared libs
                     --  dependencies.

                     --  ??? This processing is unix-oriented with unix
                     --  path and directory delimiters. This is somewhat
                     --  expected since this mechanism is not available on
                     --  windows, but then we still need to properly cross
                     --  compilation on windows hosts, so may need to
                     --  "posixify" the paths here.

                     declare
                        From : constant Path_Name.Object :=
                                 Self.Ctxt.Executable_Directory;
                     begin
                        Append
                          (Rpath,
                           Rpath_Origin.Value.Text & "/" &
                             String
                             (Lib_Artifact.Containing_Directory.
                                  Relative_Path (From)));
                     end;
                  else
                     Append (Rpath, String (Lib_Artifact.Dir_Name));
                  end if;

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
               begin
                  if Attr.Is_Defined then
                     for Val of Attr.Values loop
                        declare
                           Path : constant Path_Name.Object :=
                                    Path_Name.Create_File
                                      (Filename_Type (Val.Text),
                                       Link.View.Dir_Name.Value);
                        begin
                           if Path.Exists then
                              Cmd_Line.Add_Argument (Path);

                           elsif not Link.Is_Static then
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
         Status :=
           Add_Attr (PRA.Linker.Required_Switches, PAI.Undefined, True, True);

         Status := Add_Attr (PRA.Linker.Switches, Src_Idx, True, True);

         if not Status then
            Status := Add_Attr
              (PRA.Linker.Default_Switches,
               PAI.Create
                 (Self.View.Visible_Source (Self.Main_Src.Source).Language),
               True,
               True);
         end if;

         Status :=
           Add_Attr (PRA.Linker.Trailing_Switches, Src_Idx, True, True);

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
            Gnat_Version : constant String :=
                             Self.View.Tree.Ada_Compiler_Version;
         begin
            if Gnat_Version /= "" then
               Cmd_Line.Add_Argument ("-lgnat-" & Gnat_Version);
            end if;
         end;

         --  ??? We also need to add the lgnarl-XXX flag if required

         Cmd_Line.Add_Argument
           (Self.Tree.Linker_Lib_Dir_Option
            & Self.View.Tree.Runtime_Project.Object_Directory.String_Value);
      end if;

      --  Add options provided by the binder if needed

      if not Self.View.Is_Library
        or else Self.View.Is_Shared_Library
        or else (Self.View.Is_Library_Standalone
                 and then Is_Partially_Linked (Self.View))
      then
         for Option of Self.Static_Options loop
            Cmd_Line.Add_Argument (Option);
         end loop;
      end if;
      if Self.View.Is_Library
        and then not Self.Is_Static
      then
         Ign := Add_Attr (PRA.Library_Options, PAI.Undefined, True, True);
      end if;

      --  Finally remove any duplicated --specs switch as this may cause
      --  trouble by introducing duplicated symbols in the result.

      Cmd_Line.Filter_Duplicate_Switches ("--specs");
   end Compute_Command;

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

      if not Self.Signature.Add_Output (Self.Output) and then Load_Mode then
         return;
      end if;
   end Compute_Signature;

   ----------------------
   -- Embedded_Objects --
   ----------------------

   function Embedded_Objects
     (Self : Object) return Build.Tree_Db.Artifact_Sets.Set
   is
   begin
      return Result : Tree_Db.Artifact_Sets.Set do
         for Input of Self.Tree.Inputs (Self.UID) loop
            --  Inputs are either objects or libraries. Libraries are
            --  represented by an Artifact.Library class.

            if Input not in Artifacts.Library.Object'Class then
               Result.Include (Input);
            end if;
         end loop;
      end return;
   end Embedded_Objects;

   ----------------
   -- Initialize --
   ---------------

   procedure Initialize_Executable
     (Self     : in out Object;
      Src      : Compilation_Unit.Unit_Location;
      No_Rpath : Boolean;
      Output   : Filename_Optional := "")
   is
      Exec : GPR2.Path_Name.Object;
   begin
      Self.Is_Library := False;
      Self.Main_Src   := Src;
      Self.Ctxt       := Src.View;
      Self.No_Rpath   := No_Rpath;

      if Output'Length = 0 then
         Exec := Self.Ctxt.Executable (Src.Source.Simple_Name, Src.Index);
      else
         declare
            Suff : constant Filename_Optional :=
                     Self.Ctxt.Executable_Suffix;
         begin
            if Ada.Strings.Fixed.Index (String (Output), ".") = 0 then
               Exec := Self.Ctxt.Executable_Directory.Compose (Output & Suff);
            else
               Exec := Self.Ctxt.Executable_Directory.Compose (Output);
            end if;
         end;
      end if;

      Self.Executable := Artifacts.Files.Create (Exec);
   end Initialize_Executable;

   -------------------------------
   -- Initialize_Global_Archive --
   -------------------------------

   procedure Initialize_Global_Archive
     (Self    : in out Object;
      Context : GPR2.Project.View.Object)
   is
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
   end Initialize_Global_Archive;

   ------------------------
   -- Initialize_Library --
   ------------------------

   procedure Initialize_Library
     (Self     : in out Object;
      Context  : GPR2.Project.View.Object;
      No_Rpath : Boolean) is
   begin
      Self.Ctxt       := Context;
      Self.Is_Library := True;
      Self.Is_Static  := Context.Is_Static_Library;
      Self.Library    := Artifacts.Library.Create (Context.Library_Filename);
      Self.No_Rpath   := No_Rpath;
   end Initialize_Library;

   ---------------------
   -- Interface_Units --
   ---------------------

   function Interface_Units (Self : Object) return Compilation_Unit.Maps.Map
   is
      Units     : Compilation_Unit.Maps.Map;
   begin
      if Self.Ctxt.Is_Library_Standalone then
         Units := Self.Ctxt.Interface_Closure;

         for CU of Self.Extra_Intf loop
               Units.Include (CU.Name, CU);
         end loop;

      else
         Units := Self.Ctxt.Own_Units;
      end if;

      return Units;
   end Interface_Units;

   --------------------------
   -- Library_Dependencies --
   --------------------------

   function Library_Dependencies
     (Self : Object) return Actions.Action_Id_Vectors.Vector
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

      Check_Interface (Self, No_Warnings => True);

      Units := Self.Interface_Units;

      --  For each unit, add the corresponding Ali  file as an output

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

                  exit when Has_Error;
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
      Status : Execution_Status) return Boolean
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
      --  Check the library interface if needed

      Check_Interface (Self, No_Warnings => False);

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
               use Ada.Text_IO;
               C_Id : constant Actions.Compile.Ada.Ada_Compile_Id :=
                        Actions.Compile.Ada.Create (U);
               pragma Assert
                 (Self.Tree.Has_Action (C_Id),
                  "interface unit '" & String (U.Name) &
                    "' doesn't have an associated compile action");
               From : constant Path_Name.Object :=
                        Actions.Compile.Object
                          (Self.Tree.Action (C_Id)).Dependency_File.Path;
               To   : constant Path_Name.Object :=
                        Self.Ctxt.Library_Ali_Directory.Compose
                          (From.Simple_Name);
               Input  : File_Type;
               Output : File_Type;

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

                  Open (Input, In_File, From.String_Value);
                  Create (Output, Out_File, To.String_Value);

                  Traces.Trace
                    ("Installing """ & From.String_Value & """ to """ &
                       To.Containing_Directory.String_Value &
                       """ as library interface for " &
                       String (Self.Ctxt.Name));

                  while not End_Of_File (Input) loop
                     declare
                        Line : constant String := Get_Line (Input);
                     begin
                        if Line'Length > 2
                          and then Line
                            (Line'First .. Line'First + 1) = "P "
                        then
                           Put_Line
                             (Output,
                              "P SL" &
                                Line (Line'First + 1 .. Line'Last));
                        else
                           Put_Line (Output, Line);
                        end if;
                     end;
                  end loop;

                  Close (Input);
                  Close (Output);
               end if;

            exception
               when Ada.IO_Exceptions.Use_Error |
                    Ada.IO_Exceptions.Name_Error =>

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
                        if not Dest.Exists then
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

                              if not Self.Tree.Add_Output
                                (Self.UID,
                                 GPR2.Build.Artifacts.Files.Create (Dest))
                              then
                                 Has_Error := True;
                              end if;
                           end if;
                        end if;
                     end On_Unit_Part;

                  begin
                     CU.For_All_Part (On_Unit_Part'Access);

                     if Has_Error then
                        return False;
                     end if;
                  end;
               end loop;
            end;
         end if;
      end;

      return True;
   end Pre_Command;

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
