--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with GNAT.OS_Lib;

with GNATCOLL.OS.FSUtil;
with GNATCOLL.Utils;

with GPR2.Build.External_Options;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Tree;
pragma Warnings (Off, "*is not referenced");
with GPR2.Project.View.Vector;
pragma Warnings (On);
with GPR2.Source_Reference;
with GPR2.Source_Reference.Value;

package body GPR2.Build.Actions.Link is

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
     (Self     : in out Object;
      Slot     : Positive;
      Cmd_Line : in out GPR2.Build.Command_Line.Object)
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

         procedure Add (Arg : String);
         --  Check for full path whenever possible

         procedure Add (Arg : String) is
         begin
            if Arg (Arg'First) = '-' then
               Cmd_Line.Add_Argument (Arg, In_Signature);
            else
               declare
                  Full : constant Path_Name.Object :=
                           Path_Name.Create_File
                             (Filename_Type (Arg),
                              Self.Ctxt.Dir_Name.Value);
               begin
                  if Full.Exists then
                     Cmd_Line.Add_Argument (Full, In_Signature);
                  else
                     Cmd_Line.Add_Argument (Arg, In_Signature);
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
      --  -l needs to be last in the command line, so we accumulate the -l
      --  switches in Dash_l_Opts and only actually add the switch at the
      --  end of the command line computation.

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
                  Cmd_Line.Add_Argument ("csr", True);
               else
                  Cmd_Line.Add_Argument (Val.Text, True);
               end if;
            end loop;

            --  We use a temp file for the archive so that we don't re-use
            --  an old archive. This tmp file will replace the original
            --  one in the post-command phase.

            Cmd_Line.Add_Argument
              (String (Self.Output.Path.Simple_Name) & ".tmp");
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
         Cmd_Line.Add_Argument ("-o", True);
         Cmd_Line.Add_Argument
           (String (Self.Output.Path.Simple_Name), True);
      end if;

      for Obj of Objects loop
         Cmd_Line.Add_Argument
           (Artifacts.Files.Object'Class (Obj).Path, True);
      end loop;

      if not Self.Is_Static_Library then
         --  Add the runtime directory to the rpath: it won't be listed in the
         --  library dependencies.

         if Self.Ctxt.Language_Ids.Contains (Ada_Language)
           or else (Self.Ctxt.Kind = K_Aggregate_Library
                    and then (for some V of Self.Ctxt.Aggregated =>
                                  V.Language_Ids.Contains (Ada_Language)))
         then
            Rpath :=
              +Self.Ctxt.Tree.Runtime_Project.Object_Directory.String_Value;
         end if;

         for Lib of Self.Library_Dependencies loop
            declare
               Link         : constant Object'Class :=
                                Object'Class (Self.Tree.Action (Lib));
               Lib_Artifact : constant GPR2.Path_Name.Object :=
                                Link.Output.Path;
               Lib_Dir_Opt : constant Value_Type :=
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

                  if Rpath_Origin.Is_Defined and then not Self.Is_Library then
                     --  ??? $ORIGIN refers to the executable, we would need
                     --  an equivalent attribute for shared libs dependencies

                     --  ??? This processing is unix-oriented with unix path
                     --  and directory delimiters. This is somewhat expected
                     --  since this mechanism is not available on windows,
                     --  but then we still need to properly cross compilation
                     --  on windows hosts, so may need to "posixify" the
                     --  paths here.

                     declare
                        From : constant Path_Name.Object :=
                                 Self.Ctxt.Executable_Directory;
                     begin
                        Append
                          (Rpath,
                           Rpath_Origin.Value.Text & "/" &
                             String
                               (Lib_Artifact.Containing_Directory.Relative_Path
                                  (From)));
                     end;
                  else
                     Append (Rpath, String (Lib_Artifact.Dir_Name));
                  end if;

                  declare
                     Prefix : constant Value_Type :=
                                Link.View.Attribute
                                  (PRA.Shared_Library_Prefix).Value.Text;
                     BN     : constant String :=
                                String (Lib_Artifact.Base_Name);
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
                        if Starts_With (Val.Text, "-l") then
                           Dash_l_Opts.Append (Val.Text);
                        else
                           declare
                              Path : constant Path_Name.Object :=
                                       Path_Name.Create_File
                                         (Filename_Type (Val.Text),
                                          Link.View.Dir_Name.Value);
                           begin
                              if Path.Exists then
                                 Cmd_Line.Add_Argument (Path, True);
                              else
                                 Cmd_Line.Add_Argument (Val.Text, True);
                                 return;
                              end if;
                           end;
                        end if;
                     end loop;
                  end if;
               end;
            end;
         end loop;

         --  Ignore: no presence of Run_Path_Option is expected if
         --  Run_Path_Option is not available, like with windows dlls.

         Ign := Length (Rpath) > 0
           and then not Add_Attr
             (PRA.Run_Path_Option,
              PAI.Undefined,
              True,
              True,
              -Rpath);

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
                                 Cmd_Line.Add_Argument (Val.Text, True);
                              end if;
                           end if;

                        else
                           --  Check for relative paths and translate them
                           --  as absolute.

                           Cmd_Line.Add_Argument
                             (Path_Name.Create_File
                                (Filename_Type (Val.Text),
                                 C.Dir_Name.Value).String_Value, True);
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end loop;
      end if;

      for Arg of Dash_l_Opts loop
         Cmd_Line.Add_Argument (Arg, True);
      end loop;

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

      --  Runtime flags usually come from the binder. However, there is no
      --  binding phase when creating a non-standalone library. Therefore,
      --  we need to add the runtime flags manually in this case. Note that
      --  static libraries do not require any runtime flags, so we are
      --  processing only the shared libraries here.

      if Self.View.Tree.Has_Runtime_Project
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

         --  Add -largs

         for Arg
           of Self.Tree.External_Options.Fetch
             (External_Options.Linker, GPR2.No_Language)
         loop
            Cmd_Line.Add_Argument (Arg);
         end loop;

         Status :=
           Add_Attr (PRA.Linker.Trailing_Switches, Src_Idx, True, True);
      end if;
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
            if not Self.Signature.Add_Input (Link.Output)
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
     (Self    : in out Object;
      Src     : Compilation_Unit.Unit_Location;
      Output  : Filename_Optional := "")
   is
      Exec : GPR2.Path_Name.Object;
   begin
      Self.Is_Library := False;
      Self.Main_Src   := Src;
      Self.Ctxt       := Src.View;
      Self.Traces     := Create ("ACTION_LINK",
                                 GNATCOLL.Traces.Off);

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
      Self.Traces     := Create ("ACTION_LINK",
                                 GNATCOLL.Traces.Off);
   end Initialize_Global_Archive;

   ------------------------
   -- Initialize_Library --
   ------------------------

   procedure Initialize_Library
     (Self    : in out Object;
      Context : GPR2.Project.View.Object) is
   begin
      Self.Ctxt       := Context;
      Self.Is_Library := True;
      Self.Is_Static  := Context.Is_Static_Library;
      Self.Library    := Artifacts.Library.Create (Context.Library_Filename);
      Self.Traces     := Create ("ACTION_LINK",
                                 GNATCOLL.Traces.Off);
   end Initialize_Library;

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

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      return Db.Add_Output (UID, Self.Output);
   end On_Tree_Insertion;

   ------------------
   -- Post_Command --
   ------------------

   overriding function Post_Command
     (Self   : in out Object;
      Status : Execution_Status) return Boolean
   is
      Result  : Boolean;

   begin
      if Status /= Success then
         return True;
      end if;

      if Self.Is_Static_Library then
         --  archives are generated as tmp files so that we don't reuse
         --  the library from previous runs, we thus need to unlink and rename
         if Self.Output.Path.Exists then
            if not GNATCOLL.OS.FSUtil.Remove_File
              (Self.Output.Path.String_Value)
            then
               pragma Annotate (Xcov, Exempt_On, "defensive code");
               Self.Tree.Reporter.Report
                 (GPR2.Message.Create
                    (GPR2.Message.Error,
                     "cannot remove this file",
                     GPR2.Source_Reference.Create
                       (Self.Output.Path.Value, 0, 0)));

               return False;
               pragma Annotate (Xcov, Exempt_Off);
            end if;
         end if;

         GNAT.OS_Lib.Rename_File
           (Self.Output.Path.String_Value & ".tmp",
            Self.Output.Path.String_Value,
            Result);

         if not Result then
            pragma Annotate (Xcov, Exempt_On, "defensive code");
            Self.Tree.Reporter.Report
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  "cannot rename this file",
                  GPR2.Source_Reference.Create
                    (Self.Output.Path.Value, 0, 0)));

            return False;
            pragma Annotate (Xcov, Exempt_Off);
         end if;
      end if;

      return True;
   end Post_Command;

   -----------------
   -- Pre_Command --
   -----------------

   overriding function Pre_Command (Self : in out Object) return Boolean is
      CU_Dep : GPR2.Build.Compilation_Unit.Object;
   begin
      if Self.Is_Library and then Self.Ctxt.Has_Any_Interfaces then
         --  Check that the interface is complete: no dependency from specs
         --  should depend on a spec that is not part of the interface.

         for CU of Self.Ctxt.Interface_Closure loop
            for Dep of CU.Known_Dependencies (Spec_Only => True) loop
               if not Self.Ctxt.Interface_Closure.Contains (Dep) then
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

                  if CU_Dep.Is_Defined then
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
               end if;
            end loop;
         end loop;
      end if;

      return True;
   end Pre_Command;

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
