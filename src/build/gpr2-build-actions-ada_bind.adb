--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings;
with Ada.Text_IO;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;

with GNATCOLL.OS.FS;
with GNATCOLL.Traces;
with GNATCOLL.Utils; use GNATCOLL.Utils;

with GPR2.Build.Actions.Compile.Ada;
with GPR2.Build.Actions.Link;
with GPR2.Build.Actions.Link_Options_Insert;
with GPR2.Build.Actions.Post_Bind;
with GPR2.Build.Artifacts.Library;
with GPR2.Build.Compilation_Unit;
pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Build.Tree_Db;
with GPR2.Build.External_Options;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.View.Set;
pragma Warnings (Off, "*is not referenced");
with GPR2.Project.View.Vector;
pragma Warnings (On);
with GPR2.Project.Tree;

package body GPR2.Build.Actions.Ada_Bind is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create
                ("GPR.BUILD.ACTIONS.ADA_BIND", GNATCOLL.Traces.Off);

   use GNAT;

   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;

   function Link (Self : Object) return Actions.Link.Object'Class;
   --  Return the link action that is a transitive successor of this
   --  action, if one exists. Return Undefined if no such action is found.

   function Link_Opt_Insert
     (Self : Object) return Actions.Link_Options_Insert.Object;
   --  Returns the Link_Options_Insert action that is a transitive
   --  successor of the current action, if one exists. Returns Undefined
   --  if no such action is found.

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean)
   is
      procedure Add_Attr
        (Id           : Q_Attribute_Id;
         Index        : PAI.Object;
         Is_List      : Boolean;
         In_Signature : Boolean);

      function Resolve_Binder return String;
      --  Resolves the path to the binder. Needs to be called after all
      --  Switches attributes have been analyzed.

      procedure Add_Mapping_File;

      procedure Create_Response_File;

      Lang_Ada_Idx      : constant PAI.Object :=
                            PAI.Create (GPR2.Ada_Language);
      Binder_From_Attrs : Unbounded_String;

      --------------
      -- Add_Attr --
      --------------

      procedure Add_Attr
        (Id           : Q_Attribute_Id;
         Index        : PAI.Object;
         Is_List      : Boolean;
         In_Signature : Boolean)
      is
         Attr                  : constant Project.Attribute.Object :=
                                   Self.View.Attribute (Id, Index);
         Exe_Ext               : constant String :=
                                   (if GPR2.On_Windows then ".exe" else "");
         Gnatbind_Prefix_Equal : constant String := "gnatbind_prefix=";
         Gnatbind_Path_Equal   : constant String := "--gnatbind_path=";
         Ada_Binder_Equal      : constant String := "ada_binder=";
         Default_Binder_Name   : constant String := "gnatbind" & Exe_Ext;
         Mode                  : constant Build.Command_Line.Signature_Mode :=
                                   (if In_Signature
                                    then Build.Command_Line.In_Signature
                                    else Build.Command_Line.Ignore);

      begin
         if not Attr.Is_Defined then
            return;
         end if;

         if Is_List then
            for Val of Attr.Values loop
               if Val.Text'Length = 0
                 or else Val.Text = "-C"
               then
                  --  Ignore -C, as the generated sources are always in Ada
                  --  Also ignore empty values
                  null;

               elsif Starts_With (Val.Text, Gnatbind_Path_Equal) then
                  Binder_From_Attrs :=
                    +(Val.Text (Val.Text'First +
                        Gnatbind_Path_Equal'Length .. Val.Text'Last) &
                      Exe_Ext);

               elsif Starts_With (Val.Text, Gnatbind_Prefix_Equal) then
                  --  There is always a '-' between <prefix> and
                  --  "gnatbind". Add one if not already in <prefix>.
                  if Val.Text'Length = Gnatbind_Prefix_Equal'Length then
                     Binder_From_Attrs := +Default_Binder_Name;
                  else
                     Binder_From_Attrs :=
                       +(Val.Text (Val.Text'First +
                           Gnatbind_Prefix_Equal'Length .. Val.Text'Last)
                         & (if Val.Text (Val.Text'Last) = '-' then "" else "-")
                         & Default_Binder_Name);
                  end if;

               elsif Starts_With (Val.Text, Ada_Binder_Equal) then
                  Binder_From_Attrs :=
                    +(Val.Text (Val.Text'First +
                        Ada_Binder_Equal'Length .. Val.Text'Last) & Exe_Ext);

               elsif Starts_With (Val.Text, "-A=") then
                  --  Ensure the path is absolute
                  declare
                     Path : constant Path_Name.Object :=
                              Path_Name.Create_File
                                (Filename_Type
                                   (Val.Text
                                      (Val.Text'First + 3 .. Val.Text'Last)),
                                 Self.Ctxt.Dir_Name.Value);
                  begin
                     Cmd_Line.Add_Argument
                       ("-A=" &
                          String (Path.Relative_Path (Self.Working_Directory)),
                        Mode);
                  end;
               else
                  Cmd_Line.Add_Argument (Val.Text, Mode);
               end if;
            end loop;
         else
            Cmd_Line.Add_Argument (Attr.Value.Text, Mode);
         end if;
      end Add_Attr;

      ----------------------
      -- Add_Mapping_File --
      ----------------------

      procedure Add_Mapping_File
      is
         use GNATCOLL.OS.FS;
         Map_File     : constant Tree_Db.Temp_File :=
                          Self.Get_Or_Create_Temp_File
                            ("mapping", Local);
         S_Suffix     : constant String :=
                          Self.View.Attribute
                            (PRA.Compiler.Mapping_Spec_Suffix,
                             PAI.Create (Ada_Language)).Value.Text;
         B_Suffix     : constant String :=
                          Self.View.Attribute
                            (PRA.Compiler.Mapping_Body_Suffix,
                             PAI.Create (Ada_Language)).Value.Text;
         Initial_Closure : GPR2.Project.View.Set.Object;
         use Standard.Ada.Characters.Handling;

      begin
         if Map_File.FD /= Invalid_FD and then Map_File.FD /= Null_FD then
            for Input of Self.Tree.Inputs (Self.UID) loop
               if Input not in Artifacts.Library.Object'Class
                 and then Self.Tree.Has_Predecessor (Input)
               then
                  declare
                     Ali      : constant Path_Name.Object :=
                                  Artifacts.Files.Object (Input).Path;
                     Comp     : constant Actions.Compile.Ada.Object :=
                                  Actions.Compile.Ada.Object
                                    (Self.Tree.Predecessor (Input));
                     Key      : constant String :=
                                  To_Lower (String (Comp.Input_Unit.Name)) &
                                  (if Comp.Input_Unit.Main_Part = S_Spec
                                   then S_Suffix else B_Suffix);

                  begin
                     if not Comp.View.Is_Runtime
                       or else (Comp.View.Is_Library
                                and then Comp.View.Is_Library_Standalone)
                     then
                        Write
                          (Map_File.FD,
                           Key & ASCII.LF &
                             String (Ali.Simple_Name) &
                             ASCII.LF &
                             Ali.String_Value & ASCII.LF);
                     end if;
                  end;
               end if;
            end loop;

            GNATCOLL.OS.FS.Close (Map_File.FD);
         end if;

         Cmd_Line.Add_Argument
           ("-F=" & String (Map_File.Path), Build.Command_Line.Ignore);
      end Add_Mapping_File;

      --------------------------
      -- Create_Response_File --
      --------------------------

      procedure Create_Response_File
      is
         use GNATCOLL.OS.FS;
         Resp_File : constant Tree_Db.Temp_File :=
                       Self.Get_Or_Create_Temp_File ("response_file", Local);
         New_Args  : GNATCOLL.OS.Process.Argument_List;
      begin
         New_Args.Append (Cmd_Line.Argument_List.First_Element);

         if Resp_File.FD /= Null_FD then
            for Arg of Cmd_Line.Argument_List loop
               if Arg /= New_Args.First_Element then
                  declare
                     Char          : Character;
                     Quotes_Needed : Boolean := False;
                  begin
                     for Index in Arg'Range loop
                        Char := Arg (Index);

                        if Char = ' '
                          or else Char = ASCII.HT
                          or else Char = '"'
                        then
                           Quotes_Needed := True;
                           exit;
                        end if;
                     end loop;

                     if Quotes_Needed then
                        declare
                           New_Arg : String (1 .. Arg'Length * 2 + 2);
                           Offset  : Integer := 0;
                        begin
                           New_Arg (1) := '"';
                           Offset := Offset + 1;

                           for Index in Arg'Range loop
                              Char := Arg (Index);
                              New_Arg (Index + Offset) := Char;

                              if Char = '"' then
                                 Offset := Offset + 1;
                                 New_Arg (Index + Offset) := '"';
                              end if;
                           end loop;

                           Offset := Offset + 1;
                           New_Arg (Arg'Length + Offset) := '"';

                           Write
                             (Resp_File.FD,
                              New_Arg (1 .. Arg'Length + Offset) & ASCII.LF);
                        end;
                     else
                        Write (Resp_File.FD, Arg & ASCII.LF);
                     end if;
                  end;
               end if;
            end loop;
         end if;

         New_Args.Append ("@" & String (Resp_File.Path));

         Cmd_Line.Set_Response_File_Command (New_Args);
      end Create_Response_File;

      --------------------
      -- Resolve_Binder --
      --------------------

      function Resolve_Binder return String
      is
         Default_Binder_Name : constant String :=
                                 "gnatbind" &
                                 (if GPR2.On_Windows then ".exe" else "");
         Binder_Path         : Path_Name.Object;
      begin
         --  if "gnatbind_prefix=", "--gnatbind_path=" or "ada_binder=" weren't
         --  found, default to "gnatbind".

         if Length (Binder_From_Attrs) = 0 then
            Binder_From_Attrs := +Default_Binder_Name;
         end if;

         if OS_Lib.Is_Absolute_Path (-Binder_From_Attrs) then
            return Path_Name.Create_File (-Binder_From_Attrs).String_Value;
         end if;

         --  if we don't have an absolute path to gnatbind at this point, try
         --  to find it in the same install as the compiler.

         declare
            Compiler_Driver : constant Project.Attribute.Object :=
                                Self.View.Attribute
                                  (PRA.Compiler.Driver, Lang_Ada_Idx);
         begin
            if Compiler_Driver.Is_Defined then
               Binder_Path := Path_Name.Create_File
                 (-Binder_From_Attrs,
                  Filename_Type
                    (Directory_Operations.Dir_Name
                         (Compiler_Driver.Value.Text)));
            end if;
         end;

         if Binder_Path.Exists then
            return Binder_Path.String_Value;
         end if;

         --  At this point we try to locate either the absolute path to
         --  gnatbind or the gnatbind in the same install as the compiler.

         declare
            Full_Path : constant String :=
                          Locate_Exec_On_Path (-Binder_From_Attrs);
         begin
            if Full_Path'Length > 0 then
               return Full_Path;
            else
               --  Try with the relative path
               return -Binder_From_Attrs;
            end if;
         end;
      end Resolve_Binder;

      use type GPR2.Project.Standalone_Library_Kind;

   begin
      --  [eng/gpr/gpr-issues#446] We should rework how the binder tools is
      --  fetched from the KB.
      --  Right now, gnatbind is called no matter what and
      --  Binder.Required_Switches is parsed to fetch gnatbind prefix or path.
      --  The binder tool should simply be stored in Binder.Driver
      --  Binder.Prefix can be removed it only serves as renaming the bexch
      --  which does not exist anymore.

      if not Self.Has_Main then
         Cmd_Line.Add_Argument ("-n");
      end if;

      if Self.SAL_Closure then
         Cmd_Line.Add_Argument ("-F");
      end if;

      Cmd_Line.Add_Argument ("-o");
      Cmd_Line.Add_Argument (Self.Output_Body.Path);

      if Self.Ctxt.Is_Library then
         if Self.Ctxt.Is_Shared_Library
           and then Self.Ctxt.Library_Standalone /= GPR2.Project.Encapsulated
         then

            --  Link against a shared GNAT run time

            Cmd_Line.Add_Argument ("-shared");
         end if;

         if Self.Ctxt.Is_Library_Standalone then
            --  We need to specify the prefix for the init/final procedures.
            --  Make sure that the init procedure is never "adainit".

            if Self.Ctxt.Library_Name = "ada" then
               Cmd_Line.Add_Argument ("-Lada_");
            else
               Cmd_Line.Add_Argument
                 ("-L" & String (Self.Ctxt.Library_Name));
            end if;
         end if;

      else

         --  If at least one imported library is shared, gnatbind must also be
         --  called with the -shared flag.

         for Imported_View of Self.Ctxt.Imports (Recursive => True) loop
            if Imported_View.Is_Library
              and then Imported_View.Is_Shared_Library
            then
               Cmd_Line.Add_Argument ("-shared");
               exit;
            end if;
         end loop;
      end if;

      if Self.Ctxt.Is_Library
        and then Self.Ctxt.Is_Library_Standalone
        and then Boolean'Value
                   (Self.Ctxt.Attribute
                      (PRA.Library_Auto_Init).Value.Text)
      then
         Cmd_Line.Add_Argument ("-a");
      end if;

      for Ali of Self.Tree.Inputs (Self.UID, Explicit_Only => True) loop
         Cmd_Line.Add_Argument
           (GPR2.Build.Artifacts.Files.Object (Ali).Path,
            GPR2.Build.Command_Line.Simple);
      end loop;

      for View of Self.Ctxt.Closure (True, True, True) loop
         --  Make sure all ALI directories are visible
         if View.Language_Ids.Contains (Ada_Language) then
            if View.Is_Library then
               Cmd_Line.Add_Argument
                 ("-I" & View.Library_Ali_Directory.String_Value);

               if View.Id = Self.Ctxt.Id
                 and then Self.Ctxt.Is_Library_Standalone
               then
                  --  Need visibility on non-interface units

                  Cmd_Line.Add_Argument
                    ("-I" & View.Object_Directory.String_Value);
               end if;

            elsif View.Kind in With_Object_Dir_Kind then
               Cmd_Line.Add_Argument
                 ("-I" & View.Object_Directory.String_Value);
            end if;
         end if;
      end loop;

      --  [eng/gpr/gpr-issues#446] : This may include "-v"
      --  This switch is historically added for all GNAT version >= 6.4, it
      --  should be in the knowledge base instead of being hardcoded depending
      --  on the GNAT version.
      Add_Attr (PRA.Binder.Required_Switches, Lang_Ada_Idx, True, True);
      Add_Attr (PRA.Binder.Switches, Lang_Ada_Idx, True, True);

      --  Add -bargs and -bargs:Ada

      for Arg of Self.Tree.External_Options.Fetch
                   (External_Options.Binder, GPR2.Ada_Language)
      loop
         Cmd_Line.Add_Argument (Arg);
      end loop;

      --  ??? Modify binding option -A=<file> if <file> is not an absolute path
      --  Enforce_DashA_Absolute_Path;

      --  [eng/gpr/gpr-issues#446]
      --  Should be in the KB Required_Switches/Default_Switches/Switches and
      --  not hardcoded.
      if not (Self.Ctxt.Is_Library
              and then Self.Ctxt.Is_Library_Standalone)
      then
         Cmd_Line.Add_Argument ("-x");
      end if;

      if not Signature_Only then
         Add_Mapping_File;
      end if;

      --  Now that all switches have been analyzed, set the driver
      Cmd_Line.Set_Driver (Resolve_Binder);

      if not Signature_Only
        and then Cmd_Line.Total_Length > Command_Line_Limit
      then
         Create_Response_File;
      end if;
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature
     (Self      : in out Object;
      Load_Mode : Boolean)
   is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      if not Self.Signature.Add_Output (Self.Generated_Spec)
        and then Load_Mode
      then
         return;
      end if;

      if not Self.Signature.Add_Output (Self.Generated_Body)
        and then Load_Mode
      then
         return;
      end if;

      for Pred of Self.Tree.Inputs (UID) loop
         if not Self.Signature.Add_Input (Pred) and then Load_Mode then
            return;
         end if;
      end loop;
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self           : in out Object;
      Basename       : Simple_Name;
      Context        : GPR2.Project.View.Object;
      Has_Main       : Boolean;
      SAL_In_Closure : Boolean;
      Skip           : Boolean := False) is
   begin
      Self.Ctxt        := Context;
      Self.Basename    := +Basename;
      Self.Has_Main    := Has_Main;
      Self.SAL_Closure := SAL_In_Closure;
      Self.Output_Spec :=
        Artifacts.Files.Create
          (Context.Object_Directory.Compose ("b__" & Basename & ".ads"));
      Self.Output_Body :=
        Artifacts.Files.Create
          (Context.Object_Directory.Compose ("b__" & Basename & ".adb"));
      Self.Skip := Skip;

      if Skip then
         Self.Deactivate;
      end if;
   end Initialize;

   ----------
   -- Link --
   ----------

   function Link (Self : Object) return Actions.Link.Object'Class is

      function Internal_Link
        (Current_Artifact : Artifacts.Object'Class)
         return Actions.Link.Object'Class;

      function Internal_Link
        (Current_Artifact : Artifacts.Object'Class)
         return Actions.Link.Object'Class
      is
      begin
         for Action of Self.Tree.Successors (Current_Artifact) loop
            if Action in GPR2.Build.Actions.Link.Object'Class then
               --  Return the first partial link or link action that has this
               --  object as an input
               return Actions.Link.Object'Class (Action);
            end if;
         end loop;

         return GPR2.Build.Actions.Link.Undefined;
      end Internal_Link;
   begin
      return Internal_Link (Self.Post_Bind.Object_File);
   end Link;

   ---------------------
   -- Link_Opt_Insert --
   ---------------------

   function Link_Opt_Insert
     (Self : Object) return Actions.Link_Options_Insert.Object is
   begin
      for Action of Self.Tree.Successors (Self.Post_Bind.Object_File) loop
         if Action in GPR2.Build.Actions.Link_Options_Insert.Object'Class then
            return GPR2.Build.Actions.Link_Options_Insert.Object (Action);
         end if;
      end loop;

      return GPR2.Build.Actions.Link_Options_Insert.Undefined;
   end Link_Opt_Insert;

   -------------------
   -- On_Ali_Parsed --
   -------------------

   function On_Ali_Parsed
     (Self    : in out Object;
      Imports : GPR2.Containers.Name_Set) return Boolean
   is
      Link       : constant GPR2.Build.Actions.Link.Object'Class := Self.Link;
      To_Analyze : GPR2.Containers.Name_Set;

      function Add_Dependency (Unit : Name_Type) return Boolean;

      --------------------
      -- Add_Dependency --
      --------------------

      function Add_Dependency (Unit : Name_Type) return Boolean
      is
         CU         : Compilation_Unit.Object;
         Comp       : Actions.Compile.Ada.Object;
         Same_Scope : Boolean;

         use type GPR2.Project.View.Object;

      begin
         CU := Self.Ctxt.Namespace_Roots.First_Element.Unit (Unit);

         if not CU.Is_Defined then
            return True;
         end if;

         if CU.Owning_View.Is_Runtime then
            --  Runtime is handled specificitly by the binder, no need to
            --  handle it here.
            return True;
         end if;

         --  Check if the unit is in the same bind & link scope as the current
         --  action.

         Same_Scope := CU.Owning_View = Self.Ctxt
           or else (Self.Ctxt.Kind = K_Aggregate_Library
                    and then Self.View.Aggregated.Contains (CU.Owning_View));

         declare
            Comp_Id : constant Actions.Compile.Ada.Ada_Compile_Id :=
                        Actions.Compile.Ada.Create (CU);

         begin
            if not Self.Tree.Has_Action (Comp_Id) then
               Comp.Initialize (CU);

               if not Self.Tree.Add_Action (Comp) then
                  return False;
               end if;
            else
               Comp := Actions.Compile.Ada.Object
                 (Self.Tree.Action (Comp_Id));
            end if;
         end;

         if Same_Scope or else not Comp.View.Is_Library then
            --  If same scope or a regular project, just add the dependencies
            --  on the output of the compile action.

            Self.Tree.Add_Input
              (Self.UID, Comp.Local_Ali_File, False);
            Self.Tree.Add_Input
              (Link.UID, Comp.Object_File, False);
         else
            --  Use the .ali that's in the library dir instead of the object
            --  directory.

            Self.Tree.Add_Input
              (Self.UID, Comp.Intf_Ali_File, False);
         end if;

         for Dep of Comp.Withed_Units loop
            if not Self.Known_Inputs.Contains (Dep) then
               To_Analyze.Include (Dep);
            end if;
         end loop;

         return True;
      end Add_Dependency;

   begin
      To_Analyze := Imports;

      while not To_Analyze.Is_Empty loop
         declare
            Unit : constant Name_Type := To_Analyze.First_Element;
         begin
            To_Analyze.Delete_First;

            if not Self.Known_Inputs.Contains (Unit) then
               Self.Known_Inputs.Include (Unit);

               if not Add_Dependency (Unit) then
                  return False;
               end if;
            end if;
         end;
      end loop;

      return True;
   end On_Ali_Parsed;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      UID       : constant Actions.Action_Id'Class := Object'Class (Self).UID;
      Post_Bind : Actions.Post_Bind.Object;

   begin
      if not Db.Add_Output (UID, Self.Output_Spec)
         or else not Db.Add_Output (UID, Self.Output_Body)
      then
         return False;
      end if;

      Post_Bind :=
        Actions.Post_Bind.Create
          (Self.Output_Body, Self.View, Self, Self.Skip);

      if not Db.Add_Action (Post_Bind) then
         return False;
      end if;

      Db.Add_Input (Post_Bind.UID, Self.Output_Body, True);

      return True;
   end On_Tree_Insertion;

   -------------------------
   -- On_Tree_Propagation --
   -------------------------

   overriding function On_Tree_Propagation
     (Self : in out Object) return Boolean
   is
      To_Analyze : GPR2.Containers.Name_Set;

   begin
      for Ali of Self.Tree.Inputs (Self.UID, True) loop
         if Self.Tree.Has_Predecessor (Ali) then
            declare
               A_Comp : constant Actions.Compile.Ada.Object :=
                          Actions.Compile.Ada.Object
                            (Self.Tree.Predecessor (Ali));
            begin
               Self.Known_Inputs.Include (A_Comp.Input_Unit.Name);
               To_Analyze.Union (A_Comp.Withed_Units);
            end;
         end if;
      end loop;

      return Self.On_Ali_Parsed (To_Analyze);
   end On_Tree_Propagation;

   ---------------
   -- Post_Bind --
   ---------------

   function Post_Bind (Self : Object) return Actions.Post_Bind.Object is
   begin
      for Succ of Self.Tree.Successors (Self.Output_Body) loop
         if Succ in Actions.Post_Bind.Object'Class then
            return Actions.Post_Bind.Object (Succ);
         end if;
      end loop;

      return Actions.Post_Bind.Undefined;
   end Post_Bind;

   ------------------
   -- Post_Command --
   ------------------

   overriding
   function Post_Command
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : Unbounded_String := Null_Unbounded_String;
      Stderr : Unbounded_String := Null_Unbounded_String) return Boolean
   is
      use Ada.Text_IO;
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Runtime          : constant GPR2.Project.View.Object :=
                           Self.View.Tree.Runtime_Project;

      Add_Remaining    : Boolean := False;
      Xlinker_Seen     : Boolean := False;
      Stack_Equal_Seen : Boolean := False;
      Static_Libs      : Boolean := True;
      Adalib_Dir       : constant Path_Name.Object :=
                           Runtime.Object_Directory;

      procedure Process_Option_Or_Object_Line (Line : String);
      --  Pass options to the linker. Do not pass object file lines,
      --  as the objects to link are already obtained by parsing ALI files.

      -----------------------------------
      -- Process_Option_Or_Object_Line --
      -----------------------------------

      procedure Process_Option_Or_Object_Line (Line : String) is
         Idx              : constant PAI.Object :=
                              PAI.Create (Line, Case_Sensitive => True);
         Attr             : constant GPR2.Project.Attribute.Object :=
                              Self.View.Attribute
                                (PRA.Binder.Bindfile_Option_Substitution, Idx);

         use type GPR2.Project.Standalone_Library_Kind;

      begin
         if not Add_Remaining and then Line (Line'First) = '-' then
            --  We skip the list of objects, not reliable, the Tree_Db
            --  is more reliable. After the first option we however need
            --  to take everything since pragma Linker_Options may be
            --  anything, including non-options
            Add_Remaining := True;
         end if;

         if Line = "-shared" then
            Static_Libs := False;
         end if;

         if not Add_Remaining
           and then Path_Name.Simple_Name
             (Filename_Type (Line)) not in "g-trasym.o" | "g-trasym.obj"
         then
            --  g-trasym is a special case as it is not included in libgnat
            return;
         end if;

         if Line = "-Xlinker" then
            Xlinker_Seen := True;

         elsif Xlinker_Seen then
            Xlinker_Seen := False;

            --  Make sure that only the first switch --stack= is taken into
            --  account
            if Starts_With (Line, "-stack=") then
               if not Stack_Equal_Seen then
                  Stack_Equal_Seen := True;
                  Self.Linker_Opts.Append ("-Xlinker");
                  Self.Linker_Opts.Append (Line);
               end if;
            else
               Self.Linker_Opts.Append ("-Xlinker");
               Self.Linker_Opts.Append (Line);
            end if;

         elsif Starts_With (Line, "-Wl,--stack=") then
            if not Stack_Equal_Seen then
               Stack_Equal_Seen := True;
               Self.Linker_Opts.Append (Line);
            end if;

         elsif Attr.Is_Defined then
            for V of Attr.Values loop
               Self.Linker_Opts.Append (V.Text);
            end loop;

            --  For a number of archives, we need to indicate the full path of
            --  the arghive, if we find it, to be sure that the correct
            --  archive is used by the linker.

         elsif Line = "-lgnat" then
            if Static_Libs then
               if Self.Ctxt.Is_Library
                 and then Self.Ctxt.Library_Standalone =
                   GPR2.Project.Encapsulated
               then
                  Self.Linker_Opts.Append
                    (Adalib_Dir.Compose ("libgnat_pic.a").String_Value);
               else
                  Self.Linker_Opts.Append
                    (Adalib_Dir.Compose ("libgnat.a").String_Value);
               end if;
            else
               Self.Linker_Opts.Append (Line);
            end if;

         elsif Line = "-lgnarl" then
            if Static_Libs then
               if Self.Ctxt.Is_Library
                 and then Self.Ctxt.Library_Standalone =
                   GPR2.Project.Encapsulated
               then
                  Self.Linker_Opts.Append
                    (Adalib_Dir.Compose ("libgnarl_pic.a").String_Value);
               else
                  Self.Linker_Opts.Append
                    (Adalib_Dir.Compose ("libgnarl.a").String_Value);
               end if;
            else
               Self.Linker_Opts.Append (Line);
            end if;
            --  ??? There are other libs to look for

         else
            Self.Linker_Opts.Append (Line);
         end if;
      end Process_Option_Or_Object_Line;

      Src_File     : File_Type;
      Reading      : Boolean         := False;
      Begin_Marker : constant String := "--  BEGIN Object file/option list";
      End_Marker   : constant String := "--  END Object file/option list";
      Switch_Index : Natural;

      Link            : constant Actions.Link.Object'Class := Self.Link;
      Link_Opt_Insert : constant Actions.Link_Options_Insert.Object :=
        Self.Link_Opt_Insert;

   begin
      if Traces.Is_Active then
         Traces.Trace
           ("Parsing file '" & Self.Output_Body.Path.String_Value &
              "' generated by " & Self.UID.Image &
              " to obtain linker options");
      end if;

      Open
        (File => Src_File,
         Mode => In_File,
         Name => Self.Output_Body.Path.String_Value);

      while not End_Of_File (Src_File) loop
         declare
            Line : constant String := Get_Line (Src_File);
         begin
            if Index (Line, Begin_Marker) = Line'First then
               Reading := True;
            elsif Index (Line, End_Marker) = Line'First then
               Reading := False;
               exit;
            elsif Reading then
               Switch_Index := Index (Line, "--");

               if Switch_Index = 0 then
                  pragma Annotate (Xcov, Exempt_On, "unreachable code");
                  raise Internal_Error
                    with "Failed parsing line " & Line & " from " &
                    Self.Output_Body.Path.String_Value;
                  pragma Annotate (Xcov, Exempt_Off);
               end if;

               --  Skip the "--" comment prefix
               Switch_Index := Switch_Index + 2;
               declare
                  Trimmed_Line : constant String :=
                    Trim (Line (Switch_Index .. Line'Last), Both);
               begin
                  Traces.Trace
                    ("Processing parsed line '" & Trimmed_Line & "'");
                  Process_Option_Or_Object_Line (Trimmed_Line);
               end;
            end if;
         end;
      end loop;

      if not Link.Is_Defined then
         Traces.Trace
           ("No linker action related to "
            & Self.UID.Image
            & " has been found.");
      else
         --  Add the binder option to either the partial link object or the
         --  link object.
         --  If we have a partial link, no other object should further be added
         --  so we won't have any missed symbol resolution at the final link
         --  stage.
         Traces.Trace ("Options passed to " & Link.UID.Image & ":");
         for Opt of Self.Linker_Opts loop
            Traces.Trace ("* '" & Opt & "'");
            GPR2.Build.Actions.Link.Object'Class
              (Self.Tree.Action_Id_To_Reference (Link.UID).Element.all)
              .Add_Option (Opt);
         end loop;

         if Link_Opt_Insert.Is_Defined then
            Traces.Trace
              ("Options passed to " & Link_Opt_Insert.UID.Image & ":");
            for Opt of Self.Linker_Opts loop
               Traces.Trace ("* '" & Opt & "'");
               Actions.Link_Options_Insert.Object
                 (Self.Tree.Action_Id_To_Reference (Link_Opt_Insert.UID)
                    .Element.all)
                 .Add_Option (Opt);
            end loop;
         end if;
      end if;

      Close (Src_File);

      return True;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         Self.Tree.Reporter.Report
           ("cannot find binder generated file """ &
              String (Self.Output_Body.Path.Simple_Name) & '"',
            To_Stderr => True,
            Level     => GPR2.Message.Important);
         return False;
   end Post_Command;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      BN     : constant Simple_Name := -Self.Basename;
      Result : constant Ada_Bind_Id :=
                 (Name_Len  => BN'Length,
                  BN        => BN,
                  Ctxt      => Self.Ctxt);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Ada_Bind;
