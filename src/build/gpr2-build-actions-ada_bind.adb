--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings;
with Ada.Strings.Fixed;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;

with GNATCOLL.OS.FS;
with GNATCOLL.OS.Stat;
with GNATCOLL.Traces;
with GNATCOLL.Utils; use GNATCOLL.Utils;

with GPR2.Build.Actions.Compile.Ada;
with GPR2.Build.Actions.Link;
with GPR2.Build.Actions.Link_Options_Insert;
with GPR2.Build.Actions.Post_Bind;
with GPR2.Build.ALI_Parser;
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
with GPR2.Source_Reference;

package body GPR2.Build.Actions.Ada_Bind is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create
                ("GPR.BUILD.ACTIONS.ADA_BIND", GNATCOLL.Traces.Off);

   use GNAT;

   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;

   function Link (Self : Object) return Actions.Link.Object'Class;
   --  Return the link action that is a successor of this action,
   --  if one exists. Return Undefined if no such action is found.

   function Link_Opt_Insert
     (Self : Object) return Actions.Link_Options_Insert.Object;
   --  Returns the Link_Options_Insert action that is a successor of the
   --  current action, if one exists. Returns Undefined if no such action
   --  is found.

   function On_Ada_Dependencies
     (Self     : in out Object;
      Imports  : Containers.Name_Set;
      From_ALI : Boolean) return Boolean;

   -------------------
   -- Add_Root_Unit --
   -------------------

   procedure Add_Root_Unit
     (Self : in out Object;
      Root : GPR2.Build.Compilation_Unit.Object) is
   begin
      Self.Roots.Include (Root.Name, Root);
   end Add_Root_Unit;

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

      Lang_Ada_Idx      : constant PAI.Object :=
                            PAI.Create (GPR2.Ada_Language);
      Binder_From_Attrs : Unbounded_String;
      Exe_Ext           : constant String :=
                            (if GPR2.On_Windows then ".exe" else "");
      Roots             : GPR2.Containers.Name_Set;
      --  The list of Ada entry points to consider

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
                     Binder_From_Attrs :=
                       +(Self.Ctxt.Compiler_Prefix & Default_Binder_Name);
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
                                  To_Lower (String (Comp.Unit.Name)) &
                                  (if Comp.Unit.Main_Part = S_Spec
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

      --------------------
      -- Resolve_Binder --
      --------------------

      function Resolve_Binder return String is
         Default_Binder_Name : constant String :=
                                 Self.View.Compiler_Prefix &
                                 "gnatbind" & Exe_Ext;
         Binder_Path         : Path_Name.Object;
      begin
         --  if "gnatbind_prefix=", "--gnatbind_path=" or "ada_binder=" weren't
         --  found, default to "<prefix->gnatbind".

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

      if not Self.Main_Unit.Is_Defined then
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

      for Unit of Self.Roots loop
         declare
            UID  : constant Actions.Compile.Ada.Ada_Compile_Id :=
              Actions.Compile.Ada.Create (Unit);
            Comp : constant Actions.Compile.Ada.Object :=
              Actions.Compile.Ada.Object (Self.Tree.Action (UID));

            use type Project.View.Object;
         begin
            if Comp.Is_Defined then
               if Unit.Owning_View /= Self.Ctxt
                 and then Unit.Owning_View.Is_Library
               then
                  Cmd_Line.Add_Argument
                    (Comp.Intf_Ali_File.Path, Build.Command_Line.Simple);
               else
                  Cmd_Line.Add_Argument
                    (Comp.Local_Ali_File.Path, Build.Command_Line.Simple);
               end if;
            end if;
         end;
      end loop;

      --  For standalone libraries, we need to give the full list of ALIs that
      --  are part of the library

      for Art of Self.Tree.Inputs (Self.UID, Explicit_Only => True) loop
         if Self.Tree.Has_Predecessor (Art)
           and then Self.Tree.Predecessor (Art) in Compile.Ada.Object'Class
         then
            declare
               Comp : constant Actions.Compile.Ada.Object :=
                 Compile.Ada.Object (Self.Tree.Predecessor (Art));
            begin
               if not Self.Roots.Contains (Comp.Unit.Name) then
                  Cmd_Line.Add_Argument
                    (Comp.Local_Ali_File.Path, Build.Command_Line.Simple);
               end if;
            end;
         end if;
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

      if Self.Main_Unit.Is_Defined then
         declare
            Main_Source_Idx : constant PAI.Object :=
              PAI.Create_Source (Self.Main_Unit.Main_Part.Source.Simple_Name);
         begin
            --  If the source index does not exist, then the Ada language index
            --  will be used instead.

            Add_Attr (PRA.Binder.Switches, Main_Source_Idx, True, True);
         end;
      else
         Add_Attr (PRA.Binder.Switches, Lang_Ada_Idx, True, True);
      end if;

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
   end Compute_Command;

   ----------------------------
   -- Compute_Response_Files --
   ----------------------------

   overriding procedure Compute_Response_Files
     (Self           : in out Object;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object)
   is
      use Build.Response_Files;
   begin
      if Cmd_Line.Total_Length <= Command_Line_Limit then
         return;
      end if;

      Self.Response_Files.Initialize
        (None,
         Binder,
         Containers.Empty_Source_Value_List);

      declare
         Resp_File : constant Tree_Db.Temp_File :=
                       Self.Get_Or_Create_Temp_File
                         ("response_file", Local);
      begin
         Self.Response_Files.Register
           (Resp_File.FD,
            Resp_File.Path);
      end;

      Self.Response_Files.Create (Cmd_Line);
   end Compute_Response_Files;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature
     (Self            : in out Object;
      Check_Checksums : Boolean)
   is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      if not Self.Signature.Add_Output (Self.Generated_Spec, Check_Checksums)
      then
         return;
      end if;

      if not Self.Signature.Add_Output (Self.Generated_Body, Check_Checksums)
      then
         return;
      end if;

      for Pred of Self.Tree.Inputs (UID) loop
         if not Self.Signature.Add_Input (Pred, Check_Checksums) then
            return;
         end if;
      end loop;
   end Compute_Signature;

   ------------------------
   -- Extended_Interface --
   ------------------------

   function Extended_Interface (Self : Object) return Compilation_Unit.Maps.Map
   is
   begin
      return Result : Compilation_Unit.Maps.Map do
         for C in Self.Extra_Intf.Iterate loop
            declare
               CU : constant Compilation_Unit.Object :=
                      Extended_Interface_Map.Key (C);
            begin
               Result.Include (CU.Name, CU);
            end;
         end loop;
      end return;
   end Extended_Interface;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self           : in out Object;
      Basename       : Simple_Name;
      Context        : GPR2.Project.View.Object;
      Main_Unit      : GPR2.Build.Compilation_Unit.Object;
      SAL_In_Closure : Boolean;
      Skip           : Boolean := False)
   is
      procedure Add_Root_Attr (Attr : GPR2.Project.Attribute.Object);

      New_Roots : Compilation_Unit.Maps.Map;
      Root_Attr : Project.Attribute.Object;

      -------------------
      -- Add_Root_Attr --
      -------------------

      procedure Add_Root_Attr (Attr : GPR2.Project.Attribute.Object) is
         CU : Compilation_Unit.Object;
         Found : Boolean := False;

      begin
         for V of Attr.Values loop
            if Ada.Strings.Fixed.Index (V.Text, "*") > 0 then
               if Self.Ctxt.Is_Namespace_Root then
                  for CU of Self.Ctxt.Units loop
                     if GNATCOLL.Utils.Match
                       (Ada.Characters.Handling.To_Lower (String (CU.Name)),
                        V.Text)
                     then
                        New_Roots.Include (CU.Name, CU);
                        Found := True;
                     end if;
                  end loop;
               end if;

            elsif not New_Roots.Contains (Name_Type  (V.Text)) then
               CU := Self.Ctxt.Namespace_Roots.First_Element.Unit
                 (Name_Type (V.Text));

               if CU.Is_Defined then
                  New_Roots.Include (CU.Name, CU);
                  Found := True;
               end if;
            end if;

            if not Found then
               Self.Ctxt.Tree.Reporter.Report
                 (GPR2.Message.Create
                    (GPR2.Message.Warning,
                     "cannot find the Root unit """ & V.Text & '"',
                     V));
            end if;
         end loop;
      end Add_Root_Attr;

   begin
      --  Ensure the object wasn't previously initialized prior to this call
      Self := Undefined;

      Self.Ctxt        := Context;
      Self.Basename    := +Basename;
      Self.Main_Unit   := Main_Unit;
      Self.SAL_Closure := SAL_In_Closure;
      Self.Output_Spec :=
        Artifacts.Files.Create
          (Context.Object_Directory.Compose ("b__" & Basename & ".ads"));
      Self.Output_Body :=
        Artifacts.Files.Create
          (Context.Object_Directory.Compose ("b__" & Basename & ".adb"));
      Self.Skip := Skip;

      if Skip or else Self.Ctxt.Is_Externally_Built then
         Self.Deactivate;
      end if;

      if Main_Unit.Is_Defined then
         Self.Roots.Include (Main_Unit.Name, Main_Unit);

      elsif Self.Ctxt.Is_Library
        and then Self.Ctxt.Has_Any_Interfaces
      then
         for U of Self.Ctxt.Interface_Closure loop
            Self.Roots.Include (U.Name, U);
         end loop;

      else
         --  Consider all units of the context as root
         for U of Self.Ctxt.Own_Units loop
            Self.Roots.Include (U.Name, U);
         end loop;
      end if;

      --  Check additional entry points defined by the Roots attribute

      for U of Self.Roots loop
         Root_Attr := Self.Ctxt.Attribute
           (PRA.Roots,
            PAI.Create_Source
              (U.Main_Part.Source.Simple_Name,
               U.Main_Part.Index));

         if Root_Attr.Is_Defined then
            Add_Root_Attr (Root_Attr);
         end if;
      end loop;

      --  Also lookup for non-Ada source index

      if Self.Ctxt.Is_Library then
         --  Lookup the interface for libraries

         for Src of Self.Ctxt.Sources (Interface_Only => True) loop
            if Src.Language /= Ada_Language then
               Root_Attr := Self.Ctxt.Attribute
                 (PRA.Roots,
                  PAI.Create_Source (Src.Path_Name.Simple_Name));

               if Root_Attr.Is_Defined then
                     Add_Root_Attr (Root_Attr);
               end if;
            end if;
         end loop;

      else
         --  Lookup the mains otherwise

         for Main of Self.Ctxt.Mains loop
            declare
               Src : constant Build.Source.Object :=
                       Main.View.Visible_Source (Main.Source);
            begin
               if Src.Language /= Ada_Language then
                  Root_Attr := Self.Ctxt.Attribute
                    (PRA.Roots,
                     PAI.Create_Source (Src.Path_Name.Simple_Name));

                  if Root_Attr.Is_Defined then
                     Add_Root_Attr (Root_Attr);
                  end if;
               end if;
            end;
         end loop;
      end if;

      for CU of New_Roots loop
         Self.Roots.Include (CU.Name, CU);
      end loop;
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

   -------------------------
   -- On_Ada_Dependencies --
   -------------------------

   function On_Ada_Dependencies
     (Self     : in out Object;
      Imports  : Containers.Name_Set;
      From_ALI : Boolean) return Boolean
   is
      Link                : constant GPR2.Build.Actions.Link.Object'Class :=
                              Self.Link;
      To_Analyze_From_Ali : GPR2.Containers.Name_Set;
      To_Analyze_From_Ada : GPR2.Containers.Name_Set;
      --  We need to differentiate dependencies found from Ali and the ones
      --  from the Ada parser to allow for a proper update upon ALI creation.
      --  Else we will prune the analysis of ALI deps,

      function Add_Dependency (Unit : Name_Type) return Boolean;

      --------------------
      -- Add_Dependency --
      --------------------

      function Add_Dependency (Unit : Name_Type) return Boolean
      is
         CU         : Compilation_Unit.Object;
         Comp       : Actions.Compile.Ada.Object;
         Same_Scope : Boolean;
         Part       : Unit_Kind := S_Spec;
         S_Deps     : Containers.Name_Set;
         B_Deps     : Containers.Name_Set;

         use GPR2.Project;
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

         if CU.Owning_View.Is_Library
           and then CU.Owning_View.Is_Externally_Built
         then
            return True;
         end if;

         --  Check if the unit is in the same bind & link scope as the current
         --  action.

         Same_Scope := CU.Owning_View = Self.Ctxt
           or else
             (Self.Ctxt.Closure (False, True, True).Contains (CU.Owning_View)
              and then not CU.Owning_View.Is_Library);

         declare
            Comp_Id : constant Actions.Compile.Ada.Ada_Compile_Id :=
                        Actions.Compile.Ada.Create (CU);

         begin
            if not Self.Tree.Has_Action (Comp_Id) then
               Comp.Initialize (CU);

               if CU.Owning_View.Is_Externally_Built then
                  --  Ensure the ALI is parsed: as the project is
                  --  externally built, the signature won't be checked and
                  --  the ali is normally loaded during this phase.

                  Comp.Parse_Ali;
               end if;

               if not Self.Tree.Add_Action (Comp) then
                  return False;
               end if;
            else
               Comp := Actions.Compile.Ada.Object
                 (Self.Tree.Action (Comp_Id));
            end if;
         end;

         if Same_Scope then
            --  If same scope just add the dependencies
            --  on the output of the compile action.

            --  All units that are in a SAL and are in the closure of its
            --  interface need to be listed explicitly during the bind
            --  operation. We do that by marking them as explicit inputs
            --  of the action.

            Self.Tree.Add_Input
              (Self.UID,
               Comp.Local_Ali_File,
               Self.Ctxt.Is_Library and then Self.Ctxt.Is_Library_Standalone);

            if Link.Is_Defined then
               Self.Tree.Add_Input (Link.UID, Comp.Object_File, False);
            end if;

         elsif Self.Ctxt.Is_Library
           and then Self.Ctxt.Library_Standalone = Encapsulated
         then
            --  Part of a library, just add the ALI to the binder
            Self.Tree.Add_Input
              (Self.UID, Comp.Local_Ali_File, False);

         else
            --  Use the .ali that's in the library dir instead of the object
            --  directory.

            Self.Tree.Add_Input
              (Self.UID, Comp.Intf_Ali_File, False);
         end if;

         if Comp.Valid_Signature or else Comp.View.Is_Externally_Built then
            --  If the new dependency has a valid signature, that's necessarily
            --  because it has been just compiled (or skipped because the
            --  signature has been checked). We can thus rely on its ALI file
            --  to give us accurate dependencies, so add it in the Todo list.

            if not Comp.ALI.Is_Parsed then
               Self.Tree.Reporter.Report
                 (Message.Create
                    (Message.Error,
                     "Incorrectly formatted ali file """
                     & Comp.Dependency_File.Path.String_Value
                     & '"',
                     Source_Reference.Create
                       (Comp.Dependency_File.Path.Value, 0, 0)));
               return False;
            end if;

            To_Analyze_From_Ali.Union (Comp.ALI.Withed_From_Spec);
            To_Analyze_From_Ali.Union (Comp.ALI.Withed_From_Body);
            To_Analyze_From_Ali.Difference (Self.Analyzed);

         elsif not From_ALI then
            --  If From_ALI is unset, this means we're in the initial actions
            --  population stage, so the signature is expected to fail here.
            --  Since we want this stage to be as close as possible to the
            --  final dependency graph, let's continue analyzing the dependency
            --  using the Ada parser.

            S_Deps := CU.Known_Dependencies;
            To_Analyze_From_Ada.Union (S_Deps);
            To_Analyze_From_Ada.Difference (Self.Pre_Analyzed);
         end if;

         return True;
      end Add_Dependency;

   begin
      if From_ALI then
         To_Analyze_From_Ali := Imports;

         --  Consider the units already discovered at Ada source parsing time
         --  as analyzed (so that they're not added twice, duplicating the
         --  processing time).

         Self.Analyzed := Self.Pre_Analyzed;
      else
         To_Analyze_From_Ada := Imports;
      end if;

      while not To_Analyze_From_Ali.Is_Empty
        or else not To_Analyze_From_Ada.Is_Empty
      loop
         declare
            Unit         : constant Name_Type :=
                             (if not To_Analyze_From_Ali.Is_Empty
                              then To_Analyze_From_Ali.First_Element
                              else To_Analyze_From_Ada.First_Element);
            Dep_From_Ali : constant Boolean :=
                             not To_Analyze_From_Ali.Is_Empty;
            Pos          : Containers.Name_Type_Set.Cursor;
            Inserted     : Boolean;
         begin
            if Dep_From_Ali then
               To_Analyze_From_Ali.Delete_First;
               Self.Analyzed.Insert (Unit, Pos, Inserted);
            else
               To_Analyze_From_Ada.Delete_First;
               Self.Pre_Analyzed.Insert (Unit, Pos, Inserted);
            end if;

            if Inserted
              and then not Add_Dependency (Unit)
            then
               return False;
            end if;
         end;
      end loop;

      return True;
   end On_Ada_Dependencies;

   -------------------
   -- On_Ali_Parsed --
   -------------------

   function On_Ali_Parsed
     (Self : in out Object;
      Comp : GPR2.Build.Actions.Compile.Ada.Object) return Boolean
   is
      Scope      : Containers.Name_Set;
      To_Analyze : Extended_Interface_Map.Map;
   begin
      --  First pass: adjust the Db dependencies to take into account potential
      --  new dependencies between From_CU and the list of imports

      if not (Comp.Withed_Units_From_Spec.Is_Empty
              and then Comp.Withed_Units_From_Body.Is_Empty)
        and then not Self.On_Ada_Dependencies (Comp.Withed_Units, True)
      then
         return False;
      end if;

      --  Second stage: look if the library interface needs to be adjusted with
      --  a required new dependency

      if Self.Ctxt.Is_Library
        and then Self.Ctxt.Has_Any_Interfaces
        and then
          (Self.Roots.Contains (Comp.Unit.Name)
           or else Self.Extra_Intf.Contains (Comp.Unit))
      then
         if Comp.Spec_Needs_Body then
            Scope := Comp.Withed_Units;
         else
            Scope := Comp.Withed_Units_From_Spec;
         end if;

         for U of Scope loop
            if not Self.Itf_Analyzed.Contains (U) then
               To_Analyze.Include
                 (Self.Ctxt.Namespace_Roots.First_Element.Unit (U),
                  Comp.Unit.Name);
            end if;
         end loop;

         while not To_Analyze.Is_Empty loop
            declare
               C        : Extended_Interface_Map.Cursor := To_Analyze.First;
               CU       : constant Compilation_Unit.Object :=
                            Extended_Interface_Map.Key (C);
               From     : constant Name_Type :=
                            Extended_Interface_Map.Element (C);
               UID      : constant Compile.Ada.Ada_Compile_Id :=
                            Compile.Ada.Create (CU);
               New_Comp : Compile.Ada.Object;
               Inserted : Boolean;
               Add_Intf : Boolean := True;

               use type GPR2.Project.View.Object;

            begin
               To_Analyze.Delete (C);
               Self.Itf_Analyzed.Include (CU.Name);

               Add_Intf := CU.Is_Defined;

               --  Do not add Ada runtime to the interface in case of
               --  encapsulated library.

               if Add_Intf and then CU.Owning_View.Is_Runtime then
                  Add_Intf := False;
               end if;

               if Add_Intf
                 --  Ignore external units
                 and then CU.Owning_View /= Self.View
                 --  except for aggregated projects
                 and then
                   (Self.Ctxt.Kind /= K_Aggregate_Library
                    or else
                      not Self.Ctxt.Aggregated.Contains (CU.Owning_View))
               then
                  Add_Intf := False;
               end if;

               if Add_Intf and then Self.Roots.Contains (CU.Name) then
                  Add_Intf := False;
               end if;

               if Add_Intf then
                  Self.Extra_Intf.Insert (CU, From, C, Inserted);

                  if Inserted
                    and then Self.Tree.Has_Action (UID)
                  then
                     New_Comp := Compile.Ada.Object (Self.Tree.Action (UID));
                  end if;

                  if New_Comp.Is_Defined
                    and then New_Comp.Valid_Signature
                  then
                     if New_Comp.Spec_Needs_Body then
                        Scope := New_Comp.Withed_Units;
                     else
                        Scope := New_Comp.Withed_Units_From_Spec;
                     end if;
                  end if;

                  for U of Scope loop
                     if not Self.Itf_Analyzed.Contains (U) then
                        declare
                           CU2 : constant Compilation_Unit.Object :=
                                  Self.Ctxt.Namespace_Roots.First_Element.Unit
                                    (U);
                        begin
                           if CU2.Is_Defined then
                              To_Analyze.Include (CU2, CU.Name);
                           end if;
                        end;
                     end if;
                  end loop;
               end if;
            end;
         end loop;
      end if;

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
      use type GPR2.Project.View.Object;
      Deps : Containers.Name_Set;
   begin
      --  Now add our explicit inputs

      for CU of Self.Roots loop
         declare
            Ada_Comp : Actions.Compile.Ada.Object;
            Link     : constant Actions.Link.Object'Class := Self.Link;
         begin
            if not Self.Tree.Has_Action (Compile.Ada.Create (CU)) then
               Ada_Comp.Initialize (CU);

               if not Self.Tree.Add_Action (Ada_Comp) then
                  return False;
               end if;

            else
               Ada_Comp :=
                 Actions.Compile.Ada.Object
                   (Self.Tree.Action (Compile.Ada.Create (CU)));
            end if;

            if Self.Ctxt /= CU.Owning_View and then CU.Owning_View.Is_Library
            then
               Self.Tree.Add_Input (Self.UID, Ada_Comp.Intf_Ali_File, True);
            else
               Self.Tree.Add_Input (Self.UID, Ada_Comp.Local_Ali_File, True);
            end if;

            if Link.Is_Defined and then Ada_Comp.Object_File.Is_Defined then
               Self.Tree.Add_Input (Link.UID, Ada_Comp.Object_File, True);
            end if;
         end;
      end loop;

      for Ali of Self.Tree.Inputs (Self.UID, True) loop
         if Self.Tree.Has_Predecessor (Ali) then
            declare
               A_Comp : constant Actions.Compile.Ada.Object :=
                 Actions.Compile.Ada.Object (Self.Tree.Predecessor (Ali));
            begin
               Deps.Union (A_Comp.Unit.Known_Dependencies);
            end;
         end if;
      end loop;

      if not Deps.Is_Empty and then not Self.On_Ada_Dependencies (Deps, False)
      then
         return False;
      end if;

      return True;
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
      use GNATCOLL.OS;
      use GNATCOLL.OS.FS;
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

      Src_File     : File_Descriptor;
      Attrs        : Stat.File_Attributes;

      Link            : constant Actions.Link.Object'Class := Self.Link;
      Link_Opt_Insert : constant Actions.Link_Options_Insert.Object :=
        Self.Link_Opt_Insert;

   begin
      for C in Self.Extra_Intf.Iterate loop
         declare
            CU : constant Compilation_Unit.Object :=
                   Extended_Interface_Map.Key (C);
            From : constant Name_Type :=
                     Extended_Interface_Map.Element (C);
         begin
            Self.Tree.Reporter.Report
              (GPR2.Message.Create
                 (GPR2.Message.Warning,
                  "unit """  & String (CU.Name)
                  & """ is not in the interface set, but it is needed by """
                  & String (From) & """",
                  GPR2.Source_Reference.Create
                    (Self.Ctxt.Path_Name.Value, 0, 0)));
         end;
      end loop;

      if Traces.Is_Active then
         Traces.Trace
           ("Parsing file '" & Self.Output_Body.Path.String_Value &
              "' generated by " & Self.UID.Image &
              " to obtain linker options");
      end if;

      Attrs := Stat.Stat (Self.Output_Body.Path.String_Value);

      if not Stat.Exists (Attrs)
        or else not Stat.Is_File (Attrs)
        or else not Stat.Is_Readable (Attrs)
      then
         Self.Tree.Reporter.Report
           ("cannot find binder generated file """ &
              String (Self.Output_Body.Path.Simple_Name) & '"',
            To_Stderr => True,
            Level     => GPR2.Message.Important);

         return False;
      end if;

      Src_File := Open (Self.Output_Body.Path.String_Value, Read_Mode);

      declare
         Buffer       : String (1 .. Natural (Stat.Length (Attrs)));
         Ign          : Natural with Unreferenced;
         Pos, Last    : Natural;
         Reading      : Boolean         := False;
         Begin_Marker : constant String := "--  BEGIN Object file/option list";
         End_Marker   : constant String := "--  END Object file/option list";
         Switch_Index : Natural;
      begin
         Ign := Read (Src_File, Buffer);
         Close (Src_File);

         Pos := 1;

         while Pos < Buffer'Last loop
            Last := GNATCOLL.Utils.Line_End (Buffer, Pos);

            declare
               Line : String renames Buffer (Pos .. Last);
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
                                      Trim (Line (Switch_Index .. Line'Last),
                                            Both);
                  begin
                     Traces.Trace
                       ("Processing parsed line '" & Trimmed_Line & "'");
                     Process_Option_Or_Object_Line (Trimmed_Line);
                  end;
               end if;
            end;

            --  Use bigest between pos and last to prevent infinite recursion
            --  in case of empty strings when the line delimiter has several
            --  characters (CR/LF on windows).
            Pos := GNATCOLL.Utils.Next_Line
              (Buffer,
               Natural'Max (Pos, Last));
         end loop;
      end;

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
              .Add_Option_From_Binder (Opt);
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

      return True;
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
