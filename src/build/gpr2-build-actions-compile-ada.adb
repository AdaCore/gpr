--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNATCOLL.Traces;

with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.ALI_Parser;
with GPR2.Build.Artifacts.Key_Value;
with GPR2.Build.Tree_Db;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.View.Set;
with GPR2.Source_Reference;

package body GPR2.Build.Actions.Compile.Ada is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create
                ("GPR.BUILD.ACTIONS.COMPILE.ADA", GNATCOLL.Traces.Off);

   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;
   package Actions renames GPR2.Build.Actions;

   function Artifacts_Base_Name
     (Unit : GPR2.Build.Compilation_Unit.Object) return Simple_Name;

   function Can_Unit_Be_Imported
     (Self : Object;
      CU   : GPR2.Build.Compilation_Unit.Object) return Boolean;
   --  Ensure that a unit coming from another project can be imported.
   --  It is the case, if the unit is part of the Interfaces or the
   --  Library_Interface attributes. Otherwise, this units is reserved for
   --  internal project implementation.

   function Get_Attr
     (V       : GPR2.Project.View.Object;
      Name    : Q_Attribute_Id;
      Idx     : Language_Id;
      Default : Value_Type) return Value_Type;

   ----------------------
   -- Action_Parameter --
   ----------------------

   overriding function Action_Parameter
     (Self : Ada_Compile_Id) return Value_Type is
   begin
      if Self.Index = No_Index then
         return String (Self.Src_Name);
      else
         return String (Self.Src_Name) & "@" & Idx_Image (Self.Index);
      end if;
   end Action_Parameter;

   -------------------------
   -- Artifacts_Base_Name --
   -------------------------

   function Artifacts_Base_Name
     (Unit : GPR2.Build.Compilation_Unit.Object) return Simple_Name
   is
      Main : constant Compilation_Unit.Unit_Location := Unit.Main_Part;
      BN   : constant Simple_Name := Simple_Name (Main.Source.Base_Name);

   begin
      if Main.Index = No_Index then
         return BN;
      else
         declare
            Img : constant String := Main.Index'Image;
            Sep : constant String :=
              Get_Attr
                (Main.View, PRA.Compiler.Multi_Unit_Object_Separator,
                 Ada_Language, "~");
         begin
            return BN & Simple_Name (Sep & Img (Img'First + 1 .. Img'Last));
         end;
      end if;
   end Artifacts_Base_Name;

   --------------------------
   -- Can_Unit_Be_Imported --
   --------------------------

   function Can_Unit_Be_Imported
     (Self : Object;
      CU   : GPR2.Build.Compilation_Unit.Object) return Boolean
   is
      use GPR2.Project.View;

      CU_View : constant GPR2.Project.View.Object := CU.Owning_View;
      Allowed : Boolean := True;
      Src     : GPR2.Build.Source.Object;

   begin
      --  There is no restriction of units visibility inside the same view

      if Self.View /= CU_View then
         if CU_View.Has_Any_Interfaces
           and then not CU_View.Interface_Closure.Contains (CU.Name)
         then
            --  Two cases here:
            --  * The view is a standalone library: if the unit is not
            --    listed by the Library_Interface, or its source by the
            --    Interfaces attribute, then it can not be imported.Allowed
            --  * The view is not a standalone library: if the unit source
            --    is not listed by the Interfaces attribute, then it can
            --   not be imported.

            --  There's an exception for sources coming from --src-subdirs:
            --  since this is for instrumented code, we need to loosen the
            --  rule here and allow any source from this subdir.
            Src := CU_View.Visible_Source (CU.Main_Part.Source);

            if Src.From_Src_Subdirs then
               return True;
            end if;

            Allowed := False;
         end if;

         if not Allowed then
            Self.Tree.Reporter.Report
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  "unit """
                  & String (Self.CU.Name)
                  & """ can not import unit """
                  & String (CU.Name)
                  & """:" & ASCII.LF
                  & " it is not part of the interfaces of the project "
                  & String (CU_View.Name),
                  GPR2.Source_Reference.Object
                    (GPR2.Source_Reference.Create
                         (Self.Ctxt.Path_Name.Value, 0, 0))));
         end if;

         if Allowed
           and then Self.Tree.Build_Options.No_Indirect_Imports
           and then not Self.View.Imports.Contains (CU_View)
           and then not Self.View.Limited_Imports.Contains (CU_View)
         then
            Allowed := False;

            Self.Tree.Reporter.Report
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  "unit """ & String (Self.CU.Name) &
                    """ cannot import unit """ &
                    String (CU.Name) & ":" &
                    ASCII.LF &
                    " """ & String (Self.View.Name) &
                    """ does not directly import project """ &
                    String (CU_View.Name) & """",
                  GPR2.Source_Reference.Create
                    (Self.Src.Path_Name.Value, 0, 0)));
         end if;
      end if;

      return Allowed;
   end Can_Unit_Be_Imported;

   --------------------------
   -- Change_Intf_Ali_File --
   --------------------------

   procedure Change_Intf_Ali_File
     (Self : in out Object;
      Path : Path_Name.Object)
   is
      From   : constant Artifacts.Files.Object := Self.Lib_Ali_File;
   begin
      if From.Path = Path then
         return;
      end if;

      Self.Lib_Ali_File := Artifacts.Files.Create (Path);
      Self.Tree.Replace_Artifact (From, Self.Lib_Ali_File);
   end Change_Intf_Ali_File;

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean)
   is
      Attr : GPR2.Project.Attribute.Object;
   begin
      Compile.Object (Self).Compute_Command (Slot, Cmd_Line, Signature_Only);

      if Signature_Only then
         --  Ignore the config pragmas commands since they're already part
         --  of the signature
         return;
      end if;

      if Self.Global_Config_Pragmas.Is_Defined
        or else Self.Local_Config_Pragmas.Is_Defined
      then
         Attr := Self.View.Attribute
           (PRA.Compiler.Config_File_Switches, PAI.Create (Ada_Language));
      end if;

      if Self.Global_Config_Pragmas.Is_Defined then
         for J in Attr.Values.First_Index .. Attr.Values.Last_Index - 1 loop
            Cmd_Line.Add_Argument
              (Attr.Values.Element (J).Text, Build.Command_Line.Ignore);
         end loop;

         Cmd_Line.Add_Argument
           (Attr.Values.Last_Element.Text &
              Self.Global_Config_Pragmas.String_Value,
            Build.Command_Line.Ignore);
      end if;

      if Self.Local_Config_Pragmas.Is_Defined then
         for J in Attr.Values.First_Index .. Attr.Values.Last_Index - 1 loop
            Cmd_Line.Add_Argument
              (Attr.Values.Element (J).Text, Build.Command_Line.Ignore);
         end loop;

         Cmd_Line.Add_Argument
           (Attr.Values.Last_Element.Text &
              Self.Local_Config_Pragmas.String_Value,
            Build.Command_Line.Ignore);
      end if;
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature
     (Self      : in out Object;
      Load_Mode : Boolean)
   is
      Version : Artifacts.Key_Value.Object;

   begin
      --  The list of dependencies is only accurate if the Ali file is
      --  accurate, so check it first: if it changed there's no need to
      --  go further.

      if not Self.Signature.Add_Output (Self.Dep_File)
        and then Load_Mode
      then
         return;
      end if;

      if Self.Ctxt.Tree.Has_Ada_Compiler_Version then
         Version := Artifacts.Key_Value.Create
           ("compiler_version",
            Self.Ctxt.Tree.Ada_Compiler_Version);

         if not Self.Signature.Add_Input (Version) and then Load_Mode then
            return;
         end if;
      end if;

      declare
         Deps : constant GPR2.Containers.Filename_Set := Self.Dependencies;
      begin
         if Deps.Is_Empty then
            Self.Signature.Invalidate;
            return;
         end if;

         for Dep of Deps loop
            --  Configuration pragmas are returned as dependency but are
            --  not sources of the view, so we need to filter them. The
            --  difference is that sources are reported as simple names while
            --  the config pragma sources have a full path.

            if Dep in Simple_Name then
               declare
                  Src : constant GPR2.Build.Source.Object :=
                          Self.View.Visible_Source (Dep);
               begin
                  if not Src.Is_Defined then
                     if (Self.Global_Config_Pragmas.Is_Defined
                         and then Dep = Self.Global_Config_Pragmas.Simple_Name)
                       or else
                         (Self.Local_Config_Pragmas.Is_Defined
                          and then Dep = Self.Local_Config_Pragmas.Simple_Name)
                       or else
                         (Self.Global_Config_File.Is_Defined
                          and then Dep = Self.Global_Config_File.Simple_Name)
                       or else
                         (Self.Local_Config_File.Is_Defined
                          and then Dep = Self.Local_Config_File.Simple_Name)
                     then
                        Traces.Trace
                          ("config pragma file reported as dependency, " &
                             "ignoring : " & String (Dep));
                     else
                        Traces.Trace
                          ("Compute_Signature: cannot find dependency " &
                             String (Dep));

                        if Load_Mode then
                           Self.Signature.Invalidate;
                           return;
                        end if;
                     end if;

                  elsif not Self.Signature.Add_Input
                      (Artifacts.Files.Create (Src.Path_Name))
                    and then Load_Mode
                  then
                     return;
                  end if;
               end;
            end if;
         end loop;
      end;

      if Self.Local_Config_Pragmas.Is_Defined
        and then not Self.Signature.Add_Input
                       (Artifacts.Files.Create (Self.Local_Config_Pragmas))
        and then Load_Mode
      then
         return;
      end if;

      if Self.Global_Config_Pragmas.Is_Defined
        and then not Self.Signature.Add_Input
                       (Artifacts.Files.Create (Self.Global_Config_Pragmas))
        and then Load_Mode
      then
         return;
      end if;

      if Self.Local_Config_File.Is_Defined
        and then not Self.Signature.Add_Input
                       (Artifacts.Files.Create (Self.Local_Config_File))
        and then Load_Mode
      then
         return;
      end if;

      if Self.Global_Config_File.Is_Defined
        and then not Self.Signature.Add_Input
                       (Artifacts.Files.Create (Self.Global_Config_File))
        and then Load_Mode
      then
         return;
      end if;

      --  Object file checksum is the heaviest to compute since those are
      --  pretty large compared to the other artifacts involved in this
      --  signature. So compute it last so that if there's any other
      --  artifact that changed we don't compute it.

      if Self.Obj_File.Is_Defined
        and then not Self.Signature.Add_Output (Self.Obj_File)
        and then Load_Mode
      then
         return;
      end if;
   end Compute_Signature;

   ------------------
   -- Dependencies --
   ------------------

   overriding function Dependencies
     (Self : Object) return Containers.Filename_Set
   is
      Result   : GPR2.Containers.Filename_Set;
      UID      : constant Actions.Action_Id'Class := Object'Class (Self).UID;

   begin
      if not Self.Dep_File.Path.Exists then
         Traces.Trace
           ("The ALI file for action " & UID.Image & " does not exist");

         return Containers.Empty_Filename_Set;
      end if;

      if not GPR2.Build.ALI_Parser.Dependencies (Self.Dep_File.Path, Result)
      then
         Traces.Trace
           ("Failed to parse dependencies from the ALI file " &
              Self.Dep_File.Path.String_Value);

         return Containers.Empty_Filename_Set;
      end if;

      return Result;
   end Dependencies;

   --------------
   -- Extended --
   --------------

   overriding function Extended (Self : Object) return Object is
   begin
      return Result : Object := Self do
         Result.Ctxt :=
           Self.Inh_From;
         Result.Src  :=
           Self.Inh_From.Source (Self.Input.Path_Name.Simple_Name);
         Result.Inh_From := GPR2.Project.View.Undefined;
      end return;
   end Extended;

   --------------
   -- Get_Attr --
   --------------

   function Get_Attr
     (V       : GPR2.Project.View.Object;
      Name    : Q_Attribute_Id;
      Idx     : Language_Id;
      Default : Value_Type) return Value_Type
   is
      Attr : constant GPR2.Project.Attribute.Object :=
               V.Attribute (Name, PAI.Create (Idx));
   begin
      if Attr.Is_Defined then
         return Attr.Value.Text;
      else
         return Default;
      end if;
   end Get_Attr;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Object;
      Src  : GPR2.Build.Compilation_Unit.Object)
   is
      View    : constant GPR2.Project.View.Object := Src.Owning_View;
      No_Obj  : constant Boolean :=
                  (View.Is_Library and then View.Is_Externally_Built)
                    or else View.Is_Runtime;
      Attr     : GPR2.Project.Attribute.Object;
      Closure  : GPR2.Project.View.Set.Object;

   begin
      --  Ensure the object wasn't previously initialized prior to this call
      Self := Undefined;

      Self.Ctxt   := Src.Owning_View;
      Self.Src    := Src.Owning_View.Source (Src.Main_Part.Source.Simple_Name);
      Self.Lang   := Ada_Language;
      Self.CU     := Src;

      --  ??? For Standalone libraries, we should probably not lookup for
      --  previous compilation artifacts, since we need to amend the ali
      --  file from the library directory.
      declare
         BN        : constant Simple_Name := Artifacts_Base_Name (Src);
         Ali_BN    : constant Simple_Name := BN & Self.Dep_File_Suffix;
         O_Suff    : constant Simple_Name :=
                       Simple_Name
                         (Get_Attr
                            (Self.View, PRA.Compiler.Object_File_Suffix,
                             Ada_Language,
                             ".o"));
         Candidate : GPR2.Project.View.Object;
         Inh_Src   : GPR2.Build.Source.Object;
         Found     : Boolean := False;
         Local_O   : GPR2.Path_Name.Object;
         Local_Ali : GPR2.Path_Name.Object;
         Lkup_O    : GPR2.Path_Name.Object;
         Lkup_Ali  : GPR2.Path_Name.Object;

      begin
         if not No_Obj then
            Local_O := Self.View.Object_Directory.Compose (BN & O_Suff);
         else
            Self.Obj_File := Artifacts.Object_File.Undefined;
         end if;

         Local_Ali := Self.View.Object_Directory.Compose (Ali_BN);

         if not Self.Input.Is_Inherited
           or else ((No_Obj or else Local_O.Exists)
                    and then Local_Ali.Exists)
         then
            --  Simple case: just use the local .o and .ali
            if not No_Obj then
               Self.Obj_File := Artifacts.Object_File.Create (Local_O);
            end if;

            Self.Dep_File := Artifacts.Files.Create (Local_Ali);

         else
            --  Lookup if the object file exists in the hierarchy

            Candidate := Self.Input.Inherited_From;

            while not Found and then Candidate.Is_Defined loop
               --  Note: we cannot extend an externally built project, so
               --  there's always supposed to be an object dir and file here.
               Lkup_O := Candidate.Object_Directory.Compose (BN & O_Suff);

               if Lkup_O.Exists then
                  Lkup_Ali :=
                    Candidate.Object_Directory.Compose (Ali_BN);

                  if Lkup_Ali.Exists then
                     Found := True;

                     exit;
                  end if;
               end if;

               Inh_Src :=
                 Candidate.Source (Self.Input.Path_Name.Simple_Name);

               if Inh_Src.Is_Inherited then
                  Candidate := Inh_Src.Inherited_From;
               else
                  Candidate := GPR2.Project.View.Undefined;
               end if;
            end loop;

            --  If not found, set the value to the object generated by the
            --  compilation.

            if not Found then
               Self.Obj_File := Artifacts.Object_File.Create (Local_O);
               Self.Dep_File := Artifacts.Files.Create (Local_Ali);
            else
               Self.Obj_File := Artifacts.Object_File.Create (Lkup_O);
               Self.Dep_File := Artifacts.Files.Create (Lkup_Ali);
               Self.Inh_From := Candidate;
            end if;
         end if;
      end;

      --  Identify the copies of the ali file in libraries

      if Self.Ctxt.Is_Aggregated_In_Library then
         Closure := Self.Ctxt.Aggregate_Libraries;
      elsif Self.Ctxt.Is_Library then
         Closure.Include (Self.Ctxt);
      end if;

      for V of Closure loop
         if not V.Is_Library_Standalone
           or else V.Interface_Closure.Contains (Self.CU.Name)
         then
            Self.In_Library := V;
            exit;
         end if;
      end loop;

      if Self.In_Library.Is_Defined then
         Self.Lib_Ali_File :=
           Artifacts.Files.Create
             (Self.In_Library.Library_Ali_Directory.Compose
                (Self.Dep_File.Path.Simple_Name));
      else
         Self.Lib_Ali_File := Self.Dep_File;
      end if;

      --  Check the configuration pragmas files

      Attr := Self.View.Tree.Root_Project.Attribute
        (PRA.Builder.Global_Configuration_Pragmas);

      if Attr.Is_Defined then
         --  Note: the Global/Local configuration pragmas attribute are
         --  expanded by the GPR parser to full names, so that they still
         --  reference the initial project dir when copied/renamed accross
         --  views.

         Self.Global_Config_Pragmas :=
           Path_Name.Create_File (Filename_Type (Attr.Value.Text),
                                  Self.View.Tree.Root_Project.Dir_Name.Value);
      end if;

      Attr := Self.View.Attribute (PRA.Compiler.Local_Configuration_Pragmas);

      if Attr.Is_Defined then
         Self.Local_Config_Pragmas :=
           Path_Name.Create_File (Filename_Type (Attr.Value.Text),
                                  Self.View.Dir_Name.Value);
      end if;

      Attr := Self.View.Tree.Root_Project.Attribute
        (PRA.Builder.Global_Config_File,
         PAI.Create (Ada_Language));

      if Attr.Is_Defined then
         --  Note: the Global/Local configuration pragmas attribute are
         --  expanded by the GPR parser to full names, so that they still
         --  reference the initial project dir when copied/renamed accross
         --  views.

         Self.Global_Config_File :=
           Path_Name.Create_File (Filename_Type (Attr.Value.Text),
                                  Self.View.Tree.Root_Project.Dir_Name.Value);
      end if;

      Attr := Self.View.Attribute
        (PRA.Compiler.Local_Config_File,
         PAI.Create (Ada_Language));

      if Attr.Is_Defined then
         Self.Local_Config_File :=
           Path_Name.Create_File (Filename_Type (Attr.Value.Text),
                                  Self.View.Dir_Name.Value);
      end if;
   end Initialize;

   --------------------
   -- On_Ready_State --
   --------------------

   overriding function On_Ready_State
     (Self : in out Object) return Boolean
   is
      Binds : Action_Id_Sets.Set;

   begin
      --  Now that we know the ALI file is correct, let the bind action know
      --  the actual list of imported units from this dependency file.

      if not GPR2.Build.ALI_Parser.Imports
        (Self.Dep_File.Path,
         Self.Withed_From_Spec,
         Self.Withed_From_Body,
         Self.Needs_Body)
      then
         Self.Tree.Reporter.Report
           (GPR2.Message.Create
              (GPR2.Message.Error,
               "failure to analyze the produced ali file",
               GPR2.Source_Reference.Object
                 (GPR2.Source_Reference.Create
                      (Self.Dep_File.Path.Value, 0, 0))));
         return False;
      end if;

      --  Retrieve a list of Bind actions that are using this ali file.

      for Action of Self.Tree.Successors (Self.Dep_File) loop
         if Action in GPR2.Build.Actions.Ada_Bind.Object'Class then
            --  Note: do not call On_Ali_Parsed from this loop since we're
            --  iterating over Self.Tree.Successors so any modification to
            --  the tree within this loop may raise a Program_Error "attempt
            --  to tamper with cursors".

            Binds.Include (Action.UID);
         end if;
      end loop;

      for UID of Binds loop
         declare
            Bind : constant access Actions.Ada_Bind.Object'Class :=
                     Actions.Ada_Bind.Object'Class
                       (Self.Tree.Action_Id_To_Reference
                          (UID).Element.all)'Access;
         begin
            if not Bind.On_Ali_Parsed (Self) then
               return False;
            end if;
         end;
      end loop;

      return True;
   end On_Ready_State;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding function On_Tree_Insertion
     (Self : Object;
      Db   : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      UID       : constant Actions.Action_Id'Class := Object'Class (Self).UID;

   begin
      if Self.Obj_File.Is_Defined then
         if not Db.Add_Output (UID, Self.Obj_File) then
            return False;
         end if;
      end if;

      if not Db.Add_Output (UID, Self.Dep_File) then
         return False;
      end if;

      if Self.Lib_Ali_File.Path /= Self.Dep_File.Path
        and then not Db.Add_Output (UID, Self.Lib_Ali_File)
      then
         return False;
      end if;

      return True;
   end On_Tree_Insertion;

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
      use GPR2.Path_Name;
      CU     : Compilation_Unit.Object;
      Result : Boolean := True;

   begin
      if Status /= Skipped then
         --  If the .o and .ali stored in this action were inherited, and we
         --  finally decided to compile, we need to now redirect to the new .o
         --  and .ali

         if Self.Inh_From.Is_Defined then
            declare
               BN        : constant Simple_Name :=
                             Artifacts_Base_Name (Self.CU);
               O_Suff    : constant Simple_Name :=
                             Simple_Name
                               (Get_Attr
                                  (Self.View, PRA.Compiler.Object_File_Suffix,
                                   Ada_Language,
                                   ".o"));
               Ali_Suff  : constant Simple_Name := Self.Dep_File_Suffix;
               Local_O   : Artifacts.Object_File.Object;
               Local_Ali : Artifacts.Files.Object;
               use type Artifacts.Object_File.Object;

            begin
               Local_O := Artifacts.Object_File.Create
                 (Self.View.Object_Directory.Compose (BN & O_Suff));

               if Local_O /= Self.Obj_File then
                  Local_Ali := Artifacts.Files.Create
                    (Self.View.Object_Directory.Compose (BN & Ali_Suff));

                  Self.Tree.Replace_Artifact (Self.Obj_File, Local_O);
                  Self.Tree.Replace_Artifact (Self.Dep_File, Local_Ali);
                  Self.Obj_File := Local_O;
                  Self.Dep_File := Local_Ali;
                  Self.Inh_From := GPR2.Project.View.Undefined;
               end if;
            end;
         end if;
      end if;

      --  Now that we know the ALI file is correct, let the bind action know
      --  the actual list of imported units from this dependency file.
      --
      --  This is done by the On_Ready_State callback to adjust the tree
      --  when the compile action is done.

      if not Self.On_Ready_State then
         return False;
      end if;

      --  Check if the dependencies of the just compiled unit are allowed
      --  (direct import if --no-indirect-import is specified, or part of
      --  the interface if the owning view is a standalone library.

      for Unit of Self.CU.Known_Dependencies loop
         CU := Self.Ctxt.Namespace_Roots.First_Element.Unit (Unit);

         if CU.Is_Defined then
            Result := Self.Can_Unit_Be_Imported (CU) and then Result;
         end if;
      end loop;

      return Result;
   end Post_Command;

end GPR2.Build.Actions.Compile.Ada;
