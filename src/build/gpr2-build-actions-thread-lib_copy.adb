--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.OS.FS;
with GNATCOLL.OS.FSUtil;
with GNATCOLL.OS.Stat;
with GNATCOLL.Traces;

with GPR2.Build.Actions.Process.Compile.Ada;
with GPR2.Build.Artifacts.Source_Files;
with GPR2.Build.Source;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;
with GPR2.Reporter;
with GPR2.Source_Reference;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Tree_Db;

package body GPR2.Build.Actions.Thread.Lib_Copy is

   package PRA renames GPR2.Project.Registry.Attribute;

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPR.BUILD.ACTIONS.THREAD.LIB_COPY", GNATCOLL.Traces.Off);

   function Interface_Units (Self : Object) return Compilation_Unit.Maps.Map
   with Pre => Self.Is_Defined and then Self.View.Is_Library;
   --  Return the complete list of Ada units that are exposed by this library.
   --  For regular libraries this will be the list of units owned by the view
   --  or the aggregated views (aggregate library case).
   --  For standalone libraries this will be the list of units listed by the
   --  Library_Interface or Interfaces attributes complemented by their
   --  dependencies.

   function Register_Artifacts
     (Self  : Object;
      Units : Compilation_Unit.Maps.Map;
      Db    : in out GPR2.Build.Tree_Db.Object) return Boolean;
   --  Register ALI files and (when Library_Src_Dir is set) source files as
   --  inputs and outputs of Self in Db.

   procedure Add_Unit_To_Lib_Interface
     (Self             : in out Object;
      Compilation_Unit : GPR2.Build.Compilation_Unit.Object)
   is
      Comp_Action : Actions.Process.Compile.Ada.Object;
   begin
      Comp_Action.Initialize (Compilation_Unit);

      Self.Extended_Interface.Include
        (Compilation_Unit.Name, Compilation_Unit);
      Self.Tree.Add_Input (Self.UID, Comp_Action.Local_Ali_File);
   end Add_Unit_To_Lib_Interface;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean)
   is
      use GPR2.Build.Actions.Process;
   begin
      if not Self.View.Is_Library then
         Self.Tree.Reporter.Report
           (Message.Create
              (Message.Error,
               "unexpected non-library view for thread library copy action",
               Source_Reference.Create (Self.View.Path_Name.Value, 0, 0)));
         return;
      end if;

      declare
         Units : constant Compilation_Unit.Maps.Map := Self.Interface_Units;
      begin
         for U of Units loop
            declare
               Obj_Dir_Ali : constant Artifacts.Files.Object :=
                 Compile.Object (Self.Tree.Action (Compile.Ada.Create (U)))
                   .Dependency_File;
               Lib_Dir_Ali : constant Artifacts.Files.Object :=
                 Artifacts.Files.Create
                   (Self.Ctxt.Library_Ali_Directory.Compose
                      (Obj_Dir_Ali.Path.Simple_Name));
            begin
               if not Self.Signature.Add_Input (Obj_Dir_Ali, Check_Checksums)
               then
                  return;
               end if;

               if not Self.Signature.Add_Output (Lib_Dir_Ali, Check_Checksums)
               then
                  return;
               end if;
            end;
         end loop;

         if Self.Ctxt.Has_Library_Src_Directory then

            --  Add Ada sources copied in Library_Src_Dir as input and output

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

                        pragma Unreferenced (Kind, View, Index, Sep_Name);
                     begin

                        if not Self.Signature.Add_Input
                                 (Artifacts.Source_Files.Create (Path),
                                  Check_Checksums)
                        then
                           return;
                        end if;

                        if not Self.Signature.Add_Output
                                 (Artifacts.Source_Files.Create (Dest),
                                  Check_Checksums)
                        then
                           return;
                        end if;
                     end On_Unit_Part;

                     Comp : Compile.Ada.Object :=
                       Compile.Ada.Object
                         (Self.Tree.Action (Compile.Ada.Create (CU)));
                  begin
                     if Comp.Spec_Needs_Body or else not CU.Has_Part (S_Spec)
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
                        if not Self.Signature.Add_Input
                                 (Artifacts.Source_Files.Create
                                    (Src.Path_Name),
                                  Check_Checksums)
                        then
                           return;
                        end if;

                        Dest := Src_Dir.Compose (Src.Path_Name.Simple_Name);

                        if not Self.Signature.Add_Output
                                 (Artifacts.Source_Files.Create (Dest),
                                  Check_Checksums)
                        then
                           return;
                        end if;
                     end if;
                  end;
               end loop;
            end;
         end if;
      end;

   end Compute_Signature;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Self   : in out Object;
      Stdout : in out Unbounded_String;
      Stderr : in out Unbounded_String) return Integer
   is
      use GPR2.Build.Actions.Process;
   begin
      declare
         Units : constant Compilation_Unit.Maps.Map := Self.Interface_Units;
      begin
         --  Copy the ali files to the library dir

         --  Two cases: standalone libraries where only the interface is to
         --  be copied, and regular libraries where all units need to be taken
         --  into account.

         for U of Units loop
            declare
               C_Id  : constant Compile.Ada.Ada_Compile_Id :=
                 Compile.Ada.Create (U);
               pragma
                 Assert
                   (Self.Tree.Has_Action (C_Id),
                    "interface unit '"
                    & String (U.Name)
                    & "' doesn't have an associated compile action");
               From  : constant Path_Name.Object :=
                 Compile.Object (Self.Tree.Action (C_Id)).Dependency_File.Path;
               To    : constant Path_Name.Object :=
                 Self.Ctxt.Library_Ali_Directory.Compose (From.Simple_Name);
               Attrs : GNATCOLL.OS.Stat.File_Attributes;

            begin
               Compile.Ada.Object
                 (Self.Tree.Action_Id_To_Reference (C_Id).Element.all)
                 .Change_Intf_Ali_File (To);

               if not Self.Ctxt.Is_Library_Standalone then
                  --  Just copy the ali file for standard libraries: they
                  --  need elaboration by the caller.

                  Traces.Trace
                    ("Copying """
                     & From.String_Value
                     & """ to """
                     & To.Containing_Directory.String_Value
                     & '"');

                  if not GNATCOLL.OS.FSUtil.Copy_File
                           (From.String_Value, To.String_Value)
                  then
                     Self.Tree.Reporter.Report
                       (GPR2.Message.Create
                          (GPR2.Message.Error,
                           "could not copy ali file "
                           & String (From.Simple_Name)
                           & " to the library directory",
                           GPR2.Source_Reference.Object
                             (GPR2.Source_Reference.Create
                                (Self.Ctxt.Path_Name.Value, 0, 0))));

                     return 1;
                  end if;

               else
                  --  Amend the ALI to add the SL (StandAlone) flag to
                  --  it to prevent multiple elaboration of the unit.

                  Attrs := GNATCOLL.OS.Stat.Stat (From.String_Value);

                  declare
                     use GNATCOLL.OS;
                     use type GNATCOLL.OS.FS.File_Descriptor;

                     Last : constant Integer :=
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
                     Ign    : Natural
                     with Unreferenced;
                     Found  : Boolean := False;
                     Input  : GNATCOLL.OS.FS.File_Descriptor;
                     Output : GNATCOLL.OS.FS.File_Descriptor;

                  begin
                     Input := FS.Open (From.String_Value, FS.Read_Mode);

                     if Input = FS.Invalid_FD then
                        Self.Tree.Reporter.Report
                          (GPR2.Message.Create
                             (GPR2.Message.Error,
                              "could not read the ali file """
                              & String (From.Simple_Name)
                              & '"',
                              GPR2.Source_Reference.Object
                                (GPR2.Source_Reference.Create
                                   (Self.Ctxt.Path_Name.Value, 0, 0))));

                        return 2;
                     end if;

                     Output := FS.Open (To.String_Value, FS.Write_Mode);

                     if Output = FS.Invalid_FD then
                        Self.Tree.Reporter.Report
                          (GPR2.Message.Create
                             (GPR2.Message.Error,
                              "could not create the ali file """
                              & String (To.Simple_Name)
                              & '"',
                              GPR2.Source_Reference.Object
                                (GPR2.Source_Reference.Create
                                   (Self.Ctxt.Path_Name.Value, 0, 0))));
                        FS.Close (Input);

                        return 3;
                     end if;

                     Traces.Trace
                       ("Installing """
                        & From.String_Value
                        & """ to """
                        & To.Containing_Directory.String_Value
                        & """ as library interface for "
                        & String (Self.Ctxt.Name));

                     Offset := Long_Long_Integer (FS.Read (Input, Buffer));

                     Search_Loop : while Idx < Buffer'Last loop
                        --  Check end of line to retrieve the header char

                        while Buffer (Idx) in ASCII.CR | ASCII.LF loop
                           Idx := Idx + 1;

                           exit when Idx > Buffer'Last;

                           if Buffer (Idx) = 'P' then
                              --  Check if it's followed by a space or a new
                              --  line.

                              if Idx = Buffer'Last
                                or else
                                  Buffer (Idx + 1) in ' ' | ASCII.CR | ASCII.LF
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

                     FS.Close (Input);
                     FS.Close (Output);

                     if not Found then
                        Self.Tree.Reporter.Report
                          (GPR2.Message.Create
                             (GPR2.Message.Error,
                              "incorrectly formatted ali file """
                              & From.String_Value
                              & '"',
                              GPR2.Source_Reference.Create
                                (Self.Ctxt.Path_Name.Value, 0, 0)));

                        if not GNATCOLL.OS.FSUtil.Copy_File
                                 (From.String_Value, To.String_Value)
                        then
                           Self.Tree.Reporter.Report
                             (GPR2.Message.Create
                                (GPR2.Message.Error,
                                 "could not copy ali file "
                                 & String (From.Simple_Name)
                                 & " to the library directory",
                                 GPR2.Source_Reference.Object
                                   (GPR2.Source_Reference.Create
                                      (Self.Ctxt.Path_Name.Value, 0, 0))));

                           return 4;
                        end if;
                     end if;
                  end;
               end if;
            end;
         end loop;

         --  Copy the interface sources in Library_Src_Dir

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

                        pragma Unreferenced (Kind, View, Index, Sep_Name);
                     begin
                        if not GNATCOLL.OS.FSUtil.Copy_File
                                 (Path.String_Value, Dest.String_Value)
                        then
                           Self.Tree.Reporter.Report
                             (Message.Create
                                (Message.Error,
                                 "Cannot copy """
                                 & String (Path.Simple_Name)
                                 & """ to the Library_Src_Dir """
                                 & Src_Dir.String_Value
                                 & '"',
                                 Self.Ctxt.Attribute (PRA.Library_Src_Dir)
                                   .Value));
                           Has_Error := True;
                        end if;
                     end On_Unit_Part;

                     Comp : Compile.Ada.Object :=
                       Compile.Ada.Object
                         (Self.Tree.Action (Compile.Ada.Create (CU)));
                  begin
                     if Comp.Spec_Needs_Body or else not CU.Has_Part (S_Spec)
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
                        return 5;
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
                                 (Src.Path_Name.String_Value,
                                  Dest.String_Value)
                        then
                           Self.Tree.Reporter.Report
                             (Message.Create
                                (Message.Error,
                                 "Cannot copy """
                                 & String (Src.Path_Name.Simple_Name)
                                 & """ to the Library_Src_Dir """
                                 & Src_Dir.String_Value
                                 & '"',
                                 Self.Ctxt.Attribute (PRA.Library_Src_Dir)
                                   .Value));
                           return 6;
                        end if;
                     end if;
                  end;
               end loop;
            end;
         end if;
      end;

      return 0;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Object; Ctxt : GPR2.Project.View.Object)
   is
   begin
      Self.Ctxt := Ctxt;
   end Initialize;

   ---------------------
   -- Interface_Units --
   ---------------------

   function Interface_Units (Self : Object) return Compilation_Unit.Maps.Map is
      Units : Compilation_Unit.Maps.Map;
   begin
      if Self.Ctxt.Is_Library_Standalone then
         Units := Self.Ctxt.Interface_Closure;

         for CU of Self.Extended_Interface loop
            Units.Include (CU.Name, CU);
         end loop;
      else
         Units := Self.Ctxt.Own_Units;
      end if;

      return Units;
   end Interface_Units;

   ---------------------
   -- Needed_For_View --
   ---------------------

   function Needed_For_View (Ctxt : GPR2.Project.View.Object) return Boolean
   is
   begin
      if Ctxt.Is_Library_Standalone then
         return not Ctxt.Interface_Closure.Is_Empty;
      end if;

      return not Ctxt.Own_Units.Is_Empty or else
        not Ctxt.Interface_Sources.Is_Empty;
   end Needed_For_View;

   --------------------------
   -- On_Static_Completion --
   --------------------------

   overriding
   function On_Static_Completion (Self : in out Object) return Boolean is
   begin
      return Register_Artifacts (Self, Self.Interface_Units, Self.Tree.all);
   end On_Static_Completion;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      Units : constant Compilation_Unit.Maps.Map := Self.Interface_Units;
      --  This closure may not be complete for standalone libraries, and will
      --  be completed after the binder action has been executed, so all their
      --  dependencies are complete.

   begin
      return Register_Artifacts (Self, Units, Db);
   end On_Tree_Insertion;

   ------------------------
   -- Register_Artifacts --
   ------------------------

   function Register_Artifacts
     (Self  : Object;
      Units : Compilation_Unit.Maps.Map;
      Db    : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      use GPR2.Build.Actions.Process;
   begin
      --  Add ALI files as inputs/outputs
      for U of Units loop
         declare
            Comp : Compile.Ada.Object;
         begin
            Comp.Initialize (U);
            declare
               From : constant Path_Name.Object := Comp.Dependency_File.Path;
               To   : constant Path_Name.Object :=
                 Self.Ctxt.Library_Ali_Directory.Compose (From.Simple_Name);
            begin

               Db.Add_Input
                 (Self.UID, GPR2.Build.Artifacts.Files.Create (From));

               if not Db.Add_Output
                        (Self.UID, GPR2.Build.Artifacts.Files.Create (To))
               then
                  return False;
               end if;
            end;
         end;
      end loop;

      --  Add Ada source files as inputs/outputs if Library_Src_Dir is set

      if Self.Ctxt.Has_Library_Src_Directory then
         declare
            Src_Dir   : constant Path_Name.Object :=
              Self.Ctxt.Library_Src_Directory;
            Has_Error : Boolean := False;
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
                     pragma Unreferenced (Kind, View, Index, Sep_Name);
                  begin
                     Db.Add_Input
                       (Self.UID,
                        GPR2.Build.Artifacts.Source_Files.Create (Path));

                     if not Db.Add_Output
                              (Self.UID,
                               GPR2.Build.Artifacts.Source_Files.Create (Dest))
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

            --  Add non-Ada interface sources as inputs/outputs

            for C in Self.Ctxt.Interface_Sources.Iterate loop
               declare
                  Path : constant Filename_Type :=
                    GPR2.Containers.Source_Path_To_Sloc.Key (C);
                  Src  : constant GPR2.Build.Source.Object :=
                    Self.Ctxt.Visible_Source (Path);
                  Dest : Path_Name.Object;
               begin
                  if Src.Language /= Ada_Language then
                     Db.Add_Input
                       (Self.UID,
                        GPR2.Build.Artifacts.Source_Files.Create
                          (Src.Path_Name));

                     Dest := Src_Dir.Compose (Src.Path_Name.Simple_Name);

                     if not Db.Add_Output
                              (Self.UID,
                               GPR2.Build.Artifacts.Source_Files.Create (Dest))
                     then
                        return False;
                     end if;
                  end if;
               end;
            end loop;
         end;
      end if;

      return True;
   end Register_Artifacts;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Action_Id'Class is
   begin
      return Lib_Copy_Id'(Ctxt => Self.Ctxt);
   end UID;
end GPR2.Build.Actions.Thread.Lib_Copy;
