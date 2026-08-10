--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions.Process.Cargo_Support;
with GPR2.Build.Artifacts.Files;
with GPR2.Containers;
with GPR2.Environment;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Source_Reference;
with GPR2.Tree_Internal;
with GPR2.View_Internal;

package body GPR2.Build.Actions.Process.Cargo_Build is

   procedure Initialize_Common
     (Self : in out Object;
      View : GPR2.Project.View.Object;
      Mode : Cargo_Mode);
   --  The part of the initialization that is common to every kind of view

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding
   procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean)
   is
      pragma Unreferenced (Slot, Signature_Only);
      package PRA renames GPR2.Project.Registry.Attribute;
      package PAI renames GPR2.Project.Attribute_Index;

      Driver_Attr : constant GPR2.Project.Attribute.Object :=
        Self.Ctxt.Attribute (PRA.Compiler.Driver, PAI.Create (Rust_Language));
      Tree_Int    : constant access GPR2.Tree_Internal.Object :=
        GPR2.View_Internal.Get_RO (Self.Ctxt).Tree;
   begin
      if Tree_Int.Languages_To_Compilers.Contains (Rust_Language) then
         Cmd_Line.Set_Driver
           (Tree_Int.Languages_To_Compilers.Element (Rust_Language));
      elsif Driver_Attr.Is_Defined then
         Cmd_Line.Set_Driver (Driver_Attr.Value.Text);
      else
         Self.Tree.Reporter.Report
           (GPR2.Message.Create
              (GPR2.Message.Error,
               "no compiler driver defined for language Rust, "
               & "set Compiler.Driver (""Rust"") to the path of cargo",
               GPR2.Source_Reference.Create
                 (Self.Ctxt.Path_Name.Value, 0, 0)));
         return;
      end if;

      Cmd_Line.Add_Argument ("build");

      --  Name the binaries to build. If Binaries is empty, all the cargo
      --  binaries are built.

      for Binary of Self.Binaries loop
         Cmd_Line.Add_Argument ("--bin");
         Cmd_Line.Add_Argument (String (Binary));
      end loop;

      Cmd_Line.Add_Argument ("--target=" & To_String (Self.Rust_Triple));

      if Self.Mode = Release then
         Cmd_Line.Add_Argument ("--release");
      end if;

      Cmd_Line.Add_Argument
        ("--manifest-path="
         & Cargo_Support.Manifest (Self.Ctxt).String_Value);

      --  Cargo prints log messages to stderr, so -q disable these logs, but
      --  errors are still displayed.

      Cmd_Line.Add_Argument ("-q");

      --  Pass library search paths and names for GPR2 library dependencies
      --  to the Rust linker.
      --
      --  CARGO_ENCODED_RUSTFLAGS rather than RUSTFLAGS: Cargo splits the
      --  latter on whitespace and offers no quoting, so a library directory
      --  holding a space would be torn into two flags. The encoded form is
      --  separated by 0x1F, which no path holds, and takes precedence over
      --  RUSTFLAGS when both are set.
      declare
         Separator : constant Character := ASCII.US;

         Env : constant GPR2.Environment.Object := Tree_Int.Environment;

         --  The flags the user or the CI already set. Cargo first checks
         --  CARGO_ENCODED_RUSTFLAGS if specified, and RUSTFLAGS otherwise.

         Encoded : constant String :=
           Env.Value ("CARGO_ENCODED_RUSTFLAGS", "");
         Plain   : constant String := Env.Value ("RUSTFLAGS", "");

         Run_Path_Opt : constant GPR2.Project.Attribute.Object :=
           Self.Ctxt.Attribute (PRA.Run_Path_Option);
         --  List of switches to be used when specifying the run path option.

         Flags : GPR2.Containers.Value_List;
         --  The options generated for the library dependencies, one flag per
         --  element so that none of them can be split further
      begin
         for Input of Self.Tree.Inputs (Object'Class (Self).UID) loop
            if Input in Artifacts.Library.Object'Class then
               declare
                  Lib : constant Artifacts.Library.Object :=
                    Artifacts.Library.Object (Input);
                  Dir : constant String :=
                    Lib.Path.Containing_Directory.String_Value;
               begin
                  if Lib.Is_Static then
                     Flags.Append ("-C");
                     Flags.Append
                       (Value_Type ("link-arg=" & Lib.Path.String_Value));
                  else
                     pragma Assert
                       (Lib.Link_Name /= "",
                        "The shared library artifact carries no link name: "
                        & Lib.Path.String_Value);

                     Flags.Append (Value_Type ("-L" & Dir));
                     Flags.Append (Value_Type ("-l" & String (Lib.Link_Name)));

                     if Run_Path_Opt.Is_Defined then
                        declare
                           Values : constant
                             GPR2.Containers.Source_Value_List :=
                               Run_Path_Opt.Values;
                        begin
                           --  The directory goes on the last option

                           for J in Values.First_Index .. Values.Last_Index
                           loop
                              Flags.Append ("-C");
                              Flags.Append
                                (Value_Type
                                   ("link-arg="
                                    & Values (J).Text
                                    & (if J = Values.Last_Index
                                       then Dir
                                       else "")));
                           end loop;
                        end;
                     end if;
                  end if;
               end;
            end if;
         end loop;

         if not Flags.Is_Empty then
            declare
               Result : Unbounded_String;

               procedure Append_To_Result (Flag : Value_Type);

               ----------------------
               -- Append_To_Result --
               ----------------------

               procedure Append_To_Result (Flag : Value_Type) is
               begin
                  if Result /= Null_Unbounded_String then
                     Append (Result, Separator);
                  end if;

                  Append (Result, String (Flag));
               end Append_To_Result;

            begin
               --  Always rely on the CARGO_ENCODED_RUSTFLAGS, but make sure
               --  to fetch the content of RUSTFLAGS if CARGO_ENCODED_RUSTFLAGS
               --  was not initially defined.

               if Encoded /= "" then
                  for Flag of GPR2.Containers.Create
                                (Value_Type (Encoded), (1 => Separator))
                  loop
                     Append_To_Result (Flag);
                  end loop;

               elsif Plain /= "" then
                  --  Cargo would have split RUSTFLAGS on whitespace itself,
                  --  so reproducing that split loses nothing.

                  for Flag of GPR2.Containers.Create
                                (Value_Type (Plain), " " & ASCII.HT)
                  loop
                     Append_To_Result (Flag);
                  end loop;
               end if;

               for Flag of Flags loop
                  Append_To_Result (Flag);
               end loop;

               Cmd_Line.Add_Env_Variable
                 ("CARGO_ENCODED_RUSTFLAGS", To_String (Result));
            end;
         end if;
      end;
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean) is
   begin
      if Self.Cargo_Toml.Is_Defined
        and then
          not Self.Signature.Add_Input
                (Artifacts.Files.Create (Self.Cargo_Toml), Check_Checksums)
      then
         return;
      end if;

      --  The libraries this build links against. Compute_Command hands them
      --  to Cargo through CARGO_ENCODED_RUSTFLAGS, so a change to one of
      --  them changes what this action produces and belongs in its
      --  signature. Only the manifest was recorded before, which left a
      --  changed Ada or Rust library at an unchanged path invisible here.

      for Input of Self.Tree.Inputs (Object'Class (Self).UID) loop
         if Input in Artifacts.Library.Object'Class
           and then not Self.Signature.Add_Input (Input, Check_Checksums)
         then
            return;
         end if;
      end loop;

      for Executable of Self.Executables loop
         if not Self.Signature.Add_Output
                  (Artifacts.Files.Create (Executable), Check_Checksums)
         then
            return;
         end if;
      end loop;

      if Self.Library.Is_Defined
        and then not Self.Signature.Add_Output (Self.Library, Check_Checksums)
      then
         return;
      end if;
   end Compute_Signature;

   -----------------------
   -- Initialize_Common --
   -----------------------

   procedure Initialize_Common
     (Self : in out Object;
      View : GPR2.Project.View.Object;
      Mode : Cargo_Mode)
   is
      Triple : constant String := Cargo_Support.Rust_Triple (View);
   begin
      Self := Undefined;
      Self.Ctxt := View;
      Self.Mode := Mode;

      Self.Cargo_Toml := Cargo_Support.Manifest (View);

      if Triple /= "" then
         Self.Rust_Triple := To_Unbounded_String (Triple);
      end if;
   end Initialize_Common;

   ------------------------
   -- Initialize_Library --
   ------------------------

   procedure Initialize_Library
     (Self             : in out Object;
      View             : GPR2.Project.View.Object;
      Cargo_Target_Dir : GPR2.Path_Name.Object;
      Cargo_Lib_Name   : Filename_Optional;
      Cargo_Lib_Types  : GPR2.Containers.Value_List;
      Mode             : Cargo_Mode := Release)
   is
      use type Cargo_Support.Cargo_Lib_Kind;

      Mode_Name : constant Filename_Type :=
        (case Mode is
           when Release => "release",
           when Debug   => "debug");
   begin
      Initialize_Common (Self, View, Mode);

      if Self.Rust_Triple = Null_Unbounded_String then
         --  The action is invalid and rejected by On_Tree_Insertion, so
         --  there is no artifact to name.

         return;
      end if;

      declare
         Triple        : constant String := To_String (Self.Rust_Triple);
         Declared_Kind : constant Cargo_Support.Cargo_Lib_Kind :=
           Cargo_Support.To_Cargo_Lib_Kind (Cargo_Lib_Types);
         Expected_Kind : constant Cargo_Support.Cargo_Lib_Kind :=
           (if View.Is_Static_Library
            then Cargo_Support.Static_Library
            else Cargo_Support.Shared_Library);
         Output_Dir    : constant GPR2.Path_Name.Object :=
           Cargo_Target_Dir
             .Compose (Filename_Type (Triple), Directory => True)
             .Compose (Mode_Name, Directory => True);
      begin
         if Cargo_Lib_Name = "" then
            Self.Manifest_Error :=
              To_Unbounded_String
                (Self.Cargo_Toml.String_Value & " has no library target");

         elsif Declared_Kind = Cargo_Support.Ambiguous then
            Self.Manifest_Error :=
              To_Unbounded_String
                (Self.Cargo_Toml.String_Value & " declares both a static and "
                 & "a shared library. Keep the one that matches Library_Kind");

         elsif Declared_Kind = Cargo_Support.Unsupported then
            Self.Manifest_Error :=
              To_Unbounded_String
                ("need cdylib or staticlib crate type in "
                 & Self.Cargo_Toml.String_Value);

         elsif Declared_Kind /= Expected_Kind then
            Self.Manifest_Error :=
              To_Unbounded_String
                ("Library_Kind does not match with "
                 & Self.Cargo_Toml.String_Value);

         elsif Declared_Kind = Cargo_Support.Shared_Library
           and then View.Has_Library_Version
         then
            Self.Manifest_Error :=
              To_Unbounded_String
                ("Can not use Library_Version with Rust based project");

         elsif Cargo_Lib_Name /= View.Library_Name then
            Self.Manifest_Error :=
              To_Unbounded_String
                ("Library_Name does not match with "
                 & Self.Cargo_Toml.String_Value);

         else
            --  All coherence checks between the cargo manifest and the GPR
            --  project file have been done. We can safely create the library.

            declare
               Lib_Path : constant GPR2.Path_Name.Object :=
                 Output_Dir.Compose
                   (Cargo_Support.Library_File_Name (View, Declared_Kind));
            begin
               if Declared_Kind = Cargo_Support.Static_Library then
                  Self.Library :=
                    GPR2.Build.Artifacts.Library.Create_Static (Lib_Path);
               else
                  Self.Library :=
                    GPR2.Build.Artifacts.Library.Create_Shared
                      (Lib_Path, Link_Name => Value_Type (View.Library_Name));
               end if;
            end;
         end if;
      end;
   end Initialize_Library;

   -------------------------
   -- Initialize_Standard --
   -------------------------

   procedure Initialize_Standard
     (Self             : in out Object;
      View             : GPR2.Project.View.Object;
      Cargo_Target_Dir : GPR2.Path_Name.Object;
      Binaries         : GPR2.Containers.Filename_Set;
      Mode             : Cargo_Mode := Release)
   is
      Mode_Name : constant Filename_Type :=
        (case Mode is
           when Release => "release",
           when Debug   => "debug");
   begin
      Initialize_Common (Self, View, Mode);
      Self.Binaries := Binaries;

      if Self.Rust_Triple = Null_Unbounded_String then
         --  The action is invalid and rejected by On_Tree_Insertion, so
         --  there is nothing to name.

         return;
      end if;

      declare
         Output_Dir : constant GPR2.Path_Name.Object :=
           Cargo_Target_Dir
             .Compose
                (Filename_Type (To_String (Self.Rust_Triple)),
                 Directory => True)
             .Compose (Mode_Name, Directory => True);
         Suffix     : constant Filename_Optional := View.Executable_Suffix;
      begin
         for Binary of Binaries loop
            Self.Executables.Append
              (Output_Dir.Compose (Binary & Suffix));
         end loop;
      end;
   end Initialize_Standard;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      GPR_Target : constant GPR2.Name_Type := Self.View.Tree.Target;
   begin
      if Self.Rust_Triple = Null_Unbounded_String then
         Db.Reporter.Report
           (GPR2.Message.Create
              (GPR2.Message.Error,
               "no Rust target triple known for GPR target """
               & String (GPR_Target)
               & """, set package Cargo attribute Rust_Target "
               & "to the appropriate Rust triple",
               GPR2.Source_Reference.Create
                 (Self.Ctxt.Path_Name.Value, 0, 0)));
         return False;
      end if;

      if not Cargo_Support.Is_Compatible
               (GPR_Target, To_String (Self.Rust_Triple))
      then
         Db.Reporter.Report
           (GPR2.Message.Create
              (GPR2.Message.Error,
               "Rust target triple """
               & To_String (Self.Rust_Triple)
               & """ is not compatible with GPR target """
               & String (GPR_Target)
               & """",
               GPR2.Source_Reference.Create
                 (Self.Ctxt.Path_Name.Value, 0, 0)));
         return False;
      end if;

      if Self.Manifest_Error /= Null_Unbounded_String then
         Db.Reporter.Report
           (GPR2.Message.Create
              (GPR2.Message.Error,
               To_String (Self.Manifest_Error),
               GPR2.Source_Reference.Create
                 (Self.Ctxt.Path_Name.Value, 0, 0)));
         return False;
      end if;

      Db.Add_Input (Self.UID, Artifacts.Files.Create (Self.Cargo_Toml));

      for Executable of Self.Executables loop
         if not Db.Add_Output
                  (Self.UID, Artifacts.Files.Create (Executable))
         then
            return False;
         end if;
      end loop;

      if not Self.Library.Is_Defined then
         return True;
      end if;

      return Db.Add_Output (Self.UID, Self.Library);
   end On_Tree_Insertion;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class is
      Result : constant Cargo_Build_Id := (View => Self.Ctxt);
   begin
      return Result;
   end UID;

   -----------------------
   -- Working_Directory --
   -----------------------

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object
   is (Cargo_Support.Root_Directory (Self.Ctxt));

end GPR2.Build.Actions.Process.Cargo_Build;
