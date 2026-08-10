--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.JSON;
with GNATCOLL.Traces;

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Artifacts.Library;
with GPR2.Containers;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Source_Reference;
with GPR2.Tree_Internal;
with GPR2.View_Internal;

package body GPR2.Build.Actions.Process.Cargo_Metadata is

   --  To better understand the code in this file, here is an example of a
   --  cargo metadata call on a library:
   --
   --     {
   --    "packages": [
   --      {
   --        "name": "hello_from_rust",
   --        "version": "0.1.0",
   --        "id": "path+file:///home/.../hello_from_rust#0.1.0",
   --        "license": null,
   --        "license_file": null,
   --        "description": null,
   --        "source": null,
   --        "dependencies": [],
   --        "targets": [
   --          {
   --            "kind": [
   --              "staticlib"
   --            ],
   --            "crate_types": [
   --              "staticlib"
   --            ],
   --            "name": "hello_from_rust",
   --            "src_path": "/home/.../hello_from_rust/src/lib.rs",
   --            "edition": "2024",
   --            "doc": true,
   --            "doctest": false,
   --            "test": true
   --          }
   --        ],
   --        "features": {},
   --        "manifest_path": "/home/.../hello_from_rust/Cargo.toml",
   --        "metadata": null,
   --        "publish": null,
   --        "authors": [],
   --        "categories": [],
   --        "keywords": [],
   --        "readme": null,
   --        "repository": null,
   --        "homepage": null,
   --        "documentation": null,
   --        "edition": "2024",
   --        "links": null,
   --        "default_run": null,
   --        "rust_version": null
   --      }
   --    ],
   --    "workspace_members": [
   --      "path+file:///home/.../hello_from_rust#0.1.0"
   --    ],
   --    "workspace_default_members": [
   --      "path+file:///home/.../hello_from_rust#0.1.0"
   --    ],
   --    "resolve": null,
   --    "target_directory": "/home/.../hello_from_rust/target",
   --    "build_directory": "/home/.../hello_from_rust/target",
   --    "version": 1,
   --    "workspace_root": "/home/.../hello_from_rust",
   --    "metadata": null
   --  }

   --  For a binary, the result is similar, except for the "targets" field:
   --
   --   "targets": [
   --       {
   --         "kind": [
   --           "bin"
   --         ],
   --         "crate_types": [
   --           "bin"
   --         ],
   --         "name": "main_from_rust",
   --         "src_path": "/home/.../src/main.rs",
   --         "edition": "2024",
   --         "doc": true,
   --         "doctest": false,
   --         "test": true
   --       }
   --     ],


   Traces : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPR.BUILD.ACTIONS.CARGO_METADATA", GNATCOLL.Traces.Off);

   Library_Kinds : constant GPR2.Containers.Value_List :=
     GPR2.Containers.Create
       ("lib rlib dylib cdylib staticlib proc-macro", " ");
   --  The target kinds Cargo uses for a library

   Binary_Kinds : constant GPR2.Containers.Value_List :=
     GPR2.Containers.Create ("bin", " ");
   --  The target kind Cargo uses for an executable

   function Binary_Names
     (Root     : GNATCOLL.JSON.JSON_Value;
      Manifest : GPR2.Path_Name.Object;
      Mains    : GPR2.Build.Compilation_Unit.Unit_Location_Vector)
      return GPR2.Containers.Filename_Set;
   --  Return the binaries names specified in the provided manifest file
   --  and that are also specified as needed by the Main attribute
   --  of View.

   procedure Cargo_Library_Name_And_Types
     (Root            : GNATCOLL.JSON.JSON_Value;
      Manifest        : GPR2.Path_Name.Object;
      Cargo_Lib_Name  : out Unbounded_String;
      Cargo_Lib_Types : out GPR2.Containers.Value_List);
   --  Obtain the library crate name and types from the provided JSON.
   --  Manifest is the path of the manifest the JSON was obtained from. It is
   --  required to select the right Cargo package in a workspace that holds
   --  several of them.
   --  Cargo_Lib_Name and Cargo_Lib_Types are empty in case of error.

   function Cargo_Package_Of
     (Root     : GNATCOLL.JSON.JSON_Value;
      Manifest : GPR2.Path_Name.Object) return GNATCOLL.JSON.JSON_Value;
   --  Return the package Root describes for Manifest, or JSON_Null when
   --  there is none. Manifest is required to select the right Cargo package
   --  in a workspace that holds several of them.

   function Cargo_Target_Of
     (Cargo_Package : GNATCOLL.JSON.JSON_Value;
      Kinds         : GPR2.Containers.Value_List)
      return GNATCOLL.JSON.JSON_Value;
   --  Return the first crate whose "targets" content matches one of the
   --  provided kinds.

   function Matches_Kind
     (Cargo_Target : GNATCOLL.JSON.JSON_Value;
      Kinds        : GPR2.Containers.Value_List) return Boolean;
   --  Whether the "kind" of Cargo_Target names one of Kinds

   ------------------
   -- Binary_Names --
   ------------------

   function Binary_Names
     (Root     : GNATCOLL.JSON.JSON_Value;
      Manifest : GPR2.Path_Name.Object;
      Mains    : GPR2.Build.Compilation_Unit.Unit_Location_Vector)
      return GPR2.Containers.Filename_Set
   is
      use GNATCOLL.JSON;

      function Wanted (Cargo_Target : JSON_Value) return Boolean;
      --  Whether Mains asks for the executable Cargo_Target builds. Every
      --  binary is wanted when it names none.

      ------------
      -- Wanted --
      ------------

      function Wanted (Cargo_Target : JSON_Value) return Boolean is
      begin
         if Mains.Is_Empty then
            return True;
         end if;

         if not Cargo_Target.Has_Field ("src_path") then
            return False;
         end if;

         declare
            Source : constant GPR2.Path_Name.Full_Name :=
              GPR2.Path_Name.Create_File
                (Filename_Type (String'(Cargo_Target.Get ("src_path"))),
                 Resolve_Links => True).Value;
         begin
            for Main of Mains loop
               if GPR2.Path_Name.Create_File
                    (Main.Source.Value, Resolve_Links => True).Value = Source
               then
                  return True;
               end if;
            end loop;
         end;

         return False;
      end Wanted;

      Result        : GPR2.Containers.Filename_Set;
      Cargo_Package : constant JSON_Value :=
        Cargo_Package_Of (Root, Manifest);
   begin
      if Cargo_Package.Kind = JSON_Null_Type
        or else not Cargo_Package.Has_Field ("targets")
      then
         return Result;
      end if;

      declare
         Cargo_Targets : constant JSON_Array :=
           Cargo_Package.Get ("targets");
      begin
         for T in 1 .. Length (Cargo_Targets) loop
            declare
               Cargo_Target : constant JSON_Value := Get (Cargo_Targets, T);
            begin
               if Matches_Kind (Cargo_Target, Binary_Kinds)
                 and then Cargo_Target.Has_Field ("name")
                 and then Wanted (Cargo_Target)
               then
                  Result.Include
                    (Filename_Type (String'(Cargo_Target.Get ("name"))));
               end if;
            end;
         end loop;
      end;

      return Result;
   end Binary_Names;

   ----------------------------------
   -- Cargo_Library_Name_And_Types --
   ----------------------------------

   procedure Cargo_Library_Name_And_Types
     (Root            : GNATCOLL.JSON.JSON_Value;
      Manifest        : GPR2.Path_Name.Object;
      Cargo_Lib_Name  : out Unbounded_String;
      Cargo_Lib_Types : out GPR2.Containers.Value_List)
   is
      use GNATCOLL.JSON;

      Cargo_Package : constant JSON_Value :=
        Cargo_Package_Of (Root, Manifest);
      Cargo_Target  : constant JSON_Value :=
        (if Cargo_Package.Kind = JSON_Null_Type
         then JSON_Null
         else Cargo_Target_Of (Cargo_Package, Library_Kinds));

   begin
      Cargo_Lib_Name := Null_Unbounded_String;
      Cargo_Lib_Types.Clear;

      if Cargo_Target.Kind = JSON_Null_Type then
         return;
      end if;

      if Cargo_Target.Has_Field ("name") then
         Cargo_Lib_Name :=
           To_Unbounded_String (String'(Cargo_Target.Get ("name")));
      end if;

      declare
         Types : constant JSON_Array :=
           (if Cargo_Target.Has_Field ("crate_types")
            then Cargo_Target.Get ("crate_types")
            else Cargo_Target.Get ("kind"));
      begin
         for J in 1 .. Length (Types) loop
            Cargo_Lib_Types.Append
              (Value_Type (String'(Get (Get (Types, J)))));
         end loop;
      end;
   end Cargo_Library_Name_And_Types;

   ----------------------
   -- Cargo_Package_Of --
   ----------------------

   function Cargo_Package_Of
     (Root     : GNATCOLL.JSON.JSON_Value;
      Manifest : GPR2.Path_Name.Object) return GNATCOLL.JSON.JSON_Value
   is
      use GNATCOLL.JSON;

      function Resolved
        (Path : Filename_Type) return GPR2.Path_Name.Full_Name
      is (GPR2.Path_Name.Create_File (Path, Resolve_Links => True).Value);

      Cargo_Packages : JSON_Array;

   begin
      if not Root.Has_Field ("packages") then
         return JSON_Null;
      end if;

      Cargo_Packages := Root.Get ("packages");

      if Length (Cargo_Packages) = 1 then
         return Get (Cargo_Packages, 1);
      end if;

      declare
         Ours : constant GPR2.Path_Name.Full_Name :=
           Resolved (Manifest.Value);
      begin
         for P in 1 .. Length (Cargo_Packages) loop
            declare
               Cargo_Package : constant JSON_Value :=
                 Get (Cargo_Packages, P);
            begin
               if Cargo_Package.Has_Field ("manifest_path")
                 and then Resolved
                            (Filename_Type
                               (String'
                                  (Cargo_Package.Get ("manifest_path"))))
                          = Ours
               then
                  return Cargo_Package;
               end if;
            end;
         end loop;
      end;

      return JSON_Null;
   end Cargo_Package_Of;

   ---------------------
   -- Cargo_Target_Of --
   ---------------------

   function Cargo_Target_Of
     (Cargo_Package : GNATCOLL.JSON.JSON_Value;
      Kinds         : GPR2.Containers.Value_List)
      return GNATCOLL.JSON.JSON_Value
   is
      use GNATCOLL.JSON;

   begin
      if not Cargo_Package.Has_Field ("targets") then
         return JSON_Null;
      end if;

      declare
         Cargo_Targets : constant JSON_Array :=
           Cargo_Package.Get ("targets");
      begin
         for T in 1 .. Length (Cargo_Targets) loop
            if Matches_Kind (Get (Cargo_Targets, T), Kinds) then
               return Get (Cargo_Targets, T);
            end if;
         end loop;
      end;

      return JSON_Null;
   end Cargo_Target_Of;

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

      Cmd_Line.Add_Argument ("metadata");
      Cmd_Line.Add_Argument ("--format-version");
      Cmd_Line.Add_Argument ("1");
      Cmd_Line.Add_Argument ("--no-deps");
      Cmd_Line.Add_Argument
        ("--manifest-path="
         & Cargo_Support.Manifest (Self.Ctxt).String_Value);
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean) is
   begin
      if not Self.Signature.Add_Input
               (Artifacts.Files.Create (Self.Cargo_Toml), Check_Checksums)
      then
         return;
      end if;
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : in out Object;
      View  : GPR2.Project.View.Object;
      Mode  : Cargo_Build.Cargo_Mode := Cargo_Build.Release;
      Mains : GPR2.Build.Compilation_Unit.Unit_Location_Vector :=
                GPR2.Build.Compilation_Unit.Empty_Vector)
   is
   begin
      Self := Undefined;
      Self.Ctxt  := View;
      Self.Mode  := Mode;
      Self.Mains := Mains;

      Self.Cargo_Toml := Cargo_Support.Manifest (View);
   end Initialize;

   ------------------
   -- Matches_Kind --
   ------------------

   function Matches_Kind
     (Cargo_Target : GNATCOLL.JSON.JSON_Value;
      Kinds        : GPR2.Containers.Value_List) return Boolean
   is
      use GNATCOLL.JSON;
   begin
      if not Cargo_Target.Has_Field ("kind") then
         return False;
      end if;

      declare
         Target_Kinds : constant JSON_Array := Cargo_Target.Get ("kind");
      begin
         for J in 1 .. Length (Target_Kinds) loop
            if Kinds.Contains
                 (Value_Type (String'(Get (Get (Target_Kinds, J)))))
            then
               return True;
            end if;
         end loop;
      end;

      return False;
   end Matches_Kind;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean is
   begin
      Db.Add_Input (Self.UID, Artifacts.Files.Create (Self.Cargo_Toml));

      return True;
   end On_Tree_Insertion;

   --------------------
   -- Post_Execution --
   --------------------

   overriding
   function Post_Execution
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : Unbounded_String := Null_Unbounded_String;
      Stderr : Unbounded_String := Null_Unbounded_String) return Boolean
   is
      pragma Unreferenced (Stderr);
      use GNATCOLL.JSON;
   begin
      pragma Assert
        (Self.Force_Execution and then Stdout /= Null_Unbounded_String);

      --  Parse with the overload that reports a parsing error rather than
      --  raising: malformed output is a failure of this action, not an
      --  exception for the scheduler to let through.

      declare
         Parsed : constant Read_Result := Read (Stdout);
      begin
         if not Parsed.Success then
            Self.Tree.Reporter.Report
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  "the output of ""cargo metadata"" is not valid JSON: "
                  & Format_Parsing_Error (Parsed.Error),
                  GPR2.Source_Reference.Create
                    (Self.Ctxt.Path_Name.Value, 0, 0)));

            return False;
         end if;

         Root := Parsed.Value;
      end;

      if Root.Kind /= JSON_Object_Type
        or else not Root.Has_Field ("target_directory")
      then
         Self.Tree.Reporter.Report
           (GPR2.Message.Create
              (GPR2.Message.Error,
               "the output of ""cargo metadata"" declares no target "
               & "directory, so the library Cargo builds cannot be located",
               GPR2.Source_Reference.Create
                 (Self.Ctxt.Path_Name.Value, 0, 0)));

         return False;
      end if;

      declare
         Target_Dir_Str : constant String := Root.Get ("target_directory");
         Cargo_Build    : Actions.Process.Cargo_Build.Object;
      begin
         Traces.Trace ("target_directory: " & Target_Dir_Str);

         if Self.Ctxt.Is_Library then
            declare
               Cargo_Lib_Name  : Unbounded_String;
               Cargo_Lib_Types : GPR2.Containers.Value_List;
            begin
               Cargo_Library_Name_And_Types
                 (Root, Self.Cargo_Toml, Cargo_Lib_Name, Cargo_Lib_Types);

               Traces.Trace
                 ("library target: "
                  & (if Cargo_Lib_Name = Null_Unbounded_String
                     then "none"
                     else To_String (Cargo_Lib_Name)));

               Cargo_Build.Initialize_Library
                 (View             => Self.Ctxt,
                  Cargo_Target_Dir =>
                    GPR2.Path_Name.Create_Directory
                      (Filename_Type (Target_Dir_Str)),
                  Cargo_Lib_Name   =>
                    Filename_Optional (To_String (Cargo_Lib_Name)),
                  Cargo_Lib_Types  => Cargo_Lib_Types,
                  Mode             => Self.Mode);
            end;

         else
            --  A standard view is expected to produce at least one executable

            declare
               Binaries : constant GPR2.Containers.Filename_Set :=
                 Binary_Names (Root, Self.Cargo_Toml, Self.Mains);
            begin
               if Binaries.Is_Empty then
                  Self.Tree.Reporter.Report
                    (GPR2.Message.Create
                       (GPR2.Message.Error,
                        (if not Self.Mains.Is_Empty
                         then "the Cargo package builds none of the mains "
                              & "this project asks for"
                         else "the Cargo package builds no binary target, "
                              & "while this is a standard project"),
                        GPR2.Source_Reference.Create
                          (Self.Ctxt.Path_Name.Value, 0, 0)));

                  return False;
               end if;

               if not Self.Mains.Is_Empty
                 and then Natural (Binaries.Length)
                          < Natural (Self.Mains.Length)
               then
                  Self.Tree.Reporter.Report
                    (GPR2.Message.Create
                       (GPR2.Message.Error,
                        "the Cargo package builds no binary for some of the "
                        & "mains this project asks for",
                        GPR2.Source_Reference.Create
                          (Self.Ctxt.Path_Name.Value, 0, 0)));

                  return False;
               end if;

               for Binary of Binaries loop
                  Traces.Trace ("binary target: " & String (Binary));
               end loop;

               Cargo_Build.Initialize_Standard
                 (View             => Self.Ctxt,
                  Cargo_Target_Dir =>
                    GPR2.Path_Name.Create_Directory
                      (Filename_Type (Target_Dir_Str)),
                  Binaries         => Binaries,
                  Mode             => Self.Mode);
            end;
         end if;

         if not Self.Tree.Add_Action (Cargo_Build) then
            return False;
         end if;

         Traces.Trace
           ("created Cargo_Build action: " & Cargo_Build.UID.Image);

         --  Carry over the library dependencies that were temporarily saved as
         --  inputs to this Cargo_Metadata action while waiting for the*
         --  Cargo_Build action to be created.

         for Input of Self.Tree.Inputs (Self.UID) loop
            if Input in Artifacts.Library.Object'Class then
               Traces.Trace
                 ("wiring " & Input.Image & " to " & Cargo_Build.UID.Image);
               Self.Tree.Add_Input (Cargo_Build.UID, Input);
            end if;
         end loop;

         --  Do the same for the outputs of Cargo_Metadata, which were
         --  temporarily saved in the Cargo_Metadata action.

         if Cargo_Build.Library_Artifact.Is_Defined then
            for Act of Self.Tree.Successors (Self.UID_Artifact) loop
               Traces.Trace
                 ("wiring Cargo_Build output to " & Act.UID.Image);
               Self.Tree.Add_Input (Act.UID, Cargo_Build.Library_Artifact);
            end loop;
         end if;
      end;

      return True;
   end Post_Execution;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class is
      Result : constant Cargo_Metadata_Id := (View => Self.Ctxt);
   begin
      return Result;
   end UID;

   -----------------------
   -- Working_Directory --
   -----------------------

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object
   is (Cargo_Support.Root_Directory (Self.Ctxt));

end GPR2.Build.Actions.Process.Cargo_Metadata;
