--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Hashed_Sets;

with GPR2.Build.ALI_Parser;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Compilation_Unit;
with GPR2.Containers;
with GPR2.Path_Name;

package GPR2.Build.Actions.Process.Compile.Ada is

   package BCU renames GPR2.Build.Compilation_Unit;

   type Ada_Compile_Id is new Actions.Process.Compile.Compile_Id with private;

   function Create
     (Src : BCU.Object;
      Loc : BCU.Unit_Location) return Ada_Compile_Id;
   --  Create an Action_Id without having to create the full action object

   function Create
     (Src : BCU.Object) return Ada_Compile_Id;
   --  Create an Action_Id without having to create the full action object

   type Object is new Compile.Object with private;
   --  Action responsible for building Ada sources

   Undefined : constant Object;

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   overriding function Is_Defined (Self : Object) return Boolean;

   procedure Initialize
     (Self     : in out Object;
      Src      : BCU.Object;
      Kind     : Unit_Kind := S_Body;
      Sep_Name : Optional_Name_Type := No_Name);
   --  Initialize all object fields according to Src and Loc

   function Unit
     (Self : Object) return BCU.Object;
   --  Return the compilation unit contained in the source file

   function Intf_Ali_File (Self : Object) return Artifacts.Files.Object;
   --  Return the path of the generated ALI file. If the corresponding view
   --  is a library, then the ali file from the library directory is returned.

   procedure Change_Intf_Ali_File
     (Self : in out Object;
      Path : Path_Name.Object);
   --  Point Intf_Ali_File at Path, the copy in the library directory. Called
   --  by the action that performs the copy, which owns that artifact: the
   --  tree is left untouched here.

   function Local_Ali_File (Self : Object) return Artifacts.Files.Object;
   --  Return the path of the generated ALI file. The one located in the
   --  object directory is always returned here.

   overriding procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);

   overriding function On_Tree_Insertion
     (Self : Object;
      Db   : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding
   function On_Tree_Propagation (Self : in out Object) return Boolean;

   overriding function Pre_Execution (Self : in out Object) return Boolean;
   --  Removes any ".prep" file left over from a previous compilation,
   --  for each part (spec, body, separates) of the compiled unit, so that
   --  a ".prep" found after this compilation reliably reflects it.

   overriding function Post_Execution
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : Unbounded_String := Null_Unbounded_String;
      Stderr : Unbounded_String := Null_Unbounded_String) return Boolean;

   --  Accessors to the parsed ALI. ALI_Parser.Object holds several
   --  containers, so each returns only what the caller needs rather than
   --  the object as a whole.

   function ALI_Is_Parsed (Self : Object) return Boolean
   with Inline;

   function ALI_Path_Name (Self : Object) return GPR2.Path_Name.Object
   with Inline;

   function ALI_Has_Imports (Self : in out Object) return Boolean;
   --  Whether the ALI declares any imported unit. Parses the ALI if needed
   --  and returns False if it cannot be parsed.

   function ALI_Withed_From_Spec
     (Self : Object) return GPR2.Containers.Name_Set
   with Inline, Pre => Self.ALI_Is_Parsed;

   function ALI_Withed_From_Body
     (Self : Object) return GPR2.Containers.Name_Set
   with Inline, Pre => Self.ALI_Is_Parsed;

   function ALI_Dependencies
     (Self : Object) return GPR2.Containers.Filename_Set
   with Inline, Pre => Self.ALI_Is_Parsed;

   function ALI_Linker_Options
     (Self : Object) return GPR2.Containers.Value_List
   with Inline, Pre => Self.ALI_Is_Parsed;

   function ALI_Spec_Needs_Body (Self : Object) return Boolean
   with Inline, Pre => Self.ALI_Is_Parsed;

   function Parse_Ali (Self : in out Object) return Boolean;
   --  Parse the ALI file and store the result in the ALI_Parser object.
   --  Returns True if parsing succeeded, False otherwise.

   overriding function Dependencies
     (Self : in out Object) return GPR2.Containers.Filename_Set;
   --  Fetch dependencies from a .ali dependency file with an ALI parser

   overriding function Extended (Self : Object) return Object;

   function Withed_Units
     (Self      : in out Object;
      All_Units : Boolean := True) return Containers.Name_Set;
   --  Provides the units that are referenced on the 'W' line of the ALI files
   --  if All_Units is positionned then it is simply the union of the withed
   --  unit from both the spec and body part of the compilation unit.
   --  Otherwise only the units withed by the main part of the compilation unit
   --  are provided.

   function Withed_Units_From_Spec
     (Self : in out Object) return Containers.Name_Set;
   --  Provides the units that are referenced on the 'W' line of the ALI files
   --  only for the spec part of the compilation unit.

   function Withed_Units_From_Body
     (Self : in out Object) return Containers.Name_Set;
   --  Provides the units that are referenced on the 'W' line of the ALI files
   --  only for the body part of the compilation unit.

   function Spec_Needs_Body (Self : in out Object) return Boolean;
   --  Returns whether or not the associated ALI files mentions importing the
   --  spec of the unit also necessitate the body.

private

   use type GPR2.Path_Name.Object;

   function Idx_Image (Idx : Unit_Index) return String is
     (Idx'Image (2 .. Idx'Image'Last));

   type Ada_Compile_Id is new Compile_Id with record
      Index : Unit_Index;
      CU    : BCU.Object;
      UL    : BCU.Unit_Location;
   end record;

   overriding function Action_Parameter
     (Self : Ada_Compile_Id) return Value_Type;

   package File_Sets is new Standard.Ada.Containers.Hashed_Sets
     (Artifacts.Files.Object, Artifacts.Files.Hash,
      Artifacts.Files."=", Artifacts.Files."=");

   type Object is new Compile.Object with record
      Lib_Ali_File          : Artifacts.Files.Object;
      --  Unit's ALI file. This variant is located in the Library_ALI_Dir in
      --  case the view is a library, else it is identical to the dependency
      --  file.

      ALI_Object            : GPR2.Build.ALI_Parser.Object;
      --  The parsed information about the ALI file

      In_Library            : GPR2.Project.View.Object;
      --  The library, if any, that will contain the result of the compilation

      CU                    : BCU.Object;
      --  The Unit to build

      UL                    : BCU.Unit_Location;
      --  The exact Unit location of the unit to build

      Local_Config_Pragmas  : Path_Name.Object;
      --  The local config file as specified by the view's
      --  Local_Configuration_Pragmas attribute

      Global_Config_Pragmas : Path_Name.Object;
      --  The global configuration pragma file specified by the root project
      --  Global_Configuration_Pragmas attribute
   end record;

   overriding function Src_Index (Self : Object) return Unit_Index is
     (Self.UL.Index);

   overriding procedure Compute_Signature
     (Self            : in out Object;
      Check_Checksums : Boolean);

   overriding function On_Static_Completion
     (Self : in out Object) return Boolean;

   Undefined : constant Object := (others => <>);

   function Unit
     (Self : Object) return BCU.Object
   is (Self.CU);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Intf_Ali_File (Self : Object) return Artifacts.Files.Object is
     (Self.Lib_Ali_File);

   function Local_Ali_File (Self : Object) return Artifacts.Files.Object is
     (Self.Dep_File);

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
     (Create (Src => Self.CU, Loc => Self.UL));

   function ALI_Is_Parsed (Self : Object) return Boolean
   is (Self.ALI_Object.Is_Parsed);

   function ALI_Path_Name (Self : Object) return GPR2.Path_Name.Object
   is (Self.ALI_Object.Path_Name);

   function ALI_Withed_From_Spec
     (Self : Object) return GPR2.Containers.Name_Set
   is (Self.ALI_Object.Withed_From_Spec);

   function ALI_Withed_From_Body
     (Self : Object) return GPR2.Containers.Name_Set
   is (Self.ALI_Object.Withed_From_Body);

   function ALI_Dependencies
     (Self : Object) return GPR2.Containers.Filename_Set
   is (Self.ALI_Object.Dependencies);

   function ALI_Linker_Options
     (Self : Object) return GPR2.Containers.Value_List
   is (Self.ALI_Object.Linker_Options);

   function ALI_Spec_Needs_Body (Self : Object) return Boolean
   is (Self.ALI_Object.Spec_Needs_Body);

   function Parse_Ali (Self : in out Object) return Boolean is
     (Self.ALI_Object.Parse);

end GPR2.Build.Actions.Process.Compile.Ada;
