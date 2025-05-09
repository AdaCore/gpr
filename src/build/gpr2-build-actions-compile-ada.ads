--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Hashed_Sets;

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Compilation_Unit;
with GPR2.Containers;
with GPR2.Path_Name;

package GPR2.Build.Actions.Compile.Ada is

   type Ada_Compile_Id is new Actions.Compile.Compile_Id with private;

   function Create
     (Src : GPR2.Build.Compilation_Unit.Object) return Ada_Compile_Id;
   --  Create an Action_Id without having to create the full action object

   type Object is new Compile.Object with private;
   --  Action responsible for building Ada sources

   Undefined : constant Object;

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   overriding function Is_Defined (Self : Object) return Boolean;

   procedure Initialize
     (Self : in out Object; Src : GPR2.Build.Compilation_Unit.Object);
   --  Initialize all object fields according to Src

   function Input_Unit
     (Self : Object) return GPR2.Build.Compilation_Unit.Object;
   --  Return the name of the compiled unit

   function Intf_Ali_File (Self : Object) return Artifacts.Files.Object;
   --  Return the path of the generated ALI file. If the corresponding view
   --  is a library, then the ali file from the library directory is returned.

   procedure Change_Intf_Ali_File
     (Self : in out Object;
      Path : Path_Name.Object);
   --  Ensures that after copying the ALI file to the library directory all
   --  references are updated.

   function Local_Ali_File (Self : Object) return Artifacts.Files.Object;
   --  Return the path of the generated ALI file. The one located in the
   --  object directory is always returned here.

   overriding procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding function Post_Command
     (Self   : in out Object;
      Status : Execution_Status) return Boolean;

   overriding function Dependencies
     (Self : Object) return GPR2.Containers.Filename_Set;
   --  Fetch dependencies from a .ali dependency file with an ALI parser

   function Withed_Units
     (Self : Object) return GPR2.Containers.Name_Set
     with Pre => Self.In_Build_Tree;
   --  Return the list of withed units. If the ALI file is present then the
   --  list is retrieved from it, else it is retrieved via the
   --  Compilation_Unit's Ada parser.

   overriding function Extended (Self : Object) return Object;

private

   use type GPR2.Path_Name.Object;

   function Idx_Image (Idx : Unit_Index) return String is
     (Idx'Image (2 .. Idx'Image'Last));

   type Ada_Compile_Id is new Compile_Id with record
      Index : Unit_Index;
      CU    : GPR2.Build.Compilation_Unit.Object;
   end record;

   overriding function Action_Parameter
     (Self : Ada_Compile_Id) return Value_Type;

   function Create
     (Src : GPR2.Build.Compilation_Unit.Object) return Ada_Compile_Id
   is (Compile_Id
         (Compile.Create
            (Main_Src => Src.Main_Part.Source.Simple_Name,
             Lang     => Ada_Language,
             View     => Src.Owning_View))
       with Index => Src.Main_Part.Index,
            CU    => Src);

   package File_Sets is new Standard.Ada.Containers.Hashed_Sets
     (Artifacts.Files.Object, Artifacts.Files.Hash,
      Artifacts.Files."=", Artifacts.Files."=");

   type Object is new Compile.Object with record
      Lib_Ali_File          : Artifacts.Files.Object;
      --  Unit's ALI file. This variant is located in the Library_ALI_Dir in
      --  case the view is a library, else it is identical to the dependency
      --  file.

      In_Library            : GPR2.Project.View.Object;
      --  The library, if any, that will contain the result of the compilation

      CU                    : GPR2.Build.Compilation_Unit.Object;
      --  The Unit to build

      Local_Config_Pragmas  : Path_Name.Object;
      --  The local config file as specified by the view's
      --  Local_Configuration_Pragmas attribute

      Global_Config_Pragmas : Path_Name.Object;
      --  The global configuration pragma file specified by the root project
      --  Global_Configuration_Pragmas attribute
   end record;

   overriding function Src_Index (Self : Object) return Unit_Index is
     (Self.CU.Main_Part.Index);

   overriding procedure Compute_Signature
     (Self      : in out Object;
      Load_Mode : Boolean);

   Undefined : constant Object := (others => <>);

   function Input_Unit
     (Self : Object) return GPR2.Build.Compilation_Unit.Object
   is (Self.CU);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Intf_Ali_File (Self : Object) return Artifacts.Files.Object is
     (Self.Lib_Ali_File);

   function Local_Ali_File (Self : Object) return Artifacts.Files.Object is
     (Self.Dep_File);

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      (Create (Src => Self.CU));

end GPR2.Build.Actions.Compile.Ada;
