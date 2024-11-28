--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

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

   function Is_Defined (Self : Object) return Boolean;
   function In_Build_Tree (Self : Object) return Boolean;

   procedure Initialize
     (Self : in out Object; Src : GPR2.Build.Compilation_Unit.Object);
   --  Initialize all object fields according to Src

   overriding function View (Self : Object) return GPR2.Project.View.Object;

   function Input_Unit
     (Self : Object) return GPR2.Build.Compilation_Unit.Object;
   --  Return the name of the compiled unit

   function Ali_File (Self : Object) return Artifacts.Files.Object;
   --  Return the path of the generated ALI file

   function Dependencies
     (Self     : in out Object;
      With_RTS : Boolean := True) return GPR2.Containers.Filename_Set
     with Pre => Self.In_Build_Tree;
   --  Return the list of known dependencies for this unit. The action ALI file
   --  must be up-to-date before calling this function, as the list of
   --  dependencies comes from it.

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding function On_Tree_Propagation
     (Self : in out Object) return Boolean;

   overriding function Post_Command
     (Self   : in out Object;
      Status : Execution_Status) return Boolean;

private

   use type GPR2.Path_Name.Object;

   function Idx_Image (Idx : Unit_Index) return String is
     (Idx'Image (2 .. Idx'Image'Last));

   type Ada_Compile_Id is new Compile_Id with record
      Index : Unit_Index;
   end record;

   overriding function Action_Parameter
     (Self : Ada_Compile_Id) return Value_Type
   is (String (Self.Src_Name) &
       (if Self.Index /= No_Index then "@" & Idx_Image (Self.Index) else ""));

   function Create
     (Src : GPR2.Build.Compilation_Unit.Object) return Ada_Compile_Id
   is (Compile_Id
         (Compile.Create
            (Main_Src => Src.Main_Part.Source.Simple_Name,
             Lang     => Ada_Language,
             View     => Src.Owning_View))
       with Index => Src.Main_Part.Index);

   type Object is new Compile.Object with record
      Ali_File  : Artifacts.Files.Object;
      --  Unit's ALI file. Can be undefined if not existing on disk

      Closure   : Action_Id_Sets.Set;
      --  List of all object files that are needed to have Self's symbols
      --  fully resolved.

      CU        : GPR2.Build.Compilation_Unit.Object;
      --  The Unit to build
   end record;

   overriding function Src_Index (Self : Object) return Unit_Index is
     (Self.CU.Main_Part.Index);

   overriding function Dependency_File (Self : Object) return Simple_Name is
      (Self.CU.Dependency_File);

   overriding procedure Compute_Signature
     (Self   : in out Object;
      Stdout : Unbounded_String;
      Stderr : Unbounded_String);

   Undefined : constant Object := (others => <>);

   overriding function View (Self : Object) return GPR2.Project.View.Object is
     (Self.CU.Owning_View);

   function Input_Unit
     (Self : Object) return GPR2.Build.Compilation_Unit.Object
   is (Self.CU);

   function Ali_File (Self : Object) return Artifacts.Files.Object is
     (Self.Ali_File);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function In_Build_Tree (Self : Object) return Boolean is
     (Self.Is_Defined and then Self.Tree /= null);

end GPR2.Build.Actions.Compile.Ada;
