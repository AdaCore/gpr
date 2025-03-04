--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Source;
with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Attribute_Index;

package GPR2.Build.Actions.Compile is

   type Compile_Id (<>) is new Actions.Action_Id with private;

   function Create
     (Main_Src : Simple_Name;
      Lang     : Language_Id;
      View     : GPR2.Project.View.Object) return Compile_Id'Class;

   type Object is new Actions.Object with private;
   --  Action responsible for building Ada sources

   Undefined : constant Object;

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   function Is_Defined (Self : Object) return Boolean;
   function In_Build_Tree (Self : Object) return Boolean;

   procedure Initialize (Self : in out Object; Src : GPR2.Build.Source.Object);

   overriding function View (Self : Object) return GPR2.Project.View.Object;

   function Language (Self : Object) return Language_Id;

   function Input (Self : Object) return GPR2.Build.Source.Object;

   function Object_File (Self : Object) return Artifacts.Files.Object;

   function Dependency_File
     (Self : Object'Class) return Artifacts.Files.Object;

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding procedure Compute_Signature
     (Self      : in out Object;
      Load_Mode : Boolean);

   overriding procedure Compute_Command
     (Self     : in out Object;
      Slot     : Positive;
      Cmd_Line : in out GPR2.Build.Command_Line.Object);

   function Parse_Dependencies
     (Self : Object'Class) return GPR2.Containers.Filename_Set
     with Pre => Self.In_Build_Tree;
   --  Return the list of known dependencies for this unit. The action
   --  dependency file must be up-to-date before calling this function, as the
   --  list of dependencies comes from it.

   function Dependencies
     (Self : Object) return GPR2.Containers.Filename_Set;
   --  Fetch dependencies from a .d dependency file with a makefile parser

   overriding function Is_Deactivated (Self : Object) return Boolean;

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object;

   overriding function Is_Extending (Self : Object) return Boolean;

   overriding function Extended (Self : Object) return Object;

private

   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;

   type Compile_Id (Name_Len : Natural) is new Actions.Action_Id with record
      Lang     : Language_Id;
      Ctxt     : GPR2.Project.View.Object;
      Src_Name : Simple_Name (1 .. Name_Len);
   end record;

   overriding function View (Self : Compile_Id) return Project.View.Object is
     (Self.Ctxt);

   overriding function Action_Class (Self : Compile_Id) return Value_Type is
     ("Compile");

   overriding function Language (Self : Compile_Id) return Language_Id is
     (Self.Lang);

   overriding function Action_Parameter (Self : Compile_Id) return Value_Type
   is (Value_Type (Self.Src_Name));

   function Create
     (Main_Src : Simple_Name;
      Lang     : Language_Id;
      View     : GPR2.Project.View.Object) return Compile_Id'Class
   is (Compile_Id'(Name_Len => Main_Src'Length,
                   Lang     => Lang,
                   Ctxt     => View,
                   Src_Name => Main_Src));

   type Object is new Actions.Object with record
      Obj_File : Artifacts.Files.Object;
      --  Compiled object file, can be undefined if not compiled yet

      Dep_File : Artifacts.Files.Object;
      --  Dependency information generated by the compiler

      Lang     : GPR2.Language_Id;
      --  Language of the source

      Src      : GPR2.Build.Source.Object;
      --  Source name

      Ctxt     : GPR2.Project.View.Object;
      --  View owning the source
   end record;

   function Src_Index (Self : Object) return Unit_Index is
     (No_Index);
   --  Need that for indexed sources, for now only Ada multi-unit sources

   Undefined : constant Object := (others => <>);

   function Dependency_File
     (Self : Object'Class) return Artifacts.Files.Object is (Self.Dep_File);

   overriding function View (Self : Object) return GPR2.Project.View.Object is
     (Self.Ctxt);

   function Language (Self : Object) return Language_Id is
     (Self.Lang);

   function Input (Self : Object) return GPR2.Build.Source.Object is
     (Self.Src);

   overriding function Is_Extending (Self : Object) return Boolean is
     (Self.Input.Is_Inherited);

   function Object_File (Self : Object) return Artifacts.Files.Object is
     (Self.Obj_File);

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object is
     (Self.Ctxt.Object_Directory);

   overriding function Is_Deactivated (Self : Object) return Boolean is
     (Actions.Object (Self).Is_Deactivated
      or else Self.View.Attribute
        (PRA.Compiler.Driver, PAI.Create (Self.Lang)).Value.Text'Length = 0);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function In_Build_Tree (Self : Object) return Boolean is
     (Self.Is_Defined and then Self.Tree /= null);

end GPR2.Build.Actions.Compile;
