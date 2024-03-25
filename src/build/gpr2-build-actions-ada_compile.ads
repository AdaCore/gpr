--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Signature;
with GPR2.Containers;
with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;

private with GPR2.View_Ids;

package GPR2.Build.Actions.Ada_Compile is

   package PRA renames GPR2.Project.Registry.Attribute;

   type Ada_Compile_Id (<>) is new Actions.Action_Id with private;

   overriding function Image (Self : Ada_Compile_Id) return String;

   overriding function Db_Filename
     (Self : Ada_Compile_Id) return Simple_Name;

   overriding function "<" (L, R : Ada_Compile_Id) return Boolean;

   type Object (<>) is new Actions.Object with private;
   --  Action responsible for building Ada sources

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   overriding function Valid_Signature (Self : Object) return Boolean;

   function Create
     (Src : GPR2.Build.Compilation_Unit.Object) return Object;

   overriding function View (Self : Object) return GPR2.Project.View.Object;

   function Input_Unit (Self : Object) return Name_Type;
   function Input_Unit
     (Self : Object) return GPR2.Build.Compilation_Unit.Object;

   function Object_File (Self : Object) return GPR2.Path_Name.Object;
   function Ali_File (Self : Object) return GPR2.Path_Name.Object;

   --  function Dependencies (Self : Object) return GPR2.Containers.Name_Set;

   overriding procedure On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object;
      Messages : in out GPR2.Log.Object);

   overriding procedure Compute_Signature (Self : in out Object);

   overriding procedure Compare_Signature
     (Self     : in out Object;
      Messages : in out GPR2.Log.Object);

private

   use type GPR2.View_Ids.View_Id;

   type Ada_Compile_Id (Name_Len : Natural) is new Actions.Action_Id
     with record
      Ctxt      : GPR2.Project.View.Object;
      Unit_Name : Name_Type (1 .. Name_Len);
   end record;

   overriding function Image (Self : Ada_Compile_Id) return String is
     ("Compile Ada: " & String (Self.Unit_Name) &
        " (" & String (Self.Ctxt.Path_Name.Simple_Name) & ")");

   overriding function Db_Filename
     (Self : Ada_Compile_Id) return Simple_Name is
     (Simple_Name ("compile_ada_" & To_Lower (Self.Unit_Name) & "_"
      & To_Lower (Self.Ctxt.Name) & ".json"));

   overriding function "<" (L, R : Ada_Compile_Id) return Boolean is
     (if L.Ctxt.Id = R.Ctxt.Id then L.Unit_Name < R.Unit_Name
      else L.Ctxt.Id < R.Ctxt.Id);

   type Object (Input_Len : Natural) is new Actions.Object with record
      Ali_File  : GPR2.Path_Name.Object;
      --  Unit's ALI file. Can be undefined if not existing on disk

      Obj_File  : GPR2.Path_Name.Object;
      --  Compiled object file, can be undefined if not compiled yet

      Deps      : GPR2.Containers.Name_Set;
      --  List of known dependencies for this unit

      Signature : GPR2.Build.Signature.Object;

      UID       : Ada_Compile_Id (Input_Len);
   end record;

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
     (Self.UID);

   overriding function View (Self : Object) return GPR2.Project.View.Object is
     (Self.UID.Ctxt);

   function Input_Unit (Self : Object) return Name_Type is
     (Self.UID.Unit_Name);

   function Input_Unit
     (Self : Object) return GPR2.Build.Compilation_Unit.Object
   is (Self.UID.Ctxt.Own_Unit (Self.UID.Unit_Name));

   function Object_File (Self : Object) return GPR2.Path_Name.Object is
     (Self.Obj_File);

   function Ali_File (Self : Object) return GPR2.Path_Name.Object is
     (Self.Ali_File);

   overriding function Valid_Signature (Self : Object) return Boolean is
     (Self.Signature.Valid);
end GPR2.Build.Actions.Ada_Compile;
