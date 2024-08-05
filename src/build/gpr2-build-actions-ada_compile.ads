--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Signature;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Registry.Attribute;
with GNATCOLL.OS.Process;

private with GPR2.View_Ids;

package GPR2.Build.Actions.Ada_Compile is

   package PRA renames GPR2.Project.Registry.Attribute;

   type Ada_Compile_Id (<>) is new Actions.Action_Id with private;

   overriding function Image (Self : Ada_Compile_Id) return String;

   overriding function Db_Filename
     (Self : Ada_Compile_Id) return Simple_Name;

   overriding function "<" (L, R : Ada_Compile_Id) return Boolean;

   type Object is new Actions.Object with private;
   --  Action responsible for building Ada sources

   Undefined : constant Object;

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   overriding function Valid_Signature (Self : Object) return Boolean;

   function Is_Defined (Self : Object) return Boolean;

   procedure Initialize
     (Self : in out Object; Src : GPR2.Build.Compilation_Unit.Object);
   --  Initialize all object fields according to Src

   overriding function View (Self : Object) return GPR2.Project.View.Object;

   function Input_Unit
     (Self : Object) return GPR2.Build.Compilation_Unit.Object;
   --  Return the name of the compiled unit

   function Object_File (Self : Object) return GPR2.Path_Name.Object;
   --  Return the path of the generated object file

   function Ali_File (Self : Object) return GPR2.Path_Name.Object;
   --  Return the path of the generated ALI file

   function Dependencies
     (Self : in out Object) return GPR2.Path_Name.Set.Object;
   --  Return the list of known dependencies for this unit. The action ALI file
   --  must be up-to-date before calling this function, as the list of
   --  dependencies comes from it.

   overriding procedure On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object;
      Messages : in out GPR2.Log.Object);

   overriding procedure Compute_Signature (Self : in out Object);

   overriding procedure Compute_Command
     (Self : Object;
      Args : out GNATCOLL.OS.Process.Argument_List;
      Env  : out GNATCOLL.OS.Process.Environment_Dict);

   overriding procedure Post_Command (Self : in out Object);

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object;

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
       (Simple_Name
         ("compile_ada_" & To_Lower (Self.Unit_Name) & "_"
          & To_Lower (Self.Ctxt.Name) & ".json"));

   overriding function "<" (L, R : Ada_Compile_Id) return Boolean is
     (if L.Ctxt.Id = R.Ctxt.Id then L.Unit_Name < R.Unit_Name
      else L.Ctxt.Id < R.Ctxt.Id);

   type Object is new Actions.Object with record
      Ali_File  : GPR2.Path_Name.Object;
      --  Unit's ALI file. Can be undefined if not existing on disk

      Obj_File  : GPR2.Path_Name.Object;
      --  Compiled object file, can be undefined if not existing on disk

      Deps      : GPR2.Path_Name.Set.Object := GPR2.Path_Name.Set.Empty_Set;
      --  List of known dependencies for this unit. It is obtained from
      --  the parsing of the ALI file generated by the action. Remains empty
      --  before the action post command

      Ctxt      : GPR2.Project.View.Object;
      --  View containing the compilation unit

      Unit_Name : Unbounded_String;
      --  Compilation unit name
   end record;

   Undefined : constant Object := (others => <>);

   overriding function View (Self : Object) return GPR2.Project.View.Object is
     (Self.Ctxt);

   function Input_Unit
     (Self : Object) return GPR2.Build.Compilation_Unit.Object
   is (Self.Ctxt.Own_Unit (Name_Type (To_String (Self.Unit_Name))));

   function Object_File (Self : Object) return GPR2.Path_Name.Object is
     (Self.Obj_File);

   function Ali_File (Self : Object) return GPR2.Path_Name.Object is
     (Self.Ali_File);

   overriding function Valid_Signature (Self : Object) return Boolean is
     (Self.Signature.Valid);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object is
     (Self.View.Object_Directory);

end GPR2.Build.Actions.Ada_Compile;
