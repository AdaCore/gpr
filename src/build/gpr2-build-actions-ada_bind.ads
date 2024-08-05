--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Signature;
with GPR2.Path_Name; use GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;

with Ada.Containers.Hashed_Sets;

with GNATCOLL.OS.Process;
private with GPR2.View_Ids;

package GPR2.Build.Actions.Ada_Bind is

   package PRA renames GPR2.Project.Registry.Attribute;

   type Ada_Bind_Id (<>) is new Actions.Action_Id with private;

   overriding function Image (Self : Ada_Bind_Id) return String;

   overriding function Db_Filename
     (Self : Ada_Bind_Id) return Simple_Name;

   overriding function "<" (L, R : Ada_Bind_Id) return Boolean;

   type Object is new Actions.Object with private;

   Undefined : constant Object;

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   overriding function Valid_Signature (Self : Object) return Boolean;

   procedure Initialize
     (Self     : in out Object;
      Main_Ali : GPR2.Path_Name.Object;
      Context  : GPR2.Project.View.Object);

   overriding function View (Self : Object) return GPR2.Project.View.Object;

   package Path_Name_Sets is
     new Ada.Containers.Hashed_Sets
       (GPR2.Path_Name.Object, Hash => GPR2.Path_Name.Hash,
        Equivalent_Elements => GPR2.Path_Name."=");

   function Input_Alis (Self : Object) return Path_Name_Sets.Set;
   --  Return the ALI files that the bind action depends on

   function Output_Unit (Self : Object)
     return GPR2.Build.Compilation_Unit.Object;
   --  Return the compilation unit generated by the bind action

   procedure Parse_Ali (Self : in out Object; Ali : GPR2.Path_Name.Object);
   --  Retrieve imports from the provided ALI file. For each import, create a
   --  Pre_Bind action and add the object file as an input to the linker
   --  linked to Self.

   overriding procedure On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object;
      Messages : in out GPR2.Log.Object);

   overriding procedure Compute_Signature (Self : in out Object);

   overriding procedure Compute_Command
     (Self : Object;
      Args : out GNATCOLL.OS.Process.Argument_List;
      Env  : out GNATCOLL.OS.Process.Environment_Dict);

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object;

   overriding procedure Post_Command (Self : in out Object);
   --  Parse the object file and the options listed as a comment at the end of
   --  the main output source, then add them as switches to the related
   --  linking action.

private

   use type GPR2.View_Ids.View_Id;

   type Ada_Bind_Id (Name_Len : Natural) is new Actions.Action_Id
     with record
      Ctxt      : GPR2.Project.View.Object;
      Ali_Name  : Name_Type (1 .. Name_Len);
   end record;

   overriding function Image (Self : Ada_Bind_Id) return String is
     ("Bind Ada: " & String (Self.Ali_Name) &
        " (" & String (Self.Ctxt.Path_Name.Simple_Name) & ")");

   overriding function Db_Filename
     (Self : Ada_Bind_Id) return Simple_Name is
     (Simple_Name ("bind_ada_" & To_Lower (Self.Ali_Name) & "_"
      & To_Lower (Self.Ctxt.Name) & ".json"));

   overriding function "<" (L, R : Ada_Bind_Id) return Boolean is
     (if L.Ctxt.Id = R.Ctxt.Id then L.Ali_Name < R.Ali_Name
      else L.Ctxt.Id < R.Ctxt.Id);

   type Object is new Actions.Object with record
      Main_Ali  : GPR2.Path_Name.Object := GPR2.Path_Name.Undefined;
      --  ALI file given as argument to the binder

      Ali_Files : Path_Name_Sets.Set := Path_Name_Sets.Empty_Set;
      --  All the ALI files that the binder depends on

      Unit      : GPR2.Build.Compilation_Unit.Object :=
                    GPR2.Build.Compilation_Unit.Undefined;
      --  Generated compilation unit

      Ctxt      : GPR2.Project.View.Object;
      --  View referenced by the generated compilation unit
   end record;

   Undefined : constant Object := (others => <>);

   overriding function View (Self : Object) return GPR2.Project.View.Object is
     (Self.Ctxt);

   function Input_Alis (Self : Object) return Path_Name_Sets.Set is
     (Self.Ali_Files);

   function Output_Unit (Self : Object)
     return GPR2.Build.Compilation_Unit.Object is
     (Self.Unit);

   overriding function Valid_Signature (Self : Object) return Boolean is
     (Self.Signature.Valid);

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object is
     (Self.Ctxt.Object_Directory);


end GPR2.Build.Actions.Ada_Bind;
