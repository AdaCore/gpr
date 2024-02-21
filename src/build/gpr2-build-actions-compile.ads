--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Source;
with GPR2.Path_Name;

private with GPR2.Containers;
private with GPR2.View_Ids;

package GPR2.Build.Actions.Compile is

   type Compile_Id (<>) is new Actions.Action_Id with private;

   overriding function Image (Self : Compile_Id) return String;

   overriding function "<" (L, R : Compile_Id) return Boolean;

   type Object (<>) is new Actions.Object with private;
   --  Action responsible for building Ada sources

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   function Create (Src  : GPR2.Build.Source.Object) return Object;

   overriding function View (Self : Object) return GPR2.Project.View.Object;

   function Input (Self : Object) return Simple_Name;
   function Input (Self : Object) return GPR2.Build.Source.Object;

   function Object_File (Self : Object) return GPR2.Path_Name.Object;

   overriding procedure On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object;
      Messages : in out GPR2.Log.Object);

private

   use type GPR2.View_Ids.View_Id;

   type Compile_Id (Name_Len : Natural) is new Actions.Action_Id with record
      Lang     : Language_Id;
      Ctxt     : GPR2.Project.View.Object;
      Src_Name : Simple_Name (1 .. Name_Len);
   end record;

   overriding function Image (Self : Compile_Id) return String is
     ("Compile " & Image (Self.Lang) & ": " & String (Self.Src_Name) &
        " (" & String (Self.Ctxt.Path_Name.Simple_Name) & ")");

   overriding function "<" (L, R : Compile_Id) return Boolean is
     (if L.Ctxt.Id = R.Ctxt.Id then L.Src_Name < R.Src_Name
      else L.Ctxt.Id < R.Ctxt.Id);

   type Object (Input_Len : Natural) is new Actions.Object with record
      Obj_File : GPR2.Path_Name.Object;
      --  Compiled object file, can be undefined if not compiled yet

      Deps     : GPR2.Containers.Name_Set;
      --  List of known dependencies for this unit

      --  ??? Add the action's signature

      UID      : Compile_Id (Input_Len);
   end record;

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
     (Self.UID);

   overriding function View (Self : Object) return GPR2.Project.View.Object is
     (Self.UID.Ctxt);

   function Input (Self : Object) return Simple_Name is
     (Self.UID.Src_Name);

   function Input (Self : Object) return GPR2.Build.Source.Object is
     (Self.UID.Ctxt.Source (Self.UID.Src_Name));

   function Object_File (Self : Object) return GPR2.Path_Name.Object is
     (Self.Obj_File);

end GPR2.Build.Actions.Compile;
