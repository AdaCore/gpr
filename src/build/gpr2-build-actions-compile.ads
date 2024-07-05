--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Signature;
with GPR2.Build.Source;
with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;

private with GPR2.Containers;
private with GPR2.View_Ids;

package GPR2.Build.Actions.Compile is

   package PRA renames GPR2.Project.Registry.Attribute;

   type Compile_Id (<>) is new Actions.Action_Id with private;

   overriding function Image (Self : Compile_Id) return String;

   overriding function Db_Filename (Self : Compile_Id) return Simple_Name;

   overriding function "<" (L, R : Compile_Id) return Boolean;

   type Object is new Actions.Object with private;
   --  Action responsible for building Ada sources

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   overriding function Valid_Signature (Self : Object) return Boolean;

   procedure Initialize (Self : in out Object; Src : GPR2.Build.Source.Object);

   overriding function View (Self : Object) return GPR2.Project.View.Object;

   function Input (Self : Object) return GPR2.Build.Source.Object;

   function Object_File (Self : Object) return GPR2.Path_Name.Object;

   overriding procedure On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object;
      Messages : in out GPR2.Log.Object);

   overriding procedure Compute_Signature (Self : in out Object);

   overriding function Command (Self : Object)
    return GNATCOLL.OS.Process.Argument_List;

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

   overriding function Db_Filename (Self : Compile_Id) return Simple_Name is
     (Simple_Name ("compile_" & To_Lower (Self.Src_Name)
      & "_" & To_Lower (Self.Ctxt.Name)
      & ".json"));

   overriding function "<" (L, R : Compile_Id) return Boolean is
     (if L.Ctxt.Id = R.Ctxt.Id then L.Src_Name < R.Src_Name
      else L.Ctxt.Id < R.Ctxt.Id);

   type Object is new Actions.Object with record
      Obj_File : GPR2.Path_Name.Object;
      --  Compiled object file, can be undefined if not compiled yet

      Deps     : GPR2.Containers.Name_Set;
      --  List of known dependencies for this unit

      Lang     : GPR2.Language_Id;
      --  Language of the source

      Src_Name : Unbounded_String;
      --  Source name

      Ctxt     : GPR2.Project.View.Object;
      --  View owning the source
   end record;

   overriding function Valid_Signature (Self : Object) return Boolean is
     (Self.Signature.Valid);

   overriding function View (Self : Object) return GPR2.Project.View.Object is
     (Self.Ctxt);

   function Input (Self : Object) return GPR2.Build.Source.Object is
     (Self.Ctxt.Source (Simple_Name (To_String (Self.Src_Name))));

   function Object_File (Self : Object) return GPR2.Path_Name.Object is
     (Self.Obj_File);

end GPR2.Build.Actions.Compile;
