--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions;
with GPR2.Build.Command_Line;
with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.View;
with GPR2.Build.Tree_Db;

package External_Action is

   package PRA renames GPR2.Project.Registry.Attribute;

   type External_Action_Id is new GPR2.Build.Actions.Action_Id with private;

   type Object is new GPR2.Build.Actions.Object with private;

   overriding function UID (Self : Object) return GPR2.Build.Actions.Action_Id'Class;

   procedure Initialize
     (Self       : in out Object;
      Ctxt       : GPR2.Project.View.Object);

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding procedure Compute_Signature
     (Self      : in out Object;
      Check_Checksums : Boolean);

   overriding procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);

   overriding function Working_Directory
     (Self : Object) return GPR2.Path_Name.Object;

   overriding function Extended (Self : Object) return Object is
      (raise Constraint_Error with "Unexpected call to Extended");

private

   type Object is new GPR2.Build.Actions.Object with null record;

   type External_Action_Id is new GPR2.Build.Actions.Action_Id with record
      Ctxt  : GPR2.Project.View.Object;
   end record;

   overriding function View (Self : External_Action_Id) return GPR2.Project.View.Object
   is (Self.Ctxt);

   overriding function Action_Class (Self : External_Action_Id) return GPR2.Value_Type is
     ("External Action");

   overriding function Language (Self : External_Action_Id) return GPR2.Language_Id is
     (GPR2.No_Language);

   overriding function Action_Parameter
     (Self : External_Action_Id) return GPR2.Value_Type is
     (GPR2.Value_Type (Self.Ctxt.Name));

   overriding function Working_Directory
     (Self : Object) return GPR2.Path_Name.Object
   is (GPR2.Path_Name.Create_Directory ("."));

end External_Action;
