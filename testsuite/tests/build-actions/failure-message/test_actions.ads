--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Unbounded;

with GPR2.Build.Actions;
with GPR2.Build.Actions.Process;
with GPR2.Build.Command_Line;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Build.Tree_Db;

package Test_Actions is

   --  Action ID type shared by both action types

   type Test_Action_Id is new GPR2.Build.Actions.Action_Id with private;

   --  Action with default Failure_Message

   type Default_Msg_Action is
     new GPR2.Build.Actions.Process.Object with private;

   overriding function UID
     (Self : Default_Msg_Action)
      return GPR2.Build.Actions.Action_Id'Class;

   procedure Initialize
     (Self : in out Default_Msg_Action;
      Ctxt : GPR2.Project.View.Object);

   overriding function On_Tree_Insertion
     (Self : Default_Msg_Action;
      Db   : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding procedure Compute_Signature
     (Self : in out Default_Msg_Action; Check_Checksums : Boolean);

   overriding procedure Compute_Command
     (Self           : in out Default_Msg_Action;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);

   overriding function Working_Directory
     (Self : Default_Msg_Action) return GPR2.Path_Name.Object;

   overriding function Extended
     (Self : Default_Msg_Action) return Default_Msg_Action is
     (raise Constraint_Error with "Unexpected call to Extended");

   --  Action with overridden Failure_Message

   type Custom_Msg_Action is
     new GPR2.Build.Actions.Process.Object with private;

   overriding function UID
     (Self : Custom_Msg_Action)
      return GPR2.Build.Actions.Action_Id'Class;

   procedure Initialize
     (Self : in out Custom_Msg_Action;
      Ctxt : GPR2.Project.View.Object);

   overriding function On_Tree_Insertion
     (Self : Custom_Msg_Action;
      Db   : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding procedure Compute_Signature
     (Self : in out Custom_Msg_Action; Check_Checksums : Boolean);

   overriding procedure Compute_Command
     (Self           : in out Custom_Msg_Action;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);

   overriding function Working_Directory
     (Self : Custom_Msg_Action) return GPR2.Path_Name.Object;

   overriding function Failure_Message
     (Self : Custom_Msg_Action) return String;

   overriding function Extended
     (Self : Custom_Msg_Action) return Custom_Msg_Action is
     (raise Constraint_Error with "Unexpected call to Extended");

private

   type Default_Msg_Action is
     new GPR2.Build.Actions.Process.Object with null record;

   type Custom_Msg_Action is
     new GPR2.Build.Actions.Process.Object with null record;

   type Test_Action_Id is new GPR2.Build.Actions.Action_Id with record
      Ctxt         : GPR2.Project.View.Object;
      Action_Class : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding function View
     (Self : Test_Action_Id) return GPR2.Project.View.Object is
     (Self.Ctxt);

   overriding function Action_Class
     (Self : Test_Action_Id) return GPR2.Value_Type is
     (GPR2.Value_Type
        (Ada.Strings.Unbounded.To_String (Self.Action_Class)));

   overriding function Language
     (Self : Test_Action_Id) return GPR2.Language_Id is
     (GPR2.No_Language);

   overriding function Action_Parameter
     (Self : Test_Action_Id) return GPR2.Value_Type is
     (GPR2.Value_Type (Self.Ctxt.Name));

   overriding function Working_Directory
     (Self : Default_Msg_Action) return GPR2.Path_Name.Object is
     (GPR2.Path_Name.Create_Directory ("."));

   overriding function Working_Directory
     (Self : Custom_Msg_Action) return GPR2.Path_Name.Object is
     (GPR2.Path_Name.Create_Directory ("."));

end Test_Actions;
