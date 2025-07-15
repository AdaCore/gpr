--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

private with GPR2.Build.Artifacts.Object_File;
package GPR2.Build.Actions.Link.Partial is

   type Partial_Link_Id (<>) is new Actions.Link.Link_Id with private;

   type Object is new Actions.Link.Object with private;
   --  Action responsible for partially linking Ada sources

   Undefined : constant Object;

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   procedure Initialize
     (Self    : in out Object;
      Context : GPR2.Project.View.Object);

   overriding procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);

   overriding procedure Compute_Response_Files
     (Self           : in out Object;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);

   overriding function On_Tree_Insertion
     (Self : Object;
      Db   : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding procedure Add_Option (Self : in out Object; Option : String);

   overriding function Output
     (Self : Object) return Artifacts.Files.Object'Class;

   overriding function Post_Command
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : Unbounded_String := Null_Unbounded_String;
      Stderr : Unbounded_String := Null_Unbounded_String) return Boolean;

   overriding function Pre_Command (Self : in out Object) return Boolean;

private

   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;

   type Partial_Link_Id is new Actions.Link.Link_Id with null record;

   overriding function Action_Class
     (Self : Partial_Link_Id) return Value_Type is ("Partial Link");

   type Object is new Actions.Link.Object with record
      Partial_Object  : Artifacts.Object_File.Object;
      --  Object produced by the partial linker

      Is_Encapsulated : Boolean := False;
      --  Is the standalone library encapsulated
   end record;

   overriding procedure Compute_Signature
     (Self      : in out Object;
      Load_Mode : Boolean);

   overriding function Extended (Self : Object) return Object is
     (raise Internal_Error with "This action is not extending");

   Undefined : constant Object := (others => <>);

   overriding function Output
     (Self : Object) return Artifacts.Files.Object'Class is
     (Self.Partial_Object);

   overriding function Post_Command
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : Unbounded_String := Null_Unbounded_String;
      Stderr : Unbounded_String := Null_Unbounded_String) return Boolean is
     (True);

   overriding function Pre_Command
     (Self : in out Object) return Boolean is (True);

end GPR2.Build.Actions.Link.Partial;
