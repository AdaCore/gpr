--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Artifacts.Files;
with GPR2.Project.Registry.Attribute;

package GPR2.Build.Actions.Post_Bind is

   package PRA renames GPR2.Project.Registry.Attribute;

   type Post_Bind_Id (<>) is new Action_Id with private;

   type Object is new Actions.Object with private;
   --  Action responsible for building Ada sources generated by gnatbind

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;

   function Create
     (Impl   : Artifacts.Files.Object;
      View   : GPR2.Project.View.Object;
      Binder : GPR2.Build.Actions.Ada_Bind.Object;
      Skip   : Boolean) return Object;

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding procedure Compute_Command
     (Self     : in out Object;
      Slot     : Positive;
      Cmd_Line : in out GPR2.Build.Command_Line.Object);

   function Object_File (Self : Object) return Artifacts.Files.Object;

private

   type Post_Bind_Id (Name_Len : Natural) is new Action_Id with record
      Input : Filename_Type (1 .. Name_Len);
      View  : Project.View.Object;
   end record;

   overriding function Valid_Signature (Self : Object) return Boolean is
     (Self.Skip or else GPR2.Build.Actions.Object (Self).Valid_Signature);

   overriding function View (Self : Post_Bind_Id) return Project.View.Object is
     (Self.View);

   overriding function Action_Class (Self : Post_Bind_Id) return Value_Type is
     ("Post-Bind");

   overriding function Language (Self : Post_Bind_Id) return Language_Id is
     (Ada_Language);

   overriding function Action_Parameter (Self : Post_Bind_Id) return Value_Type
   is (Value_Type (Self.Input));

   type Object is new Actions.Object with record
      Input  : Artifacts.Files.Object;
      Output : Artifacts.Files.Object;
      Ali    : Artifacts.Files.Object;
      Binder : Ada_Bind.Object;
      --  ??? Ideally we store Ada_Bind_Id here, but it's unconstrained so
      --  we store the object (so that post-bind object is unconstrained) but
      --  need to access it via Tree_Db.Actions (Binder.UID) to make sure the
      --  information is up-to-date
      View   : GPR2.Project.View.Object;
      Skip   : Boolean := False;
   end record;

   overriding function UID (Self : Object) return Action_Id'Class;

   overriding function Post_Command
     (Self   : in out Object;
      Status : Execution_Status) return Boolean;

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object
   is (Self.View.Object_Directory);

   overriding function View (Self : Object) return GPR2.Project.View.Object is
      (Self.View);

   overriding procedure Compute_Signature
     (Self      : in out Object;
      Load_Mode : Boolean);

   overriding function Extended (Self : Object) return Object is
     (raise Internal_Error with "This action is not extending");

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Object_File (Self : Object) return Artifacts.Files.Object is
     (Self.Output);

end GPR2.Build.Actions.Post_Bind;
