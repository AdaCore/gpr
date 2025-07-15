--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.Object_File;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;

--  Extract linker options from the object file and add them to the dependent
--  linkers (the successors in the dependency graph for this action).

package GPR2.Build.Actions.Link_Options_Extract is

   type Link_Options_Extract_Id (<>) is new Actions.Action_Id with private;

   function Create
     (Object_File : Simple_Name; View : GPR2.Project.View.Object)
      return Link_Options_Extract_Id;

   type Object is new Actions.Object with private;

   Undefined : constant Object;

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class;

   function Is_Defined (Self : Object) return Boolean;

   procedure Initialize
     (Self        : in out Object;
      Object_File : Simple_Name;
      View        : GPR2.Project.View.Object);
   --  Initialize the action.
   --  @param Self The action to initialize
   --  @param Object_File The object file containing the linker options
   --                      to be extracted.
   --  @param View The view that contains the action

   overriding
   function View (Self : Object) return GPR2.Project.View.Object;

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding
   procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);

   overriding procedure Compute_Response_Files
     (Self           : in out Object;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);

   overriding function Post_Command
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : Unbounded_String := Null_Unbounded_String;
      Stderr : Unbounded_String := Null_Unbounded_String) return Boolean;

   overriding
   function Display_Output (Action : Object) return Boolean;

private

   type Link_Options_Extract_Id (Name_Len : Natural) is new Actions.Action_Id
   with record
      View        : GPR2.Project.View.Object;
      Object_File : Filename_Type (1 .. Name_Len);
   end record;

   overriding
   function View (Self : Link_Options_Extract_Id) return Project.View.Object
   is (Self.View);

   overriding
   function Action_Class (Self : Link_Options_Extract_Id) return Value_Type
   is ("Link-Options-Extract");

   overriding
   function Language (Self : Link_Options_Extract_Id) return Language_Id
   is (No_Language);

   overriding
   function Action_Parameter (Self : Link_Options_Extract_Id) return Value_Type
   is (Value_Type (Self.Object_File));

   function Create
     (Object_File : Simple_Name; View : GPR2.Project.View.Object)
      return Link_Options_Extract_Id
   is (Object_File'Length, View, Object_File);

   type Object is new Actions.Object with record
      Object_File : GPR2.Build.Artifacts.Object_File.Object;
      --  The object file containing the link option to be extracted

      Ctxt : GPR2.Project.View.Object;
   end record;

   overriding
   procedure Compute_Signature (Self : in out Object; Load_Mode : Boolean);

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object
   is (Self.Ctxt.Object_Directory);

   overriding
   function Extended (Self : Object) return Object
   is (raise Internal_Error with "This action is not extending");

   overriding
   function View (Self : Object) return GPR2.Project.View.Object
   is (Self.Ctxt);

   overriding
   function Display_Output (Action : Object) return Boolean is (False);

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean
   is (Self /= Undefined);

end GPR2.Build.Actions.Link_Options_Extract;
