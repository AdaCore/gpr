--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.Library;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;

--  Outputs the list of objects contained in the provided archive to the
--  standard output.

package GPR2.Build.Actions.Archive_Table_List is

   type Archive_Table_List_Id (<>) is new Actions.Action_Id with private;

   function Create
     (Archive : Simple_Name; View : GPR2.Project.View.Object)
      return Archive_Table_List_Id;

   type Object is new Actions.Object with private;

   Undefined : constant Object;

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class;

   function Is_Defined (Self : Object) return Boolean;

   --  Initializes the action to add link options to the build process.
   procedure Initialize
     (Self    : in out Object;
      Archive : GPR2.Build.Artifacts.Library.Object;
      View    : GPR2.Project.View.Object);

   overriding
   function View (Self : Object) return GPR2.Project.View.Object;

   --  Handles the insertion of the action into the build tree database.
   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding
   procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);

   overriding
   function Post_Command
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : Unbounded_String := Null_Unbounded_String;
      Stderr : Unbounded_String := Null_Unbounded_String) return Boolean;

   overriding
   function Display_Output (Action : Object) return Boolean;
   --
private

   type Archive_Table_List_Id (Name_Len : Natural) is new Actions.Action_Id
   with record
      View    : GPR2.Project.View.Object;
      Archive : Filename_Type (1 .. Name_Len);
   end record;

   overriding
   function View (Self : Archive_Table_List_Id) return Project.View.Object
   is (Self.View);

   overriding
   function Action_Class (Self : Archive_Table_List_Id) return Value_Type
   is ("Archive-Table-List");

   overriding
   function Language (Self : Archive_Table_List_Id) return Language_Id
   is (No_Language);

   overriding
   function Action_Parameter (Self : Archive_Table_List_Id) return Value_Type
   is (Value_Type (Self.Archive));

   function Create
     (Archive : Simple_Name; View : GPR2.Project.View.Object)
      return Archive_Table_List_Id
   is (Archive'Length, View, Archive);

   type Object is new Actions.Object with record
      Archive : GPR2.Build.Artifacts.Library.Object;
      --  The archive file to which the new section, holding the provided
      --  arguments, will be added.

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
   function Display_Output (Action : Object) return Boolean
   is (False);

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean
   is (Self /= Undefined);

end GPR2.Build.Actions.Archive_Table_List;
