--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.Object_File;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;

--  Add a section to an object file with the provided link options.

package GPR2.Build.Actions.Link_Options_Insert is

   type Link_Options_Insert_Id (<>) is new Actions.Action_Id with private;

   function Create
     (Object_File : Simple_Name; View : GPR2.Project.View.Object)
      return Link_Options_Insert_Id;

   type Object is new Actions.Object with private;

   Undefined : constant Object;

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class;

   function Is_Defined (Self : Object) return Boolean;

   procedure Initialize
     (Self        : in out Object;
      Object_File : Artifacts.Object_File.Object;
      Options     : Containers.Value_List := Containers.Empty_Value_List;
      View        : GPR2.Project.View.Object);
   --  Initialize the link option add action.
   --  @param Self The action to initialize
   --  @param Object_File The input object file that will be used as template
   --     the generate the object with linker options added.
   --  @param Options The options to add to the object file
   --  @param View The view that contains the action

   overriding
   function View (Self : Object) return GPR2.Project.View.Object;

   procedure Add_Option (Self : in out Object; Option : String);
   --  Add an option to add to the object file section
   --  @param Self The action to modify
   --  @param Option The option to add to the new linker options section

   function Output_Object_File
     (Self : Object) return Artifacts.Object_File.Object;
   --  Return the object file in which the new section, containing the
   --  provided arguments, is added.
   --  @param Self The action containing the output object file
   --  @return The object file in which the new section is added

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding
   procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);

   overriding function Post_Command
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : Unbounded_String := Null_Unbounded_String;
      Stderr : Unbounded_String := Null_Unbounded_String) return Boolean;

private

   type Link_Options_Insert_Id (Name_Len : Natural) is new Actions.Action_Id
   with record
      View        : GPR2.Project.View.Object;
      Object_File : Filename_Type (1 .. Name_Len);
   end record;

   overriding
   function View (Self : Link_Options_Insert_Id) return Project.View.Object
   is (Self.View);

   overriding
   function Action_Class (Self : Link_Options_Insert_Id) return Value_Type
   is ("Link-Options-Insert");

   overriding
   function Language (Self : Link_Options_Insert_Id) return Language_Id
   is (No_Language);

   overriding
   function Action_Parameter (Self : Link_Options_Insert_Id) return Value_Type
   is (Value_Type (Self.Object_File));

   function Create
     (Object_File : Simple_Name; View : GPR2.Project.View.Object)
      return Link_Options_Insert_Id
   is (Object_File'Length, View, Object_File);

   type Object is new Actions.Object with record
      Input_Object_File : Artifacts.Object_File.Object :=
        Artifacts.Object_File.Undefined;
      --  The object file that will be used as a template for objcopy.
      --  The result will then contain the new section with the provided linker
      --  options but will not contain the other sections of the input file.

      Output_Object_File : Artifacts.Object_File.Object :=
        Artifacts.Object_File.Undefined;
      --  Contains the linker section.

      Ctxt : GPR2.Project.View.Object;
      --  The view defining the Main, or the library

      Options : Containers.Value_List := Containers.Empty_Value_List;
      --  Command line options added manually with the Add_Option procedure
      --  and provided during the initialization of the action.
   end record;

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean);

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object
   is (Self.Ctxt.Object_Directory);

   overriding
   function Extended (Self : Object) return Object
   is (raise Internal_Error with "This action is not extending");

   overriding
   function View (Self : Object) return GPR2.Project.View.Object
   is (Self.Ctxt);

   function Output_Object_File
     (Self : Object) return Artifacts.Object_File.Object
   is (Self.Output_Object_File);

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean
   is (Self /= Undefined);

end GPR2.Build.Actions.Link_Options_Insert;
