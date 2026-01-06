--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.Library;
with GPR2.Build.Artifacts.Object_File;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;

--  Extract an object file from an archive in the current directory.

package GPR2.Build.Actions.Archive_Extract is

   type Archive_Extract_Id (<>) is new Actions.Action_Id with private;

   function Create
     (Archive           : Simple_Name;
      Object_To_Extract : Simple_Name;
      View              : GPR2.Project.View.Object) return Archive_Extract_Id;

   type Object is new Actions.Object with private;

   Undefined : constant Object;

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class;

   function Is_Defined (Self : Object) return Boolean;

   procedure Initialize
     (Self             : in out Object;
      Archive          : GPR2.Build.Artifacts.Library.Object;
      Extracted_Object : Simple_Name;
      View             : GPR2.Project.View.Object);
   --  Initialize the action.
   --  @param Archive Archive containing the object file to extract
   --  @param Extracted_Object Name of the object to extract
   --  @param View View containing the archive

   function Extracted_Object
     (Self : Object) return GPR2.Build.Artifacts.Object_File.Object;
   --  Return the extracted object artifact.
   --  @param Self Action containing the extracted object
   --  @return The extracted object file artifact

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding
   procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);

private

   type Archive_Extract_Id
     (Archive_Name_Len     : Natural;
      Extr_Object_Name_Len : Natural)
   is new Actions.Action_Id with record
      View              : GPR2.Project.View.Object;
      Archive           : Filename_Type (1 .. Archive_Name_Len);
      Object_To_Extract : Filename_Type (1 .. Extr_Object_Name_Len);
   end record;

   overriding
   function View (Self : Archive_Extract_Id) return Project.View.Object
   is (Self.View);

   overriding
   function Action_Class (Self : Archive_Extract_Id) return Value_Type
   is ("Archive-Extract");

   overriding
   function Language (Self : Archive_Extract_Id) return Language_Id
   is (No_Language);

   overriding
   function Action_Parameter (Self : Archive_Extract_Id) return Value_Type
   is (Value_Type (Self.Archive));

   function Create
     (Archive           : Simple_Name;
      Object_To_Extract : Simple_Name;
      View              : GPR2.Project.View.Object) return Archive_Extract_Id
   is (Archive'Length,
       Object_To_Extract'Length,
       View,
       Archive,
       Object_To_Extract);

   type Object is new Actions.Object with record
      Archive : GPR2.Build.Artifacts.Library.Object;
      --  The archive containing the object to extract

      Extracted_Object : GPR2.Build.Artifacts.Object_File.Object;
      --  The extracted object file
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

   function Extracted_Object
     (Self : Object) return GPR2.Build.Artifacts.Object_File.Object
   is (Self.Extracted_Object);

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean
   is (Self /= Undefined);
end GPR2.Build.Actions.Archive_Extract;
