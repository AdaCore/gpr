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
private with Ada.Strings;
private with Ada.Strings.Fixed;

package GPR2.Build.Actions.Write_File is

   package PRA renames GPR2.Project.Registry.Attribute;

   type Write_File_Id (<>) is new Actions.Action_Id with private;

   type Object is new Actions.Object with private;
   --  Action that writes its index in a file named <index>.txt thanks
   --  to the executable contained in the "write_file" directory.

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   procedure Initialize
     (Self       : in out Object;
      Ctxt       : GPR2.Project.View.Object;
      Index      : Integer;
      Executable : GPR2.Path_Name.Object;
      Ret_Code   : Integer := 0;
      With_Deps  : Boolean := True;
      With_Wait  : Natural := 0);
   --  Initialize the action with several testing parameters to pass to
   --  the executable. The executable used by test.py is mostly the one
   --  coming from the "directory" write_file.

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
     (Self : Object) return Path_Name.Object;

   overriding function Extended (Self : Object) return Object is
      (raise Constraint_Error with "Unexpected call to Extended");

private

   use type GPR2.View_Ids.View_Id;
   use all type Ada.Strings.Trim_End;

   type Write_File_Id is new Actions.Action_Id with record
      Ctxt  : GPR2.Project.View.Object;
      Index : Integer;
   end record;

   overriding function View (Self : Write_File_Id) return Project.View.Object
   is (Self.Ctxt);

   overriding function Action_Class (Self : Write_File_Id) return Value_Type is
     ("Write File");

   overriding function Language (Self : Write_File_Id) return Language_Id is
     (No_Language);

   overriding function Action_Parameter
     (Self : Write_File_Id) return Value_Type is
     (Self.Index'Image (2 .. Self.Index'Image'Last));

   type Object is new Actions.Object with record
      Index      : Integer;
      Ret_Code   : Integer;
      With_Deps  : Boolean;
      With_Wait  : Natural;
      Executable : GPR2.Path_Name.Object;
   end record;

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object
   is (Path_Name.Create_Directory ("."));

end GPR2.Build.Actions.Write_File;
