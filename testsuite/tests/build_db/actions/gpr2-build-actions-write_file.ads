--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;

private with GPR2.View_Ids;

package GPR2.Build.Actions.Write_File is

   package PRA renames GPR2.Project.Registry.Attribute;

   type Write_File_Id (<>) is new Actions.Action_Id with private;

   type Object is new Actions.Object with private;
   --  Action is intended to write its index to a file named <index>.txt.
   --  However, the executable responsible for creating the file is missing,
   --  as it is not needed for this particular test.

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   procedure Initialize
     (Self       : in out Object;
      Ctxt       : GPR2.Project.View.Object;
      Index      : Integer);
   --  Initialize the action with the provided index

   overriding function View (Self : Object) return GPR2.Project.View.Object;

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding procedure Compute_Signature (Self : in out Object;
                                           Stdout : Unbounded_String;
                                           Stderr : Unbounded_String);

   overriding procedure Compute_Command
     (Self : in out Object;
      Args : out GNATCOLL.OS.Process.Argument_List;
      Env  : out GNATCOLL.OS.Process.Environment_Dict;
      Slot : Positive);

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object;

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
      Ctxt       : GPR2.Project.View.Object;
      Index      : Integer;
   end record;

   overriding function View (Self : Object) return GPR2.Project.View.Object is
     (Self.Ctxt);

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object
   is (Path_Name.Create_Directory ("."));

end GPR2.Build.Actions.Write_File;
