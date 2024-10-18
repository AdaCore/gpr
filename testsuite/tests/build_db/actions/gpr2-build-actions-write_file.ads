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

   overriding function Image (Self : Write_File_Id) return String;

   overriding function Db_Filename (Self : Write_File_Id) return Simple_Name;

   overriding function "<" (L, R : Write_File_Id) return Boolean;

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
      Env  : out GNATCOLL.OS.Process.Environment_Dict);

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object;

private

   use type GPR2.View_Ids.View_Id;
   use all type Ada.Strings.Trim_End;

   type Write_File_Id is new Actions.Action_Id with record
      Ctxt  : GPR2.Project.View.Object;
      Index : Integer;
   end record;

   overriding function Image (Self : Write_File_Id) return String is
     ("Write_File '" & Ada.Strings.Fixed.Trim (Self.Index'Img, Both) & "' (" &
      String (Self.Ctxt.Path_Name.Simple_Name) & ")");

   overriding function Db_Filename (Self : Write_File_Id) return Simple_Name is
     (Simple_Name ("Write_File" & Ada.Strings.Fixed.Trim (Self.Index'Img, Both) &
      "_" & To_Lower (Self.Ctxt.Name) & ".json"));

   overriding function "<" (L, R : Write_File_Id) return Boolean is
     (if L.Ctxt.Id = R.Ctxt.Id then L.Index < R.Index
      else L.Ctxt.Id < R.Ctxt.Id);

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
