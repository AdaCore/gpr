--  Copyright (C) 2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception

with Ada.Strings.Unbounded;
with GPR2.Build.Actions;
with GPR2.Build.Actions.Thread;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;
with GPR2.Project.View;

package File_Action is
   type Object is new GPR2.Build.Actions.Thread.Object with private;

   Input  : constant GPR2.Build.Artifacts.Files.Object :=
     GPR2.Build.Artifacts.Files.Create ("input.txt");
   Copy   : constant GPR2.Build.Artifacts.Files.Object :=
     GPR2.Build.Artifacts.Files.Create ("copy.txt");
   Marker : constant GPR2.Build.Artifacts.Files.Object :=
     GPR2.Build.Artifacts.Files.Create ("marker.txt");

   procedure Initialize
     (Self : in out Object; View : GPR2.Project.View.Object);

   overriding function UID
     (Self : Object) return GPR2.Build.Actions.Action_Id'Class;

   overriding function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean);

   overriding function Execute
     (Self   : in out Object;
      Stdout : in out Ada.Strings.Unbounded.Unbounded_String;
      Stderr : in out Ada.Strings.Unbounded.Unbounded_String) return Integer;

   overriding function Working_Directory
     (Self : Object) return GPR2.Path_Name.Object;

   overriding function Extended (Self : Object) return Object is
     (raise Constraint_Error with "Unexpected call to Extended");

private
   type File_Action_Id is new GPR2.Build.Actions.Action_Id with record
      Ctxt : GPR2.Project.View.Object;
   end record;

   overriding function View
     (Self : File_Action_Id) return GPR2.Project.View.Object is (Self.Ctxt);

   overriding function Action_Class
     (Self : File_Action_Id) return GPR2.Value_Type is ("File action");

   overriding function Language
     (Self : File_Action_Id) return GPR2.Language_Id is
     (GPR2.No_Language);

   overriding function Action_Parameter
     (Self : File_Action_Id) return GPR2.Value_Type is
        ("not used in this test");

   type Object is new GPR2.Build.Actions.Thread.Object with null record;

   overriding function Working_Directory
     (Self : Object) return GPR2.Path_Name.Object is
     (GPR2.Path_Name.Create_Directory ("."));
end File_Action;
