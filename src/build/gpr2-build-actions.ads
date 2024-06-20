--
--  Copyright (C) 2023-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Log;
with GPR2.Project.View;
with GNATCOLL.OS.Process;

limited with GPR2.Build.Tree_Db;

private with GPR2.Build.Signature;
private with Ada.Tags;

package GPR2.Build.Actions is

   type Action_Id is interface;

   function Image (Self : Action_Id) return String is abstract;
   --  A representation of Self that can be displayed to the end user for e.g.
   --  error reporting or inspection reporting.

   function Db_Filename (Self : Action_Id) return Simple_Name is abstract;
   --  The filename that is used to store the action signature

   function "<" (L, R : Action_Id) return Boolean is abstract;

   function Less (L, R : Action_Id'Class) return Boolean;
   --  Class-wide comparison

   type Object is abstract tagged private;
   --  Actions are atomic steps in a compilation process, where an external
   --  process is called with a dedicated set of inputs to produce a set of
   --  output build artifacts (e.g. source compilation, linker invocation and
   --  so on). This object is responsible for keeping track of the
   --  signature of such execution: the various checksums of all inputs and
   --  outputs, to determine whether a previously executed action's output is
   --  still valid or needs to be re-executed.

   function UID (Self : Object) return Action_Id'Class is abstract;
   --  An action UID is used to store/restore the action data on the
   --  persistent storage, so must be unique for a given view. This means
   --  that the action should at least be prefixed by its class name and
   --  contain references to its inputs or outputs depending on what is
   --  relevant to make it unique.

   function Valid_Signature (Self : Object) return Boolean is abstract;
   --  Returns whether or not the action is inhibited. This means the loaded
   --  signature match the current action signature.

   function View (Self : Object) return GPR2.Project.View.Object is abstract;
   --  The view that is used for the context of the action's execution. The
   --  view is used to retrieve the switches for the tool, and to know where
   --  the output is stored (the Object_Dir attribute).

   procedure On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object;
      Messages : in out GPR2.Log.Object) is abstract
   with Pre'Class => not Messages.Has_Error;
   --  procedure called when Self is added to the tree's database. Allows the
   --  action to add its input and output artifacts and dependencies.

   procedure Compute_Signature (Self : in out Object) is abstract;
   --  Compute the action signature from all its artifacts and hard store it

   procedure Compare_Signature
     (Self     : in out Object;
      Messages : in out GPR2.Log.Object);
   --  Compare the current action signature to the loaded signature

   procedure Attach
     (Self : in out Object;
      Db   : in out GPR2.Build.Tree_Db.Object);

   function Command (Self : Object)
     return GNATCOLL.OS.Process.Argument_List is abstract;
   --  Return the command line corresponding to the action

   procedure Post_Command (Self : in out Object);
   --  Post-processing that should occur after executing the command

private

   use type Ada.Tags.Tag;

   type Object is abstract tagged record
      Tree      : access Tree_Db.Object;

      Signature : GPR2.Build.Signature.Object;
   end record;

   function Less (L, R : Action_Id'Class) return Boolean is
     (if L'Tag = R'Tag then L < R
      else Ada.Tags.External_Tag (L'Tag) < Ada.Tags.External_Tag (R'Tag));
end GPR2.Build.Actions;
