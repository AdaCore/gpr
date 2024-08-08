--
--  Copyright (C) 2023-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.OS.FS;
with GNATCOLL.OS.Process;

with GPR2.Containers;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.View;

limited with GPR2.Build.Tree_Db;

private with Ada.Tags;

private with GNATCOLL.Traces;

private with GPR2.Build.Signature;

package GPR2.Build.Actions is

   type Action_Id is interface;

   function Image (Self : Action_Id) return String is abstract;
   --  A representation of Self that can be displayed to the end user for e.g.
   --  error reporting or inspection reporting.

   function Db_Filename (Self : Action_Id) return Simple_Name is abstract;
   --  The filename that is used to store the action signature. Must be unique
   --  for actions of the involved view.

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
   --  Procedure called when Self is added to the tree's database. Allows the
   --  action to add its input and output artifacts and dependencies.

   function Skip (Self : Object) return Boolean is
     (False);
   --  Indicates whether the action should be skipped. By default this returns
   --  False.

   procedure Compute_Signature (Self : in out Object) is abstract;
   --  Compute the action signature from all its artifacts and hard store it

   procedure Compare_Signature
     (Self     : in out Object;
      Messages : in out GPR2.Log.Object);
   --  Compare the current action signature to the loaded signature

   procedure Attach
     (Self : in out Object;
      Db   : in out GPR2.Build.Tree_Db.Object);

   procedure Compute_Command
     (Self : in out Object;
      Args : out GNATCOLL.OS.Process.Argument_List;
      Env  : out GNATCOLL.OS.Process.Environment_Dict) is abstract;
   --  Return the command line and environment corresponding to the action

   function Working_Directory
     (Self : Object) return Path_Name.Object is abstract;

   procedure Post_Command (Self : in out Object) is null;
   --  Post-processing that should occur after executing the command

   ---------------------------
   -- Temp files management --
   ---------------------------

   type Temp_File (Path_Len : Natural) is record
      FD   : GNATCOLL.OS.FS.File_Descriptor;
      Path : Filename_Type (1 .. Path_Len);
   end record;

   type Temp_File_Scope is (Local, Global);

   function Get_Or_Create_Temp_File
     (Self    : in out Object'Class;
      Purpose : Filename_Type;
      Scope   : Temp_File_Scope) return Temp_File;
   --  Create a temporary file. If the scope is local, it will be automatically
   --  recalled upon termination of the Action, otherwise the cleanup is done
   --  at the end of the DAG execution.
   --  Purpose is used to differenciate temp files within the same action.
   --  If the temp file for the specified purpose already exists, path is set
   --  in the returned record but FD is set to Null_FD. Else FD is in write
   --  mode so can be used to generate the temp file.

   procedure Cleanup_Temp_Files
     (Self : in out Object'Class;
      Scope : Temp_File_Scope);
   --  Cleanup any existing temp file for the given scope.

private

   use type Ada.Tags.Tag;
   use GNATCOLL.Traces;

   type Tmp_Files_Array is
     array (Temp_File_Scope) of GPR2.Containers.Filename_Set;

   type Object is abstract tagged record
      Tree       : access Tree_Db.Object;
      --  Owning Tree
      Signature  : GPR2.Build.Signature.Object;
      --  Stored signature for the action
      Traces     : Trace_Handle := Create ("TRACE_NAME_TO_OVERRIDE");
      --  Used for debug info
      Tmp_Files  : Tmp_Files_Array;
      --  List of tmp files to be cleaned up
   end record;

   function Less (L, R : Action_Id'Class) return Boolean is
     (if L'Tag = R'Tag then L < R
      else Ada.Tags.External_Tag (L'Tag) < Ada.Tags.External_Tag (R'Tag));

end GPR2.Build.Actions;
