--
--  Copyright (C) 2023-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Ordered_Sets;

with GNATCOLL.OS.Process;

with GPR2.Containers;
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

   package Action_Id_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Action_Id'Class);

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

   function Valid_Signature (Self : Object) return Boolean;
   --  Returns whether or not the action is inhibited. This means the loaded
   --  signature match the current action signature.

   function View (Self : Object) return GPR2.Project.View.Object is abstract;
   --  The view that is used for the context of the action's execution. The
   --  view is used to retrieve the switches for the tool, and to know where
   --  the output is stored (the Object_Dir attribute).

   function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean is abstract;
   --  Function called when Self is added to the tree's database. Allows the
   --  action to add its input and output artifacts and dependencies.
   --  Returns True on success.

   function On_Tree_Propagation
     (Self : in out Object) return Boolean;

   function Skip (Self : Object) return Boolean is
     (False);
   --  Indicates whether the action should be skipped. By default this returns
   --  False.

   procedure Compute_Signature (Self : in out Object);
   --  Compute the action signature from all its artifacts and hard store it
   --  By default this uses the inputs and outputs of the Build_Db graph to
   --  compute the signature. To be refined when needed.

   procedure Load_Signature (Self : in out Object'Class);
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

   type Execution_Status is (Skipped, Success);

   function Post_Command
     (Self   : in out Object; Status : Execution_Status) return Boolean;
   --  Post-processing that should occur after executing the command

   ---------------------------
   -- Temp files management --
   ---------------------------

   type Temp_File_Scope is (Local, Global);

   function Get_Or_Create_Temp_File
     (Self    : in out Object'Class;
      Purpose : Filename_Type;
      Scope   : Temp_File_Scope) return Tree_Db.Temp_File;
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
   --  Cleanup any existing temp file for the given scope

private

   use type Ada.Tags.Tag;
   use GNATCOLL.Traces;

   type Object is abstract tagged record
      Tree       : access Tree_Db.Object;
      --  Owning Tree
      Signature  : GPR2.Build.Signature.Object;
      --  Stored signature for the action
      Traces     : Trace_Handle := Create ("TRACE_NAME_TO_OVERRIDE");
      --  Used for debug info
      Tmp_Files  : GPR2.Containers.Filename_Set;
      --  List of tmp files to be cleaned up
   end record;

   function Less (L, R : Action_Id'Class) return Boolean is
     (if L'Tag = R'Tag then L < R
      else Ada.Tags.External_Tag (L'Tag) < Ada.Tags.External_Tag (R'Tag));

   function Valid_Signature (Self : Object) return Boolean is
     (Object'Class (Self).View.Is_Externally_Built
      or else Self.Signature.Valid);

   function On_Tree_Propagation
     (Self : in out Object) return Boolean is
     (True);

   function Post_Command
     (Self   : in out Object; Status : Execution_Status) return Boolean is
     (True);

end GPR2.Build.Actions;
