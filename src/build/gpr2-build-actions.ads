--
--  Copyright (C) 2023-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GPR2.Build.Command_Line;
with GPR2.Build.Signature;
with GPR2.Containers;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.View_Ids;

limited with GPR2.Build.Tree_Db;

private with GNATCOLL.Traces;

package GPR2.Build.Actions is

   Command_Line_Limit : constant := 8191;
   Action_Error       : exception;

   type Action_Id is interface;
   --  An Action_Id is a unique identifier of an Action instance for the whole
   --  tree. It is composed of three to four visible parts:
   --  * the view id
   --  * the action class (e.g. "Compile", "Bind", "Link", etc...
   --  * the programming langage of the action (Ada, C, etc.) if applicable
   --  * The action parameter used to differentiate it from the other actions
   --    of the same class within a given view. Typically the input source
   --    file for a compilation or the output for a link operation.

   function View
     (Self : Action_Id) return GPR2.Project.View.Object is abstract;
   function Action_Class (Self : Action_Id) return Value_Type is abstract;
   function Language (Self : Action_Id) return Language_Id is abstract;
   function Action_Parameter (Self : Action_Id) return Value_Type is abstract;

   function Image
     (Self      : Action_Id'Class;
      With_View : Boolean := True) return String;
   --  A string representation of Self that can be displayed to the end-user,
   --  The description will omit the owning view if With_View is not set, This
   --  is used typically in case the view is already referenced in a
   --  GPR2.Message object and the Image is used in the Messages textual part.

   function Db_Filename
     (Self     : Action_Id'Class;
      Basename : Boolean := False) return Simple_Name;
   --  The filename that is used to store the action signature. Must be unique
   --  for actions of the involved view.
   --  If basename is set, then the returned value won't have the extension.

   function "<" (L, R : Action_Id'Class) return Boolean;
   --  Class-wide comparison

   package Action_Id_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Action_Id'Class);

   package Action_Id_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Action_Id'Class);

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

   function View (Self : Object) return GPR2.Project.View.Object is abstract;
   --  The view that is used for the context of the action's execution. The
   --  view is used to retrieve the switches for the tool, and to know where
   --  the output is stored (the Object_Dir attribute).

   function Working_Directory
     (Self : Object) return Path_Name.Object is abstract;
   --  The working directory used in the context of the action's execution

   function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean is abstract;
   --  Function called when Self is added to the tree's database. Allows the
   --  action to add its input and output artifacts and dependencies.
   --  Returns True on success.

   procedure Compute_Command
     (Self     : in out Object;
      Slot     : Positive;
      Cmd_Line : in out GPR2.Build.Command_Line.Object) is abstract;
   --  Return the command line and environment corresponding to the action

   procedure Compute_Signature
     (Self      : in out Object;
      Load_Mode : Boolean) is abstract;
   --  This populates the output artifacts in the signature.
   --  In load_mode, this should stop as soon as the signature don't match
   --  the saved timestamps
   --  Else, the status is ignored: the signature is prepared for saving.

   function Valid_Signature (Self : Object) return Boolean;
   --  Returns whether or not the action is inhibited. This means the loaded
   --  signature match the current action signature.

   function On_Tree_Propagation
     (Self : in out Object) return Boolean;
   --  Called after an initial population of the Build database has been done.
   --  This allows to propagate dependencies among the actions.

   function Is_Extending (Self : Object) return Boolean is (False);
   --  Whether the action is extending an action from an extended project of
   --  the owning view.

   function Extended (Self : Object) return Object is abstract
     with Pre'Class => Self.Is_Extending;
   --  In case the action's owning view is extending another view and the
   --  action is overriding an action of the extended view, the result is
   --  the overridden action.

   procedure Deactivate (Self : in out Object);
   --  Deactivates the action, can be useful to mark the action as skipped.

   function Skip (Self : Object) return Boolean;
   --  Indicates whether the action should be skipped. By default this returns
   --  False.

   function Is_Deactivated (Self : Object) return Boolean;

   function Write_Signature
     (Self   : in out Object'Class;
      Stdout : Unbounded_String;
      Stderr : Unbounded_String) return Boolean;
   --  Used to store the signature of the action after it has been executed.
   --  Returns false in case an expected artifact is missing.

   procedure Load_Signature (Self : in out Object);
   --  Compare the current action signature to the loaded signature

   function Saved_Stdout (Self : Object'Class) return Unbounded_String;
   function Saved_Stderr (Self : Object'Class) return Unbounded_String;

   function "<" (L, R : Object'Class) return Boolean;

   procedure Attach
     (Self : in out Object;
      Db   : in out GPR2.Build.Tree_Db.Object);

   procedure Update_Command_Line
     (Self : in out Object'Class;
      Slot : Positive);
   --  Updates the command line and update the signature accordingly

   function Command_Line (Self : Object) return GPR2.Build.Command_Line.Object;

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

   use GNATCOLL.Traces;
   use type GPR2.View_Ids.View_Id;

   type Object is abstract tagged record
      Tree       : access Tree_Db.Object;
      --  Owning Tree
      Signature  : GPR2.Build.Signature.Object;
      --  Stored signature for the action
      Traces     : Trace_Handle := Create ("TRACE_NAME_TO_OVERRIDE");
      --  Used for debug info
      Tmp_Files  : GPR2.Containers.Filename_Set;
      --  List of tmp files to be cleaned up
      Deactivated : Boolean := False;
      --  Set when the action is deactivated
      Cmd_Line    : GPR2.Build.Command_Line.Object;
      --  Command line used to run the action. Used also in the signature.
   end record;

   function "<" (L, R : Action_Id'Class) return Boolean is
     (if L.View.Id /= R.View.Id
      then L.View.Id < R.View.Id
      elsif L.Action_Class /= R.Action_Class
      then L.Action_Class < R.Action_Class
      elsif L.Language /= R.Language
      then L.Language < R.Language
      else L.Action_Parameter < R.Action_Parameter);

   function "<" (L, R : Object'Class) return Boolean is
      (L.UID < R.UID);

   function Valid_Signature (Self : Object) return Boolean is
     (Object'Class (Self).View.Is_Externally_Built
      or else Self.Signature.Valid);

   function On_Tree_Propagation
     (Self : in out Object) return Boolean is
     (True);

   function Post_Command
     (Self   : in out Object; Status : Execution_Status) return Boolean is
     (True);

   function Is_Deactivated (Self : Object) return Boolean
   is (Self.Deactivated);

   function Skip (Self : Object) return Boolean
   is (False);

   function Command_Line (Self : Object) return GPR2.Build.Command_Line.Object
   is (Self.Cmd_Line);

   function Saved_Stdout (Self : Object'Class) return Unbounded_String is
     (Self.Signature.Stdout);
   function Saved_Stderr (Self : Object'Class) return Unbounded_String is
     (Self.Signature.Stderr);

end GPR2.Build.Actions;
