.. index:: custom builder, GPR2.Build.Tree_DB, build action

**************************
Custom Incremental Builder
**************************

The GPR2 build infrastructure provides a complete framework for implementing
incremental builders on top of the project model. It is used by ``gprbuild``
but is fully available to third-party tools. The key packages are:

* ``GPR2.Build.Tree_Db`` - persistent build database and action DAG.
* ``GPR2.Build.Actions`` - abstract base type for a single build step.
* ``GPR2.Build.Artifacts`` - abstract base type for the inputs and outputs
  that connect actions to each other.
* ``GPR2.Build.Actions_Population`` - populates the action graph from a
  project tree using the standard GPR2 build actions.
* ``GPR2.Build.Process_Manager`` - parallel process execution engine.


Overview
========

A build proceeds in four stages:

1. **Load** the project tree (``Tree.Load``).
2. **Populate sources** (``Tree.Update_Sources``).
3. **Populate the action graph** - either via
   ``Actions_Population.Populate_Actions`` for standard GPR builds, or by
   inserting custom actions directly into ``Tree.Artifacts_Database``.
4. **Execute** the graph (``Tree.Artifacts_Database.Execute``).


The build database
==================

``Tree.Artifacts_Database`` returns an access to the
``GPR2.Build.Tree_Db.Object`` for the tree. The database is created
automatically when the tree is loaded.

.. code-block:: ada

   Db : constant GPR2.Build.Tree_Db.Object_Access :=
          Tree.Artifacts_Database;

The database holds a directed acyclic graph of actions connected by
artifacts. Each action's output artifacts become input artifacts of
downstream actions, establishing the dependency order for execution.
Signature checksums are persisted on disk so that unchanged actions are
skipped on the next run.


Populating the standard action graph
====================================

For standard GPR builds (compile, bind, link), use
``GPR2.Build.Actions_Population.Populate_Actions``:

.. code-block:: ada

   with GPR2.Build.Actions_Population;
   with GPR2.Build.Options;

   Build_Opts : GPR2.Build.Options.Build_Options;
   --  Build_Opts.Mains may be set to restrict to specific mains;
   --  leave empty to build all mains from the root project.

   if not GPR2.Build.Actions_Population.Populate_Actions
     (Tree    => Tree,
      Options => Build_Opts)
   then
      return;
   end if;

   if not Db.Propagate_Actions then
      return;
   end if;

``Populate_Actions`` inserts compile, bind, and link actions for every
source in the tree. ``Propagate_Actions`` then calls
``On_Tree_Propagation`` on each action to resolve cross-action
dependencies (e.g. the Ada binder closure).


Executing the graph
===================

Pass a process manager and options to ``Db.Execute``:

.. code-block:: ada

   with GPR2.Build.Process_Manager;

   PM      : GPR2.Build.Process_Manager.Object;
   PM_Opts : GPR2.Build.Process_Manager.PM_Options;

   PM_Opts.Jobs         := 0;    --  0 = auto-detect CPU count
   PM_Opts.Stop_On_Fail := True;

   case Db.Execute (PM, PM_Opts) is
      when GPR2.Build.Process_Manager.Success => null;
      when GPR2.Build.Process_Manager.Errors  =>
         --  some actions reported errors
         return;
      when GPR2.Build.Process_Manager.Failed  =>
         --  some actions failed to launch
         return;
   end case;

Key ``PM_Options`` fields:

``Jobs``
  Parallel job count; ``0`` auto-detects the number of CPUs.

``Force``
  Re-execute all actions regardless of signature validity.

``Stop_On_Fail``
  Abort on first failure (default ``True``).

``Keep_Temp_Files``
  Preserve temporary files after execution (useful for debugging).

``Script_File``
  If defined, records all executed commands to this file.

``Show_Progress``
  Emit progress counters as actions are dispatched.


Actions
=======

``GPR2.Build.Actions.Object`` is the abstract base type for a build step.
Each action owns a view (its context for looking up attributes and
directories) and a signature (checksums of all its inputs and outputs).

Built-in concrete actions provided by the library:

``GPR2.Build.Actions.Compile``
  Compiles one source file for any language.

``GPR2.Build.Actions.Compile.Ada``
  Ada-specific compilation (extends Compile).

``GPR2.Build.Actions.Ada_Bind``
  Runs the Ada binder (``gnatbind``) for one main.

``GPR2.Build.Actions.Post_Bind``
  Compiles the binder-generated body.

``GPR2.Build.Actions.Link``
  Links an executable or shared library.

``GPR2.Build.Actions.Link.Partial``
  Partial link step used for standalone libraries.

Action lifecycle hooks
----------------------

Each action participates in the build graph via the following hooks, called
in this order:

``On_Tree_Insertion``
  Called when the action is added to the database. The action
  declares its output artifacts and may insert follow-up actions
  (e.g. a bind action inserts the post-bind compile action here).

``On_Tree_Propagation``
  Called after initial population. Used to expand dependencies
  dynamically (e.g. the binder walks the Ada closure to pull in all
  required compilation units). Default implementation does nothing.

``Compute_Command``
  Builds the command line just before execution. Also called when the
  signature is valid (to include the command line in the signature
  check) with ``Signature_Only => True``.

``Pre_Command``
  Called immediately before the process is spawned. Not called when
  the action is skipped. Default returns ``True``.

``Post_Command``
  Called after the process completes, is skipped, or fails. Default
  returns ``True``.

``On_Static_Completion``
  Replaces ``Pre_Command``/``Post_Command`` when actions are
  populated but not executed (e.g. ``gprinstall``). Must not modify
  artifacts. Default returns ``True``.


Artifacts
=========

``GPR2.Build.Artifacts.Object`` is the interface that connects actions.
An action's outputs become inputs to downstream actions, establishing the
DAG edges. Concrete artifact types:

``Artifacts.Files.Object``
  A filesystem file (source, object, library, ...).

``Artifacts.Object_File.Object``
  A compiled object file.

``Artifacts.Library.Object``
  A static or shared library.

``Artifacts.Key_Value.Object``
  An abstract key/value pair; used for ordering actions that do not
  produce a file (e.g. the UID artifact that establishes execution
  order without file dependencies).

``Artifacts.Source_Files.Object``
  A source file as a build artifact.

Wiring actions to artifacts is done via the database:

.. code-block:: ada

   --  Register an output artifact for an action
   if not Db.Add_Output (Action.UID, My_Object_File) then
      --  artifact already owned by another action
   end if;

   --  Register an input dependency
   Db.Add_Input
     (Action   => Downstream_Action.UID,
      Artifact => My_Object_File,
      Explicit => True);


Implementing a custom action
============================

Extend ``GPR2.Build.Actions.Object``, implement ``Action_Id``, and
override the mandatory primitives:

.. code-block:: ada

   with GPR2.Build.Actions;
   with GPR2.Build.Tree_Db;
   with GPR2.Build.Command_Line;

   type My_Action_Id is new GPR2.Build.Actions.Action_Id with record
      View  : GPR2.Project.View.Object;
      Input : GPR2.Path_Name.Object;
   end record;

   overriding function View
     (Self : My_Action_Id) return GPR2.Project.View.Object
   is (Self.View);

   overriding function Action_Class
     (Self : My_Action_Id) return Value_Type
   is (+"MyAction");

   overriding function Language
     (Self : My_Action_Id) return Language_Id
   is (No_Language);

   overriding function Action_Parameter
     (Self : My_Action_Id) return Value_Type
   is (Value_Type (Self.Input.Simple_Name));

   type My_Action is new GPR2.Build.Actions.Object with record
      Id    : My_Action_Id;
      Input : GPR2.Path_Name.Object;
   end record;

   overriding function UID
     (Self : My_Action) return GPR2.Build.Actions.Action_Id'Class
   is (Self.Id);

   overriding function Working_Directory
     (Self : My_Action) return GPR2.Path_Name.Object
   is (Self.View.Object_Directory);

   overriding function On_Tree_Insertion
     (Self : My_Action;
      Db   : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
   begin
      --  Register output artifacts here
      return True;
   end On_Tree_Insertion;

   overriding procedure Compute_Command
     (Self           : in out My_Action;
      Slot           :        Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only :        Boolean)
   is
   begin
      Cmd_Line.Add_Argument ("my-tool");
      Cmd_Line.Add_Argument (String (Self.Input.Value));
   end Compute_Command;

   overriding procedure Compute_Signature
     (Self            : in out My_Action;
      Check_Checksums :        Boolean)
   is
   begin
      --  Register inputs/outputs in Self.Signature for change detection
      null;
   end Compute_Signature;

Once implemented, insert the action into the database before calling
``Execute``:

.. code-block:: ada

   Action : My_Action := ...;
   if not Db.Add_Action (Action) then
      --  action already present
   end if;
