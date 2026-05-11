.. index:: GPR2.Reporter, diagnostic message, log

********
Reporter
********

The ``GPR2.Reporter`` package defines the abstract diagnostic sink used
throughout the library. Two distinct message streams flow through it:

* **Diagnostic messages** (``Warning``, ``Error``, ``Hint``, ``Lint``) are
  produced during tree loading and source population. They are associated with
  a location in a project file and describe problems or observations about the
  project structure. Their visibility is controlled by ``Verbosity_Level``.

* **End_User messages** are live informational feedback not tied to any
  project file location. They are emitted during loading operations
  (e.g. "Using project file p.gpr") and by the incremental builder as actions
  run (e.g. "Compiling foo.adb"). Their visibility is controlled independently
  via ``User_Verbosity_Level`` and the message's own ``User_Level_Value``
  (``Optional``, ``Regular``, ``Important``).

Both streams are forwarded automatically during ``Tree.Load``,
``Tree.Update_Sources``, and by the build infrastructure.


Built-in reporters
==================

Two concrete reporters are provided out of the box.

``GPR2.Reporter.Console``
   Writes messages to standard output (``End_User`` messages) or standard
   error (warnings and errors). This is the default reporter used by
   ``Tree.Load`` when no explicit reporter is provided.

   .. code-block:: ada

      with GPR2.Reporter.Console;

      Reporter : GPR2.Reporter.Console.Object :=
                   GPR2.Reporter.Console.Create
                     (Verbosity      => GPR2.Reporter.Regular,
                      User_Verbosity => GPR2.Reporter.Unset);

   Optional ``Create`` parameters:

   ``Verbosity``
     Controls which diagnostic severity levels are shown
     (default ``Regular``).

   ``User_Verbosity``
     Independent control over ``End_User`` build-progress messages
     (default ``Unset``, meaning it follows ``Verbosity``).

   ``Use_Full_Pathname``
     When ``True``, file paths in diagnostic messages are absolute
     rather than basenames.

   ``Level_Report_Format``
     ``None``, ``Short``, or ``Long`` (default); controls the level
     prefix in formatted diagnostic output.

``GPR2.Reporter.Log``
   Stores messages in an internal ``GPR2.Log.Object`` instead of printing
   them. Useful when the caller wants to post-process or display diagnostics
   in a custom way (e.g. a GUI tool that renders messages in a panel).

   .. code-block:: ada

      with GPR2.Reporter.Log;

      Reporter : GPR2.Reporter.Log.Object :=
                   GPR2.Reporter.Log.Create
                     (Verbosity => GPR2.Reporter.Regular);

   After loading, retrieve the collected messages:

   .. code-block:: ada

      for Msg of Reporter.Log loop
         My_UI.Show (Msg.Format);
      end loop;


Installing a reporter
=====================

Pass the reporter to ``Tree.Load``:

.. code-block:: ada

   if not Tree.Load (Options, Reporter => Reporter) then
      return;
   end if;

Or set it explicitly before loading:

.. code-block:: ada

   Tree.Set_Reporter (Reporter);

Only one reporter can be active at a time. Setting a new reporter replaces
the previous one.


Verbosity levels
================

``Verbosity_Level`` controls which diagnostic messages are shown:

Quiet
  None.

No_Warnings
  Errors only.

Regular
  Errors and warnings (default).

Verbose
  Errors, warnings, hints, and lint messages.

Very_Verbose
  Same as ``Verbose``; available for subclass use.

``User_Verbosity_Level`` provides independent control over ``End_User``
build-progress messages:

Unset
  ``End_User`` visibility follows ``Verbosity_Level`` (default).

Quiet
  Suppress all ``End_User`` messages.

Important_Only
  Show only ``Important``-level ``End_User`` messages.

Regular
  Show ``Regular`` and ``Important`` ``End_User`` messages.

Verbose
  Show all ``End_User`` messages including ``Optional``-level ones.


Reporting messages manually
============================

Any code that has access to a reporter can push messages directly:

.. code-block:: ada

   --  From a GPR2.Log collected elsewhere
   Reporter.Report (Some_Log);

   --  A single diagnostic Message.Object
   Reporter.Report (GPR2.Message.Create
     (Level   => GPR2.Message.Warning,
      Message => "project has no sources"));

   --  Plain string convenience overload (creates an End_User message)
   Reporter.Report ("Build complete", Level => GPR2.Message.Important);


The Message type
================

``GPR2.Message.Object`` is the fundamental unit for both streams. Key fields:

``Msg.Level``
  ``Warning``, ``Error``, ``End_User``, ``Hint``, or ``Lint``.

``Msg.Message``
  The raw text of the message.

``Msg.Sloc``
  Source location (project file, line, column); undefined for
  ``End_User`` messages.

``Msg.User_Level``
  For ``End_User`` messages: ``Optional``, ``Regular``, or
  ``Important``. Controls under which ``User_Verbosity`` setting the
  message is shown.

``Msg.Format``
  Formatted string in compiler style:
  ``[<file>:<line>:<col>: ]<level>: <text>``. ``End_User`` messages
  without a source location are returned as plain text with no prefix.

``GPR2.Message.Treat_Warnings_As_Error`` is a process-wide flag: when
enabled, any subsequently created ``Warning``-level message is promoted to
``Error`` at construction time.


Writing a custom reporter
==========================

Extend ``GPR2.Reporter.Object`` and override two primitives:

.. code-block:: ada

   with GPR2.Reporter;
   with GPR2.Message;

   type My_Reporter is new GPR2.Reporter.Object with null record;

   overriding function Verbosity
     (Self : My_Reporter) return GPR2.Reporter.Verbosity_Level
   is (GPR2.Reporter.Regular);

   overriding procedure Internal_Report
     (Self    : in out My_Reporter;
      Message : GPR2.Message.Object)
   is
   begin
      --  Route the message wherever the tool needs it
      My_Log.Append (Message.Format);
   end Internal_Report;

``Internal_Report`` is called only for messages that pass the verbosity
filter; the filtering is done by the class-wide ``Report`` wrapper before
``Internal_Report`` is invoked. Override ``User_Verbosity`` as well to
provide independent control over the ``End_User`` stream.
