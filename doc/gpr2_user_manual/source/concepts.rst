********
Concepts
********

The GPR2 library uses different types of objects to access project files data.

Types used when loading a project into a ``GPR2.Project.Tree.Object``

 * ``GPR2.Path_Name.Object``
 
   Handle filesystem paths. ``GPR2.Path_Name`` package contains conversions from
   and to ``GNATCOLL.VFS.Virtual_File`` type.
   
 * ``GPR2.Path_Name.Set.Object``

   Handle a set of paths.

 * ``GPR2.Project.Tree.Object``
 
   Handle a project tree. Project file used to load project tree is available as ``Tree.Root_Project``.
 
 * ``GPR2.Project.Tree.View_Builder.Object``

   Used to load a project as a project file path alternative.
   Attributes (for example ``Source_Dirs``) can be defined for this object before it is used to load a project tree.

 * ``GPR2.Context.Object``
 
   Context objects are used to pass scenario variables to a tree. A context is
   provided when loading a tree. It can then be changed calling
   ``GPR2.Project.Tree.Set_Context``.

 * ``GPR2.Environment.Object``
 
   Environment object provided at load time is used by GPR2 to access
   the environment when needed.
   
 * ``GPR2.Log.Object``
 
   Used to store the log messages (error/warning/information)
   coming from the parser.
 
 * ``GPR2.Message.Object``
 
   Base object to report information at the end user level.

Types used to handle loaded gpr files. A loaded gpr project is stored in a tree
containing views. The tree contains root, imported, extended, aggregated, configuration and runtime views.

 * ``GPR2.Project.View.Object``
 
   Handle a gpr project file. Used to handle root project and any referenced
   (imported, extended, aggregated) project.
   
 * ``GPR2.Project.View.Set.Object``
 
   Handle a set of view objects.

 * ``GPR2.Project.Configuration.Object``
 
   Handle the configuration object for a project tree.

Types used to handle types, variables, attributes and packages

 * ``GPR2.Project.Typ.Object``
 
   Handle a variable type definition.
   
 * ``GPR2.Project.Typ.Set.Object``
 
   Handle a set of variable types.

 * ``GPR2.Project.Variable.Object``
 
   Handle a variable defined in a project or in a project's package.
   
 * ``GPR2.Project.Variable.Set.Object``
 
   Handle a set of variables

 * ``GPR2.Project.Attribute.Object``
 
   Handle project view attributes.

 * ``GPR2.Project.Attribute.Set.Object``
 
   Handle a set of attributes.

 * ``GPR2.Project.Pack.Object``

   Handle a project's package which is a set of attributes and variables.

Types used to handle project sources

 * ``GPR2.Project.Source.Object``

   Handle a project's source file.
   
 * ``GPR2.Project.Source.Set.Object``
 
   Handle a set of project's source files.
 
Types used to handle project units

 * ``GPR2.Project.Unit_Info.Object``
 
   Handle a view's unit.

 * ``GPR2.Project.Unit_Info.Set.Object``

   Handle a set of view's units.

Type used to handle standard gpr tools switches.

 * ``GPR2.Options.Object``
 
   Handle loading a project using common GPR tools switches (``-aP``, ``--autoconf``, ``--config``, ``--db``, ``--db-``, ``--implicit-with``, ``--no-project``, ``-P``, ``--relocate-build-tree``, ``--root-dir``, ``--RTS``, ``--src-subdirs``, ``-subdirs``, ``--target``, ``--unchecked-shared-lib-imports``, ``-X``)

Memory management of GPR2 objects is easy as all references are using reference counting.

API proper use is ensured using pre condition aspect.
These checks are activated using ``debug`` or ``release_checks`` values for ``GPR2_BUILD`` scenario variable. 
Usually object's function calls should be protected by a corresponding ``Has_XXX`` or ``Is_XXX`` check.
Some object's function calls are using a "function Check_XXX (Object, out Result) return Boolean;" pattern.

.. code-block:: ada

   function Has_Runtime_Project (Self : Object) return Boolean;
   --  Returns True if a runtime project is loaded on this tree

   function Runtime_Project (Self : Object) return View.Object
     with Pre => Self.Is_Defined and then Self.Has_Runtime_Project;
   --  Returns the runtime project for the given tree

   function Is_Extended (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the view is extended by another project

   function Extending (Self : Object) return Object
     with Pre  => Self.Is_Defined and then Self.Is_Extended,
          Post => Extending'Result.Is_Extending;
   --  Return the extending view

    function Check_Source
     (Self     : Object;
      Filename : GPR2.Simple_Name;
      Result   : in out Project.Source.Object) return Boolean
     with Pre => Self.Is_Defined;

All sets handled by the GPR2 library can be easily iterated.
They are returned as iterable objects (``GPR2.Path_Name.Set.Object``, ``GPR2.Project.View.Set.Object``, and any ``GPR2.<child_name>.Set.Object``)
or they define a ``Iterate`` API.

.. code-block:: ada

   function Iterate
     (Self        : GPR2.Log.Object;
      Information : Boolean := True;
      Warning     : Boolean := True;
      Error       : Boolean := True;
      Lint        : Boolean := False;
      Read        : Boolean := True;
      Unread      : Boolean := True)
      return Log_Iterator.Forward_Iterator'Class;

   function Iterate
     (Self   : GPR2.Project.Tree.Object;
      Kind   : Iterator_Control := Default_Iterator;
      Filter : Filter_Control   := Default_Filter;
      Status : Status_Control   := Default_Status)
      return Project_Iterator.Forward_Iterator'Class;

   function Iterate
     (Self : GPR2.Unit.List.Object)
      return Unit_Iterator.Forward_Iterator'Class;


GPR2.Path_Name.Object - Files and directories
#############################################

Files and directories are handled in GPR2 using Object type defined in ``GPR2.Path_Name`` package.

Objects can be created directly using ``Create_File`` or ``Create_Directory`` API, or from an existing ``Object`` using Compose API.

An API is provided to easily interface these objects with ``Filesystem_String`` and ``Virtual_File`` types defined in ``GNATCOLL.VFS``.

A complete API is provided to manipulate files and directories.

GPR2.Project.Configuration.Object - Configuration
#################################################

Configuration files (``.cgpr`` files) generated usually by ``gprconfig`` and provided using ``--config`` switch,
can be loaded using ``GPR2.Project.Configuration.Load`` function. These objects are then used at project file tree load time.

GPR2.Project.Tree.Object - Loaded project tree
##############################################

Handling a project file using GPR2 starts loading a ``GPR2.Project.Tree.Object``.
All imported, extended, aggregated subprojects, selected ada runtime and used configuration file will be loaded as well.

``GPR2.Options`` package provides a ``Load_Project`` function.

.. code-block:: ada

   declare
      Options : GPR2.Options.Object;
      Tree    : GPR2.Project.Tree.Object;
      Loaded  : Boolean;
   begin
      Options.Add_Switch (GPR2.Options.P, "test");
      Options.Finalize;
      Loaded := Options.Load_Project (Tree);
   end;

Tree can also be loaded using ``Load`` and ``Load_Autoconf`` primitives located in ``GPR2.Project.Tree package``.

.. code-block:: ada

    with Ada.Text_IO;  use Ada.Text_IO;

    with GPR2.Context;
    with GPR2.Log;
    with GPR2.Path_Name;
    with GPR2.Project.Tree;
    with GPR2.Project.Configuration;

    procedure Test_Project is
       Project_File   : constant GPR2.Path_Name.Object :=
                           GPR2.Project.Create ("path_to_project.gpr");
       Config_Project : constant GPR2.Path_Name.Object :=
                           GPR2.Project.Create ("project.cgpr");
       Tree           : GPR2.Project.Tree.Object;
    begin
       --  Load path_to_project.gpr & create project.cgpr file
       Tree.Load_Autoconf
         (Filename          => Project_File,
          Context           => GPR2.Context.Empty,
          Config_Project    => Config_Project);

       --  Load path_to_project.gpr using a configuration file.
       Tree.Load
         (Filename         => Project_File,
          Context          => GPR2.Context.Empty,
          Config           => GPR2.Project.Configuration.Load (Config_Project),
          Build_Path       => GPR2.Project.Create ("build_path"),
          Subdirs          => "subdirs");

       --  Display object directory taking into account build tree & subdirs
       Put_Line (Tree.Root_Project.Object_Directory.Value);
    exception
       when GPR2.Project_Error =>
          GPR2.Log.Output_Messages (Tree.Log_Messages.all);
    end Test_Project;

Tree can also be loaded from ``GPR2.Project.Tree.View_Builder.Object``
instead of ``GPR2.Path_Name.Object``. This feature is useful when you need to load a Tree
but no project file is available.

.. code-block:: ada

    declare
        Root     : GPR2.Project.Tree.View_Builder.Object :=
                      GPR2.Project.Tree.View_Builder.Create
                        (GPR2.Path_Name.Create_Directory ("demo"), "Custom_Project");
        Src_Dirs : GPR2.Containers.Value_List;
        Tree     : GPR2.Project.Tree.Object;

        package PRA renames GPR2.Project.Registry.Attribute;
    begin
        Src_Dirs.Append ("src1");
        Src_Dirs.Append ("src2");
        Root.Set_Attribute (PRA.Source_Dirs, Src_Dirs);
        Root.Set_Attribute (PRA.Object_Dir, "obj");

        GPR2.Project.Tree.View_Builder.Load_Autoconf (Tree, Root, GPR2.Context.Empty);
    end;

GPR2.Context.Object - Scenario variables
########################################

Scenario variables are defined using ``GPR2.Context.Object``.

Key/Value are added or update using ``Include`` primitive.

A loaded Tree can be updated calling ``Set_Context`` primitive when scenario variables need to be changed.

.. code-block:: ada
    
    Tree    : GPR2.Project.Tree.Object;
    Context : GPR2.Context.Object;
    
    --  Change Context and update Tree
    
    Context.Include ("KEY", "value");
    Tree.Set_Context (Context);

GPR2.Log.Object - Messages
##########################

GPR2 is reporting project and configuration file messages through ``GPR2.Log.Object``.

``GPR2.Log`` package provides an configurable iterator to list selected messages.
``Output_Messages`` primitive is provided to print filtered messages.

A message contains the following properties.

 * ``Level``, can be ``Information``, ``Warning``, ``Error`` or ``Lint``.
 * ``Status``, can be ``Read`` or ``Unread``.
 * ``Message`` text.
 * ``Sloc``, defining where Filename:Line:Column the message was issued.

GPR2.Project.View.Object - Project file
#######################################

Any project file (root, imported, extended, etc...) parsed during ``Load`` or ``Load_Autoconf`` execution is reported as ``GPR2.Project.View.Object``.
 
A View object contains attributes, types, variables, sources, units and any extra data defined in project file.

When sources files are added/deleted, ``Tree.Invalidate_Sources (View);`` should be used to update sources related data.
Calling ``Tree.Invalidate_Sources;`` updates all sources for all views in the Tree.

GPR2.Project.Attribute.Object - Attributes
##########################################

View's attributes can be accessed using a name, an index and a position. A name is mandatory.

Name parameter uses ``GPR2.Q_Attribute_Id`` type.
Predefined ``Q_Attribute_Id`` values can be found in ``GPR2.Project.Registry.Attribute`` package.

``Q_Attribute_Id`` value for Source_Dirs is attribute is ``GPR2.Project.Registry.Attribute.Source_Dirs``.

``Q_Attribute_Id`` value for Builder'Executable attribute is ``GPR2.Project.Registry.Attribute.Builder.Executable``.

New ``Q_Attribute_Id`` values (for external tools) can be defined/registered as follow:

.. code-block:: ada

    use GPR2;
    Tool_Id     : constant GPR2.Package_Id := +"tool";
    Attribute_A : constant GPR2.Q_Attribute_Id := (Tool_Id, +"attribute_a");

   --  new packages and attributes should be registered during initialization.

   GPR2.Project.Registry.Pack.Add (Tool_Id, GPR2.Project.Registry.Pack.Everywhere);
   GPR2.Project.Registry.Attribute.Add
     (Name                 => Attribute_A,
      Index_Type           => GPR2.Project.Registry.Attribute.No_Index,
      Value                => GPR2.Project.Registry.Attribute.Single,
      Value_Case_Sensitive => False,
      Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);

Index are created using ``GPR2.Project.Attribute_Index.Create`` primitives.

As an example to get Builder'Executable ("mains.adb" at 2) attribute use:

.. code-block:: ada

    Executable : constant GPR2.Project.Attribute.Object :=
                    Tree.Root_Project.Attribute
                      (Name   => GPR2.Project.Registry.Attribute.Builder.Executable,
                       Index  => GPR2.Project.Attribute_Index.Create ("mains.adb"),
                       At_Pos => 2);

GPR2.Project.Variable.Object - Variables
########################################

Variables defined in a gpr file can be accessed using ``Variables`` and ``Variable`` primitives of ``GPR2.Projet.View.Object``
``Variables`` function returns the variables set in ``GPR2.Project.Variable.Set.Object``.
``Variable`` function return the requested variable.

As usual, requests should be protected by corresponding ``Has_XXX`` requests.
If a variable has a type, its type can be stored in a ``GPR2.Project.Typ.Object``.

.. code-block:: ada

    type Build_Type is ("debug", "release", "release_checks", "gnatcov");
    Build : Build_Type := external ("GPR2_BUILD", "debug");

The following code show how a variable and its type can be accessed.
 
.. code-block:: ada

    declare
       Name          : constant GPR2.Name_Type := "Build";
       View          : GPR2.Project.View.Object := Tree.Root_Project;
       Variable      : GPR2.Project.Variable.Object;
       Variable_Type : GPR2.Project.Typ.Object;
    begin
       if View.Has_Variables (Name) then
          Variable := View.Variable (Name);
          Ada.Text_IO.Put_Line (Variable.Value.Text);
          if Variable.Has_Type then
             Variable_Type := Variable.Typ;
             Ada.Text_IO.Put (String (Variable_Type.Name.Text) & " : ");
             for V of Variable_Type.Values loop
                Ada.Text_IO.Put (V.Text & ",");
             end loop;
             Ada.Text_IO.Put_Line ("");
          end if;
       end if;
    end;

GPR2.Project.Source.Object - Sources
####################################

Sources of a project file are handled by ``GPR2.Project.Source.Object`` type.

They can be accessed through ``View.Sources`` or ``View.Source (Path_Name)`` functions.

GPR2 parses the source file using libadalang or the corresponding ali file generated previously by gnat
to report contained unit(s) or dependencies list.

GPR2.Project.Unit_Info.Object - Units
#####################################

Units of a project file are handled by ``GPR2.Project.Unit_Info.Object`` type.

They can be accessed through ``View.Units`` or ``View.Unit (Unit_Name)`` functions.

Note that the list of units is populated only when
``Tree.Update_Sources``, ``View_Has_Sources`` or ``View.Sources`` is called.

As a performance optimization, if you doesn't care about units and source dependencies,
don't forget when updating sources to explicitly ask for using no backends. (all backends are used as default)

.. code-block:: ada

      Tree.Update_Sources (Backends => GPR2.Source_Info.No_Backends);


GPR2.Options.Object - GPR tools common switches support
#######################################################

Using this ``GPR2.Options.Object``, normalize & simplify GPR tools common switches support (development & maintenance)

The following code show how this object is used.
 
.. code-block:: ada

   declare
      Options : GPR2.Options.Object;
      Tree    : GPR2.Project.Tree.Object;
      Loaded  : Boolean;
   begin
      Options.Add_Switch (GPR2.Options.AP, "added-path");
      Options.Add_Switch (GPR2.Options.P, "test");
      Options.Add_Switch (GPR2.Options.Autoconf, "autoconf.cgpr");
      Options.Add_Switch (GPR2.Options.X, "BUILD=Debug");
      Options.Finalize;
      Loaded := Options.Load_Project (Tree);
   end;

GPR tools common supported switches are:
   
   * ``-aP`` (``GPR2.Options.AP``)

      -aP<dir> or -aP <dir> Add directory dir to project search path

   * ``--autoconf`` (``GPR2.Options.Autoconf``)

      --autoconf=file.cgpr Specify/create the main config project file name

   * ``--config`` (``GPR2.Options.Config``)

      --config=file.cgpr Specify the configuration project file name

   * ``--db`` (``GPR2.Options.Db``)

      --db dir Parse dir as an additional knowledge base

   * ``--db-`` (``GPR2.Options.Db_Minus``)

      --db- Do not load the standard knowledge base

   * ``--implicit-with`` (``GPR2.Options.Implicit_With``)

      --implicit-with=filename Add the given projects as a dependency on all loaded projects

   * ``--no-project`` (``GPR2.Options.No_Project``)

      --no-project Do not use project file

   * ``-P`` (``GPR2.Options.P``)

      -Pproj<.gpr> or -P proj<.gpr> Use Project File <proj>

   * ``--relocate-build-tree`` (``GPR2.Options.Relocate_Build_Tree``)

      --relocate-build-tree[=dir] Root obj/lib/exec dirs are current-directory or dir

   * ``--root-dir`` (``GPR2.Options.Root_Dir``)

      --root-dir=dir Root directory of obj/lib/exec to relocate

   * ``--RTS`` (``GPR2.Options.RTS``)

      | --RTS=<runtime> Use runtime <runtime> for language Ada
      | --RTS:<lang>=<runtime> Use runtime <runtime> for language <lang>

   * ``--src-subdirs`` (``GPR2.Options.Src_Subdirs``)

      --src-subdirs=dir Prepend <obj>/dir to the list of source dirs for each project

   * ``-subdirs`` (``GPR2.Options.Subdirs``)

      -subdirs=dir Use dir as suffix to obj/lib/exec directories

   * ``--target`` (``GPR2.Options.Target``)

      --target=targetname Specify a target for cross platforms

   * ``--unchecked-shared-lib-imports`` (``GPR2.Options.Unchecked_Shared_Lib_Imports``)

      --unchecked-shared-lib-imports Shared lib projects may import any project

   * ``-X`` (``GPR2.Options.X``)

      -Xnm=val or -X nm=val Specify an external reference for Project Files
