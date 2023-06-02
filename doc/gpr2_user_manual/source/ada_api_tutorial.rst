****************
Ada API tutorial
****************

Preliminary setup
=================

In order to compile/run GPR2 tutorial examples you need to have GNAT compiler
and libgpr2 library installed. Preliminary setup can be checked running

.. code-block:: shell

   # Checking the GPR2 library installation
   $ gprls -P gpr2


The first thing to do in order to use gpr2 is to load a project file:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   with GPR2.Containers;
   with GPR2.Context;
   with GPR2.Path_Name;
   with GPR2.Project.Attribute;
   with GPR2.Project.Attribute_Index;
   with GPR2.Project.Registry.Attribute;
   with GPR2.Project.Registry.Pack;
   with GPR2.Project.Source;
   with GPR2.Project.Tree;
   with GPR2.Project.Typ;
   with GPR2.Project.Unit_Info;
   with GPR2.Project.Variable;
   with GPR2.Project.View;
   with GPR2.Source_Info;
   with GPR2.Unit;

   procedure Main is
      Context : GPR2.Context.Object;
      --  Context used to set scenarios

      Tree    : GPR2.Project.Tree.Object;
      --  "gpr2_tutorial.gpr" tree.

   --  Insert tutorials here.

   begin
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File ("gpr2_test.gpr"),
         Context  => Context);
      Tree.Log_Messages.all.Output_Messages;

   --  Uncomment tutorial call
   --   Views;
   --   Scenarios;
   --   Packages;
   --   Attributes;
   --   Types;
   --   Variables;
   --   Sources;
   --   Units;
   --   Registry;
   end Main;

This very simple program will allow us to make sure the build environment is
properly set to use gpr2. Save the above in a ``main.adb`` source file,
and then write the following project file to ``gpr2_test.gpr``:

.. code-block:: ada

   with "gpr2";

   project GPR2_Test is
      for Main use ("main.adb");
      for Object_Dir use "obj";
   end GPR2_Test;

Now you can run ``gprbuild`` to compile the test program:

.. code-block:: shell

    $ gprbuild -Pgpr2_test

This command should return without error and create an executable in
``obj/main`` or ``obj\main.exe`` depending on your platform. The last step is
to check if the program works properly: it should do nothing, so no errors
expected!

.. code-block:: shell

   # Empty program output
   $ obj/main
   $


Views
=====

When loading a gpr file in a Tree object a lot of gpr & cgpr files are loaded.
In autoconf mode a configuration project is automatically generated.
A configuration, a runtime project and all imported,
extended or aggregated projects are recursively loaded.

All of these projects are available through ``GPR2.Project.View.Object`` objects.
After a successful load, ``Tree.Root_Project`` contains the project passed when loading the tree.

.. code-block:: ada

   procedure Views is
   begin
      --  How to get views of a loaded tree.
      
      New_Line;
      Put_Line ("GPR2 project views");
      
      --  root, configuration & runtime projects
      
      Put_Line ("gpr2_test.gpr at " & Tree.Root_Project.Path_Name.Value);
      Put_Line ("generated configuration project at " &
                  Tree.Configuration.Corresponding_View.Path_Name.Value);
      Put_Line ("used runtime project at " &
                  Tree.Runtime_Project.Path_Name.Value);
      
      --  View can be retrieved by name
      
      Put_Line ("gpr2.gpr at " &
                  Tree.Root_Project.View_For ("gpr2").Path_Name.Value);

      --  Using a configurable iterator
 
      New_Line;
      for C in Tree.Iterate loop
         Put_Line ("view: " &
                     GPR2.Project.Tree.Element (C).Path_Name.Value);
      end loop;
      
      --  Using view's accessor functions
      
      New_Line;
      for V of Tree.Root_Project.Imports loop
         Put_Line ("imported: " & V.Path_Name.Value);
      end loop;
      
   end Views;

As for all tutorial examples don't forget to add procedure call in ``Main`` procedure.

.. code-block:: ada

   begin
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File ("gpr2_test.gpr"),
         Context  => Context);
      Tree.Log_Messages.all.Output_Messages;

   --  Uncomment tutorial call
      Views;
   --   Scenarios;
   --   Packages;
   --   Attributes;
   --   Types;
   --   Variables;
   --   Sources;
   --   Units;
   --   Registry;
   end Main;


Scenarios
=========

Various project properties can be modified based on scenarios.
The above code shows how scenarios are handled by the GPR2 library.

.. code-block:: ada

   procedure Scenarios is
      GPR2_View : constant GPR2.Project.View.Object :=
                    Tree.Root_Project.View_For ("gpr2");
   begin
      --  Print current GPR2 Library_Kind using default scenarios values.

      New_Line;
      Put_Line ("Library_Kind (default): " &
                  String (GPR2_View.Library_Kind));

      --  Change LIBRARY_TYPE to relocatable & print modified Library_Kind.

      Context.Insert ("LIBRARY_TYPE", "relocatable");
      Tree.Set_Context (Context);
      Put_Line ("Library_Kind (relocatable): " &
                  String (GPR2_View.Library_Kind));
   end Scenarios;


Packages
========

This tutorial shows how view's packages are listed.
Attributes, variables and types parts will explain how packages are handled
when accessing package content.

.. code-block:: ada

   procedure Packages is
      GPR2_View : constant GPR2.Project.View.Object :=
                    Tree.Root_Project.View_For ("gpr2");

       procedure Print (With_Defaults : Boolean; With_Config : Boolean) is
         Print_Comma : Boolean := False;
      begin
         if With_Defaults then
            Put ("including packages defined by default values: ");
         end if;
         if With_Config then
            Put ("including packages defined by configuration file: ");
         end if;
         for Id of GPR2_View.Packages (With_Defaults, With_Config) loop
            if Print_Comma then
               Put (", ");
            end if;
            Print_Comma := True;
            Put (GPR2.Image (Id));
         end loop;
         New_Line;
      end Print;
   begin
      New_Line;
      Put_Line ("GPR2 project packages");
      Print (False, False);
      Print (True, False);
      Print (False, True);
   end Packages;


Attributes
==========

This tutorial shows how attributes can be accessed or listed.

.. code-block:: ada

   procedure Attributes is
      GPR2_View : constant GPR2.Project.View.Object :=
                    Tree.Root_Project.View_For ("gpr2");

      procedure Print (Attribute : GPR2.Project.Attribute.Object);
      procedure Print (Attribute : GPR2.Project.Attribute.Object) is
         use GPR2;
         use GPR2.Project.Registry.Attribute;
      begin
         Put ("for " & Image (Attribute.Name.Id.Attr));
         if Attribute.Has_Index then
            Put (" (""" & Attribute.Index.Text & """");
            if Attribute.Index.Has_At_Pos then
               Put (" at " & Attribute.Index.At_Pos'Image);
            end if;
            Put (")");
         end if;
         if Attribute.Kind = GPR2.Project.Registry.Attribute.Single then
            Put (" use """ & Attribute.Value.Text & """");
            if Attribute.Value.Has_At_Pos then
               Put (" at " & Attribute.Value.At_Pos'Image);
            end if;
         else
            Put (" use " & GPR2.Containers.Image (Attribute.Values));
         end if;
         Put (";");
         if Attribute.Is_Default then
            Put (" -- default value");
         end if;
         if Attribute.Is_From_Config then
            Put (" -- from configuration project");
         end if;
         New_Line;
      end Print;

   begin
      New_Line;
      Put_Line ("Compiler.Default_Switches (""Ada"") attribute");
      Print
        (GPR2_View.Attribute
           (Name  => GPR2.Project.Registry.Attribute.Compiler.Default_Switches,
            Index => GPR2.Project.Attribute_Index.Create (GPR2.Ada_Language)));

      New_Line;
      Put_Line ("GPR2 project attributes");
      for Attribute of GPR2_View.Attributes loop
         Print (Attribute);
      end loop;
   end Attributes;


Types
=====

This tutorial shows how variables types can be accessed/listed.

.. code-block:: ada

   procedure Types is
      GPR2_View : constant GPR2.Project.View.Object :=
                    Tree.Root_Project.View_For ("gpr2");
 
      procedure Print (Typ : GPR2.Project.Typ.Object) is
         Print_Comma : Boolean := False;
      begin
         Put (String (Typ.Name.Text) & ", values: ");
         for Value of Typ.Values loop
            if Print_Comma then
               Put (", ");
            else
               Print_Comma := True;
            end if;
            Put (Value.Text);
         end loop;
         New_Line;
      end Print;
   begin
      New_Line;
      Put_Line ("GPR2 project types");
      Print (GPR2_View.Typ ("bool"));

      New_Line;
      Put_Line ("GPR2 project types");
      if GPR2_View.Has_Types then
         for Typ of GPR2_View.Types loop
            Print (Typ);
         end loop;
      end if;
   end Types;


Variables
=========

This tutorial shows how project level or packages variables can be accessed/listed.

.. code-block:: ada

   procedure Variables is
      GPR2_View : constant GPR2.Project.View.Object :=
                    Tree.Root_Project.View_For ("gpr2");

      procedure Print (Variable : GPR2.Project.Variable.Object);
      procedure Print (Variable : GPR2.Project.Variable.Object) is
         use GPR2.Project.Registry.Attribute; --  "=" function visibility
      begin
         Put (String (Variable.Name.Text));
         if Variable.Has_Type then
            Put (", type: " & String (Variable.Typ.Name.Text));
         else
            Put (" with no type");
         end if;
         if Variable.Kind = GPR2.Project.Registry.Attribute.Single then
            Put (", value: " & String (Variable.Value.Text));
         else
            Put (", values: " & GPR2.Containers.Image (Variable.Values));
         end if;
         New_Line;
      end Print;
   begin
      New_Line;
      Put_Line
        ("GPR2 project build & compiler.langkit_parser_options variable");
      Print (GPR2_View.Variable ("build"));
      if GPR2_View.Has_Variables
        (GPR2.Project.Registry.Pack.Compiler, "langkit_parser_options")
      then
         Print (GPR2_View.Variable
                (GPR2.Project.Registry.Pack.Compiler,
                   "langkit_parser_options"));
      end if;

      New_Line;
      Put_Line ("GPR2 project variables");
      if GPR2_View.Has_Variables then
         for Variable of GPR2_View.Variables loop
            Print (Variable);
         end loop;
      end if;

      New_Line;
      Put_Line ("GPR2 project Compiler package variables");
      if GPR2_View.Has_Variables (GPR2.Project.Registry.Pack.Compiler) then
         for Variable of
           GPR2_View.Variables (GPR2.Project.Registry.Pack.Compiler) loop
            Print (Variable);
         end loop;
      end if;
   end Variables;


Sources
=======

This tutorial shows how projects sources are parsed, listed, accessed.

.. code-block:: ada

   procedure Sources is
      GPR2_View : constant GPR2.Project.View.Object :=
                    Tree.Root_Project.View_For ("gpr2");
      Source    : GPR2.Project.Source.Object;
      Part      : GPR2.Project.Source.Source_Part;
      use GPR2.Unit;
   begin

      --  Update_sources required after load to to get a source unless
      --  Tree.For_Each_Source or View.Sources was called.

      Source := GPR2_View.Source ("gpr2-project-tree.ads");
      if not Source.Is_Defined then
         Tree.Update_Sources
           (Stop_On_Error => True,
            With_Runtime  => False,
            Backends      => GPR2.Source_Info.All_Backends);

         Source := GPR2_View.Source ("gpr2-project-tree.ads");
      end if;

      New_Line;
      Put_Line ("gpr2-project-tree.ads source");
      Source := GPR2_View.Source ("gpr2-project-tree.ads");
      Put_Line (" kind is " & Source.Kind'Image);
      Put_Line (" unit name is " & String (Source.Unit_Name));

      if Source.Has_Other_Part then
         Put_Line
           (" other part is"
            & String (Source.Other_Part.Source.Path_Name.Simple_Name));
      end if;

      --  Separate file.

      Source := GPR2_View.Source ("gpr2-project-tree-load_autoconf.adb");
      if Source.Is_Defined and then Source.Kind = GPR2.Unit.S_Separate then
         Put_Line ("gpr2-project-tree-load_autoconf.adb unit name is "
                   & String (Source.Unit_Name));
         Part := Source.Separate_From (GPR2.No_Index);
         Put_Line ("gpr2-project-tree-load_autoconf.adb separate from "
                   & String (Part.Source.Path_Name.Simple_Name));
      end if;
   end Sources;


Units
=====

This tutorial shows how units are listed & accessed

.. code-block:: ada

   procedure Units is
      GPR2_View : constant GPR2.Project.View.Object :=
                    Tree.Root_Project.View_For ("gpr2");
      Unit : GPR2.Project.Unit_Info.Object :=
               GPR2_View.Unit ("gpr2.project.tree");

      procedure Print
        (Prefix : String; SUI : GPR2.Unit.Source_Unit_Identifier) is
         use GPR2;
      begin
         Put (Prefix & String (SUI.Source.Simple_Name));
         if SUI.Index /= No_Index then
            Put (" at " & SUI.Index'Image);
         end if;
         New_Line;
      end Print;
   begin
      if GPR2_View.Units.Is_Empty then

         --  Update_sources required after load to get a source or unit
         --  unless Tree.For_Each_Source or View.Sources was called.
         Put_Line ("Tree.Update_Sources");

         Tree.Update_Sources
           (Stop_On_Error => True,
            With_Runtime  => False,
            Backends      => GPR2.Source_Info.All_Backends);

      end if;

      Unit := GPR2_View.Unit ("gpr2.project.tree");

      New_Line;
      Put_Line ("Unit:" & String (Unit.Name));
      if Unit.Has_Spec then
         Print (" Specification in ", Unit.Spec);
      end if;
      if Unit.Has_Body then
         Print (" Body in ", Unit.Main_Body);
      end if;
      declare
         Separates : constant GPR2.Unit.Source_Unit_Vectors.Vector :=
                       Unit.Separates;
      begin
         if not Separates.Is_Empty then
            for S of Separates loop
               Print (" Separate in ", S);
            end loop;
         end if;
      end;
    end Units;


Custom packages & attributes
============================

This tutorial shows as custom packages & attributes can be added to gpr2
package/attribute registry. This should be done before loading projects.

.. code-block:: ada

   procedure Registry is
      use GPR2;

      Custom_Package_Id   : constant Package_Id := +"custom";
      Custom_Attribute_Id : constant Q_Attribute_Id :=
                              (Custom_Package_Id, +"new_attribute");
   begin
      --  Add package

      Project.Registry.Pack.Add
        (Name     => Custom_Package_Id,
         Projects => GPR2.Project.Registry.Pack.Everywhere);

      --  Add new attribute
      GPR2.Project.Registry.Attribute.Add
        (Name                 => Custom_Attribute_Id,
         Index_Type           =>
           Project.Registry.Attribute.FileGlob_Or_Language_Index,
         Index_Optional       => True,
         Value                => Project.Registry.Attribute.List,
         Value_Case_Sensitive => True,
         Is_Allowed_In        => Project.Registry.Attribute.Everywhere,
         Config_Concatenable  => True);
   end Registry;
