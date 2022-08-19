--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides to GPR library the known names of project packages
--  and some of their properties.
--  Custom packages can be added by custom tools.

package GPR2.Project.Registry.Pack is

   type Projects_Kind is array (Project_Kind) of Boolean
     with Pack,
          Dynamic_Predicate => Projects_Kind /= (Project_Kind => False);
   --  A boolean array describing what project kind if allowed

   Everywhere    : constant Projects_Kind := (others => True);

   No_Aggregates : constant Projects_Kind :=
                     (Aggregate_Kind => False, others => True);

   procedure Add (Name : Package_Id; Projects : Projects_Kind);
   --  Insert package in known packages

   function All_Packages return Containers.Package_Id_List;
   --  Retrieve the Id of all defined packages. Project_Level_Scope is not
   --  part of the result.

   function Exists (Name : Package_Id) return Boolean;
   --  Returns True if Name is a known package

   function Is_Allowed_In
     (Name    : Package_Id;
      Project : Project_Kind) return Boolean
     with Pre => Exists (Name);
   --  Returns True if the package is allowed in the given project

   function Attributes_Are_Checked (Name : Package_Id) return Boolean;
   --  Returns True if the attribute name should be checked for this package.
   --  Each tool has to call procedure Check_Attributes to define the set of
   --  packages relevant to this tool where attribute names should be checked.

   procedure Check_Attributes (Name : Package_Id; Flag : Boolean := True);
   --  Attribute names for the package should be checked or not depending on
   --  parameter Flag. If Check_Attributes is not called, then the attribute
   --  names are not going to be checked.

   --  Some common package names

   Binder          : constant Package_Id := +"binder";
   Builder         : constant Package_Id := +"builder";
   Clean           : constant Package_Id := +"clean";
   Compiler        : constant Package_Id := +"compiler";
   Gnatls          : constant Package_Id := +"gnatls";
   Install         : constant Package_Id := +"install";
   Linker          : constant Package_Id := +"linker";
   Naming          : constant Package_Id := +"naming";
   Remote          : constant Package_Id := +"remote";

end GPR2.Project.Registry.Pack;
