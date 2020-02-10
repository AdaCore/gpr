------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

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

   procedure Add (Name : Name_Type; Projects : Projects_Kind);
   --  Insert package in known packages

   function Exists (Name : Name_Type) return Boolean;
   --  Returns True if Name is a known package

   function Is_Allowed_In
     (Name    : Name_Type;
      Project : Project_Kind) return Boolean
     with Pre => Exists (Name);
   --  Returns True if the package is allowed in the given project

   function Attributes_Are_Checked (Name : Name_Type) return Boolean;
   --  Returns True if the attribute name should be checked for this package.
   --  Each tool has to call procedure Check_Attributes to define the set of
   --  packages relevant to this tool where attribute names should be checked.

   procedure Check_Attributes (Name : Name_Type; Flag : Boolean := True);
   --  Attribute names for the package should be checked or not depending on
   --  parameter Flag. If Check_Attributes is not called, then the attribute
   --  names are not going to be checked.

   --  Some common package names

   Binder          : constant Name_Type := "binder";
   Builder         : constant Name_Type := "builder";
   Check           : constant Name_Type := "check";
   Clean           : constant Name_Type := "clean";
   Codepeer        : constant Name_Type := "codepeer";
   Compiler        : constant Name_Type := "compiler";
   Cross_Reference : constant Name_Type := "cross_reference";
   Eliminate       : constant Name_Type := "eliminate";
   Finder          : constant Name_Type := "finder";
   Gnatls          : constant Name_Type := "gnatls";
   Gnatstub        : constant Name_Type := "gnatstub";
   Ide             : constant Name_Type := "ide";
   Install         : constant Name_Type := "install";
   Linker          : constant Name_Type := "linker";
   Metrics         : constant Name_Type := "metrics";
   Naming          : constant Name_Type := "naming";
   Pretty_Printer  : constant Name_Type := "pretty_printer";
   Remote          : constant Name_Type := "remote";
   Stack           : constant Name_Type := "stack";

end GPR2.Project.Registry.Pack;
