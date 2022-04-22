------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2022, AdaCore                      --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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

   procedure Add (Name : Package_Id; Projects : Projects_Kind);
   --  Insert package in known packages

   function All_Packages return Containers.Package_Id_List;
   --  Retrieve the Id of all defined packages. No_Package is not part
   --  of the result.

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
