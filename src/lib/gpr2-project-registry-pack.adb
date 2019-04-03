------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Containers.Indefinite_Ordered_Maps;

package body GPR2.Project.Registry.Pack is

   type Projects_Kind is array (Project_Kind) of Boolean
     with Pack,
       Dynamic_Predicate => Projects_Kind /= (Project_Kind => False);
   --  A boolean array describing what project kind if allowed

   package Pack_Definition is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, Projects_Kind, "<");

   Store : Pack_Definition.Map;

   Everywhere    : constant Projects_Kind := (others => True);

   No_Aggregates : constant Projects_Kind :=
                     (Aggregate_Kind => False, others => True);

   ------------
   -- Exists --
   ------------

   function Exists (Name : Name_Type) return Boolean is
   begin
      return Store.Contains (Name);
   end Exists;

   -------------------
   -- Is_Allowed_In --
   -------------------

   function Is_Allowed_In
     (Name    : Name_Type;
      Project : Project_Kind) return Boolean is
   begin
      if Store.Contains (Name) then
         return Store (Name) (Project);
      else
         --  If the package is unknown, it is probably a user's package,
         --  let's assume it is allowed everywhere.
         return True;
      end if;
   end Is_Allowed_In;

begin
   Store.Insert (Naming,          No_Aggregates);
   Store.Insert (Compiler,        No_Aggregates);
   Store.Insert (Linker,          No_Aggregates);
   Store.Insert (Binder,          No_Aggregates);
   Store.Insert (Install,         (K_Aggregate => False, others => True));
   Store.Insert (Remote,          Everywhere);
   Store.Insert (Ide,             Everywhere);
   Store.Insert (Builder,         Everywhere);
   Store.Insert (Gnatls,          Everywhere);
   Store.Insert (Clean,           Everywhere);
   Store.Insert (Cross_Reference, Everywhere);
   Store.Insert (Finder,          Everywhere);
   Store.Insert (Eliminate,       Everywhere);
   Store.Insert (Check,           Everywhere);
   Store.Insert (Codepeer,        Everywhere);
   Store.Insert (Gnatstub,        Everywhere);
   Store.Insert (Metrics,         Everywhere);
   Store.Insert (Pretty_Printer,  Everywhere);
   Store.Insert (Stack,           Everywhere);
end GPR2.Project.Registry.Pack;
