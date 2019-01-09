------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2019, Free Software Foundation, Inc.          --
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
   Store.Insert ("naming",          No_Aggregates);
   Store.Insert ("compiler",        No_Aggregates);
   Store.Insert ("linker",          No_Aggregates);
   Store.Insert ("binder",          No_Aggregates);
   Store.Insert ("install",         (K_Aggregate => False, others => True));
   Store.Insert ("remote",          Everywhere);
   Store.Insert ("ide",             Everywhere);
   Store.Insert ("builder",         Everywhere);
   Store.Insert ("gnatls",          Everywhere);
   Store.Insert ("clean",           Everywhere);
   Store.Insert ("cross_reference", Everywhere);
   Store.Insert ("finder",          Everywhere);
end GPR2.Project.Registry.Pack;
