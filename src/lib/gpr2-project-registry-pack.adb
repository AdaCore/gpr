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

   type Package_Options is record
      Projects         : Projects_Kind;
      Check_Attributes : Boolean;
   end record;

   package Pack_Definition is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, Package_Options, "<");

   Store : Pack_Definition.Map;

   Everywhere    : constant Projects_Kind := (others => True);

   No_Aggregates : constant Projects_Kind :=
                     (Aggregate_Kind => False, others => True);

   procedure Store_Insert (Name : Name_Type; Projects : Projects_Kind);
   --  Insert package options into the Storage

   ----------------------------
   -- Attributes_Are_Checked --
   ----------------------------

   function Attributes_Are_Checked (Name : Name_Type) return Boolean is
      CS : constant Pack_Definition.Cursor := Store.Find (Name);
   begin
      return Pack_Definition.Has_Element (CS)
        and then Pack_Definition.Element (CS).Check_Attributes;
   end Attributes_Are_Checked;

   ----------------------
   -- Check_Attributes --
   ----------------------

   procedure Check_Attributes (Name : Name_Type; Flag : Boolean := True) is
   begin
      Store (Name).Check_Attributes := Flag;
   end Check_Attributes;

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
         return Store (Name).Projects (Project);
      else
         --  If the package is unknown, it is probably a user's package,
         --  let's assume it is allowed everywhere.
         return True;
      end if;
   end Is_Allowed_In;

   ------------------
   -- Store_Insert --
   ------------------

   procedure Store_Insert (Name : Name_Type; Projects : Projects_Kind) is
   begin
      Store.Insert (Name, (Projects, False));
   end Store_Insert;

begin
   Store_Insert (Naming,          No_Aggregates);
   Store_Insert (Compiler,        No_Aggregates);
   Store_Insert (Linker,          No_Aggregates);
   Store_Insert (Binder,          No_Aggregates);
   Store_Insert (Clean,           Everywhere);
   Store_Insert (Builder,         Everywhere);
   Store_Insert (Install,         (K_Aggregate => False, others => True));
   Store_Insert (Remote,          Everywhere);
   Store_Insert (Ide,             Everywhere);
   Store_Insert (Gnatls,          Everywhere);
   Store_Insert (Cross_Reference, Everywhere);
   Store_Insert (Finder,          Everywhere);
   Store_Insert (Eliminate,       Everywhere);
   Store_Insert (Check,           Everywhere);
   Store_Insert (Codepeer,        Everywhere);
   Store_Insert (Gnatstub,        Everywhere);
   Store_Insert (Metrics,         Everywhere);
   Store_Insert (Pretty_Printer,  Everywhere);
   Store_Insert (Stack,           Everywhere);
end GPR2.Project.Registry.Pack;
