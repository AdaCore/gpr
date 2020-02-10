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

with Ada.Containers.Indefinite_Ordered_Maps;

package body GPR2.Project.Registry.Pack is

   type Package_Options is record
      Projects         : Projects_Kind;
      Check_Attributes : Boolean;
   end record;

   package Pack_Definition is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, Package_Options, "<");

   Store : Pack_Definition.Map;

   ---------
   -- Add --
   ---------

   procedure Add (Name : Name_Type; Projects : Projects_Kind) is
   begin
      Store.Insert (Name, (Projects, False));
   end Add;

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

begin
   Add (Naming,          No_Aggregates);
   Add (Compiler,        No_Aggregates);
   Add (Linker,          No_Aggregates);
   Add (Binder,          No_Aggregates);
   Add (Clean,           Everywhere);
   Add (Builder,         Everywhere);
   Add (Install,         (K_Aggregate => False, others => True));
   Add (Remote,          Everywhere);
   Add (Ide,             Everywhere);
   Add (Gnatls,          Everywhere);
   Add (Cross_Reference, Everywhere);
   Add (Finder,          Everywhere);
   Add (Eliminate,       Everywhere);
   Add (Check,           Everywhere);
   Add (Codepeer,        Everywhere);
   Add (Gnatstub,        Everywhere);
   Add (Metrics,         Everywhere);
   Add (Pretty_Printer,  Everywhere);
   Add (Stack,           Everywhere);
end GPR2.Project.Registry.Pack;
