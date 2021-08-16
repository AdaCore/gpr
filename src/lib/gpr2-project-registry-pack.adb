------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2021, AdaCore                      --
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

with Ada.Containers.Ordered_Maps;

package body GPR2.Project.Registry.Pack is

   type Package_Options is record
      Projects         : Projects_Kind;
      Check_Attributes : Boolean;
   end record;

   package Pack_Definition is new Ada.Containers.Ordered_Maps
     (Package_Id, Package_Options, "<");

   Store : Pack_Definition.Map;

   ---------
   -- Add --
   ---------

   procedure Add (Name : Package_Id; Projects : Projects_Kind) is
   begin
      Store.Insert (Name, (Projects, False));
   end Add;

   ----------------------------
   -- Attributes_Are_Checked --
   ----------------------------

   function Attributes_Are_Checked (Name : Package_Id) return Boolean is
      CS : constant Pack_Definition.Cursor := Store.Find (Name);
   begin
      return Pack_Definition.Has_Element (CS)
        and then Pack_Definition.Element (CS).Check_Attributes;
   end Attributes_Are_Checked;

   ----------------------
   -- Check_Attributes --
   ----------------------

   procedure Check_Attributes (Name : Package_Id; Flag : Boolean := True) is
   begin
      Store (Name).Check_Attributes := Flag;
   end Check_Attributes;

   ------------
   -- Exists --
   ------------

   function Exists (Name : Package_Id) return Boolean is
   begin
      return Store.Contains (Name);
   end Exists;

   -------------------
   -- Is_Allowed_In --
   -------------------

   function Is_Allowed_In
     (Name    : Package_Id;
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
   Add (Gnatstub,        Everywhere);
   Add (Metrics,         Everywhere);
   Add (Pretty_Printer,  Everywhere);
   Add (Stack,           Everywhere);
end GPR2.Project.Registry.Pack;
