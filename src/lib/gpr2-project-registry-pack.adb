--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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

   ------------------
   -- All_Packages --
   ------------------

   function All_Packages return Containers.Package_Id_List is
      Result : Containers.Package_Id_List;
   begin
      for P in Store.Iterate loop
         declare
            Name : constant Package_Id := Pack_Definition.Key (P);
         begin
            Result.Insert (Name);
         end;
      end loop;

      return Result;
   end All_Packages;

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
      if Name = GPR2.Project_Level_Scope then
         return True;
      else
         return Store.Contains (Name);
      end if;
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
   Add (Gnatls,          Everywhere);
end GPR2.Project.Registry.Pack;
