--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides to GPR library the known description of defined
--  packages.

with Ada.Containers; use Ada.Containers;

package GPR2.Project.Registry.Pack.Description is

   procedure Set_Package_Description (Key : Package_Id; Description : String);
   --  Set a package description

   function Get_Package_Description (Key : Package_Id) return String;
   --  Retrieves a description for a given package

private

   function Hash (Key : Package_Id) return Hash_Type;

   package Pack_Package_Description is new Indefinite_Hashed_Maps
     (Package_Id, String, Hash, "=", "=");

   Package_Description : Pack_Package_Description.Map;

end GPR2.Project.Registry.Pack.Description;
