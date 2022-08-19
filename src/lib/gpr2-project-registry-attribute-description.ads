--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides to GPR library the known description of defined at-
--  tributes.

with Ada.Containers; use Ada.Containers;

package GPR2.Project.Registry.Attribute.Description is

   function Get_Attribute_Description (Key : Q_Attribute_Id)
                                       return String;
   --  Retrieve a description for a given attribute and package.

private

   function Hash (Key : Q_Attribute_Id) return Hash_Type;

   package Pack_Attribute_Description is new Indefinite_Hashed_Maps
     (Q_Attribute_Id, String, Hash, "=", "=");

   Attribute_Description : Pack_Attribute_Description.Map;

end GPR2.Project.Registry.Attribute.Description;
