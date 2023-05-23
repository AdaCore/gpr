--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Indefinite_Ordered_Maps;

package GPR2.Build.Compilation_Unit.Maps is

   package Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Name_Type, Compilation_Unit.Object);

   type Map is new Maps.Map with null record;
   subtype Cursor is Maps.Cursor;

   Empty_Map : constant Map;

private

   Empty_Map : constant Map := Map'(Maps.Empty_Map with null record);

end GPR2.Build.Compilation_Unit.Maps;
