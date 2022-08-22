--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Doubly_Linked_Lists;

package GPR2.Path_Name.Set is

   package Set is new Ada.Containers.Doubly_Linked_Lists (Object);

   subtype Object is Set.List;

   Empty_Set : constant Object := Set.Empty_List;

   function To_Set (Item : Path_Name.Object) return Object
     with Pre  => Item.Is_Defined,
          Post => To_Set'Result.First_Element = Item;
   --  Returns set constructed from single Item

end GPR2.Path_Name.Set;
