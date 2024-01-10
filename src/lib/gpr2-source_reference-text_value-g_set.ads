--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Ordered_Sets;

generic
package GPR2.Source_Reference.Text_Value.G_Set is

   package Set is new Ada.Containers.Indefinite_Ordered_Sets (Object'Class);

   subtype Object is Set.Set;

   subtype Cursor is Set.Cursor;

   Empty_Set : constant Object := Set.Empty_Set;

end GPR2.Source_Reference.Text_Value.G_Set;
