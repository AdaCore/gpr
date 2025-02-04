--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Vectors;

package GPR2.Project.View.Vector is

   package Vector is new Ada.Containers.Vectors
      (Positive, GPR2.Project.View.Object);

   type Object is new Vector.Vector with private;

   Empty_Vector : constant Object;

private

   type Object is new Vector.Vector with null record;

   Empty_Vector : constant Object := (Vector.Empty_Vector with null record);

end GPR2.Project.View.Vector;
