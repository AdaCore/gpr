--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Vectors;

package GPR2.Project.View.Vector is

   package Vector is new Ada.Containers.Vectors
      (Positive, GPR2.Project.View.Object);

   subtype Object is Vector.Vector;

   Empty_Vector : constant Object := Vector.Empty_Vector;

end GPR2.Project.View.Vector;
