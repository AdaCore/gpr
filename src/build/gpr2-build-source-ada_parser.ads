--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package GPR2.Build.Source.Ada_Parser is

   procedure Compute
     (Data             : in out Source.Object'Class;
      Get_Withed_Units : Boolean);
   --  Setup Data with the information from parsing Ada source file

end GPR2.Build.Source.Ada_Parser;
