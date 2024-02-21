--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Project.Tree;

package GPR2.Build.Source_Base.Ada_Parser is

   procedure Compute
     (Tree             : access GPR2.Project.Tree.Object;
      Data             : in out Source_Base.Object'Class;
      Get_Withed_Units : Boolean;
      Success          : out Boolean);
   --  Setup Data with the information from parsing Ada source file

end GPR2.Build.Source_Base.Ada_Parser;
