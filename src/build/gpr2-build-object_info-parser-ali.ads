--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package GPR2.Build.Object_Info.Parser.ALI is

   type Object is new Parser.Object (Kind => LI) with private;

   overriding procedure Compute
     (Self     : Object;
      Data     : in out Object_Info.Object'Class;
      Messages : in out Log.Object);
   --  Setup Data with the information from GNAT .ali file

private

   type Object is new Parser.Object (Kind => LI) with null record;

end GPR2.Build.Object_Info.Parser.ALI;
