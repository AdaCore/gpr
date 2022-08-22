--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package GPR2.Source_Info.Parser.D is

   type Object (Language : Language_Id) is
     new Parser.Object (Language, LI) with null record;

   overriding procedure Compute
     (Self   : not null access Object;
      Data   : in out Source_Info.Object'Class;
      Source : Project.Source.Object);
   --  Setup Data with the information from parsing Ada source file

end GPR2.Source_Info.Parser.D;
