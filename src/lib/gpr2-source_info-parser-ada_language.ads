--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package GPR2.Source_Info.Parser.Ada_Language is

   type Object is new Parser.Object
     (Language => GPR2.Ada_Language,
      Kind     => Source) with null record;

   overriding procedure Compute
     (Self   : not null access Object;
      Data   : in out Source_Info.Object'Class;
      Source : Project.Source.Object);
   --  Setup Data with the information from parsing Ada source file

   procedure Unregister;
   --  Unregister the Ada_Language source parser

end GPR2.Source_Info.Parser.Ada_Language;
