--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Parser objects registry. To add a new parser, the body of this package
--  needs to be modified.

package GPR2.Build.Object_Info.Parser.Registry is

   function Exists (Language : Language_Id; Kind : Backend) return Boolean;
   --  Returns True if the parser backend for Language is found. If Kind is
   --  None then True is returned if either an LI or Source parser exists.

   function Get
     (Language : Language_Id; Kind : Backend)
      return Object_Ref
     with Pre => Exists (Language, Kind);
   --  Get a parser for the given langugae and kind. If Kind if Auto then the
   --  LI based parser is returned and the Source based otherwise.

end GPR2.Build.Object_Info.Parser.Registry;
