--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package GPR2.Source_Info.Parser.Registry is

   procedure Register (Parser : Object'Class);
   --  Register a source info parser

   procedure Unregister (Parser : Object'Class);
   --  Unregister a source info parser

   function Exists (Language : Language_Id; Kind : Backend) return Boolean;
   --  Returns True if the parser backend for Language is found. If Kind is
   --  None then True is returned if either an LI or Source parser exists.

   function Get
     (Language : Language_Id; Kind : Backend)
      return not null access Object'Class
     with Pre => Exists (Language, Kind);
   --  Get a parser for the given langugae and kind. If Kind if Auto then the
   --  LI based parser is returned and the Source based otherwise.

   procedure Clear_Cache;
   --  Clear cache for all registered parsers. While parsing it is possible for
   --  each parser to store in cache some information (Ada ALI parser does that
   --  as the same ALI constains information for possibly two units). After
   --  updating all the sources for a view one can clear the cache to free
   --  up the memory.

end GPR2.Source_Info.Parser.Registry;
