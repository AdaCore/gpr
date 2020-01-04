------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Ordered_Maps;

--  We must include here all the parser engine for them to be registered as
--  there is no other places where those are withed.

pragma Warnings (Off);
with GPR2.Source_Info.Parser.Ada_Language;
with GPR2.Source_Info.Parser.ALI;
pragma Warnings (On);

package body GPR2.Source_Info.Parser.Registry is

   package Parser_Set is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, Object_Ref);

   Parser_Store : Parser_Set.Map;
   --  Record all parser for given language and kind

   function Key (Language : Name_Type; Kind : Backend) return Name_Type is
     (Name_Type (Characters.Handling.To_Lower (String (Language))
                 & '@' & Backend'Image (Kind)));
   --  The key used in the parser store

   ------------
   -- Exists --
   ------------

   function Exists (Language : Name_Type; Kind : Backend) return Boolean is
   begin
      return Parser_Store.Contains (Key (Language, Kind));
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (Language : Name_Type; Kind : Backend)
      return not null access Object'Class is
   begin
      return Parser_Store (Key (Language, Kind));
   end Get;

   --------------
   -- Register --
   --------------

   procedure Register (Parser : Object'Class) is
   begin
      Parser_Store.Insert
        (Key (Parser.Language.all, Parser.Kind), Parser.Self);
   end Register;

end GPR2.Source_Info.Parser.Registry;
