------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2021, AdaCore                      --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Ordered_Maps;

--  We must include here all the parser engine for them to be registered as
--  there is no other places where those are withed.

pragma Warnings (Off);
with GPR2.Source_Info.Parser.ALI;
with GPR2.Source_Info.Parser.Ada_Language;
pragma Warnings (On);

package body GPR2.Source_Info.Parser.Registry is

   package Parser_Set is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, Object_Ref);

   Parser_Store : Parser_Set.Map;
   --  Record all parser for given language and kind

   function Key (Language : Language_Id; Kind : Backend) return Name_Type is
     (GPR2."&" (Name (Language), Name_Type ("@" & Backend'Image (Kind))));
   --  The key used in the parser store

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache is
   begin
      for P of Parser_Store loop
         P.Clear_Cache;
      end loop;
   end Clear_Cache;

   ------------
   -- Exists --
   ------------

   function Exists (Language : Language_Id; Kind : Backend) return Boolean is
   begin
      return Parser_Store.Contains (Key (Language, Kind));
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (Language : Language_Id; Kind : Backend)
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
        (Key (Parser.Language, Parser.Kind), Parser.Self);
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Parser : Object'Class) is
   begin
      Parser_Store.Delete (Key (Parser.Language, Parser.Kind));
   end Unregister;

end GPR2.Source_Info.Parser.Registry;
