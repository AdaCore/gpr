--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Indefinite_Ordered_Maps;

--  We must include here all the parser engine for them to be registered as
--  there is no other places where those are withed.

pragma Warnings (Off);
with GPR2.Source_Info.Parser.ALI;
with GPR2.Source_Info.Parser.Ada_Language;
with GPR2.Source_Info.Parser.D;
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
      if Kind = None then
         for K in Implemented_Backend loop
            if Parser_Store.Contains (Key (Language, K)) then
               return True;
            end if;
         end loop;

         return False;

      else
         return Parser_Store.Contains (Key (Language, Kind));
      end if;
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
