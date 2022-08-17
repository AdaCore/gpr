--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Ordered_Maps;

with GPR2.Build.Object_Info.Parser.ALI;
--  with GPR2.Object_Info.Parser.Ada_Language;
--  with GPR2.Object_Info.Parser.D;

package body GPR2.Build.Object_Info.Parser.Registry is

   type Optional_Object_Ref is access all Object'Class;

   type Parser_List is array (Implemented_Backend) of Optional_Object_Ref;

   package Parser_Set is new Ada.Containers.Ordered_Maps
     (Language_Id, Parser_List);

   Parser_Store : Parser_Set.Map;
   --  Record all parser for given language and kind

   The_ALI_Parser : aliased ALI.Object;
   --  The_Ada_Parser : GPR2.Object_Info.Parser.Ada_Language;
   --  The_D_Parser   : GPR2.Object_Info.Parser.D;

   procedure Register
     (Lang   : Language_Id;
      Parser : Object_Ref);

   ------------
   -- Exists --
   ------------

   function Exists (Language : Language_Id; Kind : Backend) return Boolean is
      Cursor : constant Parser_Set.Cursor := Parser_Store.Find (Language);
   begin
      return Parser_Set.Has_Element (Cursor)
        and then Parser_Set.Element (Cursor) (Kind) /= null;
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (Language : Language_Id; Kind : Backend)
      return Object_Ref is
   begin
      return Object_Ref (Parser_Store (Language) (Kind));
   end Get;

   --------------
   -- Register --
   --------------

   procedure Register (Lang   : Language_Id;
                       Parser : Object_Ref) is
      Cursor : Parser_Set.Cursor;
      Done   : Boolean;
   begin
      Parser_Store.Insert
        (Lang, (others => null), Cursor, Done);

      Parser_Store.Reference (Cursor) (Parser.Kind) :=
        Optional_Object_Ref (Parser);
   end Register;

begin

   Register (Ada_Language, The_ALI_Parser'Access);

end GPR2.Build.Object_Info.Parser.Registry;
