------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2020-2022, AdaCore                     --
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

with GPR2.C.JSON; use GPR2.C.JSON;
with GPR2.C.JSON.Encoders; use GPR2.C.JSON.Encoders;
with GPR2.C.Utils; use GPR2.C.Utils;

package body GPR2.C.View is

   ---------------
   -- Attribute --
   ---------------

   procedure Attribute
      (Request : GPR2.C.JSON.JSON_Value; Result : GPR2.C.JSON.JSON_Value)
   is
      Tree : constant GPR_Tree_Access :=
         Get_GPR_Tree (Request, "tree_id");
      View : constant GPR_View := To_GPR_View
         (Tree.all, Get (Request, "view_id"));
      Attr_Name : constant String := To_String (Get (Request, "name"));
      Pkg  : constant String := To_String (Get (Request, "pkg"), "");

      Filename : constant String := To_String (Get
         (Request, "filename"), "");
      Position : constant Unit_Index := To_Unit_Index
         (Get (Request, "position"), No_Unit_Index);
      Language : constant String := To_String
         (Get (Request, "language"), "");
      Name     : constant String := To_String (Get (Request, "index"), "");

      Attr : GPR_Attribute;
   begin
      if Filename'Length > 0 then
         Attr := GPR2.C.Utils.Attribute
            (View,
             Name  => Attr_Name,
             Pkg   => Pkg,
             Index => GPR2.C.Utils.Filename
                (Name => Filename, Position => Position));
      elsif Language'Length > 0 then
         Attr := GPR2.C.Utils.Attribute
            (View,
             Name  => Attr_Name,
             Pkg   => Pkg,
             Index => GPR2.C.Utils.Language (Language));
      elsif Name'Length > 0 then
         Attr := GPR2.C.Utils.Attribute
            (View,
             Name  => Attr_Name,
             Pkg   => Pkg,
             Index => GPR2.C.Utils.Name (Name => Name));
      else
         Attr := GPR2.C.Utils.Attribute
            (View,
             Name  => Attr_Name,
             Pkg   => Pkg);
      end if;

      Set (Result, "attribute", From_GPR_Attribute (Attr));
   end Attribute;

   ----------
   -- Load --
   ----------

   procedure Load (Request : JSON_Value; Result : JSON_Value) is
      Tree : constant GPR_Tree_Access := Get_GPR_Tree (Request, "tree_id");
      View : constant GPR_View := To_GPR_View
         (Tree.all, Get (Request, "view_id"));
   begin
      Set (Result, "id", From_GPR_View (View));
      Set (Result, "path", From_GPR_Path (View.Path_Name));
      Set (Result, "dir", From_GPR_Path (View.Dir_Name));
      Set (Result, "name", From_Name (View.Name));
      Set (Result, "kind", From_Project_Kind (View.Kind));
   end Load;

   -------------
   -- Sources --
   -------------

   procedure Sources (Request : JSON_Value; Result : JSON_Value) is
      Tree : constant GPR_Tree_Access :=
         Get_GPR_Tree (Request, "tree_id");
      View : constant GPR_View := To_GPR_View
         (Tree.all, Get (Request, "view_id"));
   begin
      Set (Result, "sources", From_GPR_Sources (View.Sources));
   end Sources;

   -----------
   -- Units --
   -----------

   procedure Units (Request : JSON_Value; Result : JSON_Value) is
      Tree : constant GPR_Tree_Access := Get_GPR_Tree (Request, "tree_id");
      View : constant GPR_View := To_GPR_View
         (Tree.all, Get (Request, "view_id"));
   begin
      Set (Result, "units", From_Unit_Infos (View.Units));
   end Units;

end GPR2.C.View;
