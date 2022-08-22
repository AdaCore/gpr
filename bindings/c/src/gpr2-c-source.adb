--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.C.Utils; use GPR2.C.Utils;
with GPR2.C.JSON.Encoders; use GPR2.C.JSON.Encoders;

package body GPR2.C.Source is

   function Get_Source (Request : JSON_Value) return GPR_Source;

   ------------------
   -- Dependencies --
   ------------------

   procedure Dependencies (Request : JSON_Value; Result : JSON_Value) is
      Src     : constant GPR_Source := Get_Source (Request);
      Closure : constant Boolean :=
         To_Boolean (Get (Request, "closure"), False);
   begin
      Set (Result, "sources", From_GPR_Sources
         (Dependencies (Src, Closure => Closure)));
   end Dependencies;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source (Request : JSON_Value) return GPR_Source is
      Tree : constant GPR_Tree_Access := Get_GPR_Tree (Request, "tree_id");
      View : constant GPR_View := To_GPR_View
         (Tree.all, Get (Request, "view_id"));
      Source_Path : constant String := To_String (Get (Request, "path"));
   begin
      return GPR2.C.Utils.Source (View => View, Path => Source_Path);
   end Get_Source;

   -------------------------
   -- Update_Source_Infos --
   -------------------------

   procedure Update_Source_Infos (Request : JSON_Value; Result : JSON_Value) is
      Src : GPR_Source := Get_Source (Request);
      Allow_Source_Parsing : constant Boolean := To_Boolean
         (Get (Request, "allow_source_parsing"), Default => False);
   begin
      GPR2.C.Utils.Update_Source_Infos
         (Source => Src, Allow_Source_Parsing => Allow_Source_Parsing);
      Set (Result, "dependencies", From_GPR_Sources (Dependencies (Src)));
   end Update_Source_Infos;

end GPR2.C.Source;
