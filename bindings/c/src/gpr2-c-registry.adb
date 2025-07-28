--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

with GPR2.Project.Tree.C;
with GPR2.Project.View.C;

package body GPR2.C.Registry is

   ----------
   -- Tree --
   ----------

   package body Tree is

      package Tree_Maps is new
        Ada.Containers.Hashed_Maps
          (Ada.Strings.Unbounded.Unbounded_String,
           GPR2.Project.Tree.Object,
           Ada.Strings.Unbounded.Hash,
           Ada.Strings.Unbounded."=",
           GPR2.Project.Tree."=");

      Tree_Map : Tree_Maps.Map;

      ------------
      -- Lookup --
      ------------

      function Lookup
        (Id : GPR2.C.JSON.Values.JSON_Value) return GPR2.Project.Tree.Object
      is
         S : constant Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.To_Unbounded_String (Id.To_String);

      begin
         if Tree_Map.Contains (S) then
            return Tree_Map (S);

         else
            return GPR2.Project.Tree.Undefined;
         end if;
      end Lookup;

      --------------
      -- Register --
      --------------

      function Register
        (Tree : GPR2.Project.Tree.Object) return GPR2.C.JSON.Values.JSON_Value
      is
         Id : constant Unbounded_String := GPR2.Project.Tree.C.Id (Tree);

      begin
         if Tree.Is_Defined then
            if not Tree_Map.Contains (Id) then
               Tree_Map.Insert (Id, Tree);
            end if;
         end if;

         return GPR2.C.JSON.Values.To_JSON_Value (To_String (Id));
      end Register;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister (Id : GPR2.C.JSON.Values.JSON_Value) is
      begin
         Tree_Map.Delete
           (Ada.Strings.Unbounded.To_Unbounded_String (Id.To_String));
      end Unregister;

   end Tree;

   package body View is

      package View_Maps is new
        Ada.Containers.Hashed_Maps
          (Ada.Strings.Unbounded.Unbounded_String,
           GPR2.Project.View.Object,
           Ada.Strings.Unbounded.Hash,
           Ada.Strings.Unbounded."=",
           GPR2.Project.View."=");

      View_Map : View_Maps.Map;

      ------------
      -- Lookup --
      ------------

      function Lookup
        (Id : GPR2.C.JSON.Values.JSON_Value) return GPR2.Project.View.Object
      is
         S : constant Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.To_Unbounded_String (Id.To_String);

      begin
         if View_Map.Contains (S) then
            return View_Map (S);

         else
            return GPR2.Project.View.Undefined;
         end if;
      end Lookup;

      --------------
      -- Register --
      --------------

      function Register
        (View : GPR2.Project.View.Object) return GPR2.C.JSON.Values.JSON_Value
      is
         Id : constant Unbounded_String := GPR2.Project.View.C.Id (View);

      begin
         if Id = "" then
            return GPR2.C.JSON.Values.Null_Value;

         else
            if View.Is_Defined then
               if not View_Map.Contains (Id) then
                  View_Map.Insert (Id, View);
               end if;
            end if;

            return GPR2.C.JSON.Values.To_JSON_Value (To_String (Id));
         end if;
      end Register;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister (Id : GPR2.C.JSON.Values.JSON_Value) is
      begin
         View_Map.Delete
           (Ada.Strings.Unbounded.To_Unbounded_String (Id.To_String));
      end Unregister;

   end View;

end GPR2.C.Registry;
