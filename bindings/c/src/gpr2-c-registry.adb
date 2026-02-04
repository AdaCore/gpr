--
--  Copyright (C) 2025-2026, AdaCore
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

      type View_Record is record
         View  : GPR2.Project.View.Object;
         Count : Natural;
      end record;

      function Equal (Left : View_Record; Right : View_Record) return Boolean;

      package View_Maps is new
        Ada.Containers.Hashed_Maps
          (Ada.Strings.Unbounded.Unbounded_String,
           View_Record,
           Ada.Strings.Unbounded.Hash,
           Ada.Strings.Unbounded."=",
           Equal);

      View_Map : View_Maps.Map;

      -----------
      -- Equal --
      -----------

      function Equal
        (Left : View_Record; Right : View_Record) return Boolean
      is
         use type GPR2.Project.View.Object;

      begin
         return Left.View = Right.View;
      end Equal;

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
            return View_Map (S).View;

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
                  View_Map.Insert (Id, (View, 1));

               else
                  View_Map.Replace (Id, (View, View_Map (Id).Count + 1));
               end if;
            end if;

            return GPR2.C.JSON.Values.To_JSON_Value (To_String (Id));
         end if;
      end Register;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister (Id : GPR2.C.JSON.Values.JSON_Value) is
         Identifier : constant Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.To_Unbounded_String (Id.To_String);

      begin
         if View_Map (Identifier).Count = 1 then
            View_Map.Delete (Identifier);

         else
            View_Map.Replace
              (Identifier,
               (View_Map (Identifier).View, View_Map (Identifier).Count - 1));
         end if;
      end Unregister;

   end View;

end GPR2.C.Registry;
