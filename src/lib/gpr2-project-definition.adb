------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2017, Free Software Foundation, Inc.          --
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

with GPR2.Project.Tree;
with GPR2.Project.View.Set;

package body GPR2.Project.Definition is

   package Project_Data is
     new Ada.Containers.Indefinite_Ordered_Maps (View.Object, Data);

   package Project_View is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, View.Set.Object, "=" => View.Set.Set."=");

   protected Shared is

      function Get
        (Path_Name    : Path_Name_Type;
         Context_View : GPR2.Project.View.Object;
         Status       : Relation_Status;
         Tree         : GPR2.Project.Tree.Object) return Project.View.Object;

      function Get
        (Name         : Name_Type;
         Context_View : Project.View.Object;
         Tree         : GPR2.Project.Tree.Object) return Project.View.Object;

      function Get (View : Project.View.Object) return Data;

      procedure Register (Def : Data; View : out Project.View.Object);

      procedure Set (View : Project.View.Object; Def : Data);

      procedure Unregister (View : Project.View.Object);

   private
      Views_Data : Project_Data.Map;
      Views      : Project_View.Map;
      N          : View.Id := 0;
   end Shared;

   ---------
   -- Get --
   ---------

   function Get (View : Project.View.Object) return Data is
   begin
      return Shared.Get (View);
   end Get;

   function Get
     (Path_Name    : Path_Name_Type;
      Context_View : GPR2.Project.View.Object;
      Status       : Relation_Status;
      Tree         : GPR2.Project.Tree.Object) return Project.View.Object is
   begin
      return Shared.Get (Path_Name, Context_View, Status, Tree);
   end Get;

   function Get
     (Name         : Name_Type;
      Context_View : Project.View.Object;
      Tree         : GPR2.Project.Tree.Object) return Project.View.Object is
   begin
      return Shared.Get (Name, Context_View, Tree);
   end Get;

   --------------
   -- Register --
   --------------

   function Register (Def : Data) return View.Object is
      Result : View.Object;
   begin
      Shared.Register (Def, Result);
      return Result;
   end Register;

   ---------
   -- Set --
   ---------

   procedure Set (View : Project.View.Object; Def : Data) is
   begin
      Shared.Set (View, Def);
   end Set;

   ------------
   -- Shared --
   ------------

   protected body Shared is

      ---------
      -- Get --
      ---------

      function Get (View : Project.View.Object) return Data is
      begin
         return Views_Data (View);
      end Get;

      function Get
        (Path_Name    : Path_Name_Type;
         Context_View : GPR2.Project.View.Object;
         Status       : Relation_Status;
         Tree         : GPR2.Project.Tree.Object) return Project.View.Object
      is
         use type GPR2.Project.Tree.Object;
         Key : constant Name_Type := Name_Type (Value (Path_Name));
      begin
         if Views.Contains (Key) then
            for V of Views (Key) loop
               declare
                  Defs : constant Data := Views_Data (V);
               begin
                  if Defs.Tree.all = Tree
                    and then Defs.Context_View = Context_View
                    and then (Defs.Status = Status
                              or else Status /= Aggregated)
                  then
                     return V;
                  end if;
               end;
            end loop;
         end if;

         return Project.View.Undefined;
      end Get;

      function Get
        (Name         : Name_Type;
         Context_View : Project.View.Object;
         Tree         : GPR2.Project.Tree.Object) return Project.View.Object
      is
         use type GPR2.Project.Tree.Object;
      begin
         if Views.Contains (Name) then
            for V of Views (Name) loop
               declare
                  Defs : constant Data := Views_Data (V);
               begin
                  if Defs.Tree.all = Tree
                    and then Defs.Context_View = Context_View
                  then
                     return V;
                  end if;
               end;
            end loop;
         end if;

         return Project.View.Undefined;
      end Get;

      --------------
      -- Register --
      --------------

      procedure Register (Def : Data; View : out Project.View.Object) is
         Result : constant Project.View.Object := Project.View.From_Id (N + 1);
      begin
         N := N + 1;
         Views_Data.Insert (Result, Def);
         View := Result;

         --  Now update the map from pathname to views

         declare
            Path_Name : constant Name_Type :=
                          Name_Type (Value (Def.Trees.Project.Path_Name));
            Name      : constant Name_Type :=
                          Def.Trees.Project.Name;
         begin
            if not Views.Contains (Path_Name) then
               Views.Insert (Path_Name, Project.View.Set.Set.Empty_Set);
            end if;

            if not Views.Contains (Name) then
               Views.Insert (Name, Project.View.Set.Set.Empty_Set);
            end if;

            Views (Path_Name).Insert (Result);
            Views (Name).Insert (Result);
         end;
      end Register;

      ---------
      -- Set --
      ---------

      procedure Set (View : Project.View.Object; Def : Data) is
      begin
         Views_Data (View) := Def;
      end Set;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister (View : Project.View.Object) is
         Def : Data := Views_Data (View);
      begin
         Def.Context_View.Release;
         Def.Tree := null;
         Views_Data.Delete (View);
         Views (Name_Type (Value (View.Path_Name))).Delete (View);
         Views (View.Name).Delete (View);
      end Unregister;

   end Shared;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (View : Project.View.Object) is
   begin
      Shared.Unregister (View);
   end Unregister;

end GPR2.Project.Definition;
