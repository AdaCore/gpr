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

with Ada.Containers.Ordered_Maps;

with GPR2.Parser.Project;
with GPR2.Project.View;

package body GPR2.Parser.Registry is

   --  Project with reference counter

   type Data is record
      Project : Parser.Project.Object;
      Ref     : Natural;
   end record;

   package Project_Store is new Ada.Containers.Ordered_Maps
     (Path_Name_Type, Data, "<");

   protected Shared is

      function Exist (Pathname : Path_Name_Type) return Boolean;

      function Get (Pathname : Path_Name_Type) return Project.Object;

      procedure Register
        (Pathname : Path_Name_Type; Project : Parser.Project.Object);

      procedure Unregister (Pathname : Path_Name_Type);

   private
      Store : Project_Store.Map;
   end Shared;

   ------------
   -- Exists --
   ------------

   function Exists (Pathname : Path_Name_Type) return Boolean is
   begin
      return Shared.Exist (Pathname);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Pathname : Path_Name_Type) return Project.Object is
   begin
      return Shared.Get (Pathname);
   end Get;

   --------------
   -- Register --
   --------------

   procedure Register
     (Pathname : Path_Name_Type; Project : Parser.Project.Object) is
   begin
      Shared.Register (Pathname, Project);
   end Register;

   ------------
   -- Shared --
   ------------

   protected body Shared is

      -----------
      -- Exist --
      -----------

      function Exist (Pathname : Path_Name_Type) return Boolean is
      begin
         return Store.Contains (Pathname);
      end Exist;

      ---------
      -- Get --
      ---------

      function Get (Pathname : Path_Name_Type) return Project.Object is
      begin
         return Store (Pathname).Project;
      end Get;

      --------------
      -- Register --
      --------------

      procedure Register
        (Pathname : Path_Name_Type; Project : Parser.Project.Object)
      is
         use type Project_Store.Cursor;
         Pos : constant Project_Store.Cursor := Store.Find (Pathname);
      begin
         if Pos = Project_Store.No_Element then
            Store.Insert (Pathname, Data'(Project, 1));

         else
            Store (Pos).Ref := Store (Pos).Ref + 1;
         end if;
      end Register;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister (Pathname : Path_Name_Type) is
         Pos : constant Project_Store.Cursor := Store.Find (Pathname);
         D   : Data := Store (Pos);
      begin
         D.Ref := D.Ref - 1;

         if D.Ref = 0 then
            --  No more reference to this tree, clean it

            D.Project.Unload;
            Store.Delete (Pathname);

         else
            Store (Pos).Ref := D.Ref;
         end if;
      end Unregister;

   end Shared;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Pathname : Path_Name_Type) is
   begin
      Shared.Unregister (Pathname);
   end Unregister;

end GPR2.Parser.Registry;
