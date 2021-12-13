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

with Ada.Containers.Ordered_Maps;

package body GPR2.Project.Parser.Registry is

   --  Project with reference counter

   type Data is record
      Project : GPR2.Project.Parser.Object;
      Ref     : Natural;
   end record;

   package Project_Store is
     new Ada.Containers.Ordered_Maps (GPR2.Path_Name.Object, Data);

   protected Shared is

      function Exist (Pathname : GPR2.Path_Name.Object) return Boolean;

      function Get
        (Pathname : GPR2.Path_Name.Object) return Project.Parser.Object;

      procedure Register
        (Pathname : GPR2.Path_Name.Object;
         Project  : GPR2.Project.Parser.Object);

      procedure Check_Registry
        (Pathname : GPR2.Path_Name.Object;
         Project  : out GPR2.Project.Parser.Object);

      procedure Unregister (Pathname : GPR2.Path_Name.Object);

   private
      Store : Project_Store.Map;
   end Shared;

   -------------------
   -- Check_Project --
   -------------------

   function Check_Project
     (Pathname : GPR2.Path_Name.Object;
      Project  : out GPR2.Project.Parser.Object) return Boolean is
   begin
      Shared.Check_Registry (Pathname, Project);
      return Project.Is_Defined;
   end Check_Project;

   ------------
   -- Exists --
   ------------

   function Exists (Pathname : GPR2.Path_Name.Object) return Boolean is
   begin
      return Shared.Exist (Pathname);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (Pathname : GPR2.Path_Name.Object) return GPR2.Project.Parser.Object
   is
   begin
      return Shared.Get (Pathname);
   end Get;

   --------------
   -- Register --
   --------------

   procedure Register
     (Pathname : GPR2.Path_Name.Object; Project : GPR2.Project.Parser.Object)
   is
   begin
      Shared.Register (Pathname, Project);
   end Register;

   ------------
   -- Shared --
   ------------

   protected body Shared is

      --------------------
      -- Check_Registry --
      --------------------

      procedure Check_Registry
        (Pathname : GPR2.Path_Name.Object;
         Project  : out GPR2.Project.Parser.Object)
      is
         CP : constant Project_Store.Cursor := Store.Find (Pathname);
      begin
         if Project_Store.Has_Element (CP) then
            declare
               Ref : constant Project_Store.Reference_Type :=
                       Store.Reference (CP);
            begin
               Project := Ref.Project;
               Ref.Ref := Ref.Ref + 1;
            end;
         else
            Project := GPR2.Project.Parser.Undefined;
         end if;
      end Check_Registry;

      -----------
      -- Exist --
      -----------

      function Exist (Pathname : GPR2.Path_Name.Object) return Boolean is
      begin
         return Store.Contains (Pathname);
      end Exist;

      ---------
      -- Get --
      ---------

      function Get
        (Pathname : GPR2.Path_Name.Object) return Project.Parser.Object
      is
      begin
         return Store (Pathname).Project;
      end Get;

      --------------
      -- Register --
      --------------

      procedure Register
        (Pathname : GPR2.Path_Name.Object;
         Project  : GPR2.Project.Parser.Object)
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

      procedure Unregister (Pathname : GPR2.Path_Name.Object) is
         Pos : constant Project_Store.Cursor := Store.Find (Pathname);
         D   : Data := Store (Pos);
      begin
         D.Ref := D.Ref - 1;

         if D.Ref = 0 then
            --  No more reference to this tree, clean it

            Store.Delete (Pathname);

         else
            Store (Pos).Ref := D.Ref;
         end if;
      end Unregister;

   end Shared;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Pathname : GPR2.Path_Name.Object) is
   begin
      Shared.Unregister (Pathname);
   end Unregister;

end GPR2.Project.Parser.Registry;
