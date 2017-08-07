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

   --  Project Tree

   function "<" (Left, Right : Path_Name_Type) return Boolean is
      (Value (Left) < Value (Right));

   package Project_Store is new Ada.Containers.Ordered_Maps
     (Path_Name_Type, Parser.Project.Object, "<", Parser.Project."=");

   protected Shared is

      function Exist (Pathname : Path_Name_Type) return Boolean;

      function Get (Pathname : Path_Name_Type) return Project.Object;

      procedure Register
        (Pathname : Path_Name_Type; Project : Parser.Project.Object);

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
         return Store (Pathname);
      end Get;

      --------------
      -- Register --
      --------------

      procedure Register
        (Pathname : Path_Name_Type; Project : Parser.Project.Object) is
      begin
         Store.Insert (Pathname, Project);
      end Register;

   end Shared;

end GPR2.Parser.Registry;
