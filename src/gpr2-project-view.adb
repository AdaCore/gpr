------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2016, Free Software Foundation, Inc.            --
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

with GPR2.Project.Definition;

package body GPR2.Project.View is

   ----------------
   -- Attributes --
   ----------------

   function Attributes (Project : Object) return Attribute.Set.Object is
   begin
      return Definition.Get (Project).Attrs;
   end Attributes;

   -------------
   -- From_Id --
   -------------

   function From_Id (Id : View.Id) return Object is
   begin
      return Object'(Id => Id);
   end From_Id;

   --------------------
   -- Has_Attributes --
   --------------------

   function Has_Attributes (Project : Object) return Boolean is
   begin
      return Definition.Get (Project).Attrs.Length > 0;
   end Has_Attributes;

   -----------------
   -- Has_Imports --
   -----------------

   function Has_Imports (Project : Object) return Boolean is
      use type Ada.Containers.Count_Type;
   begin
      return Definition.Get (Project).Trees.Imports.Length > 0;
   end Has_Imports;

   ------------------
   -- Has_Packages --
   ------------------

   function Has_Packages (Project : Object) return Boolean is
   begin
      return Definition.Get (Project).Packs.Length > 0;
   end Has_Packages;

   -------------------
   -- Has_Variables --
   -------------------

   function Has_Variables (Project : Object) return Boolean is
   begin
      return Definition.Get (Project).Vars.Length > 0;
   end Has_Variables;

   ----------
   -- Name --
   ----------

   function Name (Project : Object) return Name_Type is
   begin
      return Definition.Get (Project).Trees.Project.Name;
   end Name;

   --------------
   -- Packages --
   --------------

   function Packages (Project : Object) return Pack.Set.Object is
   begin
      return Definition.Get (Project).Packs;
   end Packages;

   ---------------
   -- Path_Name --
   ---------------

   function Path_Name (Project : Object) return Path_Name_Type is
   begin
      return Definition.Get (Project).Trees.Project.Path_Name;
   end Path_Name;

   ---------------
   -- Qualifier --
   ---------------

   function Qualifier (Project : Object) return Project_Qualifier is
   begin
      return Definition.Get (Project).Trees.Project.Qualifier;
   end Qualifier;

   ---------------
   -- Variables --
   ---------------

   function Variables (Project : Object) return Variable.Set.Object is
   begin
      return Definition.Get (Project).Vars;
   end Variables;

end GPR2.Project.View;
