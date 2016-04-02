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

package body GPR2.Project.Name_Values is

   ------------------
   -- Count_Values --
   ------------------

   function Count_Values (Self : Object) return Containers.Count_Type is
   begin
      return Self.Values.Length;
   end Count_Values;

   ------------
   -- Create --
   ------------

   function Create
     (Name  : Name_Type;
      Value : Value_Type;
      Sloc  : Source_Reference.Object) return Object is
   begin
      return Object'
        (Sloc
         with Single,
              To_Unbounded_String (String (Name)),
              Containers.Value_Type_List.To_Vector (String (Value), 1));
   end Create;

   function Create
     (Name   : Name_Type;
      Values : Containers.Value_List;
      Sloc   : Source_Reference.Object) return Object is
   begin
      return Object'
        (Sloc with List, To_Unbounded_String (String (Name)), Values);
   end Create;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Object'Class) return Registry.Attribute.Value_Kind is
   begin
      return Self.Kind;
   end Kind;

   ----------
   -- Name --
   ----------

   function Name (Self : Object) return Name_Type is
   begin
      return To_String (Self.Name);
   end Name;

   -----------
   -- Value --
   -----------

   function Value (Self : Object) return Value_Type is
   begin
      return Self.Values.First_Element;
   end Value;

   ------------
   -- Values --
   ------------

   function Values (Self : Object) return Containers.Value_List is
   begin
      return Self.Values;
   end Values;

end GPR2.Project.Name_Values;
