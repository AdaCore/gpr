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

package body GPR2.Project.Attribute is

   ------------
   -- Create --
   ------------

   function Create
     (Name  : Name_Type;
      Index : Name_Type;
      Value : Value_Type) return Object is
   begin
      return A : Object := Create (Name, Value) do
         A.Index := To_Unbounded_String (Index);
      end return;
   end Create;

   function Create
     (Name   : Name_Type;
      Index  : Name_Type;
      Values : Containers.Value_List) return Object is
   begin
      return A : Object := Create (Name, Values) do
         A.Index := To_Unbounded_String (Index);
      end return;
   end Create;

   overriding function Create
     (Name  : Name_Type;
      Value : Value_Type) return Object is
   begin
      return Object'
        (Name_Values.Create (Name, Value)
         with Index => Null_Unbounded_String);
   end Create;

   overriding function Create
     (Name   : Name_Type;
      Values : Containers.Value_List) return Object is
   begin
      return Object'
        (Name_Values.Create (Name, Values)
         with Index => Null_Unbounded_String);
   end Create;

   ---------------
   -- Has_Index --
   ---------------

   function Has_Index (Self : Object) return Boolean is
   begin
      return Self.Index /= Null_Unbounded_String;
   end Has_Index;

   -----------
   -- Index --
   -----------

   function Index (Self : Object) return Name_Type is
   begin
      return To_String (Self.Index);
   end Index;

end GPR2.Project.Attribute;
