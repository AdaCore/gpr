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

--  This package represents an entity source reference. It is used for
--  variables, attributes and packages declared in projects.

package GPR2.Source_Reference is

   type Object is tagged private;

   subtype Source_Reference is Object;

   function "<" (Left, Right : Object) return Boolean;

   Undefined : constant Object;

   function Create
     (Filename     : Full_Path_Name;
      Line, Column : Natural) return Object'Class;

   function Filename (Self : Object) return Full_Path_Name
     with Pre => Self /= Undefined;
   --  Returns the full pathname where the entity is defined

   function Has_Source_Reference (Self : Object) return Boolean;
   --  Returns True if Self has source references. That is, in this case the
   --  Line and Column have meaningful values.

   function Line (Self : Object) return Positive
     with Pre => Self.Has_Source_Reference;
   --  Returns the starting line of the entity declaration

   function Column (Self : Object) return Positive
     with Pre => Self.Has_Source_Reference;
   --  Returns the starting column of the entity declaration

private

   type Object is tagged record
      Line     : Natural;
      Column   : Natural;
      Filename : Unbounded_String;
   end record
     with Dynamic_Predicate => Filename /= Null_Unbounded_String;

   function "<" (Left, Right : Object) return Boolean is
     (Left.Filename < Right.Filename
       or else
      (Left.Filename = Right.Filename and then Left.Line < Right.Line));

   function Has_Source_Reference (Self : Object) return Boolean
     is (Self.Column > 0 and then Self.Line > 0);

   Undefined : constant Object :=
                 (0, 0, To_Unbounded_String ("@"));

end GPR2.Source_Reference;
