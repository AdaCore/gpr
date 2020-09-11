------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2020, AdaCore                      --
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

with GPR2.Path_Name;

package GPR2.Source_Reference is

   type Object is tagged private;

   function "<" (Left, Right : Object) return Boolean;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   Builtin   : constant Object;
   --  Source reference for built-in objects

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural) return Object'Class
     with Post => Create'Result.Is_Defined;

   function Filename (Self : Object) return Path_Name.Full_Name
     with Pre => Self.Is_Defined;
   --  Returns the full pathname where the entity is defined

   function Has_Source_Reference (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self has source references. That is, in this case the
   --  Line and Column have meaningful values.

   function Line (Self : Object) return Positive
     with Pre => Self.Is_Defined and then Self.Has_Source_Reference;
   --  Returns the starting line of the entity declaration

   function Column (Self : Object) return Positive
     with Pre => Self.Is_Defined and then Self.Has_Source_Reference;
   --  Returns the starting column of the entity declaration

private

   type Object is tagged record
      Line     : Natural := 0;
      Column   : Natural := 0;
      Filename : Unbounded_String := To_Unbounded_String ("@");
   end record
     with Dynamic_Predicate => Filename /= Null_Unbounded_String;

   function "<" (Left, Right : Object) return Boolean is
     (if Left.Filename /= Right.Filename then Left.Filename < Right.Filename
      elsif Left.Line /= Right.Line then Left.Line < Right.Line
      else Left.Column < Right.Column);

   function Has_Source_Reference (Self : Object) return Boolean
     is (Self.Column > 0 and then Self.Line > 0);

   Undefined : constant Object := (others => <>);

   Builtin   : constant Object := (0, 0, To_Unbounded_String ("<builtin>"));

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

end GPR2.Source_Reference;
