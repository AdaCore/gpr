------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  This package represents an entity source reference with an associated
--  message. This is mostly to report warnings/errors while parsing sources.

generic
   type Text_Type is new String;
package GPR2.Source_Reference.Text_Value is

   type Object is new GPR2.Source_Reference.Object with private;

   overriding function "<" (Left, Right : Object) return Boolean;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   overriding function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural;
      Text         : Text_Type) return Object'Class;

   function Create
     (Sloc  : GPR2.Source_Reference.Object;
      Text  : Text_Type) return Object'Class
     with Pre => Sloc.Is_Defined;

   function Text (Self : Object) return Text_Type;
   --  Returns the message associated with the reference

private

   type Object is new GPR2.Source_Reference.Object with record
      Text : Unbounded_String;
   end record;

   overriding function "<" (Left, Right : Object) return Boolean is
     (if GPR2.Source_Reference.Object (Left) /=
          GPR2.Source_Reference.Object (Right)
      then GPR2.Source_Reference.Object (Left) <
          GPR2.Source_Reference.Object (Right)
      else Left.Text < Right.Text);

   Undefined : constant Object :=
                 (GPR2.Source_Reference.Undefined with others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

end GPR2.Source_Reference.Text_Value;
