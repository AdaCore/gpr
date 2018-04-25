------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2017-2018, Free Software Foundation, Inc.          --
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

--  This package represents an entity source reference with an associated
--  message. This is mostly to report warnings/errors while parsing sources.

package GPR2.Source_Reference.Identifier is

   type Object is new GPR2.Source_Reference.Object with private;

   Undefined : constant Object;

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural;
      Identifier   : Name_Type) return Object'Class;

   function Identifier (Self : Object) return Name_Type;
   --  Return the message associated with the reference

private

   type Object is new GPR2.Source_Reference.Object with record
      Identifier : Unbounded_String;
   end record;

   Undefined : constant Object :=
                 (GPR2.Source_Reference.Undefined with Null_Unbounded_String);

end GPR2.Source_Reference.Identifier;
