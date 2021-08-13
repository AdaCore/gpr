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

--  This package represents an entity source reference with an associated
--  message. This is mostly to report warnings/errors while parsing sources.

package body GPR2.Source_Reference.Text_Value is

   ------------
   -- Create --
   ------------

   function Create
     (Sloc : GPR2.Source_Reference.Object;
      Text : Text_Type) return Object'Class is
   begin
      return Object'
        (Sloc with Text => To_Unbounded_String (String (Text)));
   end Create;

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural;
      Text         : Text_Type)
      return Object'Class is
   begin
      return Create
        (GPR2.Source_Reference.Object
           (GPR2.Source_Reference.Create (Filename, Line, Column)),
         Text);
   end Create;

   ----------
   -- Text --
   ----------

   function Text (Self : Object) return Text_Type is
   begin
      return Text_Type (To_String (Self.Text));
   end Text;

   --------------------
   -- Unchecked_Text --
   --------------------

   function Unchecked_Text (Self : Object) return Unbounded_String is
   begin
      return Self.Text;
   end Unchecked_Text;

end GPR2.Source_Reference.Text_Value;
