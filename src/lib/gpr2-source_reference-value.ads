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

--  This package represents an entity source reference with an associated
--  message. This is mostly to report warnings/errors while parsing sources.

with GPR2.Source_Reference.Text_Value;

package GPR2.Source_Reference.Value is

   package Text_Values is new GPR2.Source_Reference.Text_Value (Value_Type);

   type Object is new Text_Values.Object with private;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural;
      Text         : Value_Type;
      At_Pos       : Natural := 0) return Object'Class
     with Post => Create'Result.Is_Defined;

   function Create
     (Sloc   : GPR2.Source_Reference.Object;
      Text   : Value_Type;
      At_Pos : Natural := 0) return Text_Values.Object'Class
     with Post => Create'Result.Is_Defined;

   function Has_At_Pos (Self : Object) return Boolean
     with Pre => Self.Is_Defined;

   function At_Pos (Self : Object) return Positive
     with Pre => Self.Is_Defined and then Self.Has_At_Pos;

private

   type Object is new Text_Values.Object with record
      At_Pos : Natural := 0;
   end record;

   Undefined : constant Object := (Text_Values.Undefined with others => <>);

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural;
      Text         : Value_Type;
      At_Pos       : Natural := 0) return Object'Class
   is
     (Object'
        (Text_Values.Object
           (Text_Values.Create (Filename, Line, Column, Text)) with
            At_Pos => At_Pos));

   function Create
     (Sloc   : GPR2.Source_Reference.Object;
      Text   : Value_Type;
      At_Pos : Natural := 0) return Text_Values.Object'Class
   is
     (Object'
        (Text_Values.Object
           (Text_Values.Create (Sloc, Text)) with At_Pos => At_Pos));

   function Has_At_Pos (Self : Object) return Boolean is
     (Self.At_Pos > 0);

   function At_Pos (Self : Object) return Positive is
     (Self.At_Pos);

end GPR2.Source_Reference.Value;
