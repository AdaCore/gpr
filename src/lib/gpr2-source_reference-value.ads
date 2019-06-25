------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with GPR2.Source_Reference.Text_Value;

package GPR2.Source_Reference.Value is

   package Text_Values is new GPR2.Source_Reference.Text_Value (Value_Type);

   type Object is new Text_Values.Object with private;

   Undefined : constant Object;

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural;
      Text         : Value_Type;
      At_Num       : Natural := 0) return Object'Class;

   function Create
     (Sloc   : GPR2.Source_Reference.Object;
      Text   : Value_Type;
      At_Num : Natural := 0) return Text_Values.Object'Class;

   function Has_At_Num (Self : Object) return Boolean
     with Pre => Self.Is_Defined;

   function At_Num (Self : Object) return Positive
     with Pre => Self.Is_Defined and then Self.Has_At_Num;

private

   type Object is new Text_Values.Object with record
      At_Num : Natural := 0;
   end record;

   Undefined : constant Object := (Text_Values.Undefined with others => <>);

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural;
      Text         : Value_Type;
      At_Num       : Natural := 0) return Object'Class
   is
     (Object'
        (Text_Values.Object
           (Text_Values.Create (Filename, Line, Column, Text)) with
            At_Num => At_Num));

   function Create
     (Sloc   : GPR2.Source_Reference.Object;
      Text   : Value_Type;
      At_Num : Natural := 0) return Text_Values.Object'Class
   is
     (Object'
        (Text_Values.Object
           (Text_Values.Create (Sloc, Text)) with At_Num => At_Num));

   function Has_At_Num (Self : Object) return Boolean is
     (Self.At_Num > 0);

   function At_Num (Self : Object) return Positive is
     (Self.At_Num);

end GPR2.Source_Reference.Value;
