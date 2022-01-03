------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2022, AdaCore                      --
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

--  This package represents an entity source reference.

generic
   type Scalar_Type is private;
   No_Value : Scalar_Type;
   with function "<" (Left, Right : Scalar_Type) return Boolean is <>;
package GPR2.Source_Reference.Scalar_Value is

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
      Id           : Scalar_Type) return Object'Class
     with Post => Create'Result.Is_Defined;

   function Create
     (Sloc : GPR2.Source_Reference.Object;
      Id   : Scalar_Type) return Object'Class
     with Pre  => Sloc.Is_Defined,
          Post => Create'Result.Is_Defined;

   function Id (Self : Object) return Scalar_Type
     with Pre => Self.Is_Defined;
   --  Returns the message associated with the reference

private

   type Object is new GPR2.Source_Reference.Object with record
      Id : Scalar_Type := No_Value;
   end record;

   overriding function "<" (Left, Right : Object) return Boolean is
     (if GPR2.Source_Reference.Object (Left) /=
          GPR2.Source_Reference.Object (Right)
      then GPR2.Source_Reference.Object (Left) <
          GPR2.Source_Reference.Object (Right)
      else Left.Id < Right.Id);

   Undefined : constant Object :=
                 (GPR2.Source_Reference.Undefined with others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural;
      Id           : Scalar_Type) return Object'Class is
     (Object'(GPR2.Source_Reference.Object
               (Source_Reference.Create (Filename, Line, Column))
              with Id => Id));

   function Create
     (Sloc : GPR2.Source_Reference.Object;
      Id   : Scalar_Type) return Object'Class is
      (Object'(Sloc with Id => Id));

   function Id (Self : Object) return Scalar_Type is
     (Self.Id);

end GPR2.Source_Reference.Scalar_Value;
