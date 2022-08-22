--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package represents an entity source reference.

generic
   type Scalar_Type is private;
   No_Value : Scalar_Type;
package GPR2.Source_Reference.Scalar_Value is

   type Object is new GPR2.Source_Reference.Object with private;

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
