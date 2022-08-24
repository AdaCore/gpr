--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.Source_Reference;

package GPR2.Project.Import is

   type Object is new Source_Reference.Object with private;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   overriding function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Create
     (Path_Name  : GPR2.Path_Name.Object;
      Sloc       : Source_Reference.Object;
      Is_Limited : Boolean) return Object
     with Pre => Sloc.Is_Defined;

   function Path_Name (Self : Object) return Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Full pathname of the corresponding project file

   function Is_Limited (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if this is a limited import

private

   type Object is new Source_Reference.Object with record
      Path_Name  : GPR2.Path_Name.Object;
      Is_Limited : Boolean := False;
   end record;

   Undefined : constant Object :=
                 (Source_Reference.Undefined with others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

end GPR2.Project.Import;
