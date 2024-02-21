--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Source_Base;
with GPR2.Project.View;

package GPR2.Build.Source is

   type Object is new GPR2.Build.Source_Base.Object with private;

   Undefined : constant Object;

   overriding function Is_Defined (Self : Object) return Boolean;

   function Create
     (Base_Source    : Source_Base.Object;
      Owning_View    : GPR2.Project.View.Object;
      Defining_View  : GPR2.Project.View.Object;
      Inherited_From : GPR2.Project.View.Object) return Object;

   function Owning_View (Self : Object) return Project.View.Object
     with Pre  => Self.Is_Defined,
          Post => Owning_View'Result.Is_Defined;
   function Is_Inherited (Self : Object) return Boolean;
   function Inherited_From (Self : Object) return Project.View.Object
     with Post => (if Self.Is_Inherited then Inherited_From'Result.Is_Defined
                   else not Inherited_From'Result.Is_Defined);

private

   type Object is new GPR2.Build.Source_Base.Object with record
      Owning_View    : GPR2.Project.View.Object;
      Defining_View  : GPR2.Project.View.Object;
      Inherited_From : GPR2.Project.View.Object;
   end record;

   Undefined : constant Object := (Source_Base.Undefined with
                                   others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Create
     (Base_Source    : Source_Base.Object;
      Owning_View    : GPR2.Project.View.Object;
      Defining_View  : GPR2.Project.View.Object;
      Inherited_From : GPR2.Project.View.Object) return Object is
     (Base_Source with
      Owning_View    => Owning_View,
      Defining_View  => Defining_View,
      Inherited_From => Inherited_From);

   function Owning_View (Self : Object) return Project.View.Object is
     (Self.Owning_View);
   function Is_Inherited (Self : Object) return Boolean is
     (Self.Inherited_From.Is_Defined);
   function Inherited_From (Self : Object) return Project.View.Object is
     (Self.Inherited_From);

end GPR2.Build.Source;
