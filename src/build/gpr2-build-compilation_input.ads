--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.Build.Source;
private with GPR2.Project.View;

package GPR2.Build.Compilation_Input is

   type Compilation_Input_Kind is
     (Unit_Kind, Body_Kind);

   type Object is tagged private;

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;

   function Kind (Self : Object) return Compilation_Input_Kind
     with Pre => Self.Is_Defined;

   function Source (Self : Object)
      return GPR2.Build.Source.Object
     with Pre => Self.Is_Defined;
   --  Source file to be used as compiler parameter

   function Index (Self : Object) return Unit_Index
     with Pre  => Self.Is_Defined,
          Post => (if Self.Kind = Body_Kind then Index'Result = No_Index);
   --  If the source is a multi-index source, returns the index of this
   --  compilation input. Otherwise, returns No_Index.

   function "<" (L, R : Object) return Boolean;

private

   use type GPR2.Project.View.Object;

   type Object is tagged record
      Src_Owner       : Project.View.Object;
      Kind            : Compilation_Input_Kind := Body_Kind;
      Source_Basename : Unbounded_String;
      Index           : Unit_Index := No_Index;
   end record;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Kind (Self : Object) return Compilation_Input_Kind is
     (Self.Kind);

   function "<" (L, R : Object) return Boolean
   is (if L.Src_Owner /= R.Src_Owner
       then L.Src_Owner < R.Src_Owner
       elsif L.Source_Basename /= R.Source_Basename
       then L.Source_Basename < R.Source_Basename
       else L.Index < R.Index);

   function Source (Self : Object) return Build.Source.Object
   is (Self.Src_Owner.Source (Simple_Name (-Self.Source_Basename)));

   function Index (Self : Object) return Unit_Index is
      (Self.Index);

end GPR2.Build.Compilation_Input;
