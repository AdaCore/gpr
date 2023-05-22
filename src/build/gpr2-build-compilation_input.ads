--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Source_Info;
private with GPR2.Build.View_Db;

package GPR2.Build.Compilation_Input is

   type Object is tagged private;

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;

   function Source (Self : Object)
      return GPR2.Build.Source_Info.Object
     with Pre => Self.Is_Defined;
   --  Source file to be used as compiler parameter

   function Index (Self : Object) return Unit_Index
     with Pre => Self.Is_Defined;
   --  If the source is a multi-index source, returns the index of this
   --  compilation input. Otherwise, returns No_Index.

   function "<" (L, R : Object) return Boolean;

private

   use type View_Db.Object;

   type Object is tagged record
      Src_Owner       : View_Db.Object;
      Source_Basename : Unbounded_String;
      Index           : Unit_Index := No_Index;
   end record;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function "<" (L, R : Object) return Boolean
   is (if L.Src_Owner /= R.Src_Owner
       then L.Src_Owner < R.Src_Owner
       elsif L.Source_Basename /= R.Source_Basename
       then L.Source_Basename < R.Source_Basename
       else L.Index < R.Index);

   function Source (Self : Object) return Source_Info.Object
   is (Self.Src_Owner.Source (Simple_Name (-Self.Source_Basename)));

   function Index (Self : Object) return Unit_Index is
      (Self.Index);

end GPR2.Build.Compilation_Input;
