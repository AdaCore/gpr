--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

private with GPR2.Tree_Internal.View_Builder;

package GPR2.Project.Tree.View_Builder is

   type Object is tagged private;

   function Is_Defined (Self : Object) return Boolean;

   function Create
     (Project_Dir : GPR2.Path_Name.Object;
      Name        : Name_Type;
      Qualifier   : Project_Kind := K_Standard) return Object;

   procedure Set_Attribute
     (Self  : in out Object;
      Attr  : Q_Attribute_Id;
      Value : Value_Type);

   procedure Set_Attribute
     (Self   : in out Object;
      Attr   : Q_Attribute_Id;
      Values : Containers.Value_List);

   procedure Set_Attribute
     (Self  : in out Object;
      Attr  : Q_Attribute_Id;
      Index : Value_Type;
      Value : Value_Type);

   procedure Set_Attribute
     (Self   : in out Object;
      Attr   : Q_Attribute_Id;
      Index  : Value_Type;
      Values : Containers.Value_List);

private

   type Object is tagged record
      Internal : GPR2.Tree_Internal.View_Builder.Object;
   end record;

   function Is_Defined (Self : Object) return Boolean is
     (Self.Internal.Is_Defined);

end GPR2.Project.Tree.View_Builder;
