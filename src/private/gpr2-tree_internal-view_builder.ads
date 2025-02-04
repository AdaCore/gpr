--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package GPR2.Tree_Internal.View_Builder is

   type Object is tagged record
      Data : GPR2.View_Internal.Data;
   end record;

   function Is_Defined (Self : Object) return Boolean is
     (Self.Data.Path.Is_Defined);

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

end GPR2.Tree_Internal.View_Builder;
