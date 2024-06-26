--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.View_Internal;

package body GPR2.Project.Tree.View_Builder is

   function Get (Self : Object) return View_Internal.Data is
      (Self.Internal.Data);

   ------------
   -- Create --
   ------------

   function Create
     (Project_Dir : GPR2.Path_Name.Object;
      Name        : Name_Type;
      Qualifier   : Project_Kind := K_Standard) return Object
   is
   begin
      return (Internal => Tree_Internal.View_Builder.Create
                (Project_Dir, Name, Qualifier));
   end Create;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute
     (Self : in out Object;
      Attr  : Q_Attribute_Id;
      Value : Value_Type) is
   begin
      Self.Internal.Set_Attribute (Attr, Value);
   end Set_Attribute;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute
     (Self   : in out Object;
      Attr   : Q_Attribute_Id;
      Values : Containers.Value_List) is
   begin
      Self.Internal.Set_Attribute (Attr, Values);
   end Set_Attribute;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute
     (Self  : in out Object;
      Attr  : Q_Attribute_Id;
      Index : Value_Type;
      Value : Value_Type) is
   begin
      Self.Internal.Set_Attribute (Attr, Index, Value);
   end Set_Attribute;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute
     (Self   : in out Object;
      Attr   : Q_Attribute_Id;
      Index  : Value_Type;
      Values : Containers.Value_List) is
   begin
      Self.Internal.Set_Attribute (Attr, Index, Values);
   end Set_Attribute;

begin

   GPR2.Project.Tree.Get_View_Data := Get'Access;

end GPR2.Project.Tree.View_Builder;
