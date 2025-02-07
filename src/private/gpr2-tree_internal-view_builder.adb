--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Pack_Internal;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Variable.Set;
with GPR2.Project_Parser.Create;
with GPR2.Source_Reference.Attribute;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Value;
with GPR2.Source_Reference.Pack;

package body GPR2.Tree_Internal.View_Builder is

   procedure Set_Attribute
     (Self   : in out Object;
      Q_Name : Q_Attribute_Id;
      Attr   : GPR2.Project.Attribute.Object);

   function SR_Attr
     (Self : Object;
      Attr : Q_Attribute_Id)
      return Source_Reference.Attribute.Object
   is (GPR2.Source_Reference.Attribute.Object
         (GPR2.Source_Reference.Attribute.Create
            (Self.Data.Trees.Project.Path_Name.Value, 0, 0, Attr)));

   function SR_Identifier
     (Self : Object;
      Id   : Name_Type)
      return Source_Reference.Identifier.Object
   is (GPR2.Source_Reference.Identifier.Object
         (GPR2.Source_Reference.Identifier.Create
            (Self.Data.Trees.Project.Path_Name.Value, 0, 0, Id)));

   function SR_Index
     (Self  : Object;
      Index : Value_Type)
     return GPR2.Project.Attribute_Index.Object
   is (GPR2.Project.Attribute_Index.Create
         (GPR2.Source_Reference.Value.Object
            (GPR2.Source_Reference.Value.Create
               (Self.Data.Trees.Project.Path_Name.Value, 0, 0,
                Index)),
          Is_Others      => False,
          Case_Sensitive => False));

   function SR_Value
     (Self : Object; Value : Value_Type) return Source_Reference.Value.Object
   is (GPR2.Source_Reference.Value.Object
       (GPR2.Source_Reference.Value.Create
          (Self.Data.Trees.Project.Path_Name.Value, 0, 0,
           Text     => Value)));

   function SR_Values
     (Self : Object; Values : Containers.Value_List)
      return Containers.Source_Value_List;

   ------------
   -- Create --
   ------------

   function Create
     (Project_Dir : GPR2.Path_Name.Object;
      Name        : Name_Type;
      Qualifier   : Project_Kind := K_Standard) return Object
   is
      Gpr    : constant Filename_Type :=
                 Filename_Type (To_Lower (Name)) & ".gpr";
      Gpr_Path : constant GPR2.Path_Name.Object :=
                   Project_Dir.Compose (Gpr);
      Result : Object;

   begin
      Result.Data.Kind          := Qualifier;
      Result.Data.Path          := Project_Dir;
      Result.Data.Is_Root       := True;
      Result.Data.Unique_Id     := GPR2.View_Ids.Create (Gpr_Path);
      Result.Data.Trees.Project :=
        GPR2.Project_Parser.Create
          (Name      => Name,
           File      => Gpr_Path,
           Qualifier => Qualifier);

      return Result;
   end Create;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute
     (Self   : in out Object;
      Q_Name : Q_Attribute_Id;
      Attr   : GPR2.Project.Attribute.Object)
   is
   begin
      if Q_Name.Pack = Project_Level_Scope then
         Self.Data.Attrs.Include (Attr);

      elsif not Self.Data.Packs.Contains (Q_Name.Pack) then
         declare
            Pack : GPR2.Pack_Internal.Object;
         begin
            Pack := GPR2.Pack_Internal.Object'
              (Source_Reference.Pack.Object
                 (Source_Reference.Pack.Create
                      (Attr.Filename, 0, 0, Q_Name.Pack)) with
               Project.Attribute.Set.Empty_Set,
               Project.Variable.Set.Empty_Set);
            Pack.Attrs.Insert (Attr);
            Self.Data.Packs.Insert (Q_Name.Pack, Pack);
         end;

      else
         Self.Data.Packs (Q_Name.Pack).Attrs.Include (Attr);
      end if;
   end Set_Attribute;

   procedure Set_Attribute
     (Self  : in out Object;
      Attr  : Q_Attribute_Id;
      Value : Value_Type)
   is
      use Project.Registry.Attribute;
      Attr_Def : constant Def := Project.Registry.Attribute.Get (Attr);
   begin
      if Attr_Def.Value = Single then
         Self.Set_Attribute
           (Attr,
            GPR2.Project.Attribute.Create
              (SR_Attr (Self, Attr), SR_Value (Self, Value)));
      else
         declare
            Values : Containers.Value_List;
         begin
            Values.Append (Value);
            Self.Set_Attribute (Attr, Values);
         end;
      end if;
   end Set_Attribute;

   procedure Set_Attribute
     (Self   : in out Object;
      Attr   : Q_Attribute_Id;
      Values : Containers.Value_List) is
   begin
      Self.Set_Attribute
        (Attr,
         Project.Attribute.Create
           (SR_Attr (Self, Attr),
            SR_Values (Self, Values)));
   end Set_Attribute;

   procedure Set_Attribute
     (Self  : in out Object;
      Attr  : Q_Attribute_Id;
      Index : Value_Type;
      Value : Value_Type)
   is
      use Project.Registry.Attribute;
      Attr_Def : constant Def := Project.Registry.Attribute.Get (Attr);
   begin
      if Attr_Def.Value = Single then
         Self.Set_Attribute
           (Attr,
            GPR2.Project.Attribute.Create
              (SR_Attr (Self, Attr),
               SR_Index (Self, Index),
               SR_Value (Self, Value)));
      else
         declare
            Values : Containers.Value_List;
         begin
            Values.Append (Value);
            Self.Set_Attribute (Attr, Index, Values);
         end;
      end if;
   end Set_Attribute;

   procedure Set_Attribute
     (Self   : in out Object;
      Attr   : Q_Attribute_Id;
      Index  : Value_Type;
      Values : Containers.Value_List) is
   begin
      Self.Set_Attribute
        (Attr,
         GPR2.Project.Attribute.Create
           (SR_Attr (Self, Attr),
            SR_Index (Self, Index),
            SR_Values (Self, Values)));
   end Set_Attribute;

   procedure Set_Variable
     (Self     : in out Object;
      Var_Name : Name_Type;
      Value    : Value_Type)
   is
      Var : constant GPR2.Project.Variable.Object :=
              GPR2.Project.Variable.Create
                (SR_Identifier (Self, Var_Name), SR_Value (Self, Value));
   begin
      Self.Data.Vars.Include (Var_Name, Var);
   end Set_Variable;

   ---------------
   -- SR_Values --
   ---------------

   function SR_Values
     (Self : Object; Values : Containers.Value_List)
      return Containers.Source_Value_List
   is
      Result : Containers.Source_Value_List;
   begin
      for V of Values loop
         Result.Append (SR_Value (Self, V));
      end loop;

      return Result;
   end SR_Values;

end GPR2.Tree_Internal.View_Builder;
