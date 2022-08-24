--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body GPR2.Project.Unit_Info is

   ------------
   -- Create --
   ------------

   function Create
     (Name      : Name_Type;
      Spec      : Unit.Source_Unit_Identifier := Unit.Undefined_Id;
      Main_Body : Unit.Source_Unit_Identifier := Unit.Undefined_Id;
      Separates : Unit.Source_Unit_Vectors.Vector :=
        Unit.Source_Unit_Vectors.Empty_Vector) return Object is
   begin
      return Object'(To_Unbounded_String (String (Name)),
                     Spec,
                     Main_Body,
                     Separates);
   end Create;

   -----------------
   -- Remove_Body --
   -----------------

   procedure Remove_Body (Self : in out Object) is
   begin
      Self.Main_Body := Unit.Undefined_Id;
   end Remove_Body;

   -----------------
   -- Update_Body --
   -----------------

   procedure Update_Body
     (Self : in out Object; Source : Unit.Source_Unit_Identifier) is
   begin
      Self.Main_Body := Source;
   end Update_Body;

   -----------------
   -- Update_Name --
   -----------------

   procedure Update_Name
     (Self : in out Object; Name : Name_Type) is
   begin
      Self.Name := To_Unbounded_String (String (Name));
   end Update_Name;

   ----------------------
   -- Update_Separates --
   ----------------------

   procedure Update_Separates
     (Self : in out Object; Source : Unit.Source_Unit_Identifier) is
   begin
      Self.Separates.Append (Source);
   end Update_Separates;

   -----------------
   -- Update_Spec --
   -----------------

   procedure Update_Spec
     (Self : in out Object; Source : Unit.Source_Unit_Identifier) is
   begin
      Self.Spec := Source;
   end Update_Spec;

end GPR2.Project.Unit_Info;
