--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.Project.Tree;

package body GPR2.Build.Compilation_Unit is

   ---------
   -- Add --
   ---------

   procedure Add
     (Self     : in out Object;
      Kind     : Unit_Kind;
      View     : GPR2.Project.View.Object;
      Path     : GPR2.Path_Name.Object;
      Index    : Unit_Index := No_Index;
      Sep_Name : Optional_Name_Type := "")
   is
      UL : constant Unit_Location :=
             (View  => View,
              Path  => Path,
              Index => Index);
   begin
      case Kind is
         when S_Spec =>
            Self.Spec := UL;
         when S_Body =>
            Self.Implem := UL;
         when S_Separate =>
            declare
               Up : constant String :=
                      Ada.Characters.Handling.To_Upper (String (Sep_Name));
            begin
               Self.Separates.Include (Name_Type (Up), UL);
            end;
      end case;
   end Add;

   ------------------
   -- Add_Separate --
   ------------------

   procedure Add_Separate
     (Self  : in out Object;
      Name  : Name_Type;
      View  : Project.View.Object;
      Path  : Path_Name.Object;
      Index : Unit_Index := No_Index)
   is
   begin
      Self.Add
        (S_Separate,
         View,
         Path,
         Index,
         Name);
   end Add_Separate;

   ------------
   -- Create --
   ------------

   function Create
     (Name : Name_Type) return Object
   is
   begin
      return (Name   => To_Unbounded_String
                          (Ada.Characters.Handling.To_Upper (String (Name))),
              others => <>);
   end Create;

   ------------------
   -- For_All_Part --
   ------------------

   procedure For_All_Part
     (Self : Object;
      Action : access procedure
        (Kind     : Unit_Kind;
         View     : Project.View.Object;
         Path     : Path_Name.Object;
         Index    : Unit_Index;
         Sep_Name : Optional_Name_Type)) is
   begin
      if Self.Has_Part (S_Spec) then
         Action
           (S_Spec,
            Self.Spec.View,
            Self.Spec.Path,
            Self.Spec.Index,
            "");
      end if;

      if Self.Has_Part (S_Body) then
         Action
           (S_Body,
            Self.Implem.View,
            Self.Implem.Path,
            Self.Implem.Index,
            "");
      end if;

      for C in Self.Separates.Iterate loop
         declare
            Sep_Name : constant Name_Type :=
                         Separate_Maps.Key (C);
            Unit     : constant Unit_Location :=
                         Separate_Maps.Element (C);
         begin
            Action
              (S_Separate,
               Unit.View,
               Unit.Path,
               Unit.Index,
               Sep_Name);
         end;
      end loop;
   end For_All_Part;

   ---------
   -- Get --
   ---------

   function Get
     (Self     : Object;
      Kind     : Unit_Kind;
      Sep_Name : Optional_Name_Type) return Unit_Location
   is
   begin
      case Kind is
         when S_Spec =>
            return Self.Spec;
         when S_Body =>
            return Self.Implem;
         when S_Separate =>
            return Self.Separates.Element (Sep_Name);
      end case;
   end Get;

   -----------------
   -- Object_File --
   -----------------

   function Object_File (Self : Object) return Simple_Name
   is
      Main : constant Unit_Location := Self.Main_Part;
      BN   : constant Simple_Name := Main.Path.Base_Filename;
   begin
      if Main.Index = No_Index then
         return BN & Main.View.Tree.Object_Suffix (Ada_Language);
      else
         declare
            Idx_Img : constant String := Main.Index'Image;
         begin
            return BN & "~" &
              Simple_Name (Idx_Img (Idx_Img'First + 1 .. Idx_Img'Last)) &
              Main.View.Tree.Object_Suffix (Ada_Language);
         end;
      end if;
   end Object_File;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self     : in out Object;
      Kind     : Unit_Kind;
      Sep_Name : Optional_Name_Type := "") is
   begin
      case Kind is
         when S_Spec =>
            Self.Spec := No_Unit;
         when S_Body =>
            Self.Implem := No_Unit;
         when S_Separate =>
            Self.Separates.Delete (Sep_Name);
      end case;
   end Remove;

   -----------------
   -- Remove_Body --
   -----------------

   procedure Remove_Body (Self : in out Object) is
   begin
      Self.Implem := No_Unit;
   end Remove_Body;

   ---------------------
   -- Remove_Separate --
   ---------------------

   procedure Remove_Separate
     (Self : in out Object;
      Name : Name_Type) is
   begin
      Self.Separates.Delete (Name);
   end Remove_Separate;

   -----------------
   -- Remove_Spec --
   -----------------

   procedure Remove_Spec (Self : in out Object) is
   begin
      Self.Spec := No_Unit;
   end Remove_Spec;

   -----------------
   -- Update_Body --
   -----------------

   procedure Update_Body
     (Self : in out Object;
      View : Project.View.Object;
      Path : Path_Name.Object;
      Index : Unit_Index := No_Index)
   is
   begin
      Self.Implem :=
        (View  => View,
         Path  => Path,
         Index => Index);
   end Update_Body;

   -----------------
   -- Update_Spec --
   -----------------

   procedure Update_Spec
     (Self  : in out Object;
      View  : Project.View.Object;
      Path  : Path_Name.Object;
      Index : Unit_Index := No_Index)
   is
   begin
      Self.Spec :=
        (View  => View,
         Path  => Path,
         Index => Index);
   end Update_Spec;

end GPR2.Build.Compilation_Unit;
