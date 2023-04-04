--
--  Copyright (C) 2022-2023, AdaCore
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
      View     : GPR2.View_Ids.View_Id;
      Path     : GPR2.Path_Name.Object;
      Index    : Unit_Index := No_Index;
      Sep_Name : Optional_Name_Type := "";
      Success  : out Boolean)
   is
      UL : constant Unit_Location :=
             (View  => View,
              Path  => Path,
              Index => Index);
   begin
      Success := False;

      case Kind is
         when S_Spec =>
            if Self.Spec = No_Unit then
               Self.Spec := UL;
               Success := True;
            end if;

         when S_Body =>
            if Self.Implem = No_Unit then
               Self.Implem := UL;
               Success := True;
            end if;

         when S_Separate =>
            declare
               Up : constant String :=
                      Ada.Characters.Handling.To_Upper (String (Sep_Name));
               C  : Separate_Maps.Cursor;
            begin
               Self.Separates.Insert (Name_Type (Up), UL, C, Success);
            end;
      end case;

      if not Success then
         Self.Duplicates.Append ((Sep_Name'Length, UL, Kind, Sep_Name));
      end if;
   end Add;

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
         View     : View_Ids.View_Id;
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

   function Object_File
     (Self : Object;
      Tree : GPR2.Project.Tree.Object) return Simple_Name
   is
      Main : constant Unit_Location := Self.Main_Part;
      BN   : constant Simple_Name := Main.Path.Base_Filename;
   begin
      if Main.Index = No_Index then
         return BN & Tree.Object_Suffix (Ada_Language);
      else
         declare
            Idx_Img : constant String := Main.Index'Image;
         begin
            return BN & "~" &
              Simple_Name (Idx_Img (Idx_Img'First + 1 .. Idx_Img'Last)) &
              Tree.Object_Suffix (Ada_Language);
         end;
      end if;
   end Object_File;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self     : in out Object;
      Kind     : Unit_Kind;
      View     : GPR2.View_Ids.View_Id;
      Path     : GPR2.Path_Name.Object;
      Index    : Unit_Index := No_Index;
      Sep_Name : Optional_Name_Type := "")
   is
      UL    : constant Unit_Location :=
                (View  => View,
                 Path  => Path,
                 Index => Index);
      C     : Separate_Maps.Cursor;
      CD    : Duplicates_List.Cursor;
      Found : Boolean := False;

   begin
      case Kind is
         when S_Spec =>
            if Self.Spec = UL then
               Self.Spec := No_Unit;

               Found := True;
            end if;

         when S_Body =>
            if Self.Implem = UL then
               Self.Implem := No_Unit;

               Found := True;
            end if;

         when S_Separate =>
            C := Self.Separates.Find (Sep_Name);

            if not Separate_Maps.Has_Element (C) then
               return;
            end if;

            if Separate_Maps.Element (C) = UL then
               Self.Separates.Delete (C);

               Found := True;
            end if;
      end case;

      if not Found then
         --  no matching unit part: check the list of duplicates
         CD := Self.Duplicates.First;

         while Duplicates_List.Has_Element (CD) loop
            declare
               Elem : constant Duplicates_List.Constant_Reference_Type :=
                        Self.Duplicates.Constant_Reference (CD);
            begin
               if Elem.Kind = Kind
                 and then Elem.Loc = UL
               then
                  Found := True;
               end if;
            end;

            if Found then
               Self.Duplicates.Delete (CD);
               exit;
            else
               Duplicates_List.Next (CD);
            end if;
         end loop;

      elsif not Self.Duplicates.Is_Empty then
         --  We removed a unit part, but then we have somewhere else
         --  another source that contains a part of this compilation unit.
         --  Let's check if this part replaces the removed part

         Found := False;
         CD := Self.Duplicates.First;

         while Duplicates_List.Has_Element (CD) loop
            declare
               Elem : constant Clashing_Unit :=
                        Duplicates_List.Element (CD);
            begin
               if Elem.Kind = Kind then
                  Self.Duplicates.Delete (CD);
                  Self.Add (Elem.Kind,
                            Elem.Loc.View,
                            Elem.Loc.Path,
                            Elem.Loc.Index,
                            Elem.Sep_Name,
                            Found);

                  exit;
               else
                  Duplicates_List.Next (CD);
               end if;
            end;
         end loop;
      end if;
   end Remove;

end GPR2.Build.Compilation_Unit;
