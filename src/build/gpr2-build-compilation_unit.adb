--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Maps.Constants;

with GPR2.Message;
with GPR2.Project.Tree;
with GPR2.Source_Reference;

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
      Sep_Name : Optional_Name_Type := "";
      Success  : out Boolean)
   is
      UL : constant Unit_Location :=
             (View   => View,
              Source => Path,
              Index  => Index);
   begin
      Success := False;

      case Kind is
         when S_Spec =>
            if Self.Spec = No_Unit then
               Self.Spec := UL;

               if not Self.Has_Part (S_Body) then
                  Self.Owner := UL.View;
               end if;

               Success := True;
            end if;

         when S_Body =>
            if Self.Implem = No_Unit then
               Self.Implem := UL;
               Self.Owner  := UL.View;
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

   -------------------------
   -- Check_Name_Validity --
   -------------------------

   procedure Check_Name_Validity
     (Self     : Object;
      Messages : in out GPR2.Log.Object)
   is
      use Ada.Strings.Maps;

      procedure Error (Message : String);
      function Sloc return GPR2.Source_Reference.Object'Class;

      procedure Error (Message : String)
      is
      begin
         Messages.Append
           (GPR2.Message.Create (GPR2.Message.Error, Message, Sloc));
      end Error;

      function Sloc return GPR2.Source_Reference.Object'Class
      is
         Path : GPR2.Path_Name.Object;
      begin
         if Self.Implem /= No_Unit then
            Path := Self.Implem.Source;
         elsif Self.Spec /= No_Unit then
            Path := Self.Spec.Source;
         elsif not Self.Separates.Is_Empty then
            Path := Self.Separates.First_Element.Source;
         end if;

         if Path.Is_Defined then
            return GPR2.Source_Reference.Create (Path.Value, 0, 0);
         else
            return GPR2.Source_Reference.Undefined;
         end if;
      end Sloc;

      Unit_Name : constant String := To_String (Self.Name);

      Not_Valid : constant String :=
                    "invalid name for unit '" & Unit_Name & "', ";

   begin
      --  Must start with a letter

      if not Is_In
        (Unit_Name (Unit_Name'First),
         Constants.Letter_Set or To_Set ("_"))
      then
         Error (Not_Valid & "should start with a letter or an underscore");
         return;
      end if;

      --  Cannot have dots and underscores one after another and should
      --  contain only alphanumeric characters.

      for K in Unit_Name'First + 1 .. Unit_Name'Last loop
         declare
            Two_Chars : constant String := Unit_Name (K - 1 .. K);
         begin
            if Two_Chars = "_." then
               Error (Not_Valid & "cannot contain dot after underscore");
               return;

            elsif Two_Chars = "__" then
               Error (Not_Valid & "two consecutive underscores not permitted");
               return;

            elsif Two_Chars = "._" then
               Error (Not_Valid & "cannot contain underscore after dot");
               return;

            elsif Two_Chars = ".." then
               Error (Not_Valid & "two consecutive dots not permitted");
               return;

            elsif not Characters.Handling.Is_Alphanumeric (Unit_Name (K))
              and then Unit_Name (K) not in '.' | '_'
            then
               Error (Not_Valid & "should have only alpha numeric characters");
               return;
            end if;
         end;
      end loop;
   end Check_Name_Validity;

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
            Self.Spec.Source,
            Self.Spec.Index,
            "");
      end if;

      if Self.Has_Part (S_Body) then
         Action
           (S_Body,
            Self.Implem.View,
            Self.Implem.Source,
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
               Unit.Source,
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
      BN   : constant Simple_Name := Main.Source.Base_Filename;
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
      View     : GPR2.Project.View.Object;
      Path     : GPR2.Path_Name.Object;
      Index    : Unit_Index := No_Index;
      Sep_Name : Optional_Name_Type := "")
   is
      UL    : constant Unit_Location :=
                (View   => View,
                 Source => Path,
                 Index  => Index);
      C     : Separate_Maps.Cursor;
      CD    : Duplicates_List.Cursor;
      Found : Boolean := False;

   begin
      case Kind is
         when S_Spec =>
            if Self.Spec = UL then
               Self.Spec := No_Unit;

               if not Self.Has_Part (S_Body) then
                  Self.Owner := Project.View.Undefined;
               end if;

               Found := True;
            end if;

         when S_Body =>
            if Self.Implem = UL then
               Self.Implem := No_Unit;

               if not Self.Has_Part (S_Spec) then
                  Self.Owner := Project.View.Undefined;
               else
                  Self.Owner := Self.Spec.View;
               end if;

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
                            Elem.Loc.Source,
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
