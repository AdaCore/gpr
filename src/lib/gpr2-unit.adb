--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Maps.Constants;

package body GPR2.Unit is

   ------------
   -- As_Ref --
   ------------

   function As_Ref
     (Element : Source_Reference.Identifier.Set.Object)
      return Dependencies_Ref.Ref
   is
      Result : Dependencies_Ref.Ref;
   begin
      Result.Set (Element);
      return Result;
   end As_Ref;

   -----------------------
   -- Set_Separate_From --
   -----------------------

   procedure Set_Separate_From (Self : in out Object; Name : Name_Type) is
   begin
      Self.Sep_From := +String (Name);
      Self.Kind     := S_Separate;
   end Set_Separate_From;

   ------------------
   -- Update_Index --
   ------------------

   procedure Update_Index (Self : in out Object; Index : Unit_Index) is
   begin
      Self.Index := Index;
   end Update_Index;

   -----------------
   -- Update_Kind --
   -----------------

   procedure Update_Kind (Self : in out Object; Kind : Library_Unit_Type) is
   begin
      Self.Kind := Kind;
   end Update_Kind;

   ------------------------
   -- Update_Name_Casing --
   ------------------------

   procedure Update_Name (Self : in out Object; Name : Name_Type) is
   begin
      Self.Name := +String (Name);
   end Update_Name;

   ---------------------
   -- Valid_Unit_Name --
   ---------------------

   function Valid_Unit_Name
     (Unit_Name : Name_Type;
      On_Error  : access procedure (Message : String) := null) return Boolean
   is
      use Ada.Strings.Maps;

      function Not_Valid return String is
        ("unit '" & String (Unit_Name)  & "' not valid, ");

      procedure Error (Message : String);

      -----------
      -- Error --
      -----------

      procedure Error (Message : String) is
      begin
         if On_Error /= null then
            On_Error (Message);
         end if;
      end Error;

   begin
      --  Must start with a letter

      if not Is_In
        (Unit_Name (Unit_Name'First),
         Constants.Letter_Set or To_Set ("_"))
      then
         Error (Not_Valid & "should start with a letter or an underscore");
         return False;
      end if;

      --  Cannot have dot and underscores one after anothers and should
      --  contains only alphanumeric characters.

      for K in Unit_Name'First + 1 .. Unit_Name'Last loop
         declare
            Two_Chars : constant Name_Type := Unit_Name (K - 1 .. K);
         begin
            if Two_Chars = "_." then
               Error (Not_Valid & "cannot contain dot after underscore");
               return False;

            elsif Two_Chars = "__" then
               Error (Not_Valid & "two consecutive underlines not permitted");
               return False;

            elsif Two_Chars = "._" then
               Error (Not_Valid & "cannot contain underscore after dot");
               return False;

            elsif Two_Chars = ".." then
               Error (Not_Valid & "two consecutive dots not permitted");
               return False;

            elsif not Characters.Handling.Is_Alphanumeric (Unit_Name (K))
              and then Unit_Name (K) not in '.' | '_'
            then
               Error (Not_Valid & "should have only alpha numeric characters");
               return False;
            end if;
         end;
      end loop;

      return True;
   end Valid_Unit_Name;

end GPR2.Unit;
