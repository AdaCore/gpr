--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Sets;
use type Ada.Containers.Hash_Type;
with Ada.Text_IO;

with GPR2;
with GPR2.Path_Name;

procedure Main is

   use Ada;
   use GPR2;

   package Name_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Optional_Name_Type, GPR2.Hash, GPR2."=");
   --  The pair every hashed container keyed by a name relies on

   function Upper (Name : Optional_Name_Type) return Optional_Name_Type;
   function Lower (Name : Optional_Name_Type) return Optional_Name_Type;
   function Alternating (Name : Optional_Name_Type) return Optional_Name_Type;

   procedure Check (Label : String; Name : Optional_Name_Type);
   --  Report whether the case variants of Name hash equally, compare equal,
   --  and are found in a set holding the lower case one only

   procedure Check_Bounds (Label : String; Length : Positive);
   --  A name ending at Positive'Last hashes like the same name starting at 1

   function Repeat
     (Pattern : Optional_Name_Type; Length : Natural)
      return Optional_Name_Type;
   --  Pattern repeated up to exactly Length characters

   ----------------
   -- Alternating --
   ----------------

   function Alternating (Name : Optional_Name_Type) return Optional_Name_Type
   is
      Result : String := String (Name);
      Up     : Boolean := True;
   begin
      for C of Result loop
         C  := (if Up
                then Characters.Handling.To_Upper (C)
                else Characters.Handling.To_Lower (C));
         Up := not Up;
      end loop;

      return Optional_Name_Type (Result);
   end Alternating;

   -----------
   -- Lower --
   -----------

   function Lower (Name : Optional_Name_Type) return Optional_Name_Type is
      Result : String := String (Name);
   begin
      for C of Result loop
         C := Characters.Handling.To_Lower (C);
      end loop;

      return Optional_Name_Type (Result);
   end Lower;

   ------------
   -- Repeat --
   ------------

   function Repeat
     (Pattern : Optional_Name_Type; Length : Natural)
      return Optional_Name_Type
   is
      Result : String (1 .. Length);
      Source : constant String := String (Pattern);
   begin
      for J in Result'Range loop
         Result (J) := Source (Source'First + (J - 1) mod Source'Length);
      end loop;

      return Optional_Name_Type (Result);
   end Repeat;

   -----------
   -- Upper --
   -----------

   function Upper (Name : Optional_Name_Type) return Optional_Name_Type is
      Result : String := String (Name);
   begin
      for C of Result loop
         C := Characters.Handling.To_Upper (C);
      end loop;

      return Optional_Name_Type (Result);
   end Upper;

   -----------
   -- Check --
   -----------

   procedure Check (Label : String; Name : Optional_Name_Type) is
      L       : constant Optional_Name_Type := Lower (Name);
      U       : constant Optional_Name_Type := Upper (Name);
      A       : constant Optional_Name_Type := Alternating (Name);
      Hashes  : constant Boolean :=
                  GPR2.Hash (L) = GPR2.Hash (U)
                    and then GPR2.Hash (L) = GPR2.Hash (A);
      Equals  : constant Boolean := L = U and then L = A;
      Set     : Name_Sets.Set;
      Found   : Boolean;
   begin
      Set.Include (L);
      Found := Set.Contains (U) and then Set.Contains (A);

      Text_IO.Put_Line
        (Label & ": length" & Natural'Image (String (Name)'Length)
         & ", hashes equal " & Boolean'Image (Hashes)
         & ", equal " & Boolean'Image (Equals)
         & ", found " & Boolean'Image (Found));
   end Check;

   ------------------
   -- Check_Bounds --
   ------------------

   procedure Check_Bounds (Label : String; Length : Positive) is
      Normal : constant Optional_Name_Type := Repeat ("AbCdEfGh", Length);
      High   : constant Optional_Name_Type
                 (Positive'Last - (Length - 1) .. Positive'Last) := Normal;
   begin
      Text_IO.Put_Line
        (Label & ": hashes match across bounds "
         & Boolean'Image (GPR2.Hash (Normal) = GPR2.Hash (High)));
   end Check_Bounds;

   --  Latin-1 letters that fold: E acute, A grave, O circumflex, U diaeresis

   Accented : constant Optional_Name_Type :=
                Optional_Name_Type
                  (String'(Character'Val (16#C9#), Character'Val (16#E0#),
                           Character'Val (16#D4#), Character'Val (16#FC#)));

begin
   Check ("empty          ", "");
   Check ("short          ", "Foo_Bar");
   Check ("accented short ", Accented);

   --  256 is the size of the folding buffer: exercise both sides of it

   Check ("exactly 256    ", Repeat ("AbCdEfGh", 256));
   Check ("just over 256  ", Repeat ("AbCdEfGh", 257));
   Check ("well over 256  ", Repeat ("AbCdEfGh", 1000));
   Check ("accented long  ", Repeat (Accented, 600));

   Check_Bounds ("high bound 1   ", 1);
   Check_Bounds ("high bound 256 ", 256);
   Check_Bounds ("high bound 257 ", 257);
   Check_Bounds ("high bound 512 ", 512);

   --  An undefined path has an empty comparing form, which must not reach
   --  the hash either

   Text_IO.Put_Line
     ("undefined path : hash"
      & Ada.Containers.Hash_Type'Image
          (GPR2.Path_Name.Hash (GPR2.Path_Name.Undefined)));
end Main;
