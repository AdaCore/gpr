--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Sets;
use type Ada.Containers.Hash_Type;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Text_IO;

with GPR2;
with GPR2.Build.Unit_Info;
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
      Result : String := Strings.UTF_Encoding.Strings.Decode (String (Name));
      Up     : Boolean := True;
   begin
      for C of Result loop
         C  := (if Up
                then Characters.Handling.To_Upper (C)
                else Characters.Handling.To_Lower (C));
         Up := not Up;
      end loop;

      return Optional_Name_Type
        (Strings.UTF_Encoding.Strings.Encode
           (Result, Strings.UTF_Encoding.UTF_8));
   end Alternating;

   -----------
   -- Lower --
   -----------

   function Lower (Name : Optional_Name_Type) return Optional_Name_Type is
      Result : String := Strings.UTF_Encoding.Strings.Decode (String (Name));
   begin
      for C of Result loop
         C := Characters.Handling.To_Lower (C);
      end loop;

      return Optional_Name_Type
        (Strings.UTF_Encoding.Strings.Encode
           (Result, Strings.UTF_Encoding.UTF_8));
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
      Result : String := Strings.UTF_Encoding.Strings.Decode (String (Name));
   begin
      for C of Result loop
         C := Characters.Handling.To_Upper (C);
      end loop;

      return Optional_Name_Type
        (Strings.UTF_Encoding.Strings.Encode
           (Result, Strings.UTF_Encoding.UTF_8));
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

   --  Exercise UTF-8 case variants across the hash buffer boundary

   Accented : constant Optional_Name_Type :=
                Optional_Name_Type
                  (Strings.UTF_Encoding.Strings.Encode
                     (String'
                        (Character'Val (16#C9#), Character'Val (16#E0#),
                         Character'Val (16#D4#), Character'Val (16#FC#)),
                      Strings.UTF_Encoding.UTF_8));

begin
   declare
      UTF8_Name : constant Name_Type :=
                    "Foo_" & Character'Val (16#C3#) & Character'Val (16#89#);
      Expected  : constant String :=
                    "foo_" & Character'Val (16#C3#) & Character'Val (16#A9#);
      Path_Name : constant String :=
                    "foo_" & Character'Val (16#C3#) & Character'Val (16#89#);
   begin
      if GPR2.To_Lower (UTF8_Name) /= Expected
        or else GPR2.To_Lower (Filename_Optional (UTF8_Name)) /= Path_Name
      then
         raise Program_Error with "unexpected identifier or path casing";
      end if;

      if GPR2.To_Lower (Filename_Optional'("")) /= "" then
         raise Program_Error with "empty filename casing failed";
      end if;
   end;

   for Code in 128 .. 255 loop
      declare
         C        : constant Character := Character'Val (Code);
         Name     : constant Name_Type := Name_Type
           (Strings.UTF_Encoding.Strings.Encode
              (String'(1 => C), Strings.UTF_Encoding.UTF_8));
         Expected : constant String :=
           (if Code = 16#B5# then "μ"
            else Strings.UTF_Encoding.Strings.Encode
              (String'(1 => Characters.Handling.To_Lower (C))));
         Upper    : constant String :=
           (if Code = 16#B5# then "Μ"
            elsif Code = 16#FF# then "Ÿ"
            else Strings.UTF_Encoding.Strings.Encode
              (String'(1 => Characters.Handling.To_Upper (C))));
         High     : constant Name_Type
                      (Positive'Last - Name'Length + 1 .. Positive'Last) :=
                        Name;
         Info     : constant Build.Unit_Info.Object := Build.Unit_Info.Create
           (Unit_Name     => High,
            Index         => No_Index,
            Kind          => S_Separate,
            Separate_Name => "Child_" & Name);
      begin
         if GPR2.To_Lower (Name) /= Expected
           or else GPR2.To_Lower (High) /= Expected
           or else GPR2.To_Lower (Name_Type (GPR2.To_Lower (Name))) /= Expected
         then
            raise Program_Error with "Latin-1 identifier folding failed";
         end if;

         if GPR2.To_Upper (Name) /= Upper
           or else GPR2.To_Upper (High) /= Upper
           or else GPR2.To_Upper (Name_Type (GPR2.To_Upper (Name))) /= Upper
         then
            raise Program_Error with "Latin-1 identifier uppercase failed";
         end if;

         if String (Info.Name) /= Upper
           or else String (Info.Separate_Name) /= "CHILD_" & Upper
           or else String (Info.Full_Name) /=
                     Upper & ".CHILD_" & Upper
         then
            raise Program_Error with "Latin-1 unit name folding failed";
         end if;
      end;
   end loop;

   declare
      Wide_Name : constant Name_Type := "Foo_Ā字𐐨";
      High      : constant Name_Type
                    (Positive'Last .. Positive'Last) := "A";
      Info      : constant Build.Unit_Info.Object := Build.Unit_Info.Create
        (Unit_Name => Wide_Name, Index => No_Index, Kind => S_Body);
      No_Body   : constant Build.Unit_Info.Object := Build.Unit_Info.Create
        (Unit_Name => No_Name, Index => No_Index, Kind => S_No_Body);
   begin
      if GPR2.To_Lower (Wide_Name) /= "foo_ā字𐐨"
        or else GPR2.To_Lower (High) /= "a"
        or else GPR2.To_Upper (Wide_Name) /= "FOO_Ā字𐐀"
        or else GPR2.To_Upper (High) /= "A"
        or else String (Info.Name) /= "FOO_Ā字𐐀"
        or else No_Body.Name'Length /= 0
        or else No_Body.Separate_Name'Length /= 0
      then
         raise Program_Error with "non-Latin-1 identifier folding failed";
      end if;
   end;

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
