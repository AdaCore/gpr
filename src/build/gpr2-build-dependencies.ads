--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Ordered_Sets;

package GPR2.Build.Dependencies is

   subtype Part_Kind is Unit_Kind
     with Static_Predicate => Part_Kind in S_Spec | S_Body;

   type Unit_Part (Name_Length : Natural) is record
      Name : Name_Type (1 .. Name_Length);
      --  Unit name without the ALI %s or %b suffix

      Part : Part_Kind;
      --  Specification or body of this unit
   end record;

   function "<" (Left, Right : Unit_Part) return Boolean;

   package Unit_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => Unit_Part);
   subtype Unit_Set is Unit_Sets.Set;

   type Dependency (Filename_Length : Natural) is record
      Filename : Filename_Type (1 .. Filename_Length);
      --  Dependency filename as recorded by the compiler

      Units : Unit_Set;
      --  Unit parts described for this file. An empty set means no unit
      --  information is available, including for non-Ada dependencies and
      --  older ALI formats.
   end record;

   function "<" (Left, Right : Dependency) return Boolean;
   --  Compare filenames only: each file has one dependency record

   package Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => Dependency);
   subtype Set is Sets.Set;

   function Filename (Item : Dependency) return Filename_Type;

   package Filenames is new Sets.Generic_Keys
     (Key_Type => Filename_Type, Key => Filename);
   --  Filename lookup into the set

   procedure Include (Self : in out Set; Filename : Filename_Type);
   --  Add a dependency with the provided filename and no units. Does nothing
   --  if a dependency with this filename already exists.

   procedure Include
     (Self : in out Set; Filename : Filename_Type; Unit : Unit_Part);
   --  Merge a unit part into the dependency for Filename, without duplicates

private

   function "<" (Left, Right : Unit_Part) return Boolean
   is (Left.Name < Right.Name
       or else (Left.Name = Right.Name and then Left.Part < Right.Part));

   function "<" (Left, Right : Dependency) return Boolean
   is (Left.Filename < Right.Filename);

   function Filename (Item : Dependency) return Filename_Type
   is (Item.Filename);

end GPR2.Build.Dependencies;
