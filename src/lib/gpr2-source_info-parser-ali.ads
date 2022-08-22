--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

private with Ada.Containers.Indefinite_Hashed_Maps;
private with GPR2.Path_Name;
private with GPR2.Unit;

package GPR2.Source_Info.Parser.ALI is

   type Object is new Parser.Object
     (Language => Ada_Language,
      Kind     => LI) with private;

   overriding procedure Compute
     (Self   : not null access Object;
      Data   : in out Source_Info.Object'Class;
      Source : Project.Source.Object);
   --  Setup Data with the information from GNAT .ali file

   overriding procedure Clear_Cache (Self : not null access Object);
   --  Clear cached ALI data

private

   use type Ada.Containers.Hash_Type;
   use type GPR2.Unit.Object;

   type Cache_Holder is record
      Unit      : GPR2.Unit.Object;
      Checksum  : Word;
      Timestamp : Ada.Calendar.Time;
      Depends   : Dependency_Vectors_Ref.Ref;
   end record;
   --  When parsing ALI files, we retrieve information for the whole
   --  compilation unit at once. This information will be split into
   --  potentially several source_info objects (spec, body and separates).
   --  In order to not re-parse the ALI file for each of those parts, we
   --  cache the information.

   overriding function "=" (L, R : Cache_Holder) return Boolean is
     (L.Unit = R.Unit and then
      L.Checksum = R.Checksum);

   type Cache_Key (LI_Length  : Natural; Src_Length : Natural) is record
      LI      : Filename_Type (1 .. LI_Length);
      Src     : Filename_Type (1 .. Src_Length);
      LI_Kind : GPR2.Unit.Library_Unit_Type;
   end record;

   function Image (Key : Cache_Key) return String is
     (String (Key.LI) & "@" &
        GPR2.Path_Name.Simple_Name (String (Key.Src)) & "%" &
      (case Key.LI_Kind is
       when GPR2.Unit.Body_Kind => "b",
       when GPR2.Unit.Spec_Kind => "s",
       when GPR2.Unit.S_Separate => "sep"));

   function Hash (Key : Cache_Key) return Ada.Containers.Hash_Type is
     (Hash (Key.LI) + Hash (Key.Src) +
          Ada.Containers.Hash_Type
           (GPR2.Unit.Library_Unit_Type'Pos (Key.LI_Kind)));

   function Equivalent_Cache_Keys (Left, Right : Cache_Key) return Boolean is
     (Left.LI = Right.LI
      and then Left.Src = Right.Src
      and then Left.LI_Kind = Right.LI_Kind);
   --  This function is for map container. We can't use default "=" function
   --  because it is binary comparision, but we should compare Filename_Type
   --  components with its overriden "=" function.

   package Cache_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Cache_Key, Cache_Holder, Hash, Equivalent_Keys => Equivalent_Cache_Keys);
   --  This caches the information for spec and body units. The key identifies
   --  the ALI file, the source and the kind of library unit.

   package Sep_Cache_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Cache_Holder, Ada.Strings.Hash, "=");
   --  This caches the information for separate units. Those can be retrieved
   --  by their unit name (lower-cased).

   type Object is new Parser.Object
     (Language => Ada_Language,
      Kind     => LI)
   with record
      Cache     : Cache_Map.Map;
      Sep_Cache : Sep_Cache_Map.Map;
   end record;

end GPR2.Source_Info.Parser.ALI;
