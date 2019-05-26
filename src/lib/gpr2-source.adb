------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Unbounded;

with GPR2.Compilation_Unit.Map;
with GPR2.Containers;
with GPR2.Source.Parser;
with GPR2.Source.Registry;

package body GPR2.Source is

   use Ada.Strings.Unbounded;

   function Key (Self : Object) return Value_Type
     with Inline, Pre => Self.Is_Defined;
   --  Returns the key for Self, this is used to compare a source object

   procedure Update (Self : Object)
     with Inline;
   --  Run the parser on the given source and register information in the
   --  registry.

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Object) return Boolean is
   begin
      return Key (Left) < Key (Right);
   end "<";

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Object) return Boolean is
   begin
      if not Left.Pathname.Is_Defined
        and then not Right.Pathname.Is_Defined
      then
         return True;
      else
         return Left.Pathname.Is_Defined = Right.Pathname.Is_Defined
           and then Key (Left) = Key (Right);
      end if;
   end "=";

   ------------------
   -- Compil_Units --
   ------------------

   function Compilation_Units
     (Self : Object) return Compilation_Unit.List.Object is
   begin
      Update (Self);
      return Registry.Shared.Get (Self).CU_List;
   end Compilation_Units;

   ------------
   -- Create --
   ------------

   function Create
     (Filename : GPR2.Path_Name.Object;
      Language : Name_Type;
      Kind     : Kind_Type) return Object is
   begin
      return Result : Object do
         Registry.Shared.Register
           (Registry.Data'
              (Is_Ada_Source => False,
               Path_Name     => Filename,
               Timestamp     => Directories.Modification_Time (Filename.Value),
               Language      => To_Unbounded_String (String (Language)),
               Other_Part    => GPR2.Path_Name.Undefined,
               Kind          => Kind,
               Ref_Count     => 1));

         Result.Pathname := Filename;
      end return;
   end Create;

   ----------------
   -- Create_Ada --
   ----------------

   function Create_Ada
     (Filename          : GPR2.Path_Name.Object;
      Compilation_Units : Compilation_Unit.List.Object;
      Is_RTS_Source     : Boolean) return Object
   is
      CU_Map : Compilation_Unit.Map.Object;
   begin
      for CU of Compilation_Units loop
         CU_Map.Insert (CU.Index, CU);
      end loop;

      return Result : Object do
         Registry.Shared.Register
           (Registry.Data'
              (Is_Ada_Source => True,
               Path_Name     => Filename,
               Timestamp     => Directories.Modification_Time (Filename.Value),
               Language      => To_Unbounded_String (String'("Ada")),
               Other_Part    => GPR2.Path_Name.Undefined,
               Parsed        => False,
               Is_RTS_Source => Is_RTS_Source,
               Cu_List       => Compilation_Units,
               CU_Map        => CU_Map,
               Ref_Count     => 1));

         Result.Pathname := Filename;
      end return;
   end Create_Ada;

   -----------------------------
   -- Has_Compilation_Unit_At --
   -----------------------------

   function Has_Compilation_Unit_At
     (Self : Object; Index : Natural) return Boolean is
   begin
      return Registry.Shared.Get (Self).CU_Map.Contains (Index);
   end Has_Compilation_Unit_At;

   ---------------------
   -- Has_Single_Unit --
   ---------------------

   function Has_Single_Unit (Self : Object) return Boolean is
      use type Containers.Count_Type;
   begin
      return Registry.Shared.Get (Self).CU_List.Length = 1;
   end Has_Single_Unit;

   --------------
   -- Has_Unit --
   --------------

   function Has_Units (Self : Object) return Boolean is
   begin
      Update (Self);
      return Registry.Shared.Get (Self).Is_Ada_Source;
   end Has_Units;

   ---------
   -- Key --
   ---------

   function Key (Self : Object) return Value_Type is
      use Ada.Characters;
      Data : constant Registry.Data := Registry.Shared.Get (Self);
   begin
      if Data.Is_Ada_Source then
         --  In this case, the relevant information is unit name + unit kind
         declare
            Result : Unbounded_String;
         begin
            for CU of Data.CU_List loop
               Result := Result & Kind_Type'Image (CU.Kind)
                 & "|" & Handling.To_Lower (String (CU.Unit_Name));
            end loop;

            return To_String (Result);
         end;
      else
         --  Not unit based: just use the full path
         return Data.Path_Name.Value;
      end if;
   end Key;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Object; Index : Natural := 1) return Kind_Type is
   begin
      Update (Self);
      if Self.Has_Units then
         return Registry.Shared.Get (Self).CU_Map (Index).Kind;
      else
         return Registry.Shared.Get (Self).Kind;
      end if;
   end Kind;

   --------------
   -- Language --
   --------------

   function Language (Self : Object) return Name_Type is
   begin
      return Name_Type (To_String (Registry.Shared.Get (Self).Language));
   end Language;

   ----------------
   -- Other_Part --
   ----------------

   function Other_Part (Self : Object) return Object is
      Other_Part : GPR2.Path_Name.Object := GPR2.Path_Name.Undefined;
   begin
      Other_Part := Registry.Shared.Get (Self).Other_Part;

      if Other_Part = GPR2.Path_Name.Undefined then
         return Undefined;
      else
         return Object'(Pathname => Other_Part);
      end if;
   end Other_Part;

   ---------------
   -- Path_Name --
   ---------------

   function Path_Name (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Registry.Shared.Get (Self).Path_Name;
   end Path_Name;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Object) is
   begin
      Registry.Shared.Unregister (Self);
   end Release;

   --------------------
   -- Set_Other_Part --
   --------------------

   procedure Set_Other_Part (Self : Object; Other_Part : Object) is
   begin
      Registry.Shared.Set_Other_Part (Self, Other_Part);
   end Set_Other_Part;

   ----------------
   -- Time_Stamp --
   ----------------

   function Time_Stamp (Self : Object) return Calendar.Time is
   begin
      return Registry.Shared.Get (Self).Timestamp;
   end Time_Stamp;

   ---------------
   -- Unit_Name --
   ---------------

   function Unit_Name (Self : Object; Index : Natural := 1) return Name_Type is
   begin
      Update (Self);
      return Registry.Shared.Get (Self).CU_Map (Index).Unit_Name;
   end Unit_Name;

   ------------
   -- Update --
   ------------

   procedure Update (Self : Object) is
      use type Calendar.Time;

      Filename : constant String := Self.Pathname.Value;
      S        : Registry.Data := Registry.Shared.Get (Self);
   begin
      pragma Assert (Directories.Exists (Filename));

      declare
         New_TS      : constant Calendar.Time :=
                         Directories.Modification_Time (Filename);

         Updated     : Boolean := False;

         New_CU_List : Compilation_Unit.List.Object;
         New_CU_Map  : Compilation_Unit.Map.Object;

      begin
         if S.Timestamp /= New_TS then
            S.Timestamp := New_TS;
            Updated := True;
         end if;

         if S.Is_Ada_Source and then (not S.Parsed or else Updated) then
            New_CU_List := Source.Parser.Parse (S.Path_Name);

            for CU of New_CU_List loop
               declare
                  New_CU : Compilation_Unit.Object;
                  Kind   : Kind_Type := CU.Kind;

               begin
                  if CU.Kind /= S_Separate
                    and then S.CU_Map.Contains (CU.Index)
                    and then S.CU_Map.Element (CU.Index).Kind = S_Separate
                  --  ??? Add check on the unit name, but we need to compare
                  --  the new name stripped from the old unit's "sep from".
                  then
                     --  It was a separate but not anymore, the source may
                     --  have been changed to be a child unit.

                     Kind := S_Body;
                  end if;

                  --  Why not assign the kind given by the parser, and
                  --  directly insert (New_CU.Index, CU)???

                  New_CU := Compilation_Unit.Create
                    (Unit_Name    => CU.Unit_Name,
                     Index        => CU.Index,
                     Kind         => Kind,
                     Withed_Units => CU.Withed_Units,
                     Is_Sep_From  => (if CU.Is_Separate
                                      then CU.Is_Separate_From
                                      else No_Name));

                  New_CU_Map.Insert (New_CU.Index, New_CU);
               end;
            end loop;

            if New_CU_List.Is_Empty and then S.Is_RTS_Source then
               --  Source from RTS with a pragma No_Body?
               --  In this case we keep the current compilation unit record.

               null;

            else
               S.CU_Map := New_CU_Map;
               S.CU_List := New_CU_List;
            end if;

            --  TODO: if we find inconsistencies on the unit name or kind
            --  deduced previously by means of the naming scheme/exceptions,
            --  raise an exception.

            S.Parsed := True;

            Updated := True;
         end if;

         if Updated then
            Registry.Shared.Set (Self, S);
         end if;
      end;
   end Update;

   ------------------
   -- Withed_Units --
   ------------------

   function With_Clauses
     (Self  : Object;
      Index : Natural := 1) return Source_Reference.Identifier.Set.Object
   is
   begin
      Update (Self);
      return Registry.Shared.Get (Self).CU_Map (Index).Withed_Units;
   end With_Clauses;

   function With_Clauses
     (Self : Object;
      Unit : Name_Type) return Source_Reference.Identifier.Set.Object
   is
      Res : Source_Reference.Identifier.Set.Object;
   begin
      Update (Self);
      for CU of Registry.Shared.Get (Self).CU_List loop
         if CU.Unit_Name = Unit then
            Res.Union (CU.Withed_Units);
         end if;
      end loop;
      return Res;
   end With_Clauses;

end GPR2.Source;
