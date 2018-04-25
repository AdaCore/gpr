------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2018, Free Software Foundation, Inc.          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Unbounded;

with GPR2.Source.Registry;
with GPR2.Source.Parser;

package body GPR2.Source is

   use Ada.Strings.Unbounded;

   function Key (Self : Object) return Value_Type
     with Inline, Pre => Self /= Undefined;
   --  Returns the key for Self, this is used to compare a source object

   procedure Parse (Self : Object) with Inline;
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
      use type GPR2.Path_Name.Object;
   begin
      if Left.Pathname = Path_Name.Undefined
        and then Right.Pathname = Path_Name.Undefined
      then
         return True;
      else
         return not (Left.Pathname = Path_Name.Undefined
                     xor Right.Pathname = Path_Name.Undefined)
           and then Key (Left) = Key (Right);
      end if;
   end "=";

   ------------
   -- Create --
   ------------

   function Create
     (Filename  : Path_Name.Object;
      Kind      : Kind_Type;
      Language  : Name_Type;
      Unit_Name : Optional_Name_Type) return Object is
   begin
      return Result : Object do
         Registry.Shared.Register
           (Registry.Data'
              (Path_Name  => Filename,
               Timestamp  => Directories.Modification_Time (Filename.Value),
               Language   => To_Unbounded_String (String (Language)),
               Unit_Name  => To_Unbounded_String (String (Unit_Name)),
               Kind       => Kind,
               Other_Part => Path_Name.Undefined,
               Units      => <>,
               Parsed     => False,
               Ref_Count  => 1));

         Result.Pathname := Filename;
      end return;
   end Create;

   --------------
   -- Filename --
   --------------

   function Filename (Self : Object) return Path_Name.Full_Name is
   begin
      return Registry.Shared.Get (Self).Path_Name.Value;
   end Filename;

   ---------
   -- Key --
   ---------

   function Key (Self : Object) return Value_Type is
      use Ada.Characters;
      Data : constant Registry.Data := Registry.Shared.Get (Self);
   begin
      if Data.Unit_Name = Null_Unbounded_String then
         --  Not unit based
         return Data.Path_Name.Value;

      else
         return Kind_Type'Image (Data.Kind)
           & "|" & Handling.To_Lower (To_String (Data.Unit_Name));
      end if;
   end Key;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Object) return Kind_Type is
   begin
      Parse (Self);
      return Registry.Shared.Get (Self).Kind;
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
      use type Path_Name.Object;

      Other_Part : constant Path_Name.Object :=
                     Registry.Shared.Get (Self).Other_Part;
   begin
      if Other_Part = Path_Name.Undefined then
         return Undefined;
      else
         return Object'(Pathname => Other_Part);
      end if;
   end Other_Part;

   -----------
   -- Parse --
   -----------

   procedure Parse (Self : Object) is
      use type Calendar.Time;

      S        : Registry.Data := Registry.Shared.Get (Self);
      Filename : constant String := Self.Pathname.Value;
   begin
      --  Parse if not yet parsed or if the file has changed on disk

      if not S.Parsed
        or else
          (Directories.Exists (Filename)
           and then S.Timestamp < Directories.Modification_Time (Filename))
      then
         declare
            Data : constant Source.Parser.Data :=
                     Source.Parser.Check (S.Path_Name);
         begin
            --  Check if separate unit

            if Data.Is_Separate then
               S.Kind := S_Separate;

            elsif S.Kind = S_Separate then
               --  It was a separate but not anymore, the source may have been
               --  changed to be a child unit.

               S.Kind := S_Body;
            end if;

            --  Record the withed units

            S.Units := Data.W_Units;

            --  The unit-name from the source if possible

            if Data.Unit_Name /= Null_Unbounded_String then
               S.Unit_Name := Data.Unit_Name;
            end if;

            --  Record that this is now parsed

            S.Parsed := True;

            --  Update registry

            Registry.Shared.Set (Self, S);
         end;
      end if;
   end Parse;

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

   procedure Set_Other_Part
     (Self       : Object;
      Other_Part : Object) is
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

   function Unit_Name (Self : Object) return Optional_Name_Type is
   begin
      Parse (Self);
      return Optional_Name_Type
        (To_String (Registry.Shared.Get (Self).Unit_Name));
   end Unit_Name;

   ------------------
   -- Withed_Units --
   ------------------

   function Withed_Units (Self : Object) return Source_Reference.Set.Object is
   begin
      Parse (Self);
      return Registry.Shared.Get (Self).Units;
   end Withed_Units;

end GPR2.Source;
