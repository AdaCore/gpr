------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2017, Free Software Foundation, Inc.          --
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

with GPR2.Source.Registry;

package body GPR2.Source is

   function Key (Self : Object) return Value_Type with Inline;
   --  Returns the key for Self, this is used to compare a source object

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
      return Key (Left) = Key (Right);
   end "=";

   ------------
   -- Create --
   ------------

   function Create
     (Filename  : Path_Name_Type;
      Kind      : Kind_Type;
      Language  : Name_Type;
      Unit_Name : Optional_Name_Type) return Object is
   begin
      Registry.Store.Append
        (Registry.Data'
           (Filename,
            To_Unbounded_String (String (Language)),
            To_Unbounded_String (String (Unit_Name)),
            Kind,
            0));

      return Result : Object do
         Result.Id := Registry.Store.Last_Index;
      end return;
   end Create;

   --------------
   -- Filename --
   --------------

   function Filename (Self : Object) return Full_Path_Name is
   begin
      return Value (Registry.Store (Self.Id).Path_Name);
   end Filename;

   ---------
   -- Key --
   ---------

   function Key (Self : Object) return Value_Type is
      Data : constant Registry.Data := Registry.Store (Self.Id);
   begin
      if Data.Unit_Name = Null_Unbounded_String then
         --  Not unit based
         return Value (Data.Path_Name);

      else
         return Kind_Type'Image (Data.Kind) & "|" & To_String (Data.Unit_Name);
      end if;
   end Key;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Object) return Kind_Type is
   begin
      return Registry.Store (Self.Id).Kind;
   end Kind;

   --------------
   -- Language --
   --------------

   function Language (Self : Object) return Name_Type is
   begin
      return Name_Type (To_String (Registry.Store (Self.Id).Language));
   end Language;

   ----------------
   -- Other_Part --
   ----------------

   function Other_Part (Self : Object) return Object is
      Other_Id : constant Natural := Registry.Store (Self.Id).Other_Part;
   begin
      if Other_Id = 0 then
         return Undefined;
      else
         return Object'(Id => Other_Id);
      end if;
   end Other_Part;

   --------------------
   -- Set_Other_Part --
   --------------------

   procedure Set_Other_Part
     (Self       : in out Object;
      Other_Part : in out Object) is
   begin
      Registry.Store (Self.Id).Other_Part := Other_Part.Id;
      Registry.Store (Other_Part.Id).Other_Part := Self.Id;
   end Set_Other_Part;

   ---------------
   -- Unit_Name --
   ---------------

   function Unit_Name (Self : Object) return Optional_Name_Type is
   begin
      return Optional_Name_Type
        (To_String (Registry.Store (Self.Id).Unit_Name));
   end Unit_Name;

end GPR2.Source;
