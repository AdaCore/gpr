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

with Ada.Calendar;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

with GPR2.Path_Name;

private package GPR2.Source.Registry is

   use Ada.Strings.Unbounded;

   type Data is record
      Path_Name  : GPR2.Path_Name.Object;
      Timestamp  : Calendar.Time;
      Language   : Unbounded_String;
      Unit_Name  : Unbounded_String;
      Kind       : Kind_Type;
      Other_Part : GPR2.Path_Name.Object;
      Units      : Source_Reference.Set.Object;
      Parsed     : Boolean := False;
      Ref_Count  : Natural := 0;
   end record;

   package Source_Store is
     new Ada.Containers.Ordered_Maps (GPR2.Path_Name.Object, Data);

   protected Shared is

      procedure Register (Def : Data)
        with Pre => Def.Path_Name /= GPR2.Path_Name.Undefined;
      --  Registers element in Store

      procedure Unregister (Object : in out Source.Object)
        with Pre => Object /= Undefined;
      --  Unregisters the given source, release memory if not used anymore

      function Get (Object : Source.Object) return Data
        with Pre => Object /= Undefined;
      --  Gets the source data for the given source object

      procedure Set (Object : Source.Object; Def : Data)
        with Pre => Object /= Undefined
             and then Def.Path_Name /= GPR2.Path_Name.Undefined;
      --  Sets the source data for the given source object

      procedure Set_Other_Part (Object1, Object2 : Object)
        with Pre => Object1 /= Undefined and then Object2 /= Undefined;
      --  Registers that Def1 is the other part for Def2 and the other way
      --  around too.

   private
      Store : Source_Store.Map;
   end Shared;

end GPR2.Source.Registry;
