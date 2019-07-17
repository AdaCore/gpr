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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;

with GPR2.Compilation_Unit.Map;
with GPR2.Path_Name;

private package GPR2.Source.Registry is

   use Ada.Strings.Unbounded;

   type Data (Is_Ada_Source : Boolean) is record
      Path_Name    : GPR2.Path_Name.Object;
      Timestamp    : Calendar.Time;
      Language     : Unbounded_String;
      Other_Part   : GPR2.Path_Name.Object;
      Ref_Count    : Natural := 0;

      case Is_Ada_Source is
         when True =>
            Parsed        : Boolean := False;
            Is_RTS_Source : Boolean := False;
            CU_List       : Compilation_Unit.List.Object;
            CU_Map        : Compilation_Unit.Map.Object;
            Ada_Key       : Unbounded_String;

         when False =>
            Kind : Kind_Type;
      end case;
   end record;
   --  Record that holds relevant source information, including details about
   --  the compilation unit(s) for Ada sources.

   package Source_Store is
     new Ada.Containers.Indefinite_Ordered_Maps (GPR2.Path_Name.Object, Data);

   protected Shared is
      function Print_Store return Boolean;

      procedure Register (Def : Data)
        with Pre => Def.Path_Name.Is_Defined;
      --  Registers element in Store

      procedure Unregister (Object : in out Source.Object)
        with Pre => Object.Is_Defined;
      --  Unregisters the given source, release memory if not used anymore

      function Get (Object : Source.Object) return Data
        with Pre => Object.Is_Defined;
      --  Gets the source data for the given source object

      procedure Set (Object : Source.Object; Def : Data)
        with Pre => Object.Is_Defined
             and then Def.Path_Name.Is_Defined;
      --  Sets the source data for the given source object

      procedure Set_Other_Part (Object1, Object2 : Object)
        with Pre => Object1.Is_Defined and then Object2.Is_Defined;
      --  Registers that Def1 is the other part for Def2 and the other way
      --  around too.

   private
      Store : Source_Store.Map;
   end Shared;

end GPR2.Source.Registry;
