------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2020, AdaCore                      --
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

private with Ada.Containers.Indefinite_Ordered_Maps;
private with GPR2.Unit;

package GPR2.Source_Info.Parser.ALI is

   Language : aliased constant Name_Type := "Ada";

   type Object is new Parser.Object
     (Language => Language'Unrestricted_Access,
      Kind     => LI) with private;

   overriding procedure Compute
     (Self   : not null access Object;
      Data   : in out Source_Info.Object'Class;
      Source : Project.Source.Object);
   --  Setup Data with the information from GNAT .ali file

   overriding procedure Clear_Cache (Self : not null access Object);
   --  Clear cached ALI data

private

   type Cache_Holder is record
      Unit      : GPR2.Unit.Object;
      Depends   : Dependency_Maps.Map;
      Checksum  : Word;
      Timestamp : Ada.Calendar.Time;
   end record;

   package Cache_Map is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Cache_Holder);
   --  Cache by 2 kind of keys.
   --  First one is complex key concatenated from full ALI path name, source
   --  file simple name and body/spec. This key is lowercased when
   --  File_Names_Case_Sensitive is False.
   --  Second kind of key is lowercased separate unit name.

   type Object is new Parser.Object
     (Language => Language'Unrestricted_Access,
      Kind     => LI)
   with record
      Cache : Cache_Map.Map;
   end record;

end GPR2.Source_Info.Parser.ALI;
