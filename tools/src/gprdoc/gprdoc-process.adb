------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2024, AdaCore                     --
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

with GPR2.Containers;
with GPR2.Project.Registry.Exchange;
with GPR2.Project.Registry.Pack;

procedure GPRdoc.Process (Options : GPRdoc.GPRdoc_Options) is

   use GPR2;

   function Everything return GPR2.Containers.Package_Id_List;

   function Everything return GPR2.Containers.Package_Id_List is
      All_Registry : GPR2.Containers.Package_Id_List :=
                       GPR2.Project.Registry.Pack.All_Packages;
   begin
      GPR2.Containers.Package_Id_Type_List.Insert
        (All_Registry, GPR2.Project_Level_Scope);
      return All_Registry;
   end Everything;

begin

   case Options.Kind_Of_Display is
      when GPRtools.K_JSON_Compact =>
         GPR2.Project.Registry.Exchange.Export
           (Included => Everything,
            Excluded => GPR2.Containers.Package_Id_Type_List.Empty,
            Format   => GPR2.Project.Registry.Exchange.K_JSON_COMPACT);

      when GPRtools.K_JSON =>
         GPR2.Project.Registry.Exchange.Export
           (Included => Everything,
            Excluded => GPR2.Containers.Package_Id_Type_List.Empty,
            Format   => GPR2.Project.Registry.Exchange.K_JSON);

      when GPRtools.K_Textual_IO =>
         GPR2.Project.Registry.Exchange.Export
           (Included => Everything,
            Excluded => GPR2.Containers.Package_Id_Type_List.Empty,
            Format   => GPR2.Project.Registry.Exchange.K_TEXT);
   end case;

end GPRdoc.Process;
