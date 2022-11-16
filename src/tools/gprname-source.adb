------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

package body GPRname.Source is

   -----------------
   -- Append_Unit --
   -----------------

   procedure Append_Unit
     (Self : in out Object;
      Unit : GPRname.Unit.Object) is
   begin
      Self.Units.Append (Unit);
   end Append_Unit;

   ------------
   -- Create --
   ------------

   function Create
     (File       : Path_Name.Object;
      Language   : Language_Type;
      Unit_Based : Boolean := False) return Object is
   begin
      return Result : Object (Unit_Based) do
         Result.File := File;
         Result.Language := +String (Language);
      end return;
   end Create;

end GPRname.Source;
