------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

package body GPR2.Project.Source.Part_Set is

   -------------
   -- Include --
   -------------

   procedure Include
     (Self    : in out Object;
      Element : Source_Part) is
   begin
      Source_Part_Sets.Include (Self.S, Element);
   end Include;

   -------------
   -- Include --
   -------------

   procedure Insert
     (Self    : in out Object;
      Element : Source_Part) is
   begin
      Source_Part_Sets.Insert (Self.S, Element);
   end Insert;

   procedure Insert
     (Self     : in out Object;
      Element  : Source_Part;
      Position : out Cursor;
      Inserted : out Boolean) is
   begin
      Source_Part_Sets.Insert (Self.S, Element, Position.C, Inserted);
   end Insert;

   -----------
   -- Union --
   -----------

   procedure Union
     (Self  : in out Object;
      Other : Object)
   is
   begin
      Self.S.Union (Other.S);
   end Union;

end GPR2.Project.Source.Part_Set;
