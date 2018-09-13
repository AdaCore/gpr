------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

package body GPRname.Unit is

   ------------
   -- Create --
   ------------

   function Create
     (F_Name            : GPR2.Name_Type;
      F_Kind            : Unit_Kind;
      F_Index_In_Source : Natural) return Object
   is
     (Object'(Name            => +String (F_Name),
              Kind            => F_Kind,
              Index_In_Source => F_Index_In_Source));

   ----------
   -- Name --
   ----------

   function Name (Self : Object) return GPR2.Name_Type is
     (GPR2.Name_Type (To_String (Self.Name)));

   ----------
   -- Kind --
   ----------

   function Kind (Self : Object) return Unit_Kind is
     (Self.Kind);

   ---------------------
   -- Index_In_Source --
   ---------------------

   function Index_In_Source (Self : Object) return Natural is
     (Self.Index_In_Source);

end GPRname.Unit;
