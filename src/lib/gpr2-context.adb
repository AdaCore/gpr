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

package body GPR2.Context is

   ---------------
   -- Signature --
   ---------------

   function Signature
     (Self      : Object;
      Externals : Containers.Name_List) return Context.Binary_Signature
   is
      Position : Context.Key_Value.Cursor;
      P_Ctx    : Context.Object;
   begin
      --  Compute the project's own context. That is, the context based only on
      --  the project's external variables.

      for E of Externals loop
         Position := Self.Find (E);

         if Context.Key_Value.Has_Element (Position) then
            P_Ctx.Include
              (Context.Key_Value.Key (Position),
               Context.Key_Value.Element (Position));
         end if;
      end loop;

      if P_Ctx.Is_Empty then
         return Context.Default_Signature;

      else
         declare
            C : MD5.Context;
         begin
            for E in P_Ctx.Iterate loop
               MD5.Update (C, String (Key_Value.Key (E)));
               MD5.Update (C, "=");
               MD5.Update (C, Key_Value.Element (E));
               MD5.Update (C, ";");
            end loop;

            return MD5.Digest (C);
         end;
      end if;
   end Signature;

end GPR2.Context;
