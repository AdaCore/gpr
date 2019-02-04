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

with GPR2.Source;

package body GPR2.Unit is

   ------------
   -- Create --
   ------------

   function Create
     (Spec   : Project.Source.Object;
      Bodies : Project.Source.Set.Object) return Object is
   begin
      return Object'(Spec, Bodies);
   end Create;

   -------------------
   -- Update_Bodies --
   -------------------

   procedure Update_Bodies
     (Self : in out Object; Source : Project.Source.Object) is
   begin
      --  If the spec is already defined associate it with the new body

      if Self.Spec.Source.Is_Defined then
         Source.Source.Set_Other_Part (Self.Spec.Source);
      end if;

      Self.Bodies.Insert (Source);
   end Update_Bodies;

   -----------------
   -- Update_Spec --
   -----------------

   procedure Update_Spec
     (Self : in out Object; Source : Project.Source.Object) is
   begin
      Self.Spec := Source;

      --  And make sure the bodies (body & separate) are
      --  referencing this as the spec.

      for S of Self.Bodies loop
         S.Source.Set_Other_Part (Source.Source);
      end loop;
   end Update_Spec;

end GPR2.Unit;
