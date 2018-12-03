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

package body GPR2.Unit is

   ------------
   -- Create --
   ------------

   function Create
     (Name      : Name_Type;
      Spec      : Project.Source.Object;
      Main_Body : Project.Source.Object;
      Separates : Project.Source.Set.Object) return Object is
   begin
      return Object'(To_Unbounded_String (String (Name)),
                     Spec,
                     Main_Body,
                     Separates);
   end Create;

   -------------------
   -- Update_Bodies --
   -------------------

   procedure Update_Body
     (Self : in out Object; Source : Project.Source.Object) is
   begin
      Self.Main_Body := Source;

      --  If the spec is already defined associate it with the new body

      if Self.Spec.Source.Is_Defined then
         Source.Source.Set_Other_Part (Self.Spec.Source);
      end if;
   end Update_Body;

   ----------------------
   -- Update_Separates --
   ----------------------

   procedure Update_Separates
     (Self : in out Object; Source : Project.Source.Object) is
   begin
      Self.Separates.Include (Source);

      --  If the spec is already defined associate it with the new body

      if Self.Spec.Source.Is_Defined then
         Source.Source.Set_Other_Part (Self.Spec.Source);
      end if;
   end Update_Separates;

   -----------------
   -- Update_Spec --
   -----------------

   procedure Update_Spec
     (Self : in out Object; Source : Project.Source.Object) is
   begin
      Self.Spec := Source;

      --  If the body is already defined associate it with the new spec

      if Self.Main_Body.Source.Is_Defined then
         Source.Source.Set_Other_Part (Self.Main_Body.Source);
      end if;

      --  And make sure the separates are referencing this as the spec

      for S of Self.Separates loop
         S.Source.Set_Other_Part (Source.Source);
      end loop;
   end Update_Spec;

end GPR2.Unit;
