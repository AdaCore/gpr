--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.Dependencies is

   -------------
   -- Include --
   -------------

   procedure Include (Self : in out Set; Filename : Filename_Type) is
   begin
      if not Filenames.Contains (Self, Filename) then
         Self.Insert
           ((Filename_Length => Filename'Length,
             Filename        => Filename,
             Units           => Unit_Sets.Empty_Set));
      end if;
   end Include;

   procedure Include
     (Self : in out Set; Filename : Filename_Type; Unit : Unit_Part)
   is
      procedure Add_Unit (Item : in out Dependency);

      procedure Add_Unit (Item : in out Dependency) is
      begin
         Item.Units.Include (Unit);
      end Add_Unit;
   begin
      Include (Self, Filename);

      Filenames.Update_Element_Preserving_Key
        (Self, Filenames.Find (Self, Filename), Add_Unit'Access);
   end Include;

end GPR2.Build.Dependencies;
