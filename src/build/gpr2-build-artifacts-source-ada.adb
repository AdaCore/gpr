--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.Artifacts.Source.Ada is

   function Serialize
     (Basename : String;
      Index    : Unit_Index) return String;

   --------
   -- Id --
   --------

   overriding function Id (Self : Object) return Artifact_Ids.Artifact_Id is
   begin
      return Artifact_Ids.Create
        (Class => Self.Class,
         View  => Self.Src_Owner.Id,
         Path  => Serialize (Self.Src_Basename, Self.Src_Index));
   end Id;

   ---------------
   -- Serialize --
   ---------------

   function Serialize
     (Basename : String;
      Index    : Unit_Index) return String is
   begin
      if Index = No_Index then
         return Basename;
      end if;

      declare
         Index_Img : constant String := Index'Image;
      begin
         return
           '@' & Index_Img (1 .. Index_Img'Last) & '@' & Basename;
      end;
   end Serialize;

end GPR2.Build.Artifacts.Source.Ada;
