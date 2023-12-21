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
        (Class => A_Class,
         View  => Self.Src_Owner.Id,
         Path  => Serialize (Self.Src_Basename, Self.Src_Index));
   end Id;

   function Id
     (View     : GPR2.Project.View.Object;
      Basename : Simple_Name;
      Index    : Unit_Index) return Artifact_Ids.Artifact_Id is
   begin
      return Artifact_Ids.Create
        (Class => A_Class,
         View  => View.Id,
         Path  => Serialize (String (Basename), Index));
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

   -----------------
   -- Set_Is_Main --
   -----------------

   procedure Set_Is_Main
     (Self  : in out Object;
      Value : Boolean) is
   begin
      Self.Is_Main := Value;
   end Set_Is_Main;

end GPR2.Build.Artifacts.Source.Ada;
