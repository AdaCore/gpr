--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Ordered_Maps;

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Artifacts.File_Part;
with GPR2.Build.Artifacts.Library;

package body GPR2.Build.Artifacts is

   package Protocol_Artifacts_Map is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Object'Class);

   Map : Protocol_Artifacts_Map.Map;

   --------------
   -- From_Uri --
   --------------

   function From_Uri (Uri : Uri_Type) return Object'Class
   is
      Del_Index : Integer := -1;
   begin
      for J in Uri'First .. Uri'Last - 2 loop
         if Uri (J .. J + 2) = "://" then
            Del_Index := J;
            exit;
         end if;
      end loop;

      if Del_Index = -1 then
         raise Constraint_Error with
           "unexpected uri format: missing the protocol part in """ &
           Uri & '"';
      end if;

      declare
         Prot : String renames Uri (Uri'First .. Del_Index - 1);
         Img  : String renames Uri (Del_Index + 3 .. Uri'Last);
         C    : Protocol_Artifacts_Map.Cursor;
      begin
         C := Map.Find (Prot);

         if not Protocol_Artifacts_Map.Has_Element (C) then
            raise Constraint_Error with
              "unrecognized protocol in """ & Uri & '"';
         end if;

         return Result : Object'Class := Protocol_Artifacts_Map.Element (C) do
            Unserialize (Img, Result);
         end return;
      end;
   end From_Uri;

   -----------------------------
   -- Register_Artifact_Class --
   -----------------------------

   procedure Register_Artifact_Class
     (Artifact : Object'Class) is
   begin
      Map.Insert (Artifact.Protocol, Artifact);
   end Register_Artifact_Class;

   ------------
   -- To_Uri --
   ------------

   function To_Uri (Artifact : Object'Class) return Uri_Type is
   begin
      return Artifact.Protocol & "://" & Artifact.Image;
   end To_Uri;

begin

   Register_Artifact_Class (Files.Undefined);
   Register_Artifact_Class (File_Part.Undefined);
   Register_Artifact_Class (Library.Undefined);

end GPR2.Build.Artifacts;
