--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.ALI;
with GPR2.Build.Artifacts.Object_File;
with GPR2.Path_Name;

package body GPR2.Build.Actions.Ada_Compile is

   ----------
   -- Fill --
   ----------

   overriding procedure Fill
     (Self  : Object;
      Graph : access DAG.Object;
      Input : Artifact_Ids.Artifact_Id)
   is
      Src        : constant Artifacts.Source.Ada.Object :=
                     (Artifacts.Source.Ada.Object
                        (DAG.Artifact (Graph.all, Input)));
   begin
      if not Src.Is_Main then
         return;
      end if;

      declare
         Idx_Img    : constant Simple_Name := Simple_Name (Src.Index'Image);
         Idx_Suffix : constant Filename_Optional :=
                        (if Src.Index /= No_Index
                         then "~" & Idx_Img (Idx_Img'First + 1 .. Idx_Img'Last)
                         else "");
         BN         : constant Simple_Name :=
                        Path_Name.Base_Name
                          (Src.Source_Simple_Name) & Idx_Suffix;
         Ali        : constant Artifacts.ALI.Object :=
                        Artifacts.ALI.Create
                          (Src.Owning_View, BN & ".ali");
         Obj        : constant Artifacts.Object_File.Object :=
                        Artifacts.Object_File.Create
                          (Src.Owning_View, BN & ".o");
         --  ??? get extensions from configuration project

      begin
         Graph.Add_Artifact (Ali);
         Graph.Add_Explicit_Dependency (Ali.Id, Input);
         Graph.Add_Artifact (Obj);
         Graph.Add_Explicit_Dependency (Obj.Id, Input);
      end;
   end Fill;

   --------------
   -- Register --
   --------------

   procedure Register (DAG : access Build.DAG.Object) is
      Self : Object;
   begin
      DAG.Register_Action (Self);
   end Register;

end GPR2.Build.Actions.Ada_Compile;
