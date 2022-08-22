--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with GPR2;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.Source;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Set;
with GPR2.Source;

with GPR2.Source_Info.Parser.Ada_Language;

procedure Main is
   use Ada;

   use GPR2;

   Tree : Project.Tree.Object;
   Ctx  : Context.Object;

   ------------------
   -- Print_Source --
   ------------------

   procedure Print_Source (S : Project.Source.Object) is
      DN  : Path_Name.Object;
   begin
      Text_IO.Put_Line (String (S.Path_Name.Simple_Name));
      Text_IO.Put_Line ("  single-unit          = "
                        & S.Has_Single_Unit'Image);
      Text_IO.Put_Line ("  has naming exception = "
                        & S.Has_Naming_Exception'Image);
      for CU of S.Units loop
         Text_IO.Put_Line ("  - compilation unit at" & CU.Index'Image);
         Text_IO.Put_Line ("    unit name    = " & String (CU.Name));
         Text_IO.Put_Line ("    kind         = " & CU.Kind'Image);

         if not CU.Dependencies.Is_Empty then
            Text_IO.Put      ("    withed units = { ");

            for W of CU.Dependencies loop
               Text_IO.Put (String (W.Text) & " ");
            end loop;

            Text_IO.Put_Line ("}");
         end if;

         if S.Artifacts.Has_Object_Code (CU.Index) then
            Text_IO.Put_Line
              ("    object file  = "
               & String (S.Artifacts.Object_Code (CU.Index).Simple_Name));
         end if;
         if S.Artifacts.Has_Dependency (CU.Index) then
            DN := S.Artifacts.Dependency (CU.Index);
            Text_IO.Put_Line ("    deps file    = " & String (DN.Simple_Name));
            if DN.Exists then
               Directories.Delete_File (DN.Value);
            end if;
         end if;
      end loop;
   end Print_Source;


begin

   Tree.Load (Filename => Project.Create ("files/multi.gpr"),
              Context  => Ctx);

   for S of Tree.Root_Project.Sources loop
      Print_Source (S);
   end loop;

   Tree.Invalidate_Sources;

   for S of Tree.Root_Project.Sources loop
      Print_Source (S);
   end loop;

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));

      for M of Tree.Log_Messages.all loop
         Text_IO.Put_Line (M.Format);
      end loop;
end Main;
