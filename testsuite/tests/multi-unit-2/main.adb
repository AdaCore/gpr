------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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
with GPR2.Unit;

with GPR2.Source_Info.Parser.Ada_Language;

procedure Main is
   use Ada;

   use GPR2;

   Tree : Project.Tree.Object;
   Ctx  : Context.Object;

   procedure Print (S : Project.Source.Object);
   --  Print source information and remove dependency files if exists

   -----------
   -- Print --
   -----------

   procedure Print (S : Project.Source.Object) is
      Src : Source.Object := S.Source;
      Dep : Path_Name.Object;

      procedure Print_Dependency
        (Src : Project.Source.Object; Unit : GPR2.Unit.Object);

      ----------------------
      -- Print_Dependency --
      ----------------------

      procedure Print_Dependency
        (Src : Project.Source.Object; Unit : GPR2.Unit.Object) is
      begin
         Text_IO.Put_Line
           ("    dependency unit " & String (Unit.Name) & ' ' & Unit.Kind'Img
            & " in " & String (Src.Path_Name.Simple_Name)
            & (if Src.Source.Has_Single_Unit then ""
               else " at" & Unit.Index'Img));
      end Print_Dependency;

   begin
      Text_IO.Put_Line (String (Src.Path_Name.Simple_Name));
      Text_IO.Put_Line ("  single-unit          = "
                        & Src.Has_Single_Unit'Image);
      Text_IO.Put_Line ("  has naming exception = "
                        & S.Has_Naming_Exception'Image);
      for CU of Src.Units loop
         Text_IO.Put_Line ("  - compilation unit at" & CU.Index'Image);
         Text_IO.Put_Line ("    unit name    = " & String (CU.Name));
         Text_IO.Put_Line ("    kind         = " & CU.Kind'Image);

         if not CU.Dependencies.Is_Empty then
            Text_IO.Put ("    withed units = { ");

            for W of CU.Dependencies loop
               Text_IO.Put (String (W.Text) & " ");
            end loop;

            Text_IO.Put_Line ("}");
         end if;

         if S.Artifacts.Has_Dependency (CU.Index) then
            Dep := S.Artifacts.Dependency (CU.Index);
            Text_IO.Put_Line
              ("    object file  = " & String (Dep.Simple_Name));

            S.Dependencies
              (Print_Dependency'Access,
               Index => Source_Info.Unit_Index (CU.Index));

            if Dep.Exists then
               Directories.Delete_File (Dep.Value);
            end if;
         end if;
      end loop;
   end Print;

begin
   --  Source_Info.Parser.Ada_Language.Unregister;

   Tree.Load (Filename => Project.Create ("files/p.gpr"),
              Context  => Ctx);

   for S of Tree.Root_Project.Sources loop
      Print (S);
   end loop;

   --  First iteration over the sources removes dependency files, i.e. next
   --  source parsing going to be with Ada_Language parser.

   Tree.Invalidate_Sources;

   for S of Tree.Root_Project.Sources loop
      Print (S);
   end loop;

exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));

      for M of Tree.Log_Messages.all loop
         Text_IO.Put_Line (M.Format);
      end loop;
end Main;
