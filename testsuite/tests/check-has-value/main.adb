------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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
with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Project.Attribute;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Registry.Attribute;
with GPR2.Context;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
      package A renames GPR2.Project.Registry.Attribute;

      Att : Project.Attribute.Object;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      Att := Prj.Attribute (A.Languages);
      Text_IO.Put_Line (Att.Image);

      Text_IO.Put_Line ("ada " & Att.Has_Value ("ada")'Img);
      Text_IO.Put_Line ("Ada " & Att.Has_Value ("Ada")'Img);
      Text_IO.Put_Line ("FORTRAN " & Att.Has_Value ("FORTRAN")'Img);
      Text_IO.Put_Line ("fortran " & Att.Has_Value ("fortran")'Img);

      Att := Prj.Attributes.Element (A.Source_Dirs);
      Text_IO.Put_Line (Att.Image);

      Text_IO.Put_Line (". " & Att.Has_Value (".")'Img);
      Text_IO.Put_Line ("src1 " & Att.Has_Value ("src1")'Img);
      Text_IO.Put_Line ("sRc1 " & Att.Has_Value ("sRc1")'Img);
      Text_IO.Put_Line ("SRC2 " & Att.Has_Value ("SRC2")'Img);
      Text_IO.Put_Line ("Src2 " & Att.Has_Value ("Src2")'Img);
   end Display;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Project.Tree.Load (Prj, Create ("demo.gpr"), Ctx);
   Display (Prj.Root_Project);
exception
   when GPR2.Project_Error =>
      if Prj.Has_Messages then
         Text_IO.Put_Line ("Messages found:");

         for M of Prj.Log_Messages.all loop
            declare
               Mes : constant String := M.Format;
               L   : constant Natural :=
                       Strings.Fixed.Index (Mes, "/check-has-value");
            begin
               if L /= 0 then
                  Text_IO.Put_Line (Mes (L .. Mes'Last));
               else
                  Text_IO.Put_Line (Mes);
               end if;
            end;
         end loop;
      end if;
end Main;
