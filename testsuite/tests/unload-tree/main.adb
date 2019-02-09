------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--          Copyright (C) 2017-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;
with GPR2.Source;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object);

   procedure Changed_Callback (Prj : Project.View.Object);

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

   ----------------------
   -- Changed_Callback --
   ----------------------

   procedure Changed_Callback (Prj : Project.View.Object) is
   begin
      Text_IO.Put_Line
        (">>> Changed_Callback for "
         & Directories.Simple_Name (Prj.Path_Name.Value));
   end Changed_Callback;

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Prj.Has_Attributes then
         for A in Prj.Attributes.Iterate loop
            Text_IO.Put ("A:   " & String (Attribute.Set.Element (A).Name));
            Text_IO.Put (" ->");

            for V of Element (A).Values loop
               Text_IO.Put (" " & V.Text);
            end loop;
            Text_IO.New_Line;
         end loop;
      end if;

      for Source of Prj.Sources loop
         declare
            S : constant GPR2.Source.Object := Source.Source;
            U : constant Optional_Name_Type := S.Unit_Name;
         begin
            Output_Filename (S.Path_Name.Value);

            Text_IO.Set_Col (16);
            Text_IO.Put ("   language: " & String (S.Language));

            Text_IO.Set_Col (33);
            Text_IO.Put ("   Kind: " & GPR2.Source.Kind_Type'Image (S.Kind));

            if U /= "" then
               Text_IO.Put ("   unit: " & String (U));
            end if;

            Text_IO.New_Line;
         end;
      end loop;
   end Display;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      I : constant Positive := Strings.Fixed.Index (Filename, "unload-tree");
   begin
      Text_IO.Put (" > " & Filename (I + 12 .. Filename'Last));
   end Output_Filename;

   Prj1, Prj2 : Project.Tree.Object;
   Ctx1, Ctx2 : Context.Object;

begin
   Ctx1.Include ("LSRC", "one");
   Ctx2.Include ("LSRC", "two");

   Project.Tree.Load (Prj1, Create ("first.gpr"), Ctx1);
   Project.Tree.Load (Prj2, Create ("second.gpr"), Ctx2);

   Text_IO.Put_Line ("**************** Iterator Prj1");

   for C in Project.Tree.Iterate (Prj1) loop
      Display (Project.Tree.Element (C));
      if Project.Tree.Is_Root (C) then
         Text_IO.Put_Line ("   is root");
      end if;
   end loop;

   Prj1.Unload;

   Text_IO.Put_Line ("**************** Iterator Prj2");

   for C in Project.Tree.Iterate (Prj2) loop
      Display (Project.Tree.Element (C));
      if Project.Tree.Is_Root (C) then
         Text_IO.Put_Line ("   is root");
      end if;
   end loop;

exception
   when GPR2.Project_Error =>
      if Prj1.Has_Messages then
         Text_IO.Put_Line ("Messages found:");

         for M of Prj1.Log_Messages.all loop
            declare
               Mes : constant String := M.Format;
               L   : constant Natural :=
                 Strings.Fixed.Index (Mes, "/unload-tree");
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
