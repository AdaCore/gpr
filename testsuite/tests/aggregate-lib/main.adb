------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--          Copyright (C) 2016-2018, Free Software Foundation, Inc.         --
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

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object);

   procedure Changed_Callback (Prj : Project.View.Object);

   procedure Output_Filename (Filename : Path_Name.Full_Name);

   ----------------------
   -- Changed_Callback --
   ----------------------

   procedure Changed_Callback (Prj : Project.View.Object) is
   begin
      Text_IO.Put_Line (">>> Changed_Callback for " & String (Prj.Name));
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
               Text_IO.Put (" " & V);
            end loop;
            Text_IO.New_Line;
         end loop;
      end if;
   end Display;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      I : constant Positive := Strings.Fixed.Index (Filename, "aggregate-lib");
   begin
      Text_IO.Put_Line (" > " & Filename (I + 13 .. Filename'Last));
   end Output_Filename;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Text_IO.Put_Line ("//// OS set to Linux");
   Ctx.Include ("OS", "Linux");

   Project.Tree.Load (Prj, Create ("demo.gpr"), Ctx);

   for P of Prj loop
      Display (P);
   end loop;

   Text_IO.Put_Line ("sources:");
   for S of Prj.Root_Project.Sources loop
      Output_Filename (S.Source.Path_Name.Value);
   end loop;

   Text_IO.New_Line;
   Text_IO.Put_Line ("//// OS set to Windows");
   Ctx := Prj.Context;
   Ctx.Include ("OS", "Windows");
   Prj.Set_Context (Ctx, Changed_Callback'Access);

   for P of Prj loop
      Display (P);
   end loop;

   Text_IO.Put_Line ("sources:");
   for S of Prj.Root_Project.Sources loop
      Output_Filename (S.Source.Path_Name.Value);
   end loop;
end Main;
