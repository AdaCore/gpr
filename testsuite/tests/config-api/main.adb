------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Text_IO;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;

with GPR2.Context;
with GPR2.KB;
with GPR2.Log;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Configuration;
with GPR2.Project.Tree;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use Ada.Exceptions;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   procedure Display (Att : Project.Attribute.Object);

   -------------
   -- Display --
   -------------

   procedure Display (Att : Project.Attribute.Object) is
   begin
      Text_IO.Put ("   " & Image (Att.Name.Id));

      if Att.Has_Index then
         Text_IO.Put (" (" & Att.Index.Text & ")");
      end if;

      Text_IO.Put (" ->");

      for V of Att.Values loop
         Text_IO.Put (" " & V.Text);
      end loop;
      Text_IO.New_Line;
   end Display;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Full then
         if Prj.Has_Attributes then
            for A in Prj.Attributes.Iterate loop
               Text_IO.Put
                 ("A:   " & Image (Attribute.Set.Element (A).Name.Id));
               Text_IO.Put (" ->");

               for V of Element (A).Values loop
                  Text_IO.Put (" " & V.Text);
               end loop;
               Text_IO.New_Line;
            end loop;
         end if;

         if Prj.Has_Variables then
            for V in Prj.Variables.Iterate loop
               Text_IO.Put ("V:   " & String (Key (V)));
               Text_IO.Put (" -> ");
               Text_IO.Put (Element (V).Value.Text);
               Text_IO.New_Line;
            end loop;
         end if;
         Text_IO.New_Line;

         if Prj.Has_Packages then
            for Pck of Prj.Packages loop
               Text_IO.Put_Line (" " & Image (Pck.Name));

               if Pck.Has_Attributes then
                  for A of Pck.Attributes loop
                     Display (A);
                  end loop;
               end if;
            end loop;
         end if;
      end if;
   end Display;

   Gpr : constant GPR2.Path_Name.Object := Create ("demo.gpr");
   Prj : Project.Tree.Object;
   Ctx : Context.Object;

   Des : Configuration.Description :=
           Configuration.Create (Language => Ada_Language);
   KB  : GPR2.KB.Object := GPR2.KB.Create (GPR2.KB.Default_Flags);
   Cnf : Configuration.Object :=
           Configuration.Create
            (Configuration.Description_Set'(1 => Des), "all", Gpr,
             Base => KB);
begin
   if Cnf.Has_Messages then
      for M of Cnf.Log_Messages loop
         declare
            F : constant String := M.Sloc.Filename;
            I : constant Natural := Strings.Fixed.Index (F, "config-api");
         begin
            Text_IO.Put_Line ("> " & F (I - 1 .. F'Last));
            Text_IO.Put_Line (M.Level'Img);
            Text_IO.Put_Line (M.Format);
         end;
      end loop;
   end if;

   Ctx.Include ("OS", "Linux");
   Project.Tree.Load (Prj, Gpr, Ctx, Config => Cnf);

   Display (Prj.Root_Project);

   if Prj.Has_Configuration then
      Display (Prj.Configuration.Corresponding_View, Full => False);
   end if;

exception
   when E : GPR2.Project_Error =>
      Text_IO.Put_Line (Exception_Information (E));

      if Prj.Has_Messages then
         Text_IO.Put_Line ("Messages found:");

         for M of Prj.Log_Messages.all loop
            declare
               F : constant String := M.Sloc.Filename;
               I : constant Natural := Strings.Fixed.Index (F, "config-api");
            begin
               Text_IO.Put_Line ("> " & F (I - 1 .. F'Last));
               Text_IO.Put_Line (M.Level'Img);
               Text_IO.Put_Line (M.Format);
            end;
         end loop;
      end if;
end Main;
