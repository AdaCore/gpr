--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Variable.Set;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Full then
         for A of Prj.Attributes (With_Defaults => False) loop
            Text_IO.Put
              ("A:   " & Image (A.Name.Id.Attr));
            Text_IO.Put (" ->");

            for V of A.Values loop
               Text_IO.Put (" " & '"' & V.Text & '"');
            end loop;
            Text_IO.New_Line;
         end loop;

         if Prj.Has_Variables then
            for V in Prj.Variables.Iterate loop
               Text_IO.Put ("V:   " & String (Key (V)));
               Text_IO.Put (" ->");
               for Val of Element (V).Values loop
                  Text_IO.Put (" " & '"' & Val.Text & '"');
               end loop;
               Text_IO.New_Line;
            end loop;
         end if;
      end if;
   end Display;

   ----------
   -- Test --
   ----------

   procedure Test (Project_Name : String) is
      Prj : Project.Tree.Object;
      Ctx : Context.Object;
   begin
      Project.Tree.Load (Prj, Create (Filename_Type (Project_Name)), Ctx);
      Display (Prj.Root_Project);
      Project.Tree.Unload (Prj);
   exception
      when GPR2.Project_Error =>
         if Prj.Has_Messages then
            Text_IO.Put_Line ("Messages found:");

            for C in Prj.Log_Messages.Iterate
              (Information => False,
               Warning     => False)
            loop
                declare
                   M : constant Message.Object := Log.Element (C);
                begin
                   Text_IO.Put_Line (M.Format);
                end;
            end loop;
         end if;
   end Test;

begin
   Test ("demo.gpr");
   Test ("demoe1.gpr");
end Main;
