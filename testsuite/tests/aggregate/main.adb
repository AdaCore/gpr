--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.OS_Lib;

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

   procedure Display (Prj : Project.View.Object);

   procedure Changed_Callback (Prj : Project.View.Object);

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

      for A in Prj.Attributes (With_Defaults => False,
                               With_Config   => False).Iterate loop
         Text_IO.Put
           ("A:   " & Image (Attribute.Set.Element (A).Name.Id.Attr));
         Text_IO.Put (" ->");

         for V of Element (A).Values loop
            Text_IO.Put (" " & V.Text);
         end loop;
         Text_IO.New_Line;
      end loop;
   end Display;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Text_IO.Put_Line ("//// OS set to Linux");
   Ctx.Include ("OS", "Linux");

   Project.Tree.Load (Prj, Create ("demo.gpr"), Ctx);

   for P of Prj loop
      Display (P);
   end loop;

   Text_IO.Put_Line ("//// OS set to Windows");
   Prj.Set_Context (Context.Empty, Changed_Callback'Access);

   for P of Prj loop
      Display (P);
   end loop;

exception
   when Project_Error =>

      Text_IO.Put_Line ("Messages found:");

      for C in Prj.Log_Messages.Iterate (False, True, True, True, True) loop
         declare
            use Ada.Strings;
            use Ada.Strings.Fixed;
            DS  : Character renames GNAT.OS_Lib.Directory_Separator;
            M   : constant Message.Object := Log.Element (C);
            Mes : constant String := M.Format;
            L   : constant Natural :=
                    Fixed.Index (Mes, DS & "aggregate-dup-src" & DS);
         begin
            if L /= 0 then
               Text_IO.Put_Line
                 (Replace_Slice
                    (Mes,
                     Fixed.Index (Mes (1 .. L), """", Going => Backward) + 1,
                     L - 1,
                     "<path>"));
            else
               Text_IO.Put_Line (Mes);
            end if;
         end;
      end loop;
end Main;
