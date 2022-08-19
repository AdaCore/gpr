--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with Ada.Exceptions;

with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Variable.Set;
with GPR2.Context;
with GPR2.Message;
with GPR2.Log;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   procedure Changed_Callback (Prj : Project.View.Object);

   ----------------------
   -- Changed_Callback --
   ----------------------

   procedure Changed_Callback (Prj : Project.View.Object) is
   begin
      Text_IO.Put_Line
        (">>> Changed_Callback for " & String (Prj.Path_Name.Value));
   end Changed_Callback;

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;

      procedure Display (Att : Project.Attribute.Object) is
      begin
         Text_IO.Put ("A:   " & String (Image (Att.Name.Id.Attr)));

         if Att.Has_Index then
            Text_IO.Put (" (" & Att.Index.Value & ")");
         end if;

         Text_IO.Put (" -> ");

         for V of Att.Values loop
            Text_IO.Put (V.Text & " ");
         end loop;
         Text_IO.New_Line;
      end Display;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Full then
            for A of Prj.Attributes loop
               Display (A);
            end loop;

         if Prj.Has_Variables then
            for V in Prj.Variables.Iterate loop
               Text_IO.Put ("V:   " & String (Key (V)));
               Text_IO.Put (" -> ");
               Text_IO.Put (String (Element (V).Value.Text));
               Text_IO.New_Line;
            end loop;
         end if;
         Text_IO.New_Line;
      end if;
   end Display;

   Prj1, Prj2 : Project.Tree.Object;
   Ctx        : Context.Object;

   procedure Print_Messages (Prj : Project.Tree.Object) is
   begin
      if Prj.Has_Messages then
         for C in Prj.Log_Messages.Iterate
           (False, True, True, True, True)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
      end if;
   end Print_Messages;

begin
   Project.Tree.Load
     (Self => Prj1, Filename  => Create ("demo.gpr"), Context => Ctx);
   Project.Tree.Load
     (Self => Prj2, Filename  => Create ("demo.gpr"), Context => Ctx);

   Ctx := Prj1.Context;
   Ctx.Include ("OS", "Linux");
   Prj1.Set_Context (Ctx, Changed_Callback'Access);

   Ctx := Prj2.Context;
   Ctx.Include ("OS", "Windows");
   Prj2.Set_Context (Ctx, Changed_Callback'Access);

   Display (Prj1.Root_Project);
   Display (Prj2.Root_Project);

   Ctx.Clear;
   Ctx.Include ("OS", "Linux-2");
   Prj2.Set_Context (Ctx, Changed_Callback'Access);
   Display (Prj2.Root_Project);

   --  Iterator

   Text_IO.Put_Line ("**************** Iterator Prj1");

   for C in Prj1.Iterate
     (Kind => (I_Project | I_Imported => True, others => False))
   loop
      Display (Project.Tree.Element (C), Full => False);
      if Project.Tree.Is_Root (C) then
         Text_IO.Put_Line ("   is root");
      end if;
   end loop;

   Text_IO.Put_Line ("**************** Iterator Prj2");

   for C in Project.Tree.Iterate (Prj2) loop
      Display (Project.Tree.Element (C), Full => False);
   end loop;

   Text_IO.Put_Line ("**************** Iterator Prj3");

   for C in Prj2.Iterate (Filter => (F_Library => True, others => False)) loop
      Display (Project.Tree.Element (C), Full => False);
   end loop;

   Text_IO.Put_Line ("**************** Iterator Prj4");

   for P of Prj2 loop
      Display (P, Full => False);
   end loop;

exception
   when Ex : others =>
      Text_IO.Put_Line (Ada.Exceptions.Exception_Message (Ex));
      Print_Messages (Prj1);
      Print_Messages (Prj2);
end Main;
