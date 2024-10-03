--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with Ada.Exceptions;

with GPR2.Context;
with GPR2.Options;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Source_Reference.Value;
with GPR2.Message;
with GPR2.Log;

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

      procedure Display (Att : Project.Attribute.Object) is
      begin
         Text_IO.Put ("   " & String (Image (Att.Name.Id.Attr)));

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
         for A of Prj.Attributes (With_Defaults => False,
                                  With_Config   => False)
         loop
            Display (A);
         end loop;
         Text_IO.New_Line;
      end if;
   end Display;

   Prj : Project.Tree.Object;
   Opt : Options.Object;
   Ctx : Context.Object;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");

   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Ctx.Include ("CASESTMT_OS", "Linux");

      if Prj.Set_Context (Ctx) then
         Display (Prj.Root_Project);
      else
         Text_IO.Put_Line ("!!! Error occurred");
      end if;

      Ctx.Clear;
      Ctx.Include ("CASESTMT_OS", "Windows");

      if Prj.Set_Context (Ctx) then
         Display (Prj.Root_Project);
      else
         Text_IO.Put_Line ("!!! Error occurred");
      end if;
   end if;
end Main;
