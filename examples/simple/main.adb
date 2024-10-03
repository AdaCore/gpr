--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;

with GPR2.Context;
with GPR2.Options;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Variable.Set;

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
         Text_IO.Put ("A:   " & Image (Att.Name.Id));

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
   Opt        : Options.Object;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");

   if not Prj1.Load (Opt, Absent_Dir_Error => No_Error) then
      return;
   end if;

   if not Prj2.Load (Opt, Absent_Dir_Error => No_Error) then
      return;
   end if;

   Ctx := Prj1.Context;
   Ctx.Include ("OS", "Linux");

   if not Prj1.Set_Context (Ctx, Changed_Callback'Access) then
      Text_IO.Put_Line ("!! Could not change the context");
   end if;

   Ctx := Prj2.Context;
   Ctx.Include ("OS", "Windows");

   if not Prj2.Set_Context (Ctx, Changed_Callback'Access) then
      Text_IO.Put_Line ("!! Could not change the context");
   end if;

   Display (Prj1.Root_Project);
   Display (Prj2.Root_Project);

   Ctx.Clear;
   Ctx.Include ("OS", "Linux-2");

   if not Prj2.Set_Context (Ctx, Changed_Callback'Access) then
      Text_IO.Put_Line ("!! Could not change the context");
   end if;

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
end Main;
