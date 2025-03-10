--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;

procedure Main is

   use Ada;
   use GPR2;

   procedure Display (Prj : Project.View.Object);

   procedure Display (Att : Project.Attribute.Object);

   -------------
   -- Display --
   -------------

   procedure Display (Att : Project.Attribute.Object) is
   begin
      Text_IO.Put ("   " & Image (Att.Name.Id));

      if Att.Has_Index then
         Text_IO.Put (" (" & Att.Index.Value & ")");
      end if;

      Text_IO.Put (" -> ");

      for V of Att.Values loop
         Text_IO.Put (V.Text & " ");
      end loop;
      Text_IO.New_Line;
   end Display;

   procedure Display (Prj : Project.View.Object) is
      use GPR2.Project.Attribute.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Kind'Img);

      for A of Prj.Attributes (With_Defaults => False,
                               With_Config   => False)
      loop
         Display (A);
      end loop;

      for Pck of Prj.Packages loop
         for A of Prj.Attributes (Pck,
                                  With_Defaults => False,
                                  With_Config   => False)
         loop
            Display (A);
         end loop;
      end loop;

      Text_IO.New_Line;
   end Display;

   Prj : Project.Tree.Object;
   Opt : Options.Object;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");

   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Display (Prj.Root_Project);
   end if;
end Main;
