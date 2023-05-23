--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Build.Source.Sets;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);

      if Prj.Kind /= K_Abstract then
         Text_IO.Put
           ("obj_dir=" & String (Prj.Object_Directory.Simple_Name) & ' ');
      end if;

      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Prj.Kind in With_Object_Dir_Kind then
         for Source of Prj.Sources loop
            declare
               U : constant Optional_Name_Type := Source.Unit.Name;
            begin
               Output_Filename (Source.Path_Name.Value);

               Text_IO.Set_Col (16);
               Text_IO.Put ("   language: " & Image (Source.Language));

               Text_IO.Set_Col (33);
               Text_IO.Put ("   Kind: " & Source.Kind'Image);

               if U /= "" then
                  Text_IO.Put ("   unit: " & String (U));
               end if;

               Text_IO.New_Line;
            end;
         end loop;
      end if;
   end Display;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      I : constant Positive := Strings.Fixed.Index (Filename, "extended");
   begin
      Text_IO.Put (" > " & Filename (I + 8 .. Filename'Last));
   end Output_Filename;

   Prj1, Prj2 : Project.Tree.Object;
   Ctx        : Context.Object;
   Log        : GPR2.Log.Object;
begin
   Project.Tree.Load (Prj1, Project.Create ("prj1.gpr"), Ctx);
   Prj1.Log_Messages.Output_Messages (Information => False);
   Prj1.Update_Sources (Messages => Log);
   Log.Output_Messages;

   Project.Tree.Load (Prj2, Project.Create ("prj2.gpr"), Ctx);
   Prj2.Log_Messages.Output_Messages (Information => False);
   Prj2.Update_Sources (Messages => Log);
   Log.Output_Messages;

   Text_IO.Put_Line ("**************** Iterator Prj1");

   for P of Prj1 loop
      Display (P, Full => False);
   end loop;

   Text_IO.Put_Line ("**************** Iterator Prj2");

   for C in Prj2.Iterate
     (Kind => (I_Project | I_Imported | I_Recursive => True, others => False))
   loop
      Display (Project.Tree.Element (C), Full => False);
   end loop;
end Main;
