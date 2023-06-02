--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Environment_Variables;
with Ada.Text_IO;
with Ada.Strings.Fixed;

with GNAT.OS_Lib;

with GPR2.Build.Source.Sets;
with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display_Source (Src : GPR2.Build.Source.Object);
   procedure Test_Prj (Fname : Filename_Type);

   procedure Display_Source (Src : GPR2.Build.Source.Object) is
   begin
      Text_IO.Put_Line (String (Src.Path_Name.Simple_Name) & ": " & Src.Kind'Image);
   end Display_Source;

   procedure Test_Prj (Fname : Filename_Type)
   is
      Prj : Project.Tree.Object;
      Ctx : Context.Object;
      Log : GPR2.Log.Object;

   begin
      Text_IO.Put_Line ("GPR file: " & String (Fname));
      Project.Tree.Load (Prj, Create (Fname), Ctx);
      Prj.Update_Sources (Messages => Log);
      if Log.Has_Element then
         Text_IO.Put_Line ("Messages found:");
         Log.Output_Messages;
      end if;

      for V of reverse Prj.Ordered_Views loop
         Text_IO.Put_Line (String (V.Name));
         for S of V.Sources loop
            Display_Source (S);
         end loop;
      end loop;

      Prj.Unload;
      Text_IO.New_Line;
   end Test_Prj;

begin
   Test_Prj ("data/prj.gpr");
   Test_Prj ("data/prj1.gpr");
   Ada.Environment_Variables.Set
     ("SOURCE_LIST_FILE_PATH",
      GPR2.Path_Name.Create_File("data/sources_absolute.lst").Value);
   Test_Prj ("data/prj1.gpr");
   Test_Prj ("data/prj2.gpr");
   Test_Prj ("data/prj3.gpr");
end Main;
