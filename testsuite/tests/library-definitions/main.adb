--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;
   use GPR2.Message;

   procedure Display (Prj : Project.View.Object);

   function Filter_Path (Line : Filename_Type) return String;
   --  Remove the tmp directory where test is processing

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
      use GPR2.Project.Attribute.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Kind'Img);
      Text_IO.Put_Line
        ("Library_Name : " & String (Prj.Library_Name));
      Text_IO.Put_Line
        ("Library_Directory : " & String (Prj.Library_Directory.Name));
      Text_IO.Put_Line
        ("                  : " & Filter_Path (Prj.Library_Directory.Value));
      Text_IO.Put_Line
        ("Library Filename : " & String (Prj.Library_Filename.Name));
      Text_IO.Put_Line
        ("                 : " & Filter_Path (Prj.Library_Filename.Value));
      Text_IO.Put_Line
        ("Library Version Filename : "
         & String (Prj.Library_Version_Filename.Name));
      Text_IO.Put_Line
        ("                         : "
         & Filter_Path (Prj.Library_Version_Filename.Value));
       Text_IO.Put_Line
        ("Library Major Version Filename : "
         & String (Prj.Library_Major_Version_Filename.Name));
       Text_IO.Put_Line
        ("                               : "
         & Filter_Path (Prj.Library_Major_Version_Filename.Value));
      Text_IO.Put_Line
        ("Library_Standalone : " & Prj.Library_Standalone'Img);
   end Display;

   -----------------
   -- Filter_Path --
   -----------------

   function Filter_Path (Line : Filename_Type) return String is
      S : constant String := String (Line);
      Test : constant String := "library-definitions";
      I : constant Positive := Strings.Fixed.Index (S, Test);
   begin
      return S (I + Test'Length + 1 .. S'Last);
   end Filter_Path;

begin
   Project.Tree.Load (Prj, Create ("demo.gpr"), Ctx);

   Display (Prj.Root_Project);

   Ctx.Insert ("VERSION", "1.4");
   Prj.Set_Context (Ctx);

   Display (Prj.Root_Project);

   Project.Tree.Load (Prj, Create ("demo.gpr"), Ctx, Subdirs => "release");

   Display (Prj.Root_Project);

   Ctx.Replace ("VERSION", "A.B");
   Prj.Set_Context (Ctx);

   Display (Prj.Root_Project);

exception
   when Project_Error =>
      Prj.Log_Messages.Output_Messages (Information => False);
end Main;
