--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   Context      : GPR2.Context.Object;
   use GPR2;

   procedure Print_Messages is
   begin
      if Tree.Has_Messages then
         for C in Tree.Log_Messages.Iterate
           (False, True, True, True, True)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
      end if;
   end Print_Messages;

   function Get_Aggregated
     (View : GPR2.Project.View.Object; Name : Simple_Name)
      return GPR2.Project.View.Object is
   begin
      for V of View.Aggregated loop
         if V.Path_Name.Simple_Name = Name then
            return V;
         end if;
      end loop;
      return GPR2.Project.View.Undefined;
   end Get_Aggregated;

   procedure Test (View : GPR2.Project.View.Object) is
      A     : GPR2.Project.View.Object;
      B     : GPR2.Project.View.Object;
   begin
      Ada.Text_IO.Put_Line ("testing " & String (View.Path_Name.Simple_Name));
      A := Get_Aggregated (View, "a.gpr");
      B := Get_Aggregated (View, "b.gpr");
      Ada.Text_IO.Put_Line ("A.Object_Dir:" & A.Object_Directory.Value);
      Ada.Text_IO.Put_Line ("B.Object_Dir:" & B.Object_Directory.Value);
   exception
      when Project_Error =>
         Print_Messages;
   end Test;

   procedure Load (Project_Name : GPR2.Filename_Type) is
   begin
      Ada.Text_IO.Put_Line ("loading " & String (Project_Name));
      Tree.Unload;
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File
           (GPR2.Project.Ensure_Extension (Project_Name),
            GPR2.Path_Name.No_Resolution),
         Context  => Context);
   exception
      when Project_Error =>
         Print_Messages;
   end Load;

begin
   Load ("files/aggl1.gpr");
   Test (Tree.Root_Project);
   Load ("files/aggl.gpr");
   Test (Get_Aggregated (Tree.Root_Project, "aggl1.gpr"));
end Main;
