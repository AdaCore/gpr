--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Environment_Variables;
with Ada.Text_IO;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;

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

   procedure Test (Project_Name : GPR2.Filename_Type) is
      procedure Check (Name : Name_Type) is
      begin
         if Tree.Configuration.Has_Externals
           and then Tree.Configuration.Externals.Contains (Name) then
            Ada.Text_IO.Put_Line (String (Name) & " in externals");
         else
            Ada.Text_IO.Put_Line (String (Name) & " in externals");
         end if;
--         if not Context.Contains (Name) then
--            Ada.Text_IO.Put_Line (String (Name) & " not found in context");
--         else
--            Ada.Text_IO.Put_Line
--              (String (Name) & "=" & String (Context.Element (Name)));
--         end if;
      end;
      procedure Print_Variable (Name : Name_Type) is
      begin
         if Tree.Root_Project.Variables.Contains (Name) then
            Ada.Text_IO.Put_Line
              (String (Name) & "="
               & Tree.Root_Project.Variable (Name).Value.Text);
         else
            Ada.Text_IO.Put_Line (String (Name) & " not found");
         end if;

      end Print_Variable;

   begin
      Ada.Text_IO.Put_Line ("testing " & String (Project_Name));
      Tree.Unload;
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File
           (GPR2.Project.Ensure_Extension (Project_Name),
            GPR2.Path_Name.No_Resolution),
         Context  => Context);
      Check ("EXTERNAL_WITHOUT_DEFAULT");
      Check ("EXTERNAL_WITH_DEFAULT");
      Print_Variable ("Value1");
      Print_Variable ("Value2");
   exception
      when Project_Error =>
         Print_Messages;
   end Test;

begin
   Test ("files/imported2");
   Test ("files/imported1");
   Test ("files/extending6");
   Test ("files/extending5");
   Test ("files/extending4");
   Test ("files/extending3");
   Test ("files/extending2");
   Test ("files/extending1");
   Test ("files/aggregate2");
   Test ("files/aggregate1");
   Ada.Environment_Variables.Set ("EXTERNAL_WITHOUT_DEFAULT", "environment_value_1");
   Test ("files/imported2");
   Test ("files/imported1");
   Test ("files/extending6");
   Test ("files/extending5");
   Test ("files/extending4");
   Test ("files/extending3");
   Test ("files/extending2");
   Test ("files/extending1");
   Test ("files/aggregate2");
   Test ("files/aggregate1");
   Ada.Environment_Variables.Set ("EXTERNAL_WITH_DEFAULT", "environment_value_2");
   Test ("files/imported2");
   Test ("files/imported1");
   Test ("files/extending6");
   Test ("files/extending5");
   Test ("files/extending4");
   Test ("files/extending3");
   Test ("files/extending2");
   Test ("files/extending1");
   Test ("files/aggregate2");
   Test ("files/aggregate1");
end Main;
