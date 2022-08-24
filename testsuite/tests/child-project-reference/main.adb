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
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Variable;
with GPR2.Project.Typ;
with GPR2.Project.Pretty_Printer;
with GPR_Parser.Analysis;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   Context      : GPR2.Context.Object;
   Filename     : constant GPR2.Path_Name.Object :=
                    GPR2.Path_Name.Create_File
                      (GPR2.Project.Ensure_Extension ("prj.gpr"),
                       GPR2.Path_Name.No_Resolution);
   use GPR2;

   procedure Print_Messages is
   begin
      if Tree.Has_Messages then
         for C in Tree.Log_Messages.Iterate (Information => False)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
      end if;
   end Print_Messages;

   procedure Print_Variable (Variable : GPR2.Project.Variable.Object) is
      use GPR2.Project.Registry.Attribute;
   begin
      Ada.Text_IO.Put(String (Variable.Name.Text) & ":" & Variable.Kind'Img & "=");
      if Variable.Kind = GPR2.Project.Registry.Attribute.Single then
         Ada.Text_IO.Put_Line (Variable.Value.Text);
      else
         for V of Variable.Values loop
            Ada.Text_IO.Put (V.Text & ";");
         end loop;
         Ada.Text_IO.Put_Line ("");
      end if;
   end Print_Variable;

   procedure Test_Bad (Project_Name : GPR2.Filename_Type) is
   begin
      Ada.Text_IO.Put_Line ("testing " & String (Project_Name));
      Tree.Unload;
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File
           (GPR2.Project.Ensure_Extension (Project_Name),
            GPR2.Path_Name.No_Resolution),
         Context  => Context);
   exception
      when Project_Error =>
         Print_Messages;
   end Test_Bad;

   procedure Test_Pretty_Printer (Project_Name : GPR2.Filename_Type) is
      Ctx  : constant GPR_Parser.Analysis.Analysis_Context :=
               GPR_Parser.Analysis.Create_Context;
      Path : constant GPR2.Path_Name.Object :=
               GPR2.Path_Name.Create_File
                 (GPR2.Project.Ensure_Extension (Project_Name));
      Unit : constant GPR_Parser.Analysis.Analysis_Unit :=
               GPR_Parser.Analysis.Get_From_File (Ctx, Path.Value);
      PP   : GPR2.Project.Pretty_Printer.Object;

   begin
      Ada.Text_IO.Put_Line ("testing " & String (Project_Name));
      PP.Pretty_Print (Analysis_Unit => Unit);
      Ada.Text_IO.Put_Line (PP.Result);
   exception
      when Project_Error =>
         Print_Messages;
   end Test_Pretty_Printer;

begin
   Tree.Load_Autoconf (Filename => Filename, Context => Context);
   Print_Messages;
   Print_Variable (Tree.Root_Project.Variable (Name => "V1"));
   Print_Variable (Tree.Root_Project.Variable (Name => "V2"));
   Print_Variable (Tree.Root_Project.Variable (Name => "V3"));
   Print_Variable (Tree.Root_Project.Variable (Name => "V4"));
   Print_Variable (Tree.Root_Project.Variable (Name => "V5"));
   declare
      Typ : GPR2.Project.Typ.Object :=
              Tree.Root_Project.Variable (Name => "V6").Typ;
   begin
      Ada.Text_IO.Put_Line (String (Typ.Values.First_Element.Text));
   end;

   --  check invalid ref is package renaming & extending
   Test_Bad ("bad1.gpr");
   Test_Bad ("bad2.gpr");
   Test_Bad ("bad3.gpr");
   Test_Bad ("bad4.gpr");

   --  check package renaming/extending and attribute/variable/type refs
   --  pretty pinting
   Test_Pretty_Printer ("prj.gpr");
   Test_Pretty_Printer ("build-settings.gpr");

exception
   when Project_Error =>
      Print_Messages;
end Main;
