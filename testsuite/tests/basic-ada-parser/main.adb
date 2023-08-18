--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Text_IO;
with Gpr_Parser_Support.Text;     use Gpr_Parser_Support.Text;
with Gpr_Parser_Support.Slocs;
with Gpr_Parser.Basic_Ada_Parser; use Gpr_Parser.Basic_Ada_Parser;
with Gpr_Parser.Analysis;         use Gpr_Parser.Analysis;

with Ada.Command_Line;

procedure Main is
   use all type Gpr_Parser_Support.Text.Text_Type;

   procedure On_Error (Msg : String);

   procedure On_No_Body_CB;

   procedure Unit_Name_CB
     (Unit_Name     : String; Separate_From : String := "";
      Lib_Item_Type : Gpr_Parser.Basic_Ada_Parser.Library_Item_Type;
      Generic_Unit  : Boolean);

   procedure With_Clause_CB
     (Unit_Name  : String;
      Is_Limited : Boolean);

   procedure On_Error (Msg : String) is
   begin
      Ada.Text_IO.Put_Line ("Error during parsing: " & Msg);
   end On_Error;

   procedure On_No_Body_CB is
   begin
      Ada.Text_IO.Put_Line ("No body source file");
   end On_No_Body_CB;

   procedure Unit_Name_CB
     (Unit_Name     : String; Separate_From : String := "";
      Lib_Item_Type : Gpr_Parser.Basic_Ada_Parser.Library_Item_Type;
      Generic_Unit  : Boolean)
   is
   begin
      Ada.Text_IO.Put_Line ("Unit name: " & Unit_Name);
      if Separate_From /= "" then
         Ada.Text_IO.Put_Line ("Separate from: " & Separate_From);
      end if;
   end Unit_Name_CB;

   procedure With_Clause_CB
     (Unit_Name  : String;
      Is_Limited : Boolean)
   is
   begin
      Ada.Text_IO.Put_Line ("Withed unit name: " & Unit_Name);
   end With_Clause_CB;

   Ctx         : constant Analysis_Context := Create_Context;
begin

   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line
        ("Invalid number of arguments. Usage: ./test <file to parse>");
      return;
   end if;

   declare
      File : String := Ada.Command_Line.Argument (1);
   begin
      Ada.Text_IO.Put_Line ("Loading " & File);

      Parse_Context_Clauses
        (Filename       => File,
         Context        => Ctx,
         Log_Error      => On_Error'Access,
         With_Clause_CB => With_Clause_CB'Access,
         Unit_Name_CB   => Unit_Name_CB'Access,
         No_Body_CB     => On_No_Body_CB'Access);
   end;
end Main;
