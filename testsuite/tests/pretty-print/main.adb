--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;

with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Pretty_Printer; use GPR2.Project.Pretty_Printer;
with GPR2.Project.Tree;
procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Test
     (Filename               : String;
      Write_Character        : access procedure (C : Character) := null;
      Write_String           : access procedure (S : String)    := null;
      Write_EOL              : access procedure                 := null;
      With_Comments          : Boolean              := True;
      Initial_Indent         : Natural              := 0;
      Increment              : Positive             := 3;
      Max_Line_Length        : Line_Length          := 80;
      Minimize_Empty_Lines   : Boolean              := False;
      Backward_Compatibility : Boolean              := False
     );

   ----------
   -- Test --
   ----------

   procedure Test
     (Filename               : String;
      Write_Character        : access procedure (C : Character) := null;
      Write_String           : access procedure (S : String)    := null;
      Write_EOL              : access procedure                 := null;
      With_Comments          : Boolean              := True;
      Initial_Indent         : Natural              := 0;
      Increment              : Positive             := 3;
      Max_Line_Length        : Line_Length          := 80;
      Minimize_Empty_Lines   : Boolean              := False;
      Backward_Compatibility : Boolean              := False
     ) is
      Prj : Project.Tree.Object;
      Ctx : Context.Object;
      PP  : Project.Pretty_Printer.Object :=
              Project.Pretty_Printer.Create
                (With_Comments          => With_Comments,
                 Initial_Indent         => Initial_Indent,
                 Increment              => Increment,
                 Max_Line_Length        => Max_Line_Length,
                 Minimize_Empty_Lines   => Minimize_Empty_Lines,
                 Backward_Compatibility => Backward_Compatibility);
   begin
      Project.Tree.Load (Prj, Create (Filename_Type (Filename)), Ctx);
      PP.Pretty_Print
        (View            => Prj.Root_Project,
         Write_Character => Write_Character,
         Write_String    => Write_String,
         Write_EOL       => Write_EOL);
      Ada.Text_IO.Put (PP.Result);

   exception
      when GPR2.Project_Error =>
         Text_IO.Put_Line ("Error: failed to load the tree");
         for C in Prj.Log_Messages.Iterate
           (False, False, True, True, True)
         loop
            declare
               M   : constant Message.Object := Log.Element (C);
               Mes : constant String := M.Format;
            begin
               Text_IO.Put_Line (Mes);
            end;
         end loop;
   end Test;

   procedure Write_C (C : Character) is
   begin
      Text_IO.Put (C);
   end Write_C;

   procedure Write_S (S : String) is
   begin
      Text_IO.Put (S);
   end Write_S;

   procedure EOL is
   begin
      Text_IO.Put_Line ("");
   end EOL;

begin
   Test ("demo.gpr");
   Test
     (Filename               => "abstract_prj.gpr",
      Write_Character        => Write_C'Access,
      Write_String           => Write_S'Access,
      Write_EOL              => EOL'Access);
   Test ("lib.gpr", Backward_Compatibility => True);
   Test ("aggregate_prj.gpr");
   Test ("aggregate_lib.gpr");
end Main;
