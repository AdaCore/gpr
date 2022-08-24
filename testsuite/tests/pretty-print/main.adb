--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;

with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Project.Pretty_Printer;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Load (Filename : String);

   ----------
   -- Load --
   ----------

   procedure Load (Filename : String) is
      Prj : Project.Tree.Object;
      Ctx : Context.Object;
      PP  : Project.Pretty_Printer.Object;
   begin
      Project.Tree.Load (Prj, Create (Filename_Type (Filename)), Ctx);
      PP := Project.Pretty_Printer.Create;
      PP.Pretty_Print (Prj.Root_Project);
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
   end Load;

begin
   Load ("demo.gpr");
end Main;
