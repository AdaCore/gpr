--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Text_IO;

with GPR2.Build.View_Db;
with GPR2.Project.View;

procedure Main is
   Name : constant String := GPR2.To_Lower_ASCII ("HELLO");
   View : constant GPR2.Project.View.Object := GPR2.Project.View.Undefined;
begin
   Ada.Text_IO.Put_Line (Name);

   if View.Is_Defined then
      declare
         Db : constant GPR2.Build.View_Db.Object := View.View_Db;
      begin
         Ada.Text_IO.Put_Line (Boolean'Image (Db.Is_Defined));
      end;
   end if;
end Main;
