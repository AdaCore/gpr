--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;

with GPR2.Path_Name;

procedure Main is

   use Ada;
   use GPR2;

   C1 : constant Path_Name.Object := Path_Name.Create_File ("css/*.css");
   C2 : constant Path_Name.Object := Path_Name.Create_File ("doc/README");

begin
   Text_IO.Put_Line ("C1.Simple_Name: " & String (C1.Simple_Name));
   Text_IO.Put_Line ("C2.Simple_Name: " & String (C2.Simple_Name));
   Text_IO.Put_Line ("C1.Base_Name: " & String (C1.Base_Name));
   Text_IO.Put_Line ("C2.Base_Name: " & String (C2.Base_Name));
end Main;
