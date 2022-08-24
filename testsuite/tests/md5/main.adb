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

   File : constant Path_Name.Object := Path_Name.Create_File ("demo.gpr");

begin
   Text_IO.Put_Line ("MD5: " & File.Content_MD5);
end Main;
