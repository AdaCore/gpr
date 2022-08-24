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

   P1 : constant Path_Name.Object :=
          Path_Name.Create_File ("/dir1/dir2/toto");
   P2 : constant Path_Name.Object :=
          Path_Name.Create_File ("/dir1/dir2/dir3/toto");
   P3 : constant Path_Name.Object :=
          Path_Name.Create_File ("/dir1/dira/toto");
   P4 : constant Path_Name.Object :=
          Path_Name.Create_Directory ("/dir1/dira/toto");
   P5 : constant Path_Name.Object :=
          Path_Name.Create_File ("/xyz");

   function Remove_Drive_Letter (Path : String) return String is
   begin
      if Path'Length > 2 and then not (Path (Path'First) in '/' | '\') then
         return Path (Path'First + 2 .. Path'Last);
      else
         return Path;
      end if;
   end Remove_Drive_Letter;

begin
   Text_IO.Put_Line
     ("1: " & Remove_Drive_Letter (P1.Common_Prefix (P2).Value));
   Text_IO.Put_Line
     ("2: " & Remove_Drive_Letter (P2.Common_Prefix (P3).Value));
   Text_IO.Put_Line
     ("3: " & Remove_Drive_Letter (P3.Common_Prefix (P3).Value));
   Text_IO.Put_Line
     ("4: " & Remove_Drive_Letter (P4.Common_Prefix (P4).Value));
   Text_IO.Put_Line
     ("5: " & Remove_Drive_Letter (P3.Common_Prefix (P5).Value));
end Main;
