with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   File : File_Type;
   Line : String (1 .. 255);
   Last : Natural;
begin
   Open (File, In_File, "prj.gpr");

   while not End_Of_File (File) loop
      Get_Line (File, Line, Last);
      Put_Line (Line (1 .. Last));
   end loop;

   Close (File);
end Main;

