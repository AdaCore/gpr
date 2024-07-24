with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;

function Write_File return Integer is
   Ret_Code     : Integer :=
     Integer'Value (Ada.Command_Line.Argument (1));
   File_Name    : constant String := Ada.Command_Line.Argument (2);
   File_Content : constant String := Ada.Command_Line.Argument (3);
   F            : File_Type;
begin
   Create (F, Out_File, File_Name);
   Put_Line (F, File_Content);

   if Ret_Code /= 0 then
      Ada.Text_IO.Put_Line
        (Standard_Error, "File has been created, but return code is " &
         Ret_Code'Img);
   end if;

   Close (F);

   return Ret_Code;
end Write_File;
