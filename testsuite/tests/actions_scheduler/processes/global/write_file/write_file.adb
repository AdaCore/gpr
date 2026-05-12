with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with GNATCOLL.OS.Stat;

function Write_File return Integer is
   Ret_Code     : Integer :=
                    Integer'Value (Ada.Command_Line.Argument (1));
   File_Name    : constant String := Ada.Command_Line.Argument (2);
   File_Content : constant String := Ada.Command_Line.Argument (3);
   F            : File_Type;
begin
   if Ada.Command_Line.Argument_Count >= 4 then
      --  Wait for Arg(4) to be present on disk before executing
      declare
         Wait_Name : constant String := Ada.Command_Line.Argument (4);
         Stat      : GNATCOLL.OS.Stat.File_Attributes;
      begin
         loop
            Stat := GNATCOLL.OS.Stat.Stat (Wait_Name);
            exit when GNATCOLL.OS.Stat.Is_File (Stat);
            delay 0.1;
         end loop;

         --  Ensure that the process we are waiting for has completed.
         --  Otherwise, one possible scenario is that the file is created by
         --  the process we are waiting on, but that process has not yet
         --  finished.
         delay 1.0;
      end;
   end if;

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
