with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with GNATCOLL.OS.Stat;

function Write_File return Integer is
   Ret_Code : Integer :=
                Integer'Value (Ada.Command_Line.Argument (1));
   Id       : constant String := Ada.Command_Line.Argument (2);
   Slot     : constant String := Ada.Command_Line.Argument (3);
   F        : File_Type;
begin
   Put_Line
     (Standard_Error, Id & ": executing from slot id " & Slot);

   Create (F, Out_File, Id & ".txt");
   Put_Line (F, Id);
   Close (F);

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
      end;
   end if;

   Put_Line
     (Standard_Error, Id & ": return with code" & Ret_Code'Image);

   return Ret_Code;
end Write_File;
