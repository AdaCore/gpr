with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Fixed;

with Bar;

procedure A is
   File_Name : constant String := Bar (Ada.Command_Line.Argument(1));
begin
   Ada.Text_IO.Put_Line (File_Name);
end A;
