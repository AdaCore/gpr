with Ada.Text_IO;
with Ada.Command_Line;
with System.CRC32;

package body Helper is
   procedure Do_Something is
      A : constant String := Ada.Command_Line.Argument (1);
      B : constant String := Ada.Command_Line.Argument (2);
      C : constant String := Ada.Command_Line.Argument (3);
      S : constant String := A & B & C;

      CRC : System.CRC32.CRC32;
   begin
      System.CRC32.Initialize (CRC);
      System.CRC32.Update (CRC, 'z');
      Ada.Text_IO.Put_Line (S & CRC'Image);
   end Do_Something;
end Helper;
