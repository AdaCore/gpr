with Ada.Text_IO;
with Extended;
with Imported;
with LimitedImported;
procedure main is
   procedure C_Procedure;
   pragma Import (C, C_Procedure, "c_procedure");

begin
   Ada.Text_IO.Put_Line ("From Main");
   Extended.Msg;
   Imported.Msg;
   LimitedImported.Msg;
   C_Procedure;
end main;
