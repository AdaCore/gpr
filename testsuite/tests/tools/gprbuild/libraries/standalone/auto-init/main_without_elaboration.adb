with Ada.Text_IO;
with Pkg1;

pragma Elaborate_All (Pkg1);
--  Ensure that the body and the spec are elaborated

procedure Main_Without_Elaboration is
begin
   Ada.Text_IO.Put_Line (Pkg1.Execute);
end Main_Without_Elaboration;

