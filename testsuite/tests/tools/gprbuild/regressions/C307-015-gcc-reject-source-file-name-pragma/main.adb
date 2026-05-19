pragma Source_File_Name_Project (Pkg, Spec_File_Name => "pkg_.ada");
with Ada.Text_IO; use Ada.Text_IO;
with Pkg;
procedure Main is
begin
   Put_Line (Pkg.Name);
end Main;

