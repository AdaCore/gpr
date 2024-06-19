with Ada.Text_IO;
with Foo;
with Pkg;

procedure Main is
   procedure My_Pkg is new Pkg (Ada.Text_IO.Put_Line);
begin
   My_Pkg;
end Main;
