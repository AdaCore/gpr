with GNAT.IO;  use GNAT.IO;
with A.P1;
with A.P2;
with G.Foo;
with I.Foo;
with S.Foo;
with H.Foo;
procedure Test is
begin
   Put_Line (A.P1.Msg & A.P2.Msg & A.Foo1 & A.Foo2);
end Test;
