with GNAT.IO;
with Code;
with PckA;

procedure Repl is
begin
    GNAT.IO.Put_Line ("Code" & Code.Dummy'Img & PckA.Dummy'Img);
end;
