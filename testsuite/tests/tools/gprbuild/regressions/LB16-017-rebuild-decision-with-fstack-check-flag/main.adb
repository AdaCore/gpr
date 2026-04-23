with text_io;
with unit1;
with unit2;

procedure main is
begin

text_io.put_line ("37+56="&integer'image(unit1.somme(34,56)));
text_io.put_line ("45+56="&integer'image(unit2.somme(45,56)));
end main;
