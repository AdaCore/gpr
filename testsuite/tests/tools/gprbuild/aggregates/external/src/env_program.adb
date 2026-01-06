with Ada.Environment_Variables;
with Ada.Text_IO;

procedure Env_Program is
   package Envvar renames Ada.Environment_Variables;
   package IO     renames Ada.Text_IO;
begin
   IO.Put_Line
     ("FOO=" & (if Envvar.Exists ("FOO") then Envvar.Value ("FOO") else ""));
end Env_Program;
