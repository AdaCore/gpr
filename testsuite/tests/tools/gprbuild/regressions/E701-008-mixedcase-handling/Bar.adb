with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with FOO.Config_Pkg;

function Bar (Old_Path : String) return String is
   Separator_Map : constant Ada.Strings.Maps.Character_Mapping :=
                               Ada.Strings.Maps.To_Mapping ("/\",
                                         FOO.Config_Pkg.Separator & FOO.Config_Pkg.Separator);
begin
   return Ada.Strings.Fixed.Translate (Old_Path, Separator_Map);
end Bar;
