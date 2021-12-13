procedure Main is
   procedure Toto;
   pragma Import (C, Toto);
   procedure Titi;
   pragma Import (C, Titi);
begin
   Toto;
   Titi;
end Main;

