with Ada.Text_IO;
with Pkg1;
pragma Elaborate_All (Pkg1);
procedure Main_With_Elaboration is
  procedure SAL_Init;
  pragma Import (C, SAL_Init, "salinit");
begin

   Ada.Text_IO.Put_Line (Pkg1.Execute);
   SAL_Init;
   Ada.Text_IO.Put_Line (Pkg1.Execute);
end Main_With_Elaboration;

