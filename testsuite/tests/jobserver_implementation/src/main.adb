with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Build.Jobserver; use GPR2;

function Main return Integer is
   package BJ renames Build.Jobserver;

   JS : BJ.Object'Class := BJ.Initialize;

   Char : Character;
begin
   Put_Line ("[ " & JS.Kind'Img & " ]");
   Put_Line ("   - Dry_Run    : " & JS.Dry_Run'Img);
   Put_Line ("   - Active     : " & JS.Active'Img);

   case JS.Kind is
      when BJ.JM_Undefined =>
         Put_Line ("   - Register   : " & JS.Register (Char)'Img);
         Put_Line ("   - Release    : " & JS.Release (Char)'Img);
         Put_Line ("   - Integrous  : " & JS.Is_Integrous'Img);
      when others =>
         null;
   end case;

   return 0;
exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Information (E));
      return 1;
end Main;
