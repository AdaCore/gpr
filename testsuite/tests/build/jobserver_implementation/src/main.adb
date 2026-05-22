with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with GPR2.Build.Jobserver; use GPR2;

function Main return Integer is
   package BJ renames Build.Jobserver;
begin
   GNATCOLL.Traces.Parse_Config_File;

   declare
      JS : BJ.Object;
   begin
      JS.Initialize_Protocol;
      Put_Line ("   - Dry_Run    : " & JS.Dry_Run'Img);
      Put_Line ("   - Active     : " & JS.Is_Available'Img);
      Put_Line ("   - Error      : " & JS.Has_Protocol_Error'Img);
   end;

   return 0;
exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Information (E));
      return 1;
end Main;
