with GNAT.IO; use GNAT.IO;
with Test;
with Unchecked_Deallocation;
with Unchecked_Conversion;
with GNAT.Debug_Pools;
with Ada.Exceptions; use Ada.Exceptions;
procedure main is

   procedure Debug_Pool_Test is

      type T is access Integer;
      type U is access all T;

      P : GNAT.Debug_Pools.Debug_Pool;
      for T'Storage_Pool use P;

      procedure Free is new Unchecked_Deallocation (Integer, T);
      function UC is new Unchecked_Conversion (U, T);
      A, B : aliased T;

   begin
      A := new Integer;
      B := new Integer;
      B := A;
      Free (A);
      begin
         Put_Line (Integer'Image(B.all));
      exception
         when E : others => Put_Line ("raised: " & Exception_Name (E));
      end;
      begin
         Free (B);
      exception
         when E : others => Put_Line ("raised: " & Exception_Name (E));
      end;
      B := UC(A'Access);
      begin
         Put_Line (Integer'Image(B.all));
      exception
         when E : others => Put_Line ("raised: " & Exception_Name (E));
      end;
      begin
         Free (B);
      exception
         when E : others => Put_Line ("raised: " & Exception_Name (E));
      end;
   end Debug_Pool_Test;
begin
   Debug_Pool_Test;
   Test.Start_Tasks;
   while not Test.Is_Test_Ended loop
      null;
   end loop;
   if Test.Is_Test_OK then
      Put_Line ("OK");
   else
      Put_Line ("ERROR");
   end if;
end main;
