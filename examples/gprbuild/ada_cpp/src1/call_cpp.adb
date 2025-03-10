with Text_IO; use Text_IO;

procedure Call_CPP is
   procedure cpp_routine;
   pragma Import (C, cpp_routine);
   Error : exception;

   procedure Raise_And_Catch is
   begin
      Put ("in Ada: raise ...  ");
      raise Error;
   exception
      when Error =>
         Put_Line ("and catch!");
   end Raise_And_Catch;

begin
   Put_Line ("In Call_CPP");
   Raise_And_Catch;
   cpp_routine;
   Put_Line ("Back in Call_CPP");
   Raise_And_Catch;
end Call_CPP;
