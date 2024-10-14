package Mylib with Preelaborate is
   I : Integer := 2
      with Export, Convention => C, External_Name => "mylib__i";
end Mylib;
