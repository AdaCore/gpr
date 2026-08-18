package Greeter is

   procedure Greet (Name : String);
   --  An ordinary subprogram: a client only ever needs this spec, so GNAT
   --  does not set Body_Needed_For_SAL and the body is not copied to the
   --  Library_Src_Dir.

end Greeter;
