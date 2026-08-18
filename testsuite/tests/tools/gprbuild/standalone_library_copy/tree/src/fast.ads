package Fast is

   function Double (X : Integer) return Integer;
   pragma Inline (Double);
   --  Inlined, so a client may need the body: GNAT sets Body_Needed_For_SAL
   --  on the spec and both parts are copied to the Library_Src_Dir.

end Fast;
