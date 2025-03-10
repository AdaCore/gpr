package C_Lib is
   procedure Do_Something;
   pragma Import (C, Do_Something);
   procedure Do_Something_Else;
   pragma Import (C, Do_Something_Else);
end C_Lib;

