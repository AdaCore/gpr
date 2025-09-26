with packC;

package body packA.packB
is

  procedure sep_1
  is separate;

  procedure proc_2
  is
  begin

     packC.proc_3;

     sep_1;
     
  end proc_2;

end packA.packB;
  
