with packA.packB;

package body packA
is

  procedure proc_1
    is
  begin

     packA.packB.proc_2;
     
  end proc_1;

end packA;
  
