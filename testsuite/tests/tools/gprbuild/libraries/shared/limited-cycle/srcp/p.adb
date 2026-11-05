with Q;

package body P is
   function Own_Value return Integer is (1);

   function Value return Integer is (Own_Value + Q.Value);
end P;
