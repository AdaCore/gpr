with P;

package body Q is
   function Value return Integer is (2 + P.Own_Value);
end Q;
