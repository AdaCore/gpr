with Leaf;

package body Bundle is
   function Value return Integer is (Leaf.Value + 1);
end Bundle;
