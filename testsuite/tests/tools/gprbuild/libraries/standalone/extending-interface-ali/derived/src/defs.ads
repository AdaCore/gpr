--  Redefines the type Printer and Extra depend on, so that both must be
--  recompiled although the extending library does not redefine them.

package Defs is
   type Level is (Low, Medium, High);
   Default : constant Level := Medium;
end Defs;
