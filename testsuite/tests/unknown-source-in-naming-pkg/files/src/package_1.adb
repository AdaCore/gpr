
with TEXT_IO;

package body PACKAGE_1 is
  
  procedure PUT_LINE (STR : in STRING) is
  begin
    TEXT_IO.PUT_LINE ("PACKAGE_1 : " & STR);
  end PUT_LINE;

end PACKAGE_1;
