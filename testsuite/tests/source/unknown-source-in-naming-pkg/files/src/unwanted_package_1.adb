
with TEXT_IO;

package body UNWANTED_PACKAGE_1 is
  
  procedure PUT_LINE (STR : in STRING) is
  begin
    TEXT_IO.PUT_LINE ("UNWANTED_PACKAGE_1 : " & STR);
  end PUT_LINE;

end UNWANTED_PACKAGE_1;
