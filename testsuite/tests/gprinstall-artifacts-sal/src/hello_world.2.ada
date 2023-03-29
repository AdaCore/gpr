with Ada.Text_IO;

use Ada.Text_IO;

pragma elaborate_all (Ada.Text_IO);

package body Hello_World is

  procedure Print_Hello_Implementation is
  
  begin
  
    Put_Line ("Hello My World!");
    
  end Print_Hello_Implementation;
  
end Hello_World;