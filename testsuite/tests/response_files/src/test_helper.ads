with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;

package Test_Helper is
   
   function Image (Command : Argument_List) return String;
   function Image_RF (Command : Argument_List) return String;
   procedure New_Test_Case (Message : String := "");
   
   function Result return Integer;
   
private
   
   TEST_OK : constant := 0;
   TEST_KO : constant := 1;

   Test_Result : Integer := TEST_OK;
   Test_Cases  : Integer := 1;
   
   function Result return Integer is (Test_Result);
   
end Test_Helper;
