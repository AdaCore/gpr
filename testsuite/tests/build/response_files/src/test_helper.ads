with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;

with GPR2.Path_Name; use GPR2;

package Test_Helper is
   
   function Image (Command : Argument_List) return String;
   function Image_RF 
     (Path    : Path_Name.Object;
      Command : Unbounded_String) return String;
   
   procedure New_Test_Case (Message : String := "");
   
   function Result return Integer;
   
private
   
   TEST_OK : constant := 0;
   TEST_KO : constant := 1;

   Test_Result : Integer := TEST_OK;
   Test_Cases  : Integer := 1;
   
   function Result return Integer is (Test_Result);
   
end Test_Helper;
