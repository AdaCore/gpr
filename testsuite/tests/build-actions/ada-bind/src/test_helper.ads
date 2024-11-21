with GPR2.Build.Actions.Ada_Bind;
with GPR2.Project.Tree;

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;

use GPR2;

package Test_Helper is
   
   package GBA renames Build.Actions;
   
   type Assert_Topic is (Setup, Check);

   procedure Assert 
     (Condition : Boolean; 
      Message   : String := "";
      Debug     : String := "";
      Topic     : Assert_Topic := Check);
   
   procedure Create_Ali_File (Base_Name : String);
   
   function Create_Binder_Action 
     (Tree : Project.Tree.Object) return GBA.Ada_Bind.Object;
   
   function Launch_Action (Args : Argument_List) return Integer;
   
   function Load_Project (Project : String) return GPR2.Project.Tree.Object;
   
   procedure New_Test_Case (Message : String := "");
   
   function Result return Integer;
   
private
   
   TEST_OK : constant := 0;
   TEST_KO : constant := 1;
   
   Test_Result : Integer := TEST_OK;
   Test_Cases  : Integer := 1;

   function Result return Integer is (Test_Result);

end Test_Helper;
