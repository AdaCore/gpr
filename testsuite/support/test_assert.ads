--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Helper package to implement tests that comply with the expectations
--  of the default test driver.

with Ada.Strings.UTF_Encoding;
with Ada.Calendar; use Ada.Calendar;
with GNAT.Source_Info;
with GNATCOLL.VFS;

package Test_Assert is

   package SI renames GNAT.Source_Info;
   package VFS renames GNATCOLL.VFS;
   package UTF8 renames Ada.Strings.UTF_Encoding;

   Final_Status : Natural := 0;

   procedure Assert
      (Success  : Boolean;
       Msg      : String := "";
       Location : String := SI.Source_Location);
   --  If Success is True then test case is considered PASSED, otherwise
   --  the test status is FAILED and Final_Status set to 1.

   procedure Assert
      (Left, Right : String;
       Msg         : String := "";
       Location    : String := SI.Source_Location);
   --  If Left = Right then test case is considered PASSED, otherwise
   --  the test status is FAILED and Final_Status set to 1.

   procedure Assert
      (Left     : Wide_String;
       Right    : UTF8.UTF_8_String;
       Msg      : String := "";
       Location : String := SI.Source_Location);
   --  If Left = Right then test case is considered PASSED, otherwise
   --  the test status is FAILED and Final_Status set to 1.

   procedure Assert_Inferior
      (Left     : Time;
       Right    : Time;
       Msg      : String := "";
       Location : String := SI.Source_Location);
   --  If Left <= Right then test case is considred PASSED, otherwise
   --  the test status is FAILED and Final_Status set to 1.

   procedure Assert
      (Left        : Integer;
       Right       : Integer;
       Msg         : String := "";
       Location    : String := SI.Source_Location);
   --  If Left = Right then test case is considered PASSED, otherwise
   --  the test status is FAILED and Final_Status set to 1.

   procedure Assert
      (Left, Right : VFS.Virtual_File;
       Msg         : String := "";
       Location    : String := SI.Source_Location);
   --  If Left = Right then test case is considered PASSED, otherwise
   --  the test status is FAILED and Final_Status set to 1.

   Assert_Count : Natural := 0;
   --  Incremented every time an assert is called. Can be checked against a
   --  specific value to verify that the expected number of Asserts triggered,
   --  when their number depends on execution, e.g. if they are called from
   --  inside callbacks or conditional branches.

   function Report return Natural;
   --  Report should be called the following way at the end of a test
   --  program main function:
   --
   --      return Report;
   --
   --  Testsuite driver will consider a test to PASS if all the
   --  following conditions are met:
   --
   --  * test program exit with status 0
   --  * all assert calls did succeed
   --  * test program display the message "<=== TEST PASSED ===>"
end Test_Assert;
