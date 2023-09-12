--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Text_IO;

with GPR2.KB;

package body GPR2.Version is

   use Ada.Text_IO;

   pragma Warnings (Off, "*this code can never be executed*");
   --  Kill warning for never executed code, the actual Build_Type is
   --  patched for the releases and so all branches are potentially used.

   ----------------------
   -- Copyright_Holder --
   ----------------------

   function Copyright_Holder return String is
   begin
      return "AdaCore";
   end Copyright_Holder;

   -------------
   -- Display --
   -------------

   procedure Display
     (Tool_Name      : String;
      Initial_Year   : String;
      Version_String : String) is
   begin
      Put_Line (Tool_Name & " " & Version_String);
      Put_Line
        ("Copyright (C) " & Initial_Year & '-' & Current_Year & ", "
         & Copyright_Holder);
   end Display;

   ---------------------------
   -- Display_Free_Software --
   ---------------------------

   procedure Display_Free_Software is
   begin
      Put_Line (Free_Software);
      New_Line;
   end Display_Free_Software;

   -------------------
   -- Free_Software --
   -------------------

   function Free_Software return String is
   begin
      case Build_Type is
         when GPL | FSF =>
            return
              "This is free software; see the source for copying conditions."
              & ASCII.LF
              & "There is NO warranty; not even for MERCHANTABILITY"
              & " or FITNESS FOR A PARTICULAR PURPOSE.";

         when Gnatpro =>
            return
              "This is free software; see the source for copying conditions."
              & ASCII.LF
              & "See your AdaCore support agreement for details of warranty"
              & " and support."
              & ASCII.LF
              & "If you do not have a current support agreement, then there"
              & " is absolutely"
              & ASCII.LF
              & "no warranty; not even for MERCHANTABILITY or FITNESS FOR"
              & " A PARTICULAR"
              & ASCII.LF
              & "PURPOSE.";
      end case;
   end Free_Software;

   ----------------
   -- Long_Value --
   ----------------

   function Long_Value (Host : Boolean := True) return String is
      Hostname       : constant String :=
                         " (" & String (GPR2.KB.Default_Target) & ')';
      Version_String : constant String :=
                         Version.Short_Value & " (" & Date & ")"
                         & (if Host then Hostname else "");
   begin
      case Build_Type is
         when Gnatpro =>
            return "Pro " & Version_String;
         when GPL =>
            return "Community " & Version_String;
         when FSF =>
            return Version_String;
      end case;
   end Long_Value;

end GPR2.Version;
